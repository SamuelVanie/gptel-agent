;;; gptel-agent-process.el --- Session-owned shell jobs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Background Bash jobs belong to the top-level conversation buffer.  They
;; survive individual requests and sub-agent completion, but not conversation
;; destruction.  Output is a bounded tail with absolute character offsets.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar gptel-agent--supervised-run)
(declare-function gptel-agent--run-parent-buffer "gptel-agent-tools")

(defcustom gptel-agent-process-output-limit 200000
  "Maximum characters retained in memory per background shell job.
Older output is discarded, with the lost range reported on the next read.
The value is captured when the job starts."
  :type 'natnum
  :group 'gptel-agent)

(defvar gptel-agent--process-counter 0
  "Counter for job IDs, unique across conversations in this Emacs instance.")
(defvar-local gptel-agent--processes nil
  "Background shell jobs owned by this conversation buffer.")
(put 'gptel-agent--processes 'permanent-local t)

(cl-defstruct (gptel-agent--job (:constructor gptel-agent--make-job))
  id process buffer command directory started-at limit (base 0) (cursor 0))

(defun gptel-agent--process-owner ()
  "Return the live top-level conversation owning the current tool call."
  (let ((owner (current-buffer)) seen parent)
    (while (setq parent
                 (with-current-buffer owner
                   (when (bound-and-true-p gptel-agent--supervised-run)
                     (gptel-agent--run-parent-buffer
                      gptel-agent--supervised-run))))
      (unless (and (buffer-live-p parent) (not (memq parent seen)))
        (error "The owning conversation is unavailable"))
      (push owner seen)
      (setq owner parent))
    owner))

(defun gptel-agent--process-filter (job _process output)
  "Append OUTPUT to JOB, retaining only its configured tail."
  (when (buffer-live-p (gptel-agent--job-buffer job))
    (with-current-buffer (gptel-agent--job-buffer job)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert output)
        (let ((excess (- (buffer-size) (gptel-agent--job-limit job))))
          (when (> excess 0)
            (delete-region (point-min) (+ (point-min) excess))
            (cl-incf (gptel-agent--job-base job) excess)))))))

(defun gptel-agent--process-summary (job)
  "Return JOB's identity, status and execution context."
  (let* ((process (gptel-agent--job-process job))
         (status (process-status process)))
    (format "process_id: %s\nstatus: %s%s\ncommand: %S\ndirectory: %S\nstarted_at: %s\n"
            (gptel-agent--job-id job) status
            (if (memq status '(exit signal))
                (format "\n%s: %d" (if (eq status 'exit) "exit_code" "signal")
                        (process-exit-status process))
              "")
            (gptel-agent--job-command job)
            (gptel-agent--job-directory job)
            (gptel-agent--job-started-at job))))

(defun gptel-agent--process-dispose (job)
  "Kill JOB's live process group and release its output buffer."
  (let ((process (gptel-agent--job-process job)))
    (when (processp process)
      ;; Unlike signalling a raw PID, this targets the subprocess group,
      ;; including ordinary server children of npm/shell wrappers.
      (when (process-live-p process)
        (kill-process process))
      (delete-process process))
    (when (buffer-live-p (gptel-agent--job-buffer job))
      (kill-buffer (gptel-agent--job-buffer job)))))

(defun gptel-agent--process-cleanup ()
  "Dispose of all jobs owned by the current conversation."
  (dolist (job gptel-agent--processes)
    (condition-case err
        (gptel-agent--process-dispose job)
      (error (message "Could not clean up %s: %s"
                      (gptel-agent--job-id job) (error-message-string err)))))
  (setq gptel-agent--processes nil))
(put 'gptel-agent--process-cleanup 'permanent-local-hook t)

(defun gptel-agent--start-background-process (command)
  "Start COMMAND as a managed job and return its process object.
Use `gptel-agent--manage-process' to read output or control the job."
  (unless (and (integerp gptel-agent-process-output-limit)
               (> gptel-agent-process-output-limit 0))
    (error "gptel-agent-process-output-limit must be a positive integer"))
  (let* ((owner (gptel-agent--process-owner))
         (job (gptel-agent--make-job
               :id (format "bash-%d" (cl-incf gptel-agent--process-counter))
               :command command :directory default-directory
               :started-at (format-time-string "%FT%T%z")
               :limit gptel-agent-process-output-limit
               :buffer (generate-new-buffer " *gptel-agent-process*")))
         process)
    (with-current-buffer (gptel-agent--job-buffer job)
      (buffer-disable-undo))
    (condition-case err
        (progn
          (setq process
                (make-process
                 :name (gptel-agent--job-id job)
                 :buffer (gptel-agent--job-buffer job)
                 :command (list "bash" "-c" command)
                 :connection-type 'pipe :file-handler t :noquery t
                 :filter (apply-partially #'gptel-agent--process-filter job)
                 ;; Do not mix sentinel messages into command output.
                 :sentinel #'ignore))
          (unless (processp process)
            (error "This directory does not support asynchronous processes"))
          (setf (gptel-agent--job-process job) process)
          (process-put process 'gptel-agent-job job)
          (with-current-buffer owner
            (push job gptel-agent--processes)
            (add-hook 'kill-buffer-hook #'gptel-agent--process-cleanup nil t))
          process)
      (error
       (gptel-agent--process-dispose job)
       (signal (car err) (cdr err))))))

(defun gptel-agent--process-read (job offset max-chars)
  "Read JOB output from OFFSET, returning at most MAX-CHARS characters.
Nil OFFSET consumes unread output.  Explicit offsets allow replay without
changing the shared unread cursor.  Offsets count decoded characters."
  (unless (or (null offset) (and (integerp offset) (>= offset 0)))
    (error "offset must be a nonnegative integer"))
  (setq max-chars (or max-chars 12000))
  (unless (and (integerp max-chars) (> max-chars 0) (<= max-chars 20000))
    (error "max_chars must be between 1 and 20000"))
  (unless (buffer-live-p (gptel-agent--job-buffer job))
    (error "Output buffer is no longer available for %s"
           (gptel-agent--job-id job)))
  (with-current-buffer (gptel-agent--job-buffer job)
    (let* ((base (gptel-agent--job-base job))
           (total (+ base (buffer-size)))
           (requested (or offset (gptel-agent--job-cursor job)))
           (start (max base requested))
           (end (min total (+ start max-chars))))
      (when (> requested total)
        (error "offset exceeds available output (%d characters)" total))
      (unless offset
        (setf (gptel-agent--job-cursor job) end))
      (concat
       (gptel-agent--process-summary job)
       (format "output_start: %d\nnext_offset: %d\noutput_end: %d\n" start end total)
       (when (< requested base)
         (format "Output truncated: characters %d..%d were discarded.\n"
                 requested base))
       (when (< end total) "More output is available; read again.\n")
       "STDOUT+STDERR:\n"
       (if (= start end)
           "(no new output)"
         (buffer-substring-no-properties
          (+ (point-min) (- start base)) (+ (point-min) (- end base))))))))

(defun gptel-agent--manage-process (action &optional process-id input offset max-chars)
  "Perform ACTION on PROCESS-ID in this conversation only.
INPUT is sent verbatim for `send'; OFFSET and MAX-CHARS control `read'."
  (with-current-buffer (gptel-agent--process-owner)
    (if (equal action "list")
        (if gptel-agent--processes
            (mapconcat #'gptel-agent--process-summary
                       (reverse gptel-agent--processes) "\n")
          "No background processes in this session.")
      (let* ((job (cl-find process-id gptel-agent--processes
                           :key #'gptel-agent--job-id :test #'equal))
             (process (and job (gptel-agent--job-process job))))
        (unless job
          (error "Unknown process_id in this session: %s" process-id))
        (pcase action
          ("read" (gptel-agent--process-read job offset max-chars))
          ("remove"
           (when (process-live-p process)
             (error "Process is still running; kill it before removing it"))
           (gptel-agent--process-dispose job)
           (setq gptel-agent--processes (delq job gptel-agent--processes))
           (format "Removed %s and its retained output." process-id))
          ((or "send" "interrupt" "kill")
           (unless (process-live-p process)
             (error "Process is no longer running:\n%s"
                    (gptel-agent--process-summary job)))
           (pcase action
             ("send"
              (unless (stringp input) (error "send requires an input string"))
              (process-send-string process input))
             ("interrupt" (interrupt-process process))
             ("kill" (kill-process process)))
           (concat (format "Action %s sent.\n" action)
                   (gptel-agent--process-summary job)))
          (_ (error "Unknown Process action: %s" action)))))))

(provide 'gptel-agent-process)
;;; gptel-agent-process.el ends here
