;;; gptel-agent-process.el --- Session-owned shell jobs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Background Bash jobs belong to the top-level conversation buffer.  They
;; survive individual requests and sub-agent completion, but not conversation
;; destruction.  Output is a bounded tail with absolute character offsets.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)

(defvar gptel-agent--supervised-run)
(declare-function gptel-agent--run-parent-buffer "gptel-agent-tools")

(defcustom gptel-agent-process-output-limit 200000
  "Maximum characters retained in memory per background shell job.
Older output is discarded, with the lost range reported on the next read.
The value is captured when the job starts."
  :type 'natnum
  :group 'gptel-agent)

(defcustom gptel-agent-process-input-limit 1048576
  "Maximum encoded bytes in a managed send.
Only one send may be pending per job; additional sends are rejected."
  :type 'natnum
  :group 'gptel-agent)

(defvar gptel-agent--process-writing nil
  "Non-nil while a managed pipe write is on the Lisp stack.
Prevent nested pumps: one write's timeout must not unwind another write.")

(defvar gptel-agent--process-counter 0
  "Counter for job IDs, unique across conversations in this Emacs instance.")
(defvar-local gptel-agent--processes nil
  "Background shell jobs owned by this conversation buffer.")
(put 'gptel-agent--processes 'permanent-local t)

(cl-defstruct (gptel-agent--job (:constructor gptel-agent--make-job))
  id process buffer command directory started-at limit (base 0) (cursor 0)
  waiters send input-disabled last-send)

(cl-defstruct (gptel-agent--process-send
               (:constructor gptel-agent--make-process-send))
  job source callback bytes (offset 0) (attempted-end 0) deadline timer
  fsm cleanup running reason cancelled error)

(cl-defstruct (gptel-agent--process-waiter
               (:constructor gptel-agent--make-process-waiter))
  job source callback offset max-chars view timer fsm cleanup)

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
    (concat
     (format "process_id: %s\nstatus: %s%s\ncommand: %S\ndirectory: %S\nstarted_at: %s\n"
             (gptel-agent--job-id job) status
             (if (memq status '(exit signal))
                 (format "\n%s: %d" (if (eq status 'exit) "exit_code" "signal")
                         (process-exit-status process))
               "")
             (gptel-agent--job-command job)
             (gptel-agent--job-directory job)
             (gptel-agent--job-started-at job))
     (when (gptel-agent--job-input-disabled job)
       "input_status: disabled (an interrupted write may retain unsent bytes; start a new job for further input)\n")
     (when (gptel-agent--job-send job) "input_status: pending\n")
     (gptel-agent--job-last-send job))))

(defun gptel-agent--process-dispose (job &optional notify)
  "Kill JOB's live process group and release its output buffer.
Cancel pending waits, or resolve them before removal when NOTIFY is non-nil."
  (dolist (waiter (copy-sequence (gptel-agent--job-waiters job)))
    (gptel-agent--process-finish-wait waiter (and notify "removed")))
  (when-let* ((send (gptel-agent--job-send job)))
    (gptel-agent--process-stop-send send "removed" (not notify)))
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
                 :sentinel (apply-partially #'gptel-agent--process-sentinel job)))
          (unless (processp process)
            (error "This directory does not support asynchronous processes"))
          (setf (gptel-agent--job-process job) process)
          ;; Managed input is UTF-8 (or literal unibyte data).  Use a binary
          ;; transport once, before any output is decoded; changing coding per
          ;; chunk resets decoder state and can corrupt concurrent output.
          (let ((coding (process-coding-system process)))
            (unless (file-remote-p default-directory)
              (set-process-coding-system process (car coding) 'no-conversion)))
          (process-put process 'gptel-agent-job job)
          (with-current-buffer owner
            (push job gptel-agent--processes)
            (add-hook 'kill-buffer-hook #'gptel-agent--process-cleanup nil t))
          process)
      (error
       (gptel-agent--process-dispose job)
       (signal (car err) (cdr err))))))

(defun gptel-agent--process-validate-read (job offset max-chars view)
  "Validate JOB's OFFSET, MAX-CHARS and VIEW without consuming output.
Return the effective character limit."
  (unless (or (null offset) (and (integerp offset) (>= offset 0)))
    (error "offset must be a nonnegative integer"))
  (unless (member view '(nil "unread" "tail"))
    (error "view must be unread or tail"))
  (when (and offset (equal view "tail"))
    (error "offset cannot be combined with view=tail; omit offset (including 0) for tail, or use view=unread to replay from offset"))
  (setq max-chars (or max-chars 12000))
  (unless (and (integerp max-chars) (> max-chars 0) (<= max-chars 20000))
    (error "max_chars must be between 1 and 20000"))
  (unless (buffer-live-p (gptel-agent--job-buffer job))
    (error "Output buffer is no longer available for %s"
           (gptel-agent--job-id job)))
  (when offset
    (let ((total (+ (gptel-agent--job-base job)
                    (buffer-size (gptel-agent--job-buffer job)))))
      (when (> offset total)
        (error "offset exceeds available output (%d characters)" total))))
  max-chars)

(defun gptel-agent--process-read (job offset max-chars &optional view)
  "Read JOB output from OFFSET, returning at most MAX-CHARS characters.
VIEW defaults to unread: nil OFFSET consumes unread output, while an explicit
offset replays without changing the shared cursor.  VIEW tail returns the
latest characters without moving that cursor and cannot be used with OFFSET.
Offsets count decoded characters."
  (setq max-chars (gptel-agent--process-validate-read job offset max-chars view))
  (with-current-buffer (gptel-agent--job-buffer job)
    (let* ((base (gptel-agent--job-base job))
           (total (+ base (buffer-size)))
           (tail (equal view "tail"))
           (requested (if tail (max base (- total max-chars))
                        (or offset (gptel-agent--job-cursor job))))
           (start (max base requested))
           (end (min total (+ start max-chars))))
      (unless (or offset tail)
        (setf (gptel-agent--job-cursor job) end))
      (concat
       (gptel-agent--process-summary job)
       (format "retained_start: %d\noutput_start: %d\nnext_offset: %d\noutput_end: %d\n"
               base start end total)
       (when (if tail (> base 0) (< requested base))
         (format "Output truncated: characters %d..%d were discarded.\n"
                 (if tail 0 requested) base))
       (when (and tail (> start base))
         (format "Tail view omitted retained characters %d..%d; use offset to inspect them if needed.\n"
                 base start))
       (when (< end total) "More output is available; use view=tail for the latest output or next_offset to continue.\n")
       "STDOUT+STDERR:\n"
       (if (= start end)
           "(no new output)"
         (buffer-substring-no-properties
          (+ (point-min) (- start base)) (+ (point-min) (- end base))))))))

(defun gptel-agent--process-finish-wait (waiter reason)
  "Resolve WAITER once with REASON, or cancel silently when REASON is nil."
  (let* ((job (gptel-agent--process-waiter-job waiter))
         (source (gptel-agent--process-waiter-source waiter))
         (fsm (gptel-agent--process-waiter-fsm waiter))
         (cleanup (gptel-agent--process-waiter-cleanup waiter)))
    (when (memq waiter (gptel-agent--job-waiters job))
      ;; Invalidate before calling back: callbacks may remove the job or reenter.
      (setf (gptel-agent--job-waiters job)
            (delq waiter (gptel-agent--job-waiters job)))
      (when-let* ((timer (gptel-agent--process-waiter-timer waiter)))
        (cancel-timer timer)
        (setf (gptel-agent--process-waiter-timer waiter) nil))
      (when fsm
        (setf (plist-get (gptel-fsm-info fsm) :post)
              (remq cleanup (plist-get (gptel-fsm-info fsm) :post))))
      (when (buffer-live-p source)
        (with-current-buffer source
          (remove-hook 'kill-buffer-hook cleanup t)
          (when (and reason (or (null fsm) (eq (gptel-fsm-state fsm) 'TOOL)))
            (let ((result
                   (condition-case err
                       (concat "wait_reason: " reason "\n"
                               (gptel-agent--process-read
                                job (gptel-agent--process-waiter-offset waiter)
                                (gptel-agent--process-waiter-max-chars waiter)
                                (gptel-agent--process-waiter-view waiter)))
                     (error (concat "Error: " (error-message-string err))))))
              (condition-case err
                  (funcall (gptel-agent--process-waiter-callback waiter) result)
                (error (message "Process wait callback failed: %s"
                                (error-message-string err)))))))))))

(defun gptel-agent--process-sentinel (job process _event)
  "Resolve JOB's waits when PROCESS terminates, after its output is drained."
  ;; Never insert sentinel messages into the command's output buffer.
  (unless (process-live-p process)
    (when-let* ((send (gptel-agent--job-send job)))
      (gptel-agent--process-stop-send send "exit"))
    (dolist (waiter (copy-sequence (gptel-agent--job-waiters job)))
      (gptel-agent--process-finish-wait waiter "exit"))))

(defun gptel-agent--process-finish-send (send reason)
  "Resolve SEND once with REASON, after its active write has unwound."
  (let* ((job (gptel-agent--process-send-job send))
         (source (gptel-agent--process-send-source send))
         (fsm (gptel-agent--process-send-fsm send))
         (cleanup (gptel-agent--process-send-cleanup send)))
    (when (eq send (gptel-agent--job-send job))
      ;; Invalidate and release resources before calling out to request code.
      (setf (gptel-agent--job-send job) nil
            (gptel-agent--job-last-send job)
            (format
             (concat "send_reason: %s\ninput_bytes: %d\n"
                     "written_bytes_min: %d\nwritten_bytes_max: %d\n"
                     "input_available: %s\n%s%s"
                     "Counts bound bytes accepted by the pipe; acceptance does not prove consumption.\n")
             reason (length (gptel-agent--process-send-bytes send))
             (gptel-agent--process-send-offset send)
             (gptel-agent--process-send-attempted-end send)
             (if (and (process-live-p (gptel-agent--job-process job))
                      (not (gptel-agent--job-input-disabled job))) "yes" "no")
             (if-let* ((err (gptel-agent--process-send-error send)))
                 (concat "write_error: " err "\n") "")
             (if (equal reason "sent") ""
               (concat "Unattempted input was discarded; accepted bytes cannot be retracted.\n"
                       "Managed actions will not flush any suffix retained by the interrupted write.\n")))
            (gptel-agent--process-send-bytes send) nil)
      (when-let* ((timer (gptel-agent--process-send-timer send)))
        (cancel-timer timer)
        (setf (gptel-agent--process-send-timer send) nil))
      (when fsm
        (setf (plist-get (gptel-fsm-info fsm) :post)
              (remq cleanup (plist-get (gptel-fsm-info fsm) :post))))
      (when (buffer-live-p source)
        (with-current-buffer source
          (remove-hook 'kill-buffer-hook cleanup t)
          (when (and (not (gptel-agent--process-send-cancelled send))
                     (or (null fsm) (eq (gptel-fsm-state fsm) 'TOOL)))
            (condition-case err
                (funcall (gptel-agent--process-send-callback send)
                         (gptel-agent--process-summary job))
              (error (message "Process send callback failed: %s"
                              (error-message-string err))))))))))

(defun gptel-agent--process-stop-send (send reason &optional silently)
  "Stop SEND with REASON, suppressing its callback when SILENTLY is non-nil.
During a pipe write, let the watchdog unwind it before delivering a result."
  (when (eq send (gptel-agent--job-send (gptel-agent--process-send-job send)))
    (setf (gptel-agent--process-send-reason send) reason)
    (when silently (setf (gptel-agent--process-send-cancelled send) t))
    (unless (gptel-agent--process-send-running send)
      (gptel-agent--process-finish-send send reason))))

(defun gptel-agent--process-pump-send (send)
  "Attempt the next bounded byte chunk of SEND.
Never nest pipe writes.  Emacs may service timers inside a blocked write;
a nested pump could otherwise be unwound by the outer pump's watchdog."
  (let* ((job (gptel-agent--process-send-job send))
         (process (gptel-agent--job-process job)))
    (when (eq send (gptel-agent--job-send job))
      (setf (gptel-agent--process-send-timer send) nil)
      (cond
       ;; Defer completion callbacks too: a callback may wait for I/O and must
       ;; not be unwound by another send's active watchdog.
       (gptel-agent--process-writing nil)
       ((not (process-live-p process))
        (gptel-agent--process-stop-send send "exit"))
       ((>= (float-time) (gptel-agent--process-send-deadline send))
        (gptel-agent--process-stop-send send "timeout"))
       (t
        (let* ((gptel-agent--process-writing t)
               (offset (gptel-agent--process-send-offset send))
               (end (min (+ offset 4096)
                         (length (gptel-agent--process-send-bytes send))))
               (tag (make-symbol "process-write-stop"))
               watchdog outcome)
          (setf (gptel-agent--process-send-running send) t
                (gptel-agent--process-send-attempted-end send) end)
          (unwind-protect
              (setq outcome
                    (catch tag
                      (setq watchdog
                            (run-at-time
                             0.01 0.01
                             (lambda ()
                               (cond
                                ((gptel-agent--process-send-reason send)
                                 (throw tag (gptel-agent--process-send-reason send)))
                                ((>= (float-time) (gptel-agent--process-send-deadline send))
                                 (throw tag "timeout"))))))
                      (condition-case err
                          (progn
                            ;; Encode the entire input once, so chunk boundaries
                            ;; cannot split coding state or duplicate a BOM.
                            (process-send-string
                             process (substring (gptel-agent--process-send-bytes send)
                                                offset end))
                            'written)
                        (error
                         (setf (gptel-agent--process-send-error send)
                               (error-message-string err))
                         (or (gptel-agent--process-send-reason send)
                             (unless (process-live-p process) "exit")
                             "error")))))
            (when watchdog (cancel-timer watchdog))
            (setf (gptel-agent--process-send-running send) nil)
            (unless (eq outcome 'written)
              ;; A nonlocal exit from process-send-string leaves an opaque
              ;; internal write queue.  Another send (even empty) could flush
              ;; it.  Do not retry or claim an exact count for this chunk.
              ;; It is NOT flushed by process-output polling or signals.
              ;; Any future EOF action must respect this latch too: Emacs's
              ;; process-send-eof flushes the hidden suffix before closing.
              (setf (gptel-agent--job-input-disabled job) t)
              (unless outcome
                (gptel-agent--process-stop-send send "cancelled" t))))
          (if (eq outcome 'written)
              (progn
                (setf (gptel-agent--process-send-offset send) end)
                (when (or (gptel-agent--process-send-reason send)
                          (= end (length (gptel-agent--process-send-bytes send))))
                  (gptel-agent--process-stop-send
                   send (or (gptel-agent--process-send-reason send) "sent"))))
            (gptel-agent--process-stop-send send outcome)))))
      (when (eq send (gptel-agent--job-send job))
        (setf (gptel-agent--process-send-timer send)
              (run-at-time 0.001 nil #'gptel-agent--process-pump-send send))))))

(defun gptel-agent--process-send-input (callback job input timeout-ms fsm)
  "Queue bounded INPUT for JOB and report its outcome to CALLBACK.
TIMEOUT-MS bounds writes (default 5000, maximum 60000), including time
waiting for another job's active write.  Result delivery can be delayed
until that active write unwinds.
FSM owns cancellation.  Only one send per job may be outstanding."
  (unless (process-live-p (gptel-agent--job-process job))
    (error "Process is no longer running:\n%s" (gptel-agent--process-summary job)))
  ;; POSIX pipes are nonblocking in Emacs; the C EAGAIN wait runs timers.
  ;; Windows subprocess writes do not provide this guarantee.  File handlers
  ;; can implement arbitrary transports, so do not promise bounds for them.
  (when (or (memq system-type '(windows-nt ms-dos))
            (file-remote-p (gptel-agent--job-directory job)))
    (error "Bounded send requires a local POSIX pipe job"))
  (when (gptel-agent--job-input-disabled job)
    (error "Input is disabled after an interrupted write; start a new job for further input"))
  (when (gptel-agent--job-send job)
    (error "A send is already pending for this job; wait for its result"))
  (unless (stringp input) (error "send requires an input string"))
  (unless (and (integerp gptel-agent-process-input-limit)
               (> gptel-agent-process-input-limit 0))
    (error "gptel-agent-process-input-limit must be a positive integer"))
  (setq timeout-ms (or timeout-ms 5000))
  (unless (and (integerp timeout-ms) (<= 0 timeout-ms 60000))
    (error "timeout_ms must be between 0 and 60000"))
  ;; Reject oversized source strings before allocating an encoded copy, too.
  (when (> (string-bytes input) gptel-agent-process-input-limit)
    (error "Input exceeds the %d-byte send limit" gptel-agent-process-input-limit))
  (let* ((bytes (if (multibyte-string-p input)
                    (encode-coding-string input 'utf-8-unix)
                  (substring-no-properties input)))
         (send (gptel-agent--make-process-send
                :job job :source (current-buffer) :callback callback :bytes bytes
                :deadline (+ (float-time) (/ timeout-ms 1000.0)) :fsm fsm))
         (cleanup (lambda (&rest _)
                    (gptel-agent--process-stop-send send "cancelled" t))))
    (when (> (length bytes) gptel-agent-process-input-limit)
      (error "Encoded input exceeds the %d-byte send limit" gptel-agent-process-input-limit))
    (setf (gptel-agent--process-send-cleanup send) cleanup
          (gptel-agent--job-send job) send)
    (add-hook 'kill-buffer-hook cleanup nil t)
    (when fsm
      (setf (plist-get (gptel-fsm-info fsm) :post)
            (cons cleanup (plist-get (gptel-fsm-info fsm) :post))))
    (setf (gptel-agent--process-send-timer send)
          (run-at-time 0.001 nil #'gptel-agent--process-pump-send send)))
  nil)

(defun gptel-agent--process-wait
    (callback job offset max-chars view until timeout-ms fsm)
  "Wait asynchronously for JOB to exit, delivering its output to CALLBACK.
OFFSET, MAX-CHARS and VIEW select output; VIEW defaults to tail, or unread
when OFFSET is supplied.  UNTIL must be exit.  TIMEOUT-MS defaults to 30000
and is bounded to 0..60000; expiry does not stop the job.  FSM, when non-nil,
owns the wait so cancellation cannot revive an obsolete request."
  (setq view (or view (if offset "unread" "tail"))
        timeout-ms (or timeout-ms 30000))
  (unless (member until '(nil "exit")) (error "until must be exit"))
  (unless (and (integerp timeout-ms) (<= 0 timeout-ms 60000))
    (error "timeout_ms must be between 0 and 60000"))
  (setq max-chars (gptel-agent--process-validate-read job offset max-chars view))
  (let* ((waiter (gptel-agent--make-process-waiter
                  :job job :source (current-buffer) :callback callback
                  :offset offset :max-chars max-chars :view view :fsm fsm))
         (cleanup (lambda (&rest _) (gptel-agent--process-finish-wait waiter nil))))
    (setf (gptel-agent--process-waiter-cleanup waiter) cleanup)
    (push waiter (gptel-agent--job-waiters job))
    (add-hook 'kill-buffer-hook cleanup nil t)
    (when fsm
      (setf (plist-get (gptel-fsm-info fsm) :post)
            (cons cleanup (plist-get (gptel-fsm-info fsm) :post))))
    (cond
     ((not (process-live-p (gptel-agent--job-process job)))
      (gptel-agent--process-finish-wait waiter "exit"))
     ((zerop timeout-ms) (gptel-agent--process-finish-wait waiter "timeout"))
     (t
      (setf (gptel-agent--process-waiter-timer waiter)
            (run-at-time
             (/ timeout-ms 1000.0) nil
             (lambda ()
               (gptel-agent--process-finish-wait
                waiter (if (process-live-p (gptel-agent--job-process job))
                           "timeout" "exit"))))))))
  ;; Async tools must return nil while waiting for their callback.
  nil)

(defun gptel-agent--process-tool
    (callback action &optional process-id input offset max-chars view until timeout-ms fsm)
  "Dispatch Process ACTION and deliver its result to CALLBACK.
PROCESS-ID identifies a job.  INPUT is used by send.  OFFSET, MAX-CHARS and
VIEW select read/wait output; UNTIL controls wait; TIMEOUT-MS bounds wait/send.
Optional FSM comes from the request-local dispatcher for cancellation cleanup.
Report operation errors through CALLBACK so the model can correct its call."
  ;; Some gptel versions only catch errors from synchronous tools.  Process is
  ;; asynchronous even for read/list, so validation must not escape into the
  ;; request error handler (which may still carry a successful HTTP status).
  (let ((result
         (condition-case err
             (if (member action '("wait" "send"))
                 (let ((job (with-current-buffer (gptel-agent--process-owner)
                              (cl-find process-id gptel-agent--processes
                                       :key #'gptel-agent--job-id :test #'equal))))
                   (unless job
                     (error "Unknown process_id in this session: %s" process-id))
                   (if (equal action "send")
                       (gptel-agent--process-send-input callback job input timeout-ms fsm)
                     (gptel-agent--process-wait
                      callback job offset max-chars view until timeout-ms fsm)))
               (gptel-agent--manage-process
                action process-id input offset max-chars view))
           (error (concat "Error: " (error-message-string err))))))
    ;; Wait/send deliver their own results and return nil.  Keep immediate callbacks
    ;; outside the error handler: a downstream failure must not deliver twice.
    (when result (funcall callback result)))
  nil)

(defun gptel-agent--manage-process (action &optional process-id input offset max-chars view)
  "Perform ACTION on PROCESS-ID in this conversation only.
INPUT is queued for `send'; OFFSET, MAX-CHARS and VIEW control `read'.
Use `gptel-agent--process-tool' for wait/send completion callbacks."
  (if (equal action "send")
      (let ((job (with-current-buffer (gptel-agent--process-owner)
                   (cl-find process-id gptel-agent--processes
                            :key #'gptel-agent--job-id :test #'equal))))
        (unless job (error "Unknown process_id in this session: %s" process-id))
        (gptel-agent--process-send-input #'ignore job input nil nil)
        (concat "Input queued; this is not a write confirmation.\n"
                (gptel-agent--process-summary job)))
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
            ("read" (gptel-agent--process-read job offset max-chars view))
            ("remove"
             (when (process-live-p process)
               (error "Process is still running; kill it before removing it"))
             (gptel-agent--process-dispose job t)
             (setq gptel-agent--processes (delq job gptel-agent--processes))
             (format "Removed %s and its retained output." process-id))
            ((or "interrupt" "kill")
             (unless (process-live-p process)
               (error "Process is no longer running:\n%s"
                      (gptel-agent--process-summary job)))
             (pcase action
               ("interrupt" (interrupt-process process))
               ("kill" (kill-process process)))
             (concat (format "Action %s sent.\n" action)
                     (gptel-agent--process-summary job)))
            (_ (error "Unknown Process action: %s" action))))))))

(provide 'gptel-agent-process)
;;; gptel-agent-process.el ends here
