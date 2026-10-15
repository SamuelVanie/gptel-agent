;;; gptel-agent-review.el --- Live per-file review -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; File tools stage changes immediately.  Each file has one cumulative diff;
;; comments and refusals steer the agent as soon as the user submits them.

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'subr-x)
(require 'gptel-agent-edit)
(require 'tabulated-list)

(declare-function smv/gptel-ediff-open "gptel-ediff-prehook")
(declare-function smv/gptel-ediff-refresh "gptel-ediff-prehook")
(declare-function smv/gptel-ediff-close "gptel-ediff-prehook")
(declare-function smv/gptel-ediff-validate "gptel-ediff-prehook")
(declare-function gptel-agent--run-parent-buffer "gptel-agent-tools")
(declare-function gptel-agent--run-active-p "gptel-agent-tools")
(defvar gptel-agent--supervised-run)
(defvar gptel-agent--result-file-scope)
(defvar gptel-agent-read-file-size-threshold)

(cl-defstruct (gptel-agent-review--queue
               (:constructor gptel-agent-review--make-queue))
  owner buffer files view)

(defvar-local gptel-agent-review--queue nil
  "Review owned by this conversation, or displayed in this review buffer.")
(defvar-local gptel-agent-review--next-id 0
  "Last file review ID in this conversation.")
(defvar-local gptel-agent-review--delivery nil
  "Review input awaiting delivery, as (TEXT REVISE FSM) entries.")
(defvar-local gptel-agent-review--delivery-timer nil
  "Timer for delivering review input after a response finishes.")
(defvar-local gptel-agent-review--tool-feedback nil
  "Review input for an active sub-agent's next tool result.")

(defun gptel-agent-review--owner (&optional buffer)
  "Return the top-level request buffer owning BUFFER's staged changes."
  (let ((owner (or (and buffer (get-buffer buffer)) (current-buffer))) parent)
    (while (and (buffer-live-p owner)
                (setq parent
                      (with-current-buffer owner
                        (when (bound-and-true-p gptel-agent--supervised-run)
                          (gptel-agent--run-parent-buffer gptel-agent--supervised-run))))
                (buffer-live-p parent)
                (not (eq parent owner)))
      (setq owner parent))
    owner))

(defun gptel-agent-review--staged (queue)
  "Return the latest uncancelled snapshots in QUEUE, keyed by canonical path."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (file (gptel-agent-review--queue-files queue))
      (unless (eq (plist-get file :status) 'cancelled)
        (gptel-agent-edit-stage (list file) table)))
    table))

(defun gptel-agent-review--cleanup ()
  "Discard a killed conversation's staged files and pending timers."
  (when (timerp gptel-agent-review--delivery-timer)
    (cancel-timer gptel-agent-review--delivery-timer))
  (when (and gptel-agent-review--queue
             (eq (current-buffer) (gptel-agent-review--queue-owner gptel-agent-review--queue)))
    (gptel-agent-review--close gptel-agent-review--queue)))

(defun gptel-agent-review--close (queue)
  "Close QUEUE and its Ediff view without writing files."
  (smv/gptel-ediff-close queue)
  (when (buffer-live-p (gptel-agent-review--queue-owner queue))
    (with-current-buffer (gptel-agent-review--queue-owner queue)
      (setq gptel-agent-review--queue nil)))
  (when (buffer-live-p (gptel-agent-review--queue-buffer queue))
    (kill-buffer (gptel-agent-review--queue-buffer queue))))

(defun gptel-agent-review-enqueue (name args &optional buffer)
  "Stage NAME with ARGS from BUFFER and update each affected file's diff."
  (let* ((source (or (and buffer (get-buffer buffer)) (current-buffer)))
         (owner (gptel-agent-review--owner source))
         (existing (buffer-local-value 'gptel-agent-review--queue owner))
         (queue (or existing (gptel-agent-review--make-queue :owner owner)))
         (gptel-agent-edit--staged-changes (gptel-agent-review--staged queue))
         (versions (mapcar (lambda (file)
                             (list (plist-get file :path) (plist-get file :revision)
                                   (plist-get file :status)))
                           (gptel-agent-review--queue-files queue)))
         ;; Prepare every operation before changing any existing review entry.
         (changes (gptel-agent-edit-prepare name args)))
    ;; Patch subprocesses may run another tool callback or a review decision.
    ;; Never overwrite a newer revision or silently restore a refused snapshot.
    (unless (eq existing (buffer-local-value 'gptel-agent-review--queue owner))
      (error "File review changed while preparing edits; reread staged files and retry"))
    (dolist (change changes)
      (let* ((path (plist-get change :path))
             (file (cl-find path (gptel-agent-review--queue-files queue)
                            :key (lambda (f) (plist-get f :path)) :test #'equal)))
        (unless (equal (cdr (assoc path versions))
                       (and file (list (plist-get file :revision) (plist-get file :status))))
          (error "Review changed while preparing %s; reread the file and address current feedback before retrying" path))))
    (dolist (change changes)
      (let* ((path (plist-get change :path))
             (file (cl-find path (gptel-agent-review--queue-files queue)
                            :key (lambda (f) (plist-get f :path)) :test #'equal))
             (merged (copy-sequence
                      (if (and file (not (eq (plist-get file :status) 'cancelled))) file change))))
        (setf (plist-get merged :after) (plist-get change :after)
              (plist-get merged :id) (or (plist-get file :id)
                                        (with-current-buffer owner (cl-incf gptel-agent-review--next-id)))
              (plist-get merged :revision) (1+ (or (plist-get file :revision) 0))
              (plist-get merged :status) (if (equal (plist-get merged :before) (plist-get merged :after))
                                            'unchanged 'pending)
              (plist-get merged :hunk) (or (plist-get file :hunk) 0)
              (plist-get merged :sources) (cons source (remq source (plist-get file :sources))))
        (if file
            ;; Preserve the entry's identity for the live Ediff callbacks.
            (progn (setcar file (car merged)) (setcdr file (cdr merged)))
          (setf (gptel-agent-review--queue-files queue)
                (append (gptel-agent-review--queue-files queue) (list merged))))))
    (with-current-buffer owner
      (setq gptel-agent-review--queue queue)
      (add-hook 'kill-buffer-hook #'gptel-agent-review--cleanup nil t))
    (condition-case err
        (progn
          (gptel-agent-review--refresh queue)
          (unless existing
            (display-buffer (gptel-agent-review--queue-buffer queue)
                            '((display-buffer-reuse-window display-buffer-in-side-window)
                              (side . bottom) (window-height . 0.25)))))
      (error (message "Changes staged; reopen review to refresh its display: %s" (error-message-string err))))
    (format (concat "Updated cumulative file review; NOT applied to disk.\n%s\n"
                    "Continue proposing changes. Edit/Insert/Write and Read see staged content. "
                    "Shell commands, Grep and Glob still see disk contents. "
                    "Comments and refusals arrive as user input immediately; address them in subsequent edits. "
                    "Do not claim changes are applied or test them on disk until accepted review is done.")
            (mapconcat (lambda (c) (plist-get c :path)) changes "\n"))))

(defun gptel-agent-review-read (args &optional buffer)
  "Return a tool result for staged Read ARGS in BUFFER, or nil for disk reads."
  (when-let* (((not (bound-and-true-p gptel-agent--result-file-scope)))
              (queue (buffer-local-value 'gptel-agent-review--queue (gptel-agent-review--owner buffer)))
              (path (plist-get args :file_path))
              (change (gethash (file-truename path) (gptel-agent-review--staged queue))))
    (let ((text (plist-get change :after)) (start (plist-get args :start_line)) (end (plist-get args :end_line)))
      (unless text (error "File is deleted in the pending review: %s" path))
      (unless (and (or (null start) (and (integerp start) (> start 0)))
                   (or (null end) (and (integerp end) (>= end (or start 1)))))
        (error "Read requires positive line numbers with end_line >= start_line"))
      (when (and (null start) (null end)
                 (> (string-bytes text) (* 1024 gptel-agent-read-file-size-threshold)))
        (error "Staged file is too large; specify a line range"))
      (list :confirm nil :result
            (with-temp-buffer
              (insert text)
              (goto-char (point-min))
              (forward-line (1- (or start 1)))
              (let ((beg (point)))
                (if end (forward-line (1+ (- end (or start 1)))) (goto-char (point-max)))
                (buffer-substring-no-properties beg (point))))))))

(defvar gptel-agent-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'gptel-agent-review-file)
    (define-key map (kbd "r") #'gptel-agent-review-cancel)
    (define-key map (kbd "H") #'gptel-agent-review-comment)
    (define-key map (kbd "a") #'gptel-agent-review-accept-all)
    (define-key map (kbd "D") #'gptel-agent-review-done)
    (define-key map (kbd "C") #'gptel-agent-review-cancel-all)
    (define-key map (kbd "g") #'gptel-agent-review-refresh)
    map))

(define-derived-mode gptel-agent-review-mode tabulated-list-mode "File Review"
  "Review one evolving diff per file; comments and refusals send immediately."
  (setq tabulated-list-format [("ID" 5 t) ("Revision" 9 t) ("Decision" 11 t) ("File" 0 t)]
        tabulated-list-padding 1)
  (setq-local tabulated-list-use-header-line nil)
  (tabulated-list-init-header)
  (setq-local header-line-format
              " RET: Ediff   H: comment   r: refuse file   a: accept all   D: done   C: cancel all   q: hide"))

(defun gptel-agent-review--refresh (queue)
  "Update QUEUE's file list and any open Ediff without stealing focus."
  (unless (buffer-live-p (gptel-agent-review--queue-buffer queue))
    (setf (gptel-agent-review--queue-buffer queue)
          (generate-new-buffer (format "*File review: %s*" (buffer-name (gptel-agent-review--queue-owner queue)))))
    (with-current-buffer (gptel-agent-review--queue-buffer queue)
      (gptel-agent-review-mode)
      (setq gptel-agent-review--queue queue)))
  (with-current-buffer (gptel-agent-review--queue-buffer queue)
    (setq tabulated-list-entries
          (mapcar (lambda (f)
                    (list (plist-get f :id)
                          (vector (number-to-string (plist-get f :id))
                                  (number-to-string (plist-get f :revision))
                                  (symbol-name (plist-get f :status)) (plist-get f :path))))
                  (gptel-agent-review--queue-files queue)))
    (tabulated-list-print t))
  (smv/gptel-ediff-refresh queue))

;;;###autoload
(defun gptel-agent-review ()
  "Open the current conversation's live file review."
  (interactive)
  (let ((queue (or gptel-agent-review--queue
                   (buffer-local-value 'gptel-agent-review--queue (gptel-agent-review--owner)))))
    (unless queue (user-error "No file changes pending review"))
    (gptel-agent-review--refresh queue)
    (pop-to-buffer (gptel-agent-review--queue-buffer queue))))

(defun gptel-agent-review-refresh ()
  "Refresh the current file review."
  (interactive)
  (gptel-agent-review--refresh gptel-agent-review--queue))

(defun gptel-agent-review--selected ()
  "Return the file at point, or signal a user error."
  (or (cl-find (tabulated-list-get-id) (gptel-agent-review--queue-files gptel-agent-review--queue)
               :key (lambda (f) (plist-get f :id)))
      (user-error "Select a file first")))

(defun gptel-agent-review-file ()
  "Open the selected file's live Ediff, returning immediately."
  (interactive)
  (smv/gptel-ediff-open gptel-agent-review--queue (gptel-agent-review--selected)))

(defun gptel-agent-review--feedback (queue files text)
  "Send user TEXT for FILES in QUEUE to the conversation and active authors."
  (let ((owner (gptel-agent-review--queue-owner queue)) sources)
    (dolist (file files)
      (dolist (source (plist-get file :sources)) (cl-pushnew source sources)))
    (dolist (source sources)
      (when (and (not (eq source owner)) (buffer-live-p source))
        (with-current-buffer source
          (when (and (bound-and-true-p gptel-agent--supervised-run)
                     (gptel-agent--run-active-p gptel-agent--supervised-run))
            ;; Child FSMs use a two-argument callback, so deliver through tool
            ;; results instead of gptel's UI-specific raw steering callback.
            (setq gptel-agent-review--tool-feedback
                  (append gptel-agent-review--tool-feedback (list text)))))))
    (gptel-agent-review--deliver owner text t)))

(defun gptel-agent-review--comment (queue file comment &optional context revision)
  "Submit COMMENT on FILE in QUEUE immediately, with CONTEXT and REVISION."
  (when (eq (plist-get file :status) 'cancelled) (user-error "This file's changes were refused"))
  (unless (string-empty-p (string-trim comment))
    (setf (plist-get file :status) 'feedback)
    (gptel-agent-review--feedback
     queue (list file)
     (format (concat "User review comment on %s (revision %d)%s:\n%s\n"
                     "Revise the staged content to address this feedback and adjust future changes. "
                     "Read sees the latest staged version. Nothing has been applied to disk.\n\n")
             (plist-get file :path) (or revision (plist-get file :revision))
             (or context "") comment))
    (gptel-agent-review--refresh queue)))

(defun gptel-agent-review-comment ()
  "Send a comment on the selected file to the agent immediately."
  (interactive)
  (let* ((queue gptel-agent-review--queue) (file (gptel-agent-review--selected))
         (revision (plist-get file :revision)))
    (gptel-agent-review--comment queue file (read-string "Review comment: ") nil revision)))

(defun gptel-agent-review--cancel (queue file reason)
  "Discard FILE's staged changes in QUEUE and send REASON immediately."
  (when (eq (plist-get file :status) 'cancelled) (user-error "This file's changes were already refused"))
  (when (string-empty-p (string-trim reason)) (setq reason "Refused by user."))
  (setf (plist-get file :status) 'cancelled)
  (gptel-agent-review--feedback
   queue (list file)
   (format (concat "The user refused the changes to %s (revision %d):\n%s\n"
                   "All staged changes for this file were discarded; Read now sees disk content. "
                   "Other files remain staged. Adjust future changes and reread before revising. "
                   "Do not restore the rejected content without addressing the refusal.\n\n")
           (plist-get file :path) (plist-get file :revision) reason))
  (gptel-agent-review--refresh queue))

(defun gptel-agent-review-cancel ()
  "Refuse the selected file and notify the agent immediately."
  (interactive)
  (let ((queue gptel-agent-review--queue) (file (gptel-agent-review--selected)))
    (gptel-agent-review--cancel queue file (read-string "Refusal feedback (optional): "))))

(defun gptel-agent-review-accept-all ()
  "Mark all remaining file revisions accepted, without writing them yet."
  (interactive)
  (smv/gptel-ediff-validate gptel-agent-review--queue)
  (dolist (file (gptel-agent-review--queue-files gptel-agent-review--queue))
    (when (memq (plist-get file :status) '(pending feedback))
      (setf (plist-get file :status) 'accepted)))
  (gptel-agent-review-refresh))

(defun gptel-agent-review-done ()
  "Apply accepted file revisions together and close review."
  (interactive)
  (let* ((queue (or gptel-agent-review--queue (user-error "No file changes pending review")))
         (files (gptel-agent-review--queue-files queue))
         (changes (cl-remove-if-not (lambda (f) (eq (plist-get f :status) 'accepted)) files)))
    (when (cl-some (lambda (f) (memq (plist-get f :status) '(pending feedback))) files)
      (user-error "Review pending revisions with RET, or explicitly accept all with a"))
    (smv/gptel-ediff-validate queue)
    (when changes (gptel-agent-edit-apply changes))
    (gptel-agent-review--close queue)
    (gptel-agent-review--deliver
     (gptel-agent-review--queue-owner queue)
     (concat "File review complete. Accepted file revisions have been applied:\n"
             (mapconcat (lambda (f) (format "%s (revision %d): %s" (plist-get f :path)
                                           (plist-get f :revision) (plist-get f :status))) files "\n")
             "\nRefused changes were not applied.\n\n") nil)))

(defun gptel-agent-review-cancel-all ()
  "Discard all staged files, close review, and notify the agent immediately."
  (interactive)
  (let* ((queue (or gptel-agent-review--queue (user-error "No file changes pending review")))
         (files (gptel-agent-review--queue-files queue)))
    (gptel-agent-review--close queue)
    (gptel-agent-review--feedback
     queue files
     (concat "The user cancelled all staged file changes. Nothing was applied.\n"
             (mapconcat (lambda (f) (plist-get f :path)) files "\n")
             "\nRead now sees disk contents. Adjust ongoing work; do not restore cancelled changes unless the user requests it.\n\n"))))

(defun gptel-agent-review--post-tool (call)
  "Attach immediate review feedback to a sub-agent's next tool CALL result."
  (when gptel-agent-review--tool-feedback
    (prog1 (list :result (concat (gptel--to-string (plist-get call :result)) "\n\n"
                                (mapconcat #'identity gptel-agent-review--tool-feedback "\n\n")))
      (setq gptel-agent-review--tool-feedback nil))))

(defun gptel-agent-review--live-p (fsm)
  "Return non-nil when FSM is still running."
  (and fsm (not (memq (gptel-fsm-state fsm) '(DONE ABRT ERRS)))))

(defun gptel-agent-review--steer (owner fsm)
  "Send OWNER's review input after FSM has appended all its tool results.
Keep review input separate from gptel's interactive steering slot, which
can be replaced or relocated by the user while a response is running."
  (when (buffer-live-p owner)
    (with-current-buffer owner
      (let ((entries (cl-remove-if-not
                      (lambda (entry) (eq (nth 2 entry) fsm))
                      gptel-agent-review--delivery)))
        (when entries
          (let* ((info (gptel-fsm-info fsm))
                 (text (mapconcat #'car entries "\n\n")))
            (gptel--inject-prompt
             (plist-get info :backend) (plist-get info :data)
             (gptel--parse-list (plist-get info :backend) (list text)))
            (setq gptel-agent-review--delivery
                  (cl-set-difference gptel-agent-review--delivery entries :test #'eq))
            (when (eq (plist-get info :reasoning-block) 'in)
              (funcall (plist-get info :callback) '(reasoning . t) info))
            (funcall (plist-get info :callback) (concat "\n" text "\n") info 'raw)))))))

(defun gptel-agent-review--deliver (owner text revise)
  "Deliver TEXT to OWNER once; REVISE requests a follow-up model response."
  (with-current-buffer owner
    (let ((fsm (and (gptel-agent-review--live-p gptel--fsm-last) gptel--fsm-last)))
      (when (and fsm (not (plist-get (gptel-fsm-info fsm) :agent-review-callback)))
        ;; The tool-result callback runs after results enter the request and
        ;; before its next transition.  Appending user input here preserves
        ;; tool-call/result ordering, without depending on a TSTR state.
        (let* ((info (gptel-fsm-info fsm)) (callback (plist-get info :callback)))
          (setf (gptel-fsm-info fsm) (plist-put info :agent-review-callback t))
          (setf (plist-get (gptel-fsm-info fsm) :callback)
                (lambda (response info &rest args)
                  (prog1 (apply callback response info args)
                    (when (eq (car-safe response) 'tool-result)
                      (gptel-agent-review--steer owner fsm)))))))
      (setq gptel-agent-review--delivery
            (append gptel-agent-review--delivery (list (list text revise fsm)))))
    (gptel-agent-review--schedule-delivery)))

(defun gptel-agent-review--schedule-delivery (&rest _)
  "Schedule delivery after gptel finishes its current response handlers."
  (when (and gptel-agent-review--delivery
             (not (timerp gptel-agent-review--delivery-timer)))
    (setq gptel-agent-review--delivery-timer
          (run-at-time 0 nil #'gptel-agent-review--flush (current-buffer)))))

(defun gptel-agent-review--flush (owner)
  "Finish delivering OWNER's outcomes, including when no more tools were called."
  (when (buffer-live-p owner)
    (with-current-buffer owner
      (setq gptel-agent-review--delivery-timer nil)
      (unless (gptel-agent-review--live-p gptel--fsm-last)
        (let (texts revise)
          (dolist (entry gptel-agent-review--delivery)
            (pcase-let ((`(,text ,retry ,_fsm) entry))
              (push text texts)
              (setq revise (or revise retry))))
          (when texts
            (let ((inhibit-read-only t) beg)
              (save-excursion
                (goto-char (point-max))
                (setq beg (point))
                (insert "\n\n" (mapconcat #'identity (nreverse texts) "\n\n") "\n")
                (remove-text-properties beg (point) '(gptel nil)))))
          (setq gptel-agent-review--delivery nil)
          (when (and revise
                     (not (and gptel--fsm-last
                               (memq (gptel-fsm-state gptel--fsm-last) '(ABRT ERRS)))))
            (save-excursion (goto-char (point-max)) (gptel-send))))))))

(add-hook 'gptel-post-response-functions #'gptel-agent-review--schedule-delivery)
(add-hook 'gptel-post-tool-call-functions #'gptel-agent-review--post-tool)

(provide 'gptel-agent-review)
;;; gptel-agent-review.el ends here
