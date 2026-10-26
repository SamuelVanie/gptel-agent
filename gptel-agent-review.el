;;; gptel-agent-review.el --- Live per-file review -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; File tools apply changes with snapshots by default.  Each file has one cumulative diff;
;; comments and refusals steer the agent as soon as the user submits them.

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'subr-x)
(require 'gptel-agent-edit)
(require 'gptel-agent-history)
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

(defcustom gptel-agent-review-apply-immediately t
  "Apply proposals to working files so the agent can test them during review.
Originals and revisions are saved in `gptel-agent-history-directory'.
Saved external changes join the review; refusals restore originals, including
undoing those external changes.  Nil keeps proposals in memory until Done.
Changing this setting affects new review queues only."
  :type 'boolean :group 'gptel)

(cl-defstruct (gptel-agent-review--queue
               (:constructor gptel-agent-review--make-queue))
  owner buffer files view applied)

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
  "Close a killed conversation's review and timers; retain applied files/history."
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

(defun gptel-agent-review--check-owners (changes &optional owner)
  "Reject CHANGES to files under review outside OWNER's conversation."
  (dolist (buffer (buffer-list))
    (when-let* ((queue (buffer-local-value 'gptel-agent-review--queue buffer))
                ((eq buffer (gptel-agent-review--queue-owner queue)))
                ((not (eq buffer owner))))
      (dolist (change changes)
        (when (cl-some (lambda (file)
                         (and (not (eq (plist-get file :status) 'cancelled))
                              (equal (file-truename (plist-get change :path))
                                     (plist-get file :path))))
                       (gptel-agent-review--queue-files queue))
          (user-error "Finish the review in %s before changing %s"
                      (buffer-name buffer) (plist-get change :path)))))))

(add-hook 'gptel-agent-history-before-restore-hook #'gptel-agent-review--check-owners)

(defun gptel-agent-review--reverse (files)
  "Return changes restoring FILES from their latest revision to originals."
  (mapcar (lambda (file)
            (let ((change (copy-sequence file)))
              (setf (plist-get change :before) (plist-get file :after)
                    (plist-get change :after) (plist-get file :before))
              change))
          (cl-remove-if (lambda (file) (eq (plist-get file :status) 'cancelled)) files)))

(defun gptel-agent-review--sync (queue &optional files)
  "Import saved external changes into applied QUEUE, returning changed files.
Check FILES, or all queue entries when nil.  Keep the cumulative originals,
save the external transitions in history, and invalidate earlier acceptance.
Never write working files or discard unsaved visiting-buffer edits."
  (when (gptel-agent-review--queue-applied queue)
    (let (gptel-agent-edit--staged-changes updates refresh)
      ;; Read everything before changing history or any review entry.
      (dolist (file (or files (gptel-agent-review--queue-files queue)))
        (unless (eq (plist-get file :status) 'cancelled)
          (let ((snapshot (gptel-agent-edit--snapshot (plist-get file :path))))
            (when-let* ((buffer (find-buffer-visiting (plist-get file :path))))
              (when (or (not (verify-visited-file-modtime buffer))
                        (not (equal (plist-get snapshot :before) (plist-get file :after))))
                (push (plist-get file :path) refresh)))
            (unless (equal (plist-get snapshot :before) (plist-get file :after))
              (setf (plist-get snapshot :after) (plist-get snapshot :before)
                    (plist-get snapshot :before) (plist-get file :after))
              (push (cons file snapshot) updates)))))
      (when updates
        (gptel-agent-history--complete
         (gptel-agent-history--record (mapcar #'cdr updates)))
        (dolist (update updates)
          (let* ((file (car update)) (snapshot (cdr update))
                 (merged (copy-sequence file)))
            (setf (plist-get merged :after) (plist-get snapshot :after)
                  (plist-get merged :revision) (1+ (plist-get file :revision))
                  (plist-get merged :external) t
                  (plist-get merged :status)
                  (cond ((eq (plist-get file :status) 'feedback) 'feedback)
                        ((equal (plist-get file :before) (plist-get snapshot :after)) 'unchanged)
                        (t 'pending)))
            ;; Adding a plist key may allocate a new head.  Keep the entry's
            ;; identity for both the queue and any open Ediff callbacks.
            (setcar file (car merged))
            (setcdr file (cdr merged)))))
      (when refresh
        (let ((versions (mapcar (lambda (file)
                                  (list file (plist-get file :revision) (plist-get file :status)))
                                (or files (gptel-agent-review--queue-files queue)))))
          (dolist (path refresh) (gptel-agent-edit--refresh-buffer path))
          ;; Revert hooks can run tool callbacks or close/refuse this review.
          (unless (and (eq queue (buffer-local-value 'gptel-agent-review--queue
                                                     (gptel-agent-review--queue-owner queue)))
                       (cl-every (lambda (version)
                                   (equal (cdr version)
                                          (list (plist-get (car version) :revision)
                                                (plist-get (car version) :status))))
                                 versions))
            (error "File review changed while refreshing buffers; reread and retry"))))
      (mapcar #'car updates))))

(defun gptel-agent-review--check-current (queue &optional files)
  "Refresh external changes in QUEUE's FILES before a review decision.
If anything changed, leave the decision to a subsequent command so the user
can inspect the new revision before accepting or restoring it."
  (when (gptel-agent-review--sync queue files)
    (gptel-agent-review--refresh queue)
    (user-error "Files changed on disk; inspect the refreshed review and retry (refusal also undoes external changes)")))

(defun gptel-agent-review-enqueue (name args &optional buffer)
  "Prepare NAME with ARGS from BUFFER and update each affected file's diff."
  (let* ((source (or (and buffer (get-buffer buffer)) (current-buffer)))
         (owner (gptel-agent-review--owner source))
         (existing (buffer-local-value 'gptel-agent-review--queue owner))
         (queue (or existing (gptel-agent-review--make-queue
                              :owner owner :applied gptel-agent-review-apply-immediately)))
         (gptel-agent-edit--staged-changes
          (if (gptel-agent-review--queue-applied queue)
              (make-hash-table :test #'equal)
            (gptel-agent-review--staged queue)))
         (versions (mapcar (lambda (file)
                             (list (plist-get file :path) (plist-get file :revision)
                                   (plist-get file :status)))
                           (gptel-agent-review--queue-files queue)))
         ;; Prepare every operation before changing any existing review entry.
         (changes (gptel-agent-edit-prepare name args)) synced)
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
    (when (gptel-agent-review--queue-applied queue)
      (gptel-agent-review--check-owners changes owner)
      ;; Preparation already uses current disk content.  Import external
      ;; changes before extending the diff, retaining the original baseline.
      ;; Validation still rejects writes racing with preparation or apply.
      (gptel-agent-edit-validate changes)
      (when-let* ((files (cl-remove-if-not
                         (lambda (file) (cl-find (plist-get file :path) changes
                                                 :key (lambda (c) (plist-get c :path)) :test #'equal))
                         (gptel-agent-review--queue-files queue))))
        (setq synced (gptel-agent-review--sync queue files)))
      (condition-case err
          (gptel-agent-edit-apply changes)
        ((error quit)
         (when synced
           (condition-case nil (gptel-agent-review--refresh queue) (error nil)))
         (signal (car err) (cdr err)))))
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
      (error (message "Changes recorded; reopen review to refresh its display: %s" (error-message-string err))))
    (if (gptel-agent-review--queue-applied queue)
        (format (concat "Applied changes to working files; cumulative review remains open.\n%s\n"
                        "Before/after snapshots are saved in %s. "
                        "Bash, tests, builds, Read, Grep and Glob see these changes now. "
                        "Run the relevant tests/build and fix failures before finishing. "
                        "Address incoming comments and refusals; saved external changes join this review. "
                        "Refusing a file restores its pre-review contents, including undoing external changes shown in its diff.")
                (mapconcat (lambda (c) (plist-get c :path)) changes "\n")
                gptel-agent-history-directory)
      (format (concat "Updated cumulative file review; NOT applied to disk.\n%s\n"
                    "Continue proposing changes. Edit/Insert/Write and Read see staged content. "
                    "Shell commands, Grep and Glob still see disk contents. "
                    "Comments and refusals arrive as user input immediately; address them in subsequent edits. "
                    "Do not claim changes are applied or test them on disk until accepted review is done.")
              (mapconcat (lambda (c) (plist-get c :path)) changes "\n")))))

(defun gptel-agent-review-read (args &optional buffer)
  "Return a tool result for staged Read ARGS in BUFFER, or nil for disk reads."
  (when-let* (((not (bound-and-true-p gptel-agent--result-file-scope)))
              (queue (buffer-local-value 'gptel-agent-review--queue (gptel-agent-review--owner buffer)))
              ((gptel-agent-review--queue-applied queue))
              (path (plist-get args :file_path))
              (file (cl-find (file-truename path) (gptel-agent-review--queue-files queue)
                             :key (lambda (f) (plist-get f :path)) :test #'equal)))
    (when (gptel-agent-review--sync queue (list file))
      (gptel-agent-review--refresh queue)))
  (when-let* (((not (bound-and-true-p gptel-agent--result-file-scope)))
              (queue (buffer-local-value 'gptel-agent-review--queue (gptel-agent-review--owner buffer)))
              ((not (gptel-agent-review--queue-applied queue)))
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
                                  (symbol-name (plist-get f :status))
                                  (concat (plist-get f :path)
                                          (when (plist-get f :external) " [external changes]")))))
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
    (gptel-agent-review--sync queue)
    (gptel-agent-review--refresh queue)
    (pop-to-buffer (gptel-agent-review--queue-buffer queue))))

(defun gptel-agent-review-refresh ()
  "Refresh the current file review."
  (interactive)
  (gptel-agent-review--sync gptel-agent-review--queue)
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
                     "Revise the proposed content to address this feedback and adjust future changes. "
                     "%s\n\n")
             (plist-get file :path) (or revision (plist-get file :revision))
             (or context "") comment
             (if (gptel-agent-review--queue-applied queue)
                 "The proposed changes are on disk; rerun relevant tests after revising."
               "Read sees the latest staged version. Nothing has been applied to disk.")))
    (gptel-agent-review--refresh queue)))

(defun gptel-agent-review-comment ()
  "Send a comment on the selected file to the agent immediately."
  (interactive)
  (let* ((queue gptel-agent-review--queue) (file (gptel-agent-review--selected))
         (revision (plist-get file :revision)))
    (gptel-agent-review--comment queue file (read-string "Review comment: ") nil revision)))

(defun gptel-agent-review--cancel (queue file reason)
  "Undo FILE's proposed changes in QUEUE and send REASON immediately."
  (when (eq (plist-get file :status) 'cancelled) (user-error "This file's changes were already refused"))
  (when (string-empty-p (string-trim reason)) (setq reason "Refused by user."))
  (when (gptel-agent-review--queue-applied queue)
    (gptel-agent-review--check-current queue (list file))
    (gptel-agent-edit-apply (gptel-agent-review--reverse (list file))))
  (setf (plist-get file :status) 'cancelled)
  (gptel-agent-review--feedback
   queue (list file)
   (format (concat "The user refused the changes to %s (revision %d):\n%s\n"
                   "%s Read now sees disk content. "
                   "Other files are unchanged by this refusal. Adjust future changes and reread before revising. "
                   "Do not restore the rejected content without addressing the refusal.\n\n")
           (plist-get file :path) (plist-get file :revision) reason
           (if (gptel-agent-review--queue-applied queue)
               "The file's pre-review contents have been restored. Rerun relevant tests after revising."
             "All staged changes for this file were discarded.")))
  (gptel-agent-review--refresh queue))

(defun gptel-agent-review-cancel ()
  "Refuse the selected file and notify the agent immediately."
  (interactive)
  (let ((queue gptel-agent-review--queue) (file (gptel-agent-review--selected)))
    (gptel-agent-review--cancel queue file (read-string "Refusal feedback (optional): "))))

(defun gptel-agent-review-accept-all ()
  "Mark all remaining file revisions accepted, without additional writes."
  (interactive)
  (smv/gptel-ediff-validate gptel-agent-review--queue)
  (dolist (file (gptel-agent-review--queue-files gptel-agent-review--queue))
    (when (memq (plist-get file :status) '(pending feedback))
      (setf (plist-get file :status) 'accepted)))
  (gptel-agent-review-refresh))

(defun gptel-agent-review-done ()
  "Finish accepted revisions and close review without adding a chat prompt."
  (interactive)
  (let* ((queue (or gptel-agent-review--queue (user-error "No file changes pending review")))
         (files (gptel-agent-review--queue-files queue))
         (changes (cl-remove-if-not (lambda (f) (eq (plist-get f :status) 'accepted)) files)))
    (gptel-agent-review--check-current queue)
    (when (cl-some (lambda (f) (memq (plist-get f :status) '(pending feedback))) files)
      (user-error "Review pending revisions with RET, or explicitly accept all with a"))
    (smv/gptel-ediff-validate queue)
    (if (gptel-agent-review--queue-applied queue)
        (gptel-agent-edit-validate (gptel-agent-review--reverse files))
      (when changes (gptel-agent-edit-apply changes)))
    (gptel-agent-review--close queue)))

(defun gptel-agent-review-cancel-all ()
  "Undo all proposed changes, close review, and notify the agent immediately."
  (interactive)
  (let* ((queue (or gptel-agent-review--queue (user-error "No file changes pending review")))
         (files (gptel-agent-review--queue-files queue)))
    (when (gptel-agent-review--queue-applied queue)
      (gptel-agent-review--check-current queue)
      (when-let* ((changes (gptel-agent-review--reverse files)))
        (gptel-agent-edit-apply changes)))
    (gptel-agent-review--close queue)
    (gptel-agent-review--feedback
     queue files
     (concat (if (gptel-agent-review--queue-applied queue)
                 "The user cancelled all file changes. Pre-review contents have been restored.\n"
               "The user cancelled all staged file changes. Nothing was applied.\n")
             (mapconcat (lambda (f) (plist-get f :path)) files "\n")
             "\nRead now sees disk contents. Adjust ongoing work; do not restore cancelled changes unless the user requests it.\n\n"))))

(defun gptel-agent-review--post-tool (call)
  "Sync shell changes and attach pending review feedback to tool CALL's result."
  (let (notice)
    (when (member (plist-get call :name) '("Bash" "Process"))
      (when-let* ((queue (buffer-local-value 'gptel-agent-review--queue (gptel-agent-review--owner))))
        (condition-case err
            (when-let* ((files (gptel-agent-review--sync queue)))
              (gptel-agent-review--refresh queue)
              (setq notice
                    (concat "External changes added to the pending file review (snapshots saved):\n"
                            (mapconcat (lambda (f) (plist-get f :path)) files "\n")
                            "\nReread these files before further edits. Refusing a file also undoes these external changes.")))
          (error (setq notice (concat "File review could not refresh: " (error-message-string err)))))))
    (when (or notice gptel-agent-review--tool-feedback)
      (prog1 (list :result (concat (gptel--to-string (plist-get call :result)) "\n\n"
                                  (mapconcat #'identity
                                             (append (and notice (list notice))
                                                     gptel-agent-review--tool-feedback) "\n\n")))
        (setq gptel-agent-review--tool-feedback nil)))))

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
                      (lambda (entry) (and (cadr entry) (eq (nth 2 entry) fsm)))
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
  "Deliver user feedback TEXT to OWNER once when REVISE is non-nil.
Ignore acceptance-only notices from older callers entirely: they must not
create a draft, schedule delivery, or change an active request's callback."
  (when revise
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
      (gptel-agent-review--schedule-delivery))))

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
            (pcase-let ((`(,text ,retry ,fsm) entry))
              ;; Older versions could have queued acceptance summaries before
              ;; this library was reloaded.  Never turn those into user input.
              (when retry
                (push text texts)
                ;; Do not revive a request aborted after the feedback was queued.
                ;; New explicit feedback after a terminal request has no FSM and
                ;; should still send automatically.
                (setq revise (or revise (and retry
                                             (not (and fsm (memq (gptel-fsm-state fsm)
                                                                 '(ABRT ERRS))))))))))
          (when texts
            (let ((inhibit-read-only t) beg)
              (save-excursion
                (goto-char (point-max))
                (setq beg (point))
                (insert "\n\n" (mapconcat #'identity (nreverse texts) "\n\n") "\n")
                (remove-text-properties beg (point) '(gptel nil)))))
          (setq gptel-agent-review--delivery nil)
          (when revise
            ;; A region selected during review must not replace the feedback
            ;; with that selection when gptel constructs the follow-up request.
            (let ((mark-active nil))
              (save-excursion (goto-char (point-max)) (gptel-send)))))))))

(add-hook 'gptel-post-response-functions #'gptel-agent-review--schedule-delivery)
(add-hook 'gptel-post-tool-call-functions #'gptel-agent-review--post-tool)

(provide 'gptel-agent-review)
;;; gptel-agent-review.el ends here
