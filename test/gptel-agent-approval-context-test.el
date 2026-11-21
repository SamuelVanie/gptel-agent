;;; gptel-agent-approval-context-test.el --- Approval execution context -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)
(require 'gptel-openai-responses)

(defun gptel-agent-approval-test--wait (predicate)
  "Service subprocesses until PREDICATE succeeds, for at most five seconds."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(cl-defmacro gptel-agent-approval-test--with-sessions
    ((parent child directory) &body body)
  "Run BODY with disposable PARENT, supervised CHILD, and DIRECTORY."
  (declare (indent 1))
  `(let ((,parent (generate-new-buffer " *approval-parent*"))
         (,child (generate-new-buffer " *approval-child*"))
         (,directory (file-truename (make-temp-file "gptel-approval-" t)))
         (gptel-agent-use-project-instructions nil))
     (unwind-protect
         (save-window-excursion
           (with-current-buffer ,parent
             (setq default-directory (expand-file-name "parent/" ,directory))
             (make-directory default-directory))
           (with-current-buffer ,child
             (setq default-directory (expand-file-name "child/" ,directory))
             (make-directory default-directory)
             (setq-local gptel-agent--supervised-run
                         (gptel-agent--make-run :parent-buffer ,parent
                                               :child-buffer ,child)))
           ,@body)
       (when-let* ((inspection (get-buffer "*gptel-tool-calls*")))
         (kill-buffer inspection))
       (when (buffer-live-p ,child) (kill-buffer ,child))
       (when (buffer-live-p ,parent) (kill-buffer ,parent))
       (delete-directory ,directory t))))

(defun gptel-agent-approval-test--request (source name args &optional confirm)
  "Create a real pending tool call for NAME and ARGS owned by SOURCE.
CONFIRM overrides the default `auto' confirmation policy."
  (with-current-buffer source
    (let* ((backend (gptel-make-openai-responses
                     "approval-test" :key "unused" :host "example.invalid"
                     :models '(test)))
           (call (list :id "approval-call" :name name :args args))
           pending
           (fsm (gptel-make-fsm
                 :table gptel-send--transitions
                 :handlers '((TOOL gptel--handle-tool-use))
                 :info (list :buffer source :backend backend
                             :callback
                             (lambda (response &rest _)
                               (when (eq (car-safe response) 'tool-call)
                                 (setq pending (cdr response))))
                             :tool-use (list call)
                             :data (list :input
                                         (vector (list :type "function_call"
                                                       :call_id "approval-call"
                                                       :name name
                                                       :arguments (gptel--json-encode args))))))))
      (setq-local gptel-confirm-tool-calls (or confirm 'auto)
                  gptel-tools (list (gptel-get-tool name)))
      (gptel-agent--localize-agent-tool fsm)
      (setf (plist-get (gptel-fsm-info fsm) :tools) gptel-tools)
      (gptel-agent-policy-request-capture fsm)
      (gptel--fsm-transition fsm 'TOOL)
      (should pending)
      (should (eq (gptel-fsm-state fsm) 'TOOL))
      (should-not (plist-get call :result))
      (list :fsm fsm :call call :pending pending))))

(defun gptel-agent-approval-test--display (request ui-buffer)
  "Display REQUEST's native approval in UI-BUFFER and return its visible text."
  (with-current-buffer ui-buffer
    (let ((inhibit-read-only t)
          (info (gptel-fsm-info (plist-get request :fsm))))
      (remove-overlays)
      (erase-buffer)
      (insert "Approval\n")
      (setf (plist-get info :position) (copy-marker (point-max)))
      (gptel--display-tool-calls (plist-get request :pending) info)
      (plist-put request :overlay
                 (cl-find-if (lambda (ov) (overlay-get ov 'gptel-tool))
                             (overlays-in (point-min) (point-max))))
      (should (plist-get request :overlay))
      (buffer-string))))

(defun gptel-agent-approval-test--confirm (request ui-buffer inspect &optional edited-args)
  "Confirm REQUEST from UI-BUFFER, using Inspect/Confirm when INSPECT is set.
When EDITED-ARGS is supplied, edit the actual inspection buffer before Confirm."
  (with-current-buffer ui-buffer
    (if (not inspect)
        (gptel--accept-tool-calls (plist-get request :pending) (plist-get request :overlay))
      (gptel--inspect-tool-calls
       (plist-get request :pending)
       (gptel-fsm-info (plist-get request :fsm)) (plist-get request :overlay))
      (let* ((inspection (get-buffer "*gptel-tool-calls*"))
             (window (get-buffer-window inspection)))
        (should (window-live-p window))
        (with-selected-window window
          (when edited-args
            (let ((inhibit-read-only t)
                  (ov (cl-find-if (lambda (o) (overlay-get o 'gptel-overlay))
                                  (overlays-in (point-min) (point-max)))))
              (goto-char (overlay-start ov))
              (delete-region (overlay-start ov) (overlay-end ov))
              (prin1 (list :name (plist-get (plist-get request :call) :name)
                           :args edited-args) (current-buffer))))
          (gptel--inspect-accept-tool-calls))
        ;; This is the real UI path: Confirm destroys the temporary buffer.
        (should-not (buffer-live-p inspection))))))

(defun gptel-agent-approval-test--result (request)
  "Return REQUEST's result, asserting it completed exactly once."
  (let ((fsm (plist-get request :fsm)) (call (plist-get request :call)))
    (gptel-agent-approval-test--wait (lambda () (plist-get call :result)))
    (should (eq (gptel-fsm-state fsm) 'TRET))
    (should (= (length (plist-get (gptel-fsm-info fsm) :tool-result)) 1))
    (should-not (plist-get (gptel-fsm-info fsm) :error))
    (plist-get call :result)))

(ert-deftest gptel-agent-approval-bash-runs-relative-mutations-in-child ()
  (dolist (policy '(nil allow-bash))
    (let ((gptel-agent-policy--definitions (make-hash-table)))
      (gptel-agent-define-tool-policy 'allow-bash :allow '((:tool "Bash")))
      (dolist (inspect '(nil t))
        (gptel-agent-approval-test--with-sessions (parent child directory)
          (with-current-buffer child (setq-local gptel-agent-tool-policy policy))
          (let ((request (gptel-agent-approval-test--request
                          child "Bash" '(:command "pwd; printf child > marker"
                                         :query "Report the working directory"))))
            (let ((preview (gptel-agent-approval-test--display request parent)))
              (should (string-match-p
                       (regexp-quote (buffer-local-value 'default-directory child)) preview))
              (should-not (string-match-p
                           (regexp-quote (buffer-local-value 'default-directory parent)) preview)))
            (gptel-agent-approval-test--confirm request parent inspect)
            (should (equal (gptel-agent-approval-test--result request)
                           (concat (directory-file-name
                                    (buffer-local-value 'default-directory child))
                                   "\n")))
            (should (file-exists-p (expand-file-name "child/marker" directory)))
            (should-not (file-exists-p (expand-file-name "parent/marker" directory)))))))))

(ert-deftest gptel-agent-approval-foreground-survives-inspection-close ()
  (gptel-agent-approval-test--with-sessions (parent child directory)
    (let* ((gate (expand-file-name "continue" directory))
           (request (gptel-agent-approval-test--request
                     child "Bash"
                     (list :command
                           (format "while [ ! -f %s ]; do sleep 0.01; done; printf completed"
                                   (shell-quote-argument gate))
                           :query "Read completion"))))
      (gptel-agent-approval-test--confirm request parent t)
      (let* ((jobs (buffer-local-value 'gptel-agent--bash-jobs child))
             (job (car jobs)))
        (should (= (length jobs) 1))
        (should (eq (gptel-agent--bash-source job) child))
        (should (process-live-p (gptel-agent--bash-process job)))
        (should-not (buffer-local-value 'gptel-agent--bash-jobs parent))
        (with-temp-file gate)
        (should (equal (gptel-agent-approval-test--result request) "completed"))
        (should-not (buffer-local-value 'gptel-agent--bash-jobs child))
        (should-not (buffer-live-p (gptel-agent--bash-buffer job)))))))

(ert-deftest gptel-agent-approval-background-retains-child-cwd-and-parent-lifetime ()
  (dolist (inspect '(nil t))
    (gptel-agent-approval-test--with-sessions (parent child directory)
      (let ((request (gptel-agent-approval-test--request
                      child "Bash" '(:command "pwd; read -r answer; printf '%s' \"$answer\""
                                     :query "Read replies" :background t))))
        (gptel-agent-approval-test--confirm request parent inspect)
        (let* ((jobs (buffer-local-value 'gptel-agent--processes parent))
               (job (car jobs))
               (process (and job (gptel-agent--job-process job))))
          (should (= (length jobs) 1))
          (should (equal (gptel-agent--job-directory job)
                         (buffer-local-value 'default-directory child)))
          (should (string-match-p (regexp-quote (gptel-agent--job-id job))
                                  (gptel-agent-approval-test--result request)))
          (should-not (buffer-local-value 'gptel-agent--processes child))
          (kill-buffer child)
          (should (process-live-p process))
          (with-current-buffer parent
            (gptel-agent--manage-process "send" (gptel-agent--job-id job) "done\n"))
          (gptel-agent-approval-test--wait (lambda () (not (process-live-p process))))
          (should (equal (with-current-buffer (process-buffer process) (buffer-string))
                         (concat (expand-file-name "child" directory) "\ndone")))
          (kill-buffer parent)
          (should-not (buffer-live-p (gptel-agent--job-buffer job))))))))

(ert-deftest gptel-agent-approval-process-send-reaches-supervised-parent-job ()
  (dolist (policy '(nil allow-process))
    (let ((gptel-agent-policy--definitions (make-hash-table)))
      (gptel-agent-define-tool-policy 'allow-process :allow '((:tool "Process")))
      (dolist (inspect '(nil t))
        (gptel-agent-approval-test--with-sessions (parent child directory)
          (with-current-buffer child (setq-local gptel-agent-tool-policy policy))
          (let* ((process (with-current-buffer parent
                            (gptel-agent--start-background-process
                             "read -r answer; printf '%s' \"$answer\"")))
                 (job (process-get process 'gptel-agent-job))
                 (request (gptel-agent-approval-test--request
                           child "Process"
                           (list :action "send" :process_id (gptel-agent--job-id job)
                                 :input "confirmed\n"))))
            (gptel-agent-approval-test--confirm request parent inspect)
            (should (string-match-p "send_reason: sent\n"
                                    (gptel-agent-approval-test--result request)))
            (gptel-agent-approval-test--wait (lambda () (not (process-live-p process))))
            (should (equal (with-current-buffer (process-buffer process) (buffer-string))
                           "confirmed"))))))))

(ert-deftest gptel-agent-approval-process-cannot-borrow-ui-session-jobs ()
  (dolist (inspect '(nil t))
    (gptel-agent-approval-test--with-sessions (parent child directory)
      (with-current-buffer child (setq gptel-agent--supervised-run nil))
      (let* ((process (with-current-buffer parent
                        (gptel-agent--start-background-process
                         "read -r answer; printf '%s' \"$answer\"")))
             (job (process-get process 'gptel-agent-job))
             (request (gptel-agent-approval-test--request
                       child "Process"
                       (list :action "send" :process_id (gptel-agent--job-id job)
                             :input "must not send\n"))))
        (gptel-agent-approval-test--confirm request parent inspect)
        (should (string-prefix-p "Error: Unknown process_id"
                                 (gptel-agent-approval-test--result request)))
        (should (process-live-p process))
        (should (equal (with-current-buffer (process-buffer process) (buffer-string))
                       ""))))))

(ert-deftest gptel-agent-approval-process-wait-belongs-to-requesting-child ()
  (dolist (inspect '(nil t))
    (dolist (close-child '(nil t))
      (gptel-agent-approval-test--with-sessions (parent child directory)
        (let* ((process (with-current-buffer parent
                          (gptel-agent--start-background-process
                           "read -r answer; printf completed")))
               (job (process-get process 'gptel-agent-job))
               ;; The user can require confirmation for all tools, including wait.
               (request (gptel-agent-approval-test--request
                         child "Process"
                         (list :action "wait" :process_id (gptel-agent--job-id job)
                               :until "exit" :timeout_ms 60000) t)))
          (gptel-agent-approval-test--confirm request parent inspect)
          (should (= (length (gptel-agent--job-waiters job)) 1))
          (let* ((waiter (car (gptel-agent--job-waiters job)))
                 (timer (gptel-agent--process-waiter-timer waiter)))
            (should (eq (gptel-agent--process-waiter-source waiter) child))
            (should-not (plist-get (plist-get request :call) :result))
            (if close-child
                (progn
                  (kill-buffer child)
                  (should-not (gptel-agent--job-waiters job))
                  (should (process-live-p process)))
              (with-current-buffer parent
                (gptel-agent--manage-process "send" (gptel-agent--job-id job) "go\n"))
              (should (string-suffix-p "STDOUT+STDERR:\ncompleted"
                                      (gptel-agent-approval-test--result request))))
            (should-not (gptel-agent--job-waiters job))
            (should-not (memq timer timer-list))
            (when close-child
              (should-not (plist-get (plist-get request :call) :result)))))))))

(ert-deftest gptel-agent-approval-dead-request-owner-delivers-tool-error ()
  (dolist (inspect '(nil t))
    (dolist (spec '(("Bash" (:command "printf must-not-run > marker"
                            :query "Read output"))
                    ("Process" (:action "list"))))
      (gptel-agent-approval-test--with-sessions (parent child directory)
        (let ((request (gptel-agent-approval-test--request
                        child (car spec) (cadr spec) t)))
          (kill-buffer child)
          (gptel-agent-approval-test--confirm request parent inspect)
          (let ((result (gptel-agent-approval-test--result request)))
            (should (string-prefix-p "Error: " result))
            (should (string-match-p "[Bb]uffer\\|[Oo]wner\\|conversation" result)))
          (should-not (file-exists-p (expand-file-name "parent/marker" directory)))
          (should-not (file-exists-p (expand-file-name "child/marker" directory)))
          (should-not (buffer-local-value 'gptel-agent--bash-jobs parent))
          (should-not (buffer-local-value 'gptel-agent--processes parent)))))))

(ert-deftest gptel-agent-approval-directory-drift-refuses-foreground-and-background ()
  (dolist (background '(nil t))
    (dolist (inspect '(nil t))
      (gptel-agent-approval-test--with-sessions (parent child directory)
        (let* ((args (list :command "printf wrong > marker" :query "Read output"
                           :background background))
               (request (gptel-agent-approval-test--request child "Bash" args))
               (original-directory (buffer-local-value 'default-directory child)))
          (gptel-agent-approval-test--display request parent)
          (with-current-buffer child
            (setq default-directory (buffer-local-value 'default-directory parent)))
          ;; Reopening the approval must retain the original context and warn.
          (let ((preview (gptel-agent-approval-test--display request parent)))
            (should (string-match-p (regexp-quote original-directory) preview))
            (should (string-match-p "Working directory changed" preview)))
          ;; Native inspection merges edited args into the original call object.
          (gptel-agent-approval-test--confirm
           request parent inspect
           (when inspect (plist-put (copy-sequence args) :command "printf edited > marker")))
          (should (string-match-p "working directory changed since its approval preview"
                                  (gptel-agent-approval-test--result request)))
          (when inspect
            (should (equal (plist-get (plist-get (plist-get request :call) :args) :command)
                           "printf edited > marker")))
          (should-not (file-exists-p (expand-file-name "parent/marker" directory)))
          (should-not (file-exists-p (expand-file-name "child/marker" directory)))
          (should-not (buffer-local-value 'gptel-agent--bash-jobs child))
          (should-not (buffer-local-value 'gptel-agent--processes parent)))))))

(ert-deftest gptel-agent-approval-fresh-round-uses-new-directory ()
  (gptel-agent-approval-test--with-sessions (parent child directory)
    (let* ((args '(:command "pwd" :query "Report working directory"))
           (request (gptel-agent-approval-test--request child "Bash" args))
           (fsm (plist-get request :fsm))
           (info (gptel-fsm-info fsm)))
      (gptel-agent-approval-test--display request parent)
      (gptel-agent-approval-test--confirm request parent t)
      (should (equal (gptel-agent-approval-test--result request)
                     (concat (expand-file-name "child" directory) "\n")))
      ;; Reuse the localized tool closure and FSM, as successive rounds do.
      (with-current-buffer child
        (setq default-directory (buffer-local-value 'default-directory parent))
        (let ((call (list :name "Bash" :args args)))
          (setf (plist-get request :call) call
                (plist-get request :overlay) nil
                (plist-get info :tool-use) (list call)
                (plist-get info :tool-result) nil
                (plist-get info :callback)
                (lambda (response &rest _)
                  (when (eq (car-safe response) 'tool-call)
                    (setf (plist-get request :pending) (cdr response)))))
          (gptel--fsm-transition fsm 'TOOL)))
      (should (string-match-p
               (regexp-quote (buffer-local-value 'default-directory child))
               (gptel-agent-approval-test--display request parent)))
      (gptel-agent-approval-test--confirm request parent t)
      (should (equal (gptel-agent-approval-test--result request)
                     (concat (expand-file-name "parent" directory) "\n"))))))

(ert-deftest gptel-agent-approval-symlink-retarget-refuses-same-directory-string ()
  (gptel-agent-approval-test--with-sessions (parent child directory)
    (let ((link (expand-file-name "workspace" directory)))
      (make-symbolic-link (expand-file-name "child" directory) link)
      (with-current-buffer child (setq default-directory (file-name-as-directory link)))
      (let ((request (gptel-agent-approval-test--request
                      child "Bash" '(:command "printf wrong > marker" :query "Read output"))))
        (gptel-agent-approval-test--display request parent)
        (delete-file link)
        (make-symbolic-link (expand-file-name "parent" directory) link)
        (gptel-agent-approval-test--confirm request parent t)
        (should (string-match-p "working directory changed since its approval preview"
                                (gptel-agent-approval-test--result request)))
        (should-not (file-exists-p (expand-file-name "child/marker" directory)))
        (should-not (file-exists-p (expand-file-name "parent/marker" directory)))))))

(ert-deftest gptel-agent-approval-dead-owner-preview-reports-unavailable-context ()
  (dolist (preview-before-close '(nil t))
    (gptel-agent-approval-test--with-sessions (parent child directory)
      (let ((request (gptel-agent-approval-test--request
                      child "Bash" '(:command "printf wrong > marker" :query "Read output"))))
        (when preview-before-close (gptel-agent-approval-test--display request parent))
        (kill-buffer child)
        (let ((preview (gptel-agent-approval-test--display request parent)))
          (should (string-match-p "Request buffer is no longer live" preview))
          (unless preview-before-close
            (should (string-match-p "unavailable request buffer" preview)))
          (should-not (string-match-p
                       (regexp-quote (buffer-local-value 'default-directory parent)) preview)))
        (gptel-agent-approval-test--confirm request parent t)
        (should (string-prefix-p "Error: Bash request buffer is no longer live"
                                 (gptel-agent-approval-test--result request)))
        (should-not (file-exists-p (expand-file-name "parent/marker" directory)))))))

(ert-deftest gptel-agent-approval-inspection-edits-remain-policy-checked ()
  (let ((gptel-agent-policy--definitions (make-hash-table)))
    (gptel-agent-define-tool-policy 'exact-command
      :allow '((:tool "Bash" :args (:command "printf allowed"))))
    (gptel-agent-approval-test--with-sessions (parent child directory)
      (with-current-buffer child (setq-local gptel-agent-tool-policy 'exact-command))
      (let ((request (gptel-agent-approval-test--request
                      child "Bash" '(:command "printf allowed" :query "Read output"))))
        (gptel-agent-approval-test--display request parent)
        (gptel-agent-approval-test--confirm
         request parent t '(:command "printf denied > marker" :query "Read output"))
        (should (string-prefix-p "Error: Tool policy denied Bash"
                                 (gptel-agent-approval-test--result request)))
        (should-not (file-exists-p (expand-file-name "child/marker" directory)))
        (should-not (file-exists-p (expand-file-name "parent/marker" directory)))))))

;;; gptel-agent-approval-context-test.el ends here
