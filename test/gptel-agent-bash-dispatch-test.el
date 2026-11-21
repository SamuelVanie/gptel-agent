;;; gptel-agent-bash-dispatch-test.el --- Bash error delivery -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)
(require 'gptel-openai-responses)

(defun gptel-agent-bash-dispatch-test--wait (predicate)
  "Service subprocesses until PREDICATE succeeds, for at most ten seconds."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(cl-defmacro gptel-agent-bash-dispatch-test--with-fsm ((fsm localized) &body body)
  "Run BODY in a disposable session with FSM and optional LOCALIZED Bash."
  (declare (indent 1))
  `(with-temp-buffer
     (let* ((gptel-confirm-tool-calls nil)
            (gptel-agent-use-project-instructions nil)
            (backend (gptel-make-openai-responses
                      "bash-dispatch-test" :key "unused" :host "example.invalid"
                      :models '(test)))
            (,fsm (gptel-make-fsm
                   :table gptel-send--transitions
                   :handlers '((TOOL gptel--handle-tool-use))
                   :info (list :buffer (current-buffer) :backend backend
                               :callback #'ignore :http-status "200"
                               :status "HTTP/2 200"))))
       (setq-local gptel-tools (list (gptel-get-tool "Bash")))
       (when ,localized (gptel-agent--localize-agent-tool ,fsm))
       (setf (plist-get (gptel-fsm-info ,fsm) :tools) gptel-tools)
       (gptel-agent--workers-install ,fsm)
       ,@body)))

(defun gptel-agent-bash-dispatch-test--assert-error (fsm args message)
  "Dispatch ARGS through FSM and assert a recoverable tool error MESSAGE."
  (let ((call (list :name "Bash" :args args)))
    (setf (plist-get (gptel-fsm-info fsm) :tool-use) (list call)
          (plist-get (gptel-fsm-info fsm) :tool-result) nil)
    (gptel--fsm-transition fsm 'TOOL)
    ;; An escaping error previously went to ERRS with the successful HTTP
    ;; status intact, leaving the call unresolved and the model unable to fix it.
    (should (eq (gptel-fsm-state fsm) 'TRET))
    (should-not (plist-get (gptel-fsm-info fsm) :error))
    (should (string-prefix-p "Error: " (plist-get call :result)))
    (should (string-match-p message (plist-get call :result)))
    (should (= (length (plist-get (gptel-fsm-info fsm) :tool-result)) 1))
    (should-not gptel-agent--bash-jobs)
    (should-not gptel-agent--processes)))

(defun gptel-agent-bash-dispatch-test--assert-recovery (fsm)
  "Assert FSM can finish a corrected Bash call after an operation error."
  (let ((call (list :name "Bash" :args
                    (list :command "printf recovered" :query "Read output"))))
    (setf (plist-get (gptel-fsm-info fsm) :tool-use) (list call)
          (plist-get (gptel-fsm-info fsm) :tool-result) nil)
    (gptel--fsm-transition fsm 'TOOL)
    (gptel-agent-bash-dispatch-test--wait (lambda () (plist-get call :result)))
    (should (eq (gptel-fsm-state fsm) 'TRET))
    (should (equal (plist-get call :result) "recovered"))
    (should (= (length (plist-get (gptel-fsm-info fsm) :tool-result)) 1))
    (should-not (plist-get (gptel-fsm-info fsm) :error))
    (should-not gptel-agent--bash-jobs)))

(ert-deftest gptel-agent-bash-invalid-arguments-remain-tool-results ()
  (dolist (localized '(nil t))
    (dolist (case '(((:query "Read output") "command must be a string")
                    ((:command 12 :query "Read output") "command must be a string")
                    ((:command "true") "QUERY must state")
                    ((:command "true" :query " \n\t") "QUERY must state")
                    ((:command "true" :query "Read output" :background "false")
                     "background must be a boolean")))
      (gptel-agent-bash-dispatch-test--with-fsm (fsm localized)
        (gptel-agent-bash-dispatch-test--assert-error fsm (car case) (cadr case))
        (gptel-agent-bash-dispatch-test--assert-recovery fsm)))))

(ert-deftest gptel-agent-bash-launch-failures-remain-tool-results ()
  (dolist (localized '(nil t))
    (dolist (background '(nil t))
      (gptel-agent-bash-dispatch-test--with-fsm (fsm localized)
        (let ((buffers (buffer-list)))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest _) (error "Simulated Bash launch failure"))))
            (gptel-agent-bash-dispatch-test--assert-error
             fsm (list :command "true" :query "Read output" :background background)
             "Simulated Bash launch failure"))
          (should-not
           (cl-find-if
            (lambda (buffer)
              (or (string-prefix-p " *gptel-agent-bash*" (buffer-name buffer))
                  (string-prefix-p " *gptel-agent-process*" (buffer-name buffer))))
            (cl-set-difference (buffer-list) buffers))))
        (gptel-agent-bash-dispatch-test--assert-recovery fsm)))))

(ert-deftest gptel-agent-bash-immediate-callback-errors-are-not-retried ()
  ;; In particular, a successful background launch calls back immediately.
  ;; Its callback must be outside the startup error handler too.
  (dolist (case '((nil "" nil) (nil "Read output" t)
                  (t "Read output" t) (t "Read output" nil)))
    (with-temp-buffer
      (let ((calls 0)
            (make-process-function (symbol-function 'make-process)))
        (cl-letf (((symbol-function 'make-process)
                   (if (nth 2 case)
                       (lambda (&rest _) (error "Simulated launch failure"))
                     make-process-function)))
          (should-error
           (gptel-agent--execute-bash
            (lambda (_result) (cl-incf calls) (error "Callback failed"))
            "read -r answer" (nth 1 case) (car case))
           :type 'error)
          (should (= calls 1)))))))

(ert-deftest gptel-agent-bash-synchronous-foreground-callback-errors-are-not-retried ()
  ;; A remote file handler can service output while make-process is starting.
  ;; A callback error in that stack must not be converted to a second result.
  (let ((calls 0))
    (cl-letf (((symbol-function 'gptel-agent--start-foreground-bash)
               (lambda (callback _command _query _fsm)
                 (funcall callback "finished"))))
      (let ((err (should-error
                  (gptel-agent--execute-bash
                   (lambda (_result) (cl-incf calls) (error "Callback failed"))
                   "true" "Read output"))))
        (should (equal (error-message-string err) "Callback failed")))
      (should (= calls 1)))))

;;; gptel-agent-bash-dispatch-test.el ends here
