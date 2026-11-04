;;; gptel-agent-questions-test.el --- Asynchronous questions -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent-workers-test)

(cl-defmacro gptel-agent-questions-test--with-chat (&body body)
  "Run BODY with real request/tool dispatch, mocking only transport."
  (declare (indent 0))
  `(save-window-excursion
     (let* ((source (generate-new-buffer " *questions-test*"))
            (fsm (gptel-make-fsm :table gptel-send--transitions
                                 :handlers gptel-send--handlers))
            (gptel-post-response-functions nil)
            (gptel-post-request-hook nil)
            (gptel-pre-response-hook nil)
            (gptel-log-level nil)
            requests)
       (unwind-protect
           (cl-letf (((symbol-function 'gptel--update-status) #'ignore)
                     ((symbol-function 'gptel-curl-get-response)
                      (lambda (request) (push request requests)))
                     ((symbol-function 'gptel--url-get-response)
                      (lambda (&rest _) (ert-fail "Unexpected URL transport"))))
             (with-current-buffer source
               (insert "User request\n")
               (setq-local gptel-backend
                           (or gptel-agent-workers-test--backend
                               (gptel-make-openai "questions-test" :key "unused"
                                                  :host "example.invalid" :stream t :models '(test)))
                           gptel-model 'test gptel-stream gptel-agent-workers-test--stream
                           gptel-use-curl t gptel-mode t gptel-use-tools t
                           gptel-confirm-tool-calls nil
                           gptel-pre-tool-call-functions nil gptel-post-tool-call-functions nil
                           gptel-tools (list (gptel-get-tool "AskUserQuestion")
                                             (gptel-make-tool
                                              :name "QuestionTestRead" :include t
                                              :description "Independent work" :args nil
                                              :function (lambda () "Independent work finished"))))
               (gptel-request "User request" :fsm fsm :buffer source :stream gptel-stream
                 :transforms '(gptel-agent--localize-agent-tool)))
             ,@body)
         (when (buffer-live-p source) (kill-buffer source))))))

(defun gptel-agent-questions-test--add (fsm &optional text)
  "Queue one question in FSM and return its overlay."
  (gptel-agent-workers-test--tool
   fsm "AskUserQuestion"
   (list :questions (vector (list :question (or text "Which color?")
                                  :choices [(:value "Blue") (:value "Red")]))))
  (gptel-agent--question-queue-overlay
   (plist-get (gptel-fsm-info fsm) :agent-questions)))

(ert-deftest gptel-agent-questions-add-work-add-wait-submit ()
  (dolist (backend (list (gptel-make-openai "q-openai" :key "unused" :models '(test) :stream t)
                         (gptel-make-openai-responses "q-responses" :key "unused" :models '(test) :stream t)
                         (gptel-make-anthropic "q-anthropic" :key "unused" :models '(test) :stream t)
                         (gptel-make-gemini "q-gemini" :key "unused" :models '(test) :stream t)))
    (dolist (stream '(nil t))
      (let ((gptel-agent-workers-test--backend backend)
            (gptel-agent-workers-test--stream stream))
        (gptel-agent-questions-test--with-chat
          (let* ((window (selected-window))
                 (ov (gptel-agent-questions-test--add fsm))
                 (buffer (overlay-buffer ov)))
            (should (eq window (selected-window)))
            (should-not (eq buffer source))
            (should (eq (gptel-fsm-state fsm) 'WAIT))
            (should (= 2 (length requests)))
            (with-current-buffer buffer
              (goto-char (overlay-start ov))
              (gptel-agent--ask-select-choice 1)
              (gptel-agent--ask-confirm-choice)
              (gptel-agent--ask-submit))
            ;; Saving and attempting submission while collecting do not wake it.
            (should (= 2 (length requests)))
            (should (overlay-buffer ov))
            (gptel-agent-workers-test--tool fsm "QuestionTestRead" nil)
            (should (= 3 (length requests)))
            (should (eq ov (gptel-agent-questions-test--add fsm "Which accent?")))
            (should (= 4 (length requests)))
            (should (equal (overlay-get ov 'gptel-ask--answers) ["Red" nil]))
            (should (equal (overlay-get ov 'gptel-ask--selections) [1 0]))
            (should (= 0 (overlay-get ov 'gptel-ask--index)))
            ;; UI text never enters the streaming conversation or its payload.
            (should-not (with-current-buffer source
                          (string-match-p "AGENT ASKS" (buffer-string))))
            (should-not (string-match-p "AGENT ASKS" (gptel-agent-workers-test--data fsm)))
            (gptel-agent-workers-test--tool fsm "AskUserQuestion" '(:action "wait"))
            (should (eq (gptel-fsm-state fsm) 'TOOL))
            (should (= 4 (length requests)))
            (let ((call (car (plist-get (gptel-fsm-info fsm) :tool-use)))
                  (late-callback (overlay-get ov 'gptel-ask--callback)))
              (with-current-buffer buffer
                (goto-char (overlay-start ov))
                (gptel-agent--ask-submit)
                (should (= 1 (overlay-get ov 'gptel-ask--index)))
                (should-not (plist-get call :result))
                (gptel-agent--ask-confirm-choice)
                (should-not (plist-get call :result))
                (gptel-agent--ask-submit))
              (should (equal (plist-get call :result)
                             "Q1: Which color?\nR1: Red\n\nQ2: Which accent?\nR2: Blue"))
              (should (eq (gptel-fsm-state fsm) 'WAIT))
              (should (= 5 (length requests)))
              (funcall late-callback "Stale answer")
              (should (= 5 (length requests)))
              (should-not (buffer-live-p buffer))
              (should-not (plist-get (gptel-fsm-info fsm) :agent-questions))
              (should-not (buffer-local-value 'gptel-agent--question-queues source)))))))))

(ert-deftest gptel-agent-questions-custom-answer-survives-add-during-minibuffer ()
  (gptel-agent-questions-test--with-chat
    (let ((ov (gptel-agent-questions-test--add fsm)))
      (with-current-buffer (overlay-buffer ov)
        (goto-char (overlay-start ov))
        (gptel-agent--ask-select-choice 2)
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _)
                     (gptel-agent-questions-test--add fsm "Another question")
                     "My custom color")))
          (gptel-agent--ask-confirm-choice)))
      (should (equal (overlay-get ov 'gptel-ask--answers) ["My custom color" nil]))
      (should (equal (overlay-get ov 'gptel-ask--custom-responses) ["My custom color" nil]))
      (should (= 1 (overlay-get ov 'gptel-ask--index))))))

(ert-deftest gptel-agent-questions-cancel-before-or-after-wait ()
  (dolist (waiting '(nil t))
    (dolist (kill-ui '(nil t))
      (gptel-agent-questions-test--with-chat
        (let ((ov (gptel-agent-questions-test--add fsm)))
          (when waiting
            (gptel-agent-workers-test--tool fsm "AskUserQuestion" '(:action "wait")))
          (let ((call (and waiting (car (plist-get (gptel-fsm-info fsm) :tool-use)))))
            (with-current-buffer (overlay-buffer ov)
              (if kill-ui (kill-buffer (current-buffer))
                (goto-char (overlay-start ov))
                (gptel-agent--ask-cancel)))
            (unless waiting
              (should (= 2 (length requests)))
              (gptel-agent-workers-test--tool fsm "AskUserQuestion" '(:action "wait")))
            (when call (should (equal (plist-get call :result) "User cancelled interaction.")))
            (should (= 3 (length requests)))
            (should (string-match-p "User cancelled interaction" (gptel-agent-workers-test--data fsm)))
            (should-not (plist-get (gptel-fsm-info fsm) :agent-questions))
            ;; Subsequent questions form a fresh queue in the same turn.
            (let ((new (gptel-agent-questions-test--add fsm "Fresh question")))
              (should-not (eq new ov))
              (should (= 1 (length (overlay-get new 'gptel-ask--questions)))))))))))

(ert-deftest gptel-agent-questions-terminal-cleanup-rejects-late-answers ()
  (dolist (outcome '(abort error done kill))
    (dolist (waiting '(nil t))
      (gptel-agent-questions-test--with-chat
        (let* ((ov (gptel-agent-questions-test--add fsm))
               (buffer (overlay-buffer ov))
               (callback (overlay-get ov 'gptel-ask--callback)))
          (when waiting
            (gptel-agent-workers-test--tool fsm "AskUserQuestion" '(:action "wait")))
          (pcase outcome
            ('abort (gptel-abort source))
            ('error
             (setf (plist-get (gptel-fsm-info fsm) :error) "test failure")
             (gptel--fsm-transition fsm 'ERRS))
            ('done (gptel--fsm-transition fsm 'DONE))
            ('kill (kill-buffer source)))
          (should-not (buffer-live-p buffer))
          (should-not (plist-get (gptel-fsm-info fsm) :agent-questions))
          (let ((count (length requests)))
            (funcall callback "Late answer")
            (should (= count (length requests)))))))))

(ert-deftest gptel-agent-questions-conversations-and-workers-are-isolated ()
  (gptel-agent-questions-test--with-chat
    (let ((parent source)
          (parent-ov (gptel-agent-questions-test--add fsm)))
      (gptel-agent-questions-test--with-chat
        (let ((other-ov (gptel-agent-questions-test--add fsm)))
          (should-not (eq other-ov parent-ov))
          (should (= 1 (length (buffer-local-value 'gptel-agent--question-queues parent))))))
      (gptel-agent-questions-test--with-chat
        (with-current-buffer source
          (setq-local gptel-agent--supervised-run
                      (gptel-agent--make-run :parent-buffer parent :child-buffer source)))
        (let ((child-ov (gptel-agent-questions-test--add fsm)))
          (should-not (eq child-ov parent-ov))
          (should (= 2 (length (buffer-local-value 'gptel-agent--question-queues parent)))))))))

(ert-deftest gptel-agent-questions-empty-invalid-and-concurrent-operations ()
  (gptel-agent-questions-test--with-chat
    (let ((dispatch (gptel-tool-function
                     (cl-find "AskUserQuestion" (plist-get (gptel-fsm-info fsm) :tools)
                              :key #'gptel-tool-name :test #'equal))) result)
      (funcall dispatch (lambda (value) (setq result value)) nil "wait")
      (should (string-match-p "status: empty" result))
      (dolist (args '((nil "add") ([] "add") ([(:question "")] "add")
                      ([(:question "Bad choices" :choices [(:value 7)])] "add")
                      (nil "unknown")))
        (apply dispatch (lambda (value) (setq result value)) args)
        (should (string-prefix-p "Error:" result))
        (should-not (plist-get (gptel-fsm-info fsm) :agent-questions)))
      (let ((ov (gptel-agent-questions-test--add fsm)))
        (funcall dispatch (lambda (value) (setq result value)) [(:question "Unqueued")] "wait")
        (should (string-prefix-p "Error:" result))
        (gptel-agent-workers-test--tool fsm "AskUserQuestion" '(:action "wait"))
        (dolist (args '((nil "wait") ([(:question "Too late")] "add")))
          (apply dispatch (lambda (value) (setq result value)) args)
          (should (string-prefix-p "Error:" result)))
        (should (eq (gptel-fsm-state fsm) 'TOOL))
        (should (= 1 (length (overlay-get ov 'gptel-ask--questions))))))))

(provide 'gptel-agent-questions-test)
;;; gptel-agent-questions-test.el ends here
