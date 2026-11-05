;;; gptel-agent-review-aside-test.el --- Review aside regressions -*- lexical-binding: t; -*-

(require 'gptel-agent-review-test)
(require 'gptel-agent-workers-test)
(require 'gptel-agent-review-aside)
(require 'gptel-context)

(defmacro gptel-aside-test--with-chat (&rest body)
  "Run BODY with real review/chat handlers, mocking only network transport."
  (declare (indent 0))
  `(gptel-review-test--with-files '(("one" . "old\n"))
     (let ((owner (current-buffer))
           (gptel-default-mode #'text-mode)
           (gptel-post-response-functions nil)
           (gptel-post-request-hook nil)
           (gptel-pre-response-hook nil)
           (gptel-log-level nil)
           requests aside)
       (cl-letf (((symbol-function 'gptel-curl-get-response)
                  (lambda (fsm)
                    (let ((info (gptel-fsm-info fsm)))
                      (setf (gptel-fsm-info fsm)
                            (plist-put info :callback
                                       (or (plist-get info :callback)
                                           (if (plist-get info :stream)
                                               #'gptel-curl--stream-insert-response
                                             #'gptel--insert-response)))))
                    (push fsm requests)))
                 ((symbol-function 'gptel--url-get-response)
                  (lambda (&rest _) (ert-fail "Unexpected URL transport")))
                 ((symbol-function 'display-buffer-in-side-window) (lambda (&rest _) nil)))
         (unwind-protect
             (progn
               (text-mode)
               (setq-local gptel-backend
                           (or gptel-agent-workers-test--backend
                               (gptel-make-openai "aside-test" :key "unused" :models '(test) :stream t))
                           gptel-model 'test gptel-use-curl t
                           gptel-system-prompt "Parent system instructions"
                           gptel-temperature 0.3 gptel-max-tokens 512
                           gptel-stream gptel-agent-workers-test--stream
                           gptel-agent-use-project-instructions nil
                           gptel-agent-preset nil gptel-use-tools t
                           gptel-tools (list (gptel-get-tool "Edit"))
                           gptel-prompt-transform-functions '(gptel-agent-discussion--localize))
               (gptel-mode 1)
               (insert "Initial parent question\n\n"
                       (propertize "Earlier parent answer" 'gptel 'response)
                       "\n\nLatest parent request")
               (gptel-review-test--submit "Edit" '(:mode "replace" :file_path "one" :old_str "old" :new_str "new"))
               ,@body)
           (when gptel-agent-review--queue
             (dolist (entry (gptel-agent-review--queue-asides gptel-agent-review--queue))
               (when (buffer-live-p (cdr entry)) (kill-buffer (cdr entry))))))))))

(defun gptel-aside-test--data (fsm)
  "Return serialized payload of FSM."
  (prin1-to-string (plist-get (gptel-fsm-info fsm) :data)))

(ert-deftest gptel-aside-ediff-question-keeps-review-and-parent-independent ()
  (dolist (applied '(nil t))
    (gptel-aside-test--with-chat
      ;; Exercise both applied and staged queues.
      (when applied
        (gptel-agent-edit-apply (gptel-agent-review--queue-files gptel-agent-review--queue))
        (setf (gptel-agent-review--queue-applied gptel-agent-review--queue) t))
      (gptel-send)
      ;; The author has already reached a tool boundary in a real review.
      (setq-local gptel--fsm-last (car requests))
      (let* ((parent-fsm gptel--fsm-last)
             (parent-text (buffer-string))
             (parent-data (copy-tree (plist-get (gptel-fsm-info parent-fsm) :data) t)))
        (gptel-review-test--open 1)
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Why this change?")))
          (gptel-edit-test--key "A"))
        (setq aside (cdar (gptel-agent-review--queue-asides gptel-agent-review--queue)))
        (let* ((fsm (buffer-local-value 'gptel-agent-review-aside--fsm aside))
               (data (gptel-aside-test--data fsm)))
          (dolist (text '("Initial parent question" "Earlier parent answer" "Latest parent request"
                          "Parent system instructions" "revision 1" "old" "new" "Why this change?"))
            (should (string-match-p (regexp-quote text) data)))
          (should (eq gptel-backend (plist-get (gptel-fsm-info fsm) :backend)))
          (should (eq 'test (plist-get (gptel-fsm-info fsm) :model)))
          (should (= 0.3 (plist-get (plist-get (gptel-fsm-info fsm) :data) :temperature)))
          (should-not (plist-get (gptel-fsm-info fsm) :tools))
          (should-not (plist-get (plist-get (gptel-fsm-info fsm) :data) :tools))
          (gptel-agent-workers-test--answer fsm "ASIDE-ANSWER")
          (should (string-match-p "ASIDE-ANSWER" (with-current-buffer aside (buffer-string)))))
        (should (eq parent-fsm gptel--fsm-last))
        (should (eq 'WAIT (gptel-fsm-state parent-fsm)))
        (should (equal parent-text (buffer-string)))
        (should (equal parent-data (plist-get (gptel-fsm-info parent-fsm) :data)))
        (should-not outcomes)
        (should-not gptel-agent-review--delivery)
        (should (eq 'pending (plist-get (gptel-review-test--file 1) :status)))
        (should (equal (gptel-edit-test--read "one") (if applied "new\n" "old\n")))
        ;; Asking must not require a revision before acceptance.
        (gptel-edit-test--key "q")
        (should (eq 'accepted (plist-get (gptel-review-test--file 1) :status)))))))

(ert-deftest gptel-aside-captures-selected-revision-before-minibuffer-updates ()
  (dolist (side '(original proposed))
    (gptel-aside-test--with-chat
      (gptel-review-test--open 1)
      (let* ((view (gptel-agent-review--queue-view gptel-agent-review--queue))
             (buffer (if (eq side 'original) (smv/gptel-ediff--view-original view)
                       (smv/gptel-ediff--view-proposed view))))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _)
                     (with-current-buffer owner
                       (gptel-review-test--submit "Insert" '(:file_path "one" :line_number -1 :new_str "LATER-REVISION")))
                     "Explain the selection")))
          (with-current-buffer buffer
            (goto-char (point-min))
            (call-interactively (key-binding (kbd "A")))))
        (let ((data (gptel-aside-test--data (car requests))))
          (should (string-match-p "revision 1" data))
          (should (string-match-p (if (eq side 'original) "original file" "proposed changes") data))
          (should (string-match-p "line 1" data))
          (should-not (string-match-p "LATER-REVISION" data)))
        (should (= 2 (plist-get (gptel-review-test--file 1) :revision)))
        (should (eq 'pending (plist-get (gptel-review-test--file 1) :status)))
        (gptel-edit-test--key "Q")))))

(ert-deftest gptel-aside-followups-use-own-history-across-backends ()
  (dolist (backend (list (gptel-make-openai "aside-o" :key "unused" :models '(test) :stream t)
                         (gptel-make-openai-responses "aside-r" :key "unused" :models '(test) :stream t)
                         (gptel-make-anthropic "aside-a" :key "unused" :models '(test) :stream t)
                         (gptel-make-gemini "aside-g" :key "unused" :models '(test) :stream t)))
    (dolist (stream '(nil t))
      (let ((gptel-agent-workers-test--backend backend)
            (gptel-agent-workers-test--stream stream))
        (gptel-aside-test--with-chat
          (setq aside (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) "First aside question"))
          (gptel-agent-workers-test--answer (car requests) "First aside answer" stream)
          ;; Ordinary gptel follow-ups remain in the aside too.
          (with-current-buffer aside
            (goto-char (point-max)) (insert "Follow-up question") (gptel-send))
          (let ((data (gptel-aside-test--data (car requests))))
            (dolist (text '("First aside question" "First aside answer" "Follow-up question"))
              (should (string-match-p text data))))
          (gptel-agent-workers-test--answer (car requests) "Second aside answer" stream)
          (should-not (string-match-p "aside answer" (buffer-string)))
          (should-not outcomes)
          (should (eq aside (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) "")))
          (should (= 2 (length requests))))))))

(ert-deftest gptel-aside-empty-busy-and-killed-buffers ()
  (gptel-aside-test--with-chat
    (gptel-review-test--in-queue
      (gptel-review-test--select 1)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
        (call-interactively (key-binding (kbd "A")))))
    (should-not requests)
    (setq aside (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) "Explain"))
    (should-error (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) "Again") :type 'user-error)
    (should (= 1 (length requests)))
    (let ((aborted nil))
      (cl-letf (((symbol-function 'gptel-abort) (lambda (buffer) (setq aborted buffer))))
        (kill-buffer aside))
      (should (eq aside aborted)))
    (let ((new (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) "")))
      (should (buffer-live-p new))
      (should-not (eq aside new)))
    (should-not outcomes)
    (should (eq 'pending (plist-get (gptel-review-test--file 1) :status)))))

(ert-deftest gptel-aside-inherits-dynamic-system-and-strips-provider-continuation ()
  (gptel-aside-test--with-chat
    (setq-local gptel-system-prompt (lambda () (list (concat "System for " (buffer-name))))
                gptel--request-params '(:previous_response_id "parent-only" :tool_choice "required"))
    (let ((params (copy-tree gptel--request-params)))
      (setq aside (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) "Explain"))
      (should (string-match-p (regexp-quote (concat "System for " (buffer-name owner)))
                              (gptel-aside-test--data (car requests))))
      (should-not (string-match-p "parent-only\\|tool_choice" (gptel-aside-test--data (car requests))))
      (should (equal params gptel--request-params)))))

(ert-deftest gptel-aside-preserves-completed-request-preset-and-project-context ()
  (gptel-aside-test--with-chat
    (with-temp-file "AGENTS.md" (insert "ROOT-INSTRUCTION"))
    (setq-local gptel-agent-use-project-instructions t
                gptel-prompt-transform-functions
                (list (lambda () (setq-local gptel-system-prompt "ONE-SHOT-SYSTEM"
                                              gptel-temperature 0.6))
                      #'gptel-agent-discussion--localize))
    (gptel-send)
    (gptel-agent-workers-test--answer (car requests) "Parent finished")
    (setq aside (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) "Explain"))
    (let ((data (gptel-aside-test--data (car requests))))
      (should (string-match-p "ONE-SHOT-SYSTEM" data))
      (should (string-match-p "ROOT-INSTRUCTION" data)))
    (should (= 0.6 (buffer-local-value 'gptel-temperature aside)))
    (should (equal "Parent system instructions" gptel-system-prompt))))

(ert-deftest gptel-aside-keeps-tool-history-and-attached-context ()
  (gptel-aside-test--with-chat
    (with-temp-file "context.txt" (insert "ATTACHED-CONTEXT"))
    (setq-local gptel-use-context 'user
                gptel-context (list (list (expand-file-name "context.txt"))))
    (insert "\n\n")
    (gptel--parse-list-and-insert
     '((tool :name "Read" :id "read-1" :args (:file_path "one") :result "TOOL-HISTORY")))
    (setq aside (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) "Explain"))
    (let ((data (gptel-aside-test--data (car requests))))
      (should (string-match-p "ATTACHED-CONTEXT" data))
      (should (string-match-p "TOOL-HISTORY" data)))
    (should-not (eq gptel-context (buffer-local-value 'gptel-context aside)))))

(ert-deftest gptel-aside-startup-failure-allows-retry-without-changing-review ()
  (gptel-aside-test--with-chat
    (let ((parent-text (buffer-string)))
      (cl-letf (((symbol-function 'gptel-curl-get-response)
                 (lambda (_) (error "Test transport startup failure"))))
        (should-error (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) "Explain")))
      (setq aside (cdar (gptel-agent-review--queue-asides gptel-agent-review--queue)))
      (with-current-buffer aside (gptel-agent-review-aside-send))
      (gptel-agent-workers-test--answer (car requests) "Retry answer")
      (should (string-match-p "Retry answer" (with-current-buffer aside (buffer-string))))
      (should (equal parent-text (buffer-string)))
      (should-not outcomes)
      (should (eq 'pending (plist-get (gptel-review-test--file 1) :status))))))

(ert-deftest gptel-aside-does-not-submit-an-existing-draft ()
  (gptel-aside-test--with-chat
    (setq aside (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) "Explain"))
    (gptel-agent-workers-test--answer (car requests) "Answer")
    (with-current-buffer aside (goto-char (point-max)) (insert "UNSENT-DRAFT"))
    (should-error (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) "SECOND-QUESTION") :type 'user-error)
    (should (= 1 (length requests)))
    (should-not (string-match-p "SECOND-QUESTION" (with-current-buffer aside (buffer-string))))))

(ert-deftest gptel-aside-abort-stops-only-its-own-request ()
  (gptel-aside-test--with-chat
    (gptel-send)
    (let ((parent-fsm (car requests)) parent-aborted aside-aborted)
      (setq aside (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) "Explain"))
      (let* ((fsm (car requests))
             (gptel--request-alist
              (list (cons 'parent (cons parent-fsm (lambda () (setq parent-aborted t))))
                    (cons 'aside (cons fsm (lambda () (setq aside-aborted t)))))))
        (with-current-buffer aside (call-interactively (key-binding (kbd "C-c C-k"))))
        (should (eq 'ABRT (gptel-fsm-state fsm)))
        (should (eq 'WAIT (gptel-fsm-state parent-fsm)))
        (should aside-aborted)
        (should-not parent-aborted)))))

(ert-deftest gptel-aside-return-preserves-ediff-and-survives-finished-review ()
  (gptel-aside-test--with-chat
    (gptel-review-test--open 1)
    (setq aside (gptel-agent-review-aside gptel-agent-review--queue (gptel-review-test--file 1) ""))
    (cl-letf (((symbol-function 'quit-window) #'ignore))
      (with-current-buffer aside (call-interactively (key-binding (kbd "C-c C-r")))))
    (should (eq 'pending (plist-get (gptel-review-test--file 1) :status)))
    (gptel-edit-test--key "q")
    (gptel-review-test--in-queue (gptel-agent-review-done))
    (should (buffer-live-p aside))
    (with-current-buffer aside
      (should-error (gptel-agent-review-aside-return) :type 'user-error))
    (kill-buffer aside)))

(ert-deftest gptel-aside-side-window-leaves-ediff-visible ()
  (let ((display-side (symbol-function 'display-buffer-in-side-window))
        (display (symbol-function 'display-buffer)))
    (save-window-excursion
      (gptel-aside-test--with-chat
        (cl-letf (((symbol-function 'display-buffer-in-side-window) display-side)
                  ((symbol-function 'display-buffer) display))
          (gptel-review-test--open 1)
          (let* ((queue gptel-agent-review--queue)
                 (view (gptel-agent-review--queue-view queue)))
            (should (get-buffer-window (smv/gptel-ediff--view-proposed view)))
            (setq aside (gptel-agent-review-aside queue (gptel-review-test--file 1) ""))
            (should (eq 'bottom (window-parameter (get-buffer-window aside) 'window-side)))
            (should (get-buffer-window (smv/gptel-ediff--view-original view)))
            (should (get-buffer-window (smv/gptel-ediff--view-proposed view)))
            (with-current-buffer aside (gptel-agent-review-aside-return))
            (should (buffer-live-p aside))
            (should (gptel-agent-review--queue-view queue))
            (gptel-edit-test--key "Q")))))))

(ert-deftest gptel-aside-org-file-snapshots-do-not-override-chat-settings ()
  (require 'gptel-org)
  (gptel-aside-test--with-chat
    (let ((gptel-default-mode #'org-mode)
          (gptel-org-branching-context t)
          (file (copy-sequence (gptel-review-test--file 1))))
      (setf (plist-get file :after)
            "* Sample heading\n:PROPERTIES:\n:GPTEL_SYSTEM: FILE-PROPERTY\n:END:\nExample contents")
      (setq aside (gptel-agent-review-aside gptel-agent-review--queue file "Explain"))
      (let ((data (gptel-aside-test--data (car requests))))
        (dolist (text '("Initial parent question" "Earlier parent answer"
                        "Parent system instructions" "Sample heading" "Example contents"))
          (should (string-match-p text data))))
      (should (equal "Parent system instructions" (buffer-local-value 'gptel-system-prompt aside))))))

(provide 'gptel-agent-review-aside-test)
;;; gptel-agent-review-aside-test.el ends here
