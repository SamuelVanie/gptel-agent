;;; gptel-agent-session-title-test.el --- Session title tests -*- lexical-binding: t; -*-

(require 'gptel-agent-session-test)

(ert-deftest gptel-agent-session-title-context-selects-endpoints ()
  (with-temp-buffer
    (setq-local gptel-prompt-prefix-alist '((fundamental-mode . "User: "))
                gptel-response-prefix-alist '((fundamental-mode . "Assistant: ")))
    (insert "User: Explain session naming\n\nAssistant: "
            (propertize "First answer" 'gptel 'response)
            "\n\nUser: Middle message\n\nAssistant: "
            (propertize "Private reasoning" 'gptel 'reasoning)
            (propertize "Tool output" 'gptel '(tool . "id"))
            (propertize "Ignored text" 'gptel 'ignore)
            (propertize "The session now has a title" 'gptel 'response)
            "\n\nUser: ")
    (should (equal (gptel-agent-session--title-context)
                   (concat "First message (User):\nExplain session naming"
                           "\n\nLast message (Assistant):\nThe session now has a title")))))

(ert-deftest gptel-agent-session-title-context-bounds-large-messages ()
  (with-temp-buffer
    (should-not (gptel-agent-session--title-context))
    (insert (make-string 100000 ?a))
    (let ((context (gptel-agent-session--title-context)))
      (should (< (length context) 4100))
      (should-not (string-match-p "Last message" context)))
    (insert (propertize (make-string 100000 ?b) 'gptel 'response))
    (should (< (length (gptel-agent-session--title-context)) 8100))))

(cl-defmacro gptel-agent-session-title-test--with-request (&body body)
  "Run BODY with a chat and captured asynchronous title requests."
  (declare (indent 0))
  `(gptel-agent-session-test--with-store
     (let ((buffer (gptel-agent-session-test--buffer " *dated-session*" "Explain session names"))
           (gptel-agent-session-auto-title t)
           requests)
       (gptel-agent-session-mode 1)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (prompt &rest args)
                    (push (list :prompt prompt :args args :buffer (current-buffer)
                                :backend gptel-backend :model gptel-model
                                :temperature gptel-temperature :tokens gptel-max-tokens
                                :tools gptel-tools :context gptel-context
                                :use-tools gptel-use-tools :use-context gptel-use-context
                                :transforms gptel-prompt-transform-functions)
                          requests))))
         (with-current-buffer buffer ,@body)))))

(defun gptel-agent-session-title-test--reply (request response)
  "Complete captured REQUEST with RESPONSE."
  (funcall (plist-get (plist-get request :args) :callback) response nil))

(ert-deftest gptel-agent-session-title-first-save-uses-one-message-and-never-retitles ()
  (gptel-agent-session-title-test--with-request
    (gptel-agent-session-save)
    (should (= 1 (length requests)))
    (should (equal "First message (User):\nExplain session names"
                   (plist-get (car requests) :prompt)))
    (goto-char (point-max))
    (insert (propertize "Here is the answer" 'gptel 'response))
    ;; A second save while generation is pending must not replace its request.
    (gptel-agent-session-save)
    (should (= 1 (length requests)))
    (gptel-agent-session-title-test--reply (car requests) "\"Session\n naming explained\"")
    (should (equal "Session naming explained" gptel-agent-session--title))
    (should (equal "*gptel:Session naming explained*" (buffer-name)))
    (should (gethash buffer gptel-agent-session--dirty))
    (should-not (buffer-live-p (plist-get (car requests) :buffer)))
    (should-not gptel-agent-session--title-request)
    (insert "New topic in the conversation")
    (gptel-agent-session-save)
    (should (= 1 (length requests)))
    (should (equal "Session naming explained"
                   (plist-get (gptel-agent-session--read gptel-agent-session--id) :name)))
    (should (equal "*gptel:Session naming explained*" (buffer-name)))))

(ert-deftest gptel-agent-session-title-first-save-uses-first-and-last-messages ()
  (gptel-agent-session-title-test--with-request
    (goto-char (point-max))
    (insert (propertize "First answer" 'gptel 'response)
            "Middle prompt"
            (propertize "Last answer" 'gptel 'response))
    (narrow-to-region (- (point-max) 5) (point-max))
    (gptel-agent-session-save)
    (should (buffer-narrowed-p))
    (should (equal (plist-get (car requests) :prompt)
                   (concat "First message (User):\nExplain session names"
                           "\n\nLast message (Assistant):\nLast answer")))))

(ert-deftest gptel-agent-session-title-preset-accepts-name-and-plist ()
  (gptel-agent-session-title-test--with-request
    (let* ((backend (gptel-make-openai "title-test" :key "unused" :models '(small)))
           (spec '(:backend "title-test" :model small :temperature 0.3 :max-tokens 80))
           (gptel--known-presets (cons (cons 'session-title-test spec) gptel--known-presets))
           (source-backend gptel-backend))
      (setq-local gptel-context '("attached context") gptel-use-context t
                  gptel-max-tokens 4096
                  gptel-use-tools t gptel-prompt-transform-functions '(ignore))
      (dolist (preset (list 'session-title-test spec nil))
        (setq-local gptel-agent-session-title-preset preset)
        (gptel-agent-session-generate-title)
        (let ((request (car requests)))
          (should (eq (if preset backend source-backend) (plist-get request :backend)))
          (should (eq (if preset 'small 'test) (plist-get request :model)))
          (should (= (if preset 80 4096) (plist-get request :tokens)))
          (when preset (should (= 0.3 (plist-get request :temperature))))
          (dolist (key '(:tools :context :use-tools :use-context :transforms))
            (should-not (plist-get request key)))
          (should-not (eq buffer (plist-get request :buffer)))
          (gptel-agent-session-title-test--reply request "Session naming explained")))
      (should (eq source-backend gptel-backend))
      (should (eq 'test gptel-model))
      (should (equal '("attached context") gptel-context))
      (should (equal '(ignore) gptel-prompt-transform-functions)))))

(ert-deftest gptel-agent-session-title-persists-and-restores-without-requests ()
  (gptel-agent-session-title-test--with-request
    (gptel-agent-session-save)
    (should (= 1 (length requests)))
    (gptel-agent-session-title-test--reply (car requests) "Readable session titles")
    (let ((id gptel-agent-session--id))
      (gptel-agent-session-save)
      (should (equal "Readable session titles" (plist-get (gptel-agent-session--read id) :name)))
      (should (equal "Readable session titles" (plist-get (car (gptel-agent-session--catalog)) :name)))
      (should (equal "Readable session titles" (plist-get (car (gptel-agent-session--live-catalog)) :name)))
      (gptel-agent-session-test--forget buffer)
      (with-current-buffer (gptel-agent-session-restore id)
        (should (equal "Readable session titles" gptel-agent-session--title))
        (should (equal "*gptel:Readable session titles*" (buffer-name)))
        (org-mode)
        (should (equal "Readable session titles" gptel-agent-session--title))
        (gptel-mode 1)
        (goto-char (point-max))
        (insert "More messages after restoring")
        (gptel-agent-session-save))
      (should (= 1 (length requests))))))

(ert-deftest gptel-agent-session-title-ignores-stale-and-reasoning-results ()
  (gptel-agent-session-title-test--with-request
    (gptel-agent-session-generate-title)
    (let ((old (car requests)))
      (goto-char (point-max))
      (insert (propertize "New response" 'gptel 'response))
      (gptel-agent-session-generate-title)
      (should-not (buffer-live-p (plist-get old :buffer)))
      (gptel-agent-session-title-test--reply old "Stale title")
      (should-not gptel-agent-session--title)
      (gptel-agent-session-title-test--reply (car requests) '(reasoning . "Thinking"))
      (should (buffer-live-p gptel-agent-session--title-request))
      (should-not gptel-agent-session--title)
      (gptel-agent-session-title-test--reply (car requests) "Current title")
      (should (equal "Current title" gptel-agent-session--title)))))

(ert-deftest gptel-agent-session-title-failure-keeps-name-and-allows-retry ()
  (gptel-agent-session-title-test--with-request
    (setq gptel-agent-session--title "Existing title")
    (dolist (response '(nil "  \n " abort))
      (gptel-agent-session-generate-title)
      (gptel-agent-session-title-test--reply (car requests) response)
      (should (equal "Existing title" gptel-agent-session--title))
      (should-not gptel-agent-session--title-request)
      (should-not gptel-agent-session--title-context-hash))
    (let ((gptel-agent-session-title-preset 'nonexistent-title-preset))
      (should-error (gptel-agent-session-generate-title) :type 'user-error)
      (should-not gptel-agent-session--title-request))
    (gptel-agent-session-generate-title)
    (gptel-agent-session-title-test--reply (car requests) (make-string 200 ?x))
    (should (<= (string-width gptel-agent-session--title) 80))))

(ert-deftest gptel-agent-session-title-cancelled-on-lifecycle-changes ()
  (dolist (action '(kill exclude disable))
    (gptel-agent-session-title-test--with-request
      (gptel-agent-session-generate-title)
      (let ((request (car requests)))
        (pcase action
          ('kill (kill-buffer buffer))
          ('exclude (gptel-agent-session--exclude))
          ('disable (gptel-agent-session-mode -1)))
        (should-not (buffer-live-p (plist-get request :buffer)))
        (gptel-agent-session-title-test--reply request "Too late")
        (when (buffer-live-p buffer)
          (should-not (buffer-local-value 'gptel-agent-session--title buffer)))))))

(ert-deftest gptel-agent-session-title-skips-disabled-and-ineligible-chats ()
  (gptel-agent-session-title-test--with-request
    (let ((gptel-agent-session-auto-title nil)) (gptel-agent-session--auto-title))
    (let ((gptel-agent-session--restoring t)) (gptel-agent-session--auto-title))
    (let ((gptel-agent-session--excluded t)) (gptel-agent-session--auto-title))
    (let ((gptel-agent--supervised-run t)) (gptel-agent-session--auto-title))
    (should-not requests)
    (let ((gptel-agent-session-auto-title nil)
          (gptel-agent-session-title-context-function (lambda () "Custom context")))
      (gptel-agent-session-generate-title)
      (should (equal "Custom context" (plist-get (car requests) :prompt))))))

(ert-deftest gptel-agent-session-title-empty-chat-waits-for-message ()
  (gptel-agent-session-title-test--with-request
    (erase-buffer)
    (gptel-agent-session-save)
    (should-not requests)
    (insert "The first message")
    (gptel-agent-session--drain)
    (should (= 1 (length requests)))
    (should (equal "First message (User):\nThe first message"
                   (plist-get (car requests) :prompt)))))

(ert-deftest gptel-agent-session-title-buffer-name-collisions-keep-both-chats ()
  (gptel-agent-session-title-test--with-request
    (let ((other (generate-new-buffer "*gptel:Shared title*")))
      (gptel-agent-session-save)
      (gptel-agent-session-title-test--reply (car requests) "Shared title")
      (should (buffer-live-p other))
      (should (equal "*gptel:Shared title*" (buffer-name other)))
      (should (string-prefix-p "*gptel:Shared title*" (buffer-name)))
      (should-not (equal (buffer-name other) (buffer-name)))
      (should (equal "Shared title" gptel-agent-session--title)))))

(ert-deftest gptel-agent-session-title-real-request-completes-without-chat-mutation ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (let ((gptel-agent-session-auto-title t))
        (gptel-agent-session-mode 1)
        (gptel-agent-session-save)
        (let* ((request (buffer-local-value 'gptel-agent-session--title-request parent))
               (title-fsm (cl-find-if
                           (lambda (state) (eq request (plist-get (gptel-fsm-info state) :buffer)))
                           requests))
               (original (with-current-buffer parent (buffer-string)))
               (original-fsm-last (buffer-local-value 'gptel--fsm-last parent)))
          (should (buffer-live-p request))
          (should title-fsm)
          (gptel-agent-workers-test--answer title-fsm "Readable conversation names")
          (should-not (buffer-live-p request))
          (should (equal "Readable conversation names"
                         (buffer-local-value 'gptel-agent-session--title parent)))
          (should (equal original (with-current-buffer parent (buffer-string))))
          (should (eq original-fsm-last (buffer-local-value 'gptel--fsm-last parent)))
          (should (eq fsm (buffer-local-value 'gptel-agent-discussion--fsm parent)))
          (should (eq 'WAIT (gptel-fsm-state fsm)))
          (gptel-agent-workers-test--answer fsm "Session naming is useful")
          (gptel-agent-session-save)
          (should (= 2 (length requests))))))))

(ert-deftest gptel-agent-session-title-empty-http-success-releases-request ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (gptel-agent-session-mode 1)
      (with-current-buffer parent
        (gptel-agent-session-generate-title)
        (let ((title-fsm (car requests))
              (request gptel-agent-session--title-request))
          ;; Curl skips the callback for a successful but empty response.
          (gptel--fsm-transition title-fsm 'TYPE)
          (gptel--fsm-transition title-fsm)
          (should-not (buffer-live-p request))
          (should-not gptel-agent-session--title-request)
          (should-not gptel-agent-session--title-context-hash)
          (should-not gptel-agent-session--title))))))

(provide 'gptel-agent-session-title-test)
;;; gptel-agent-session-title-test.el ends here
