;;; gptel-agent-chat-test.el --- Main-chat isolation and delivery -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent-skills-test)

(cl-defmacro gptel-agent-chat-test--with-chats (&body body)
  "Run BODY with two ordinary chats and mocked transport only."
  (declare (indent 0))
  `(let* ((sender (generate-new-buffer " *main-chat-sender*"))
          (recipient (generate-new-buffer " *main-chat-recipient*"))
          (gptel-post-response-functions nil) (gptel-post-request-hook nil)
          (gptel-pre-response-hook nil) (gptel-log-level nil)
          (gptel-agent--runs (make-hash-table :test #'equal))
          (gptel-default-mode #'text-mode)
          (gptel-prompt-prefix-alist '((fundamental-mode . "USER> ")))
          (gptel-response-prefix-alist '((fundamental-mode . "ASSISTANT> ")))
          requests tool-callback sender-fsm recipient-fsm)
     (unwind-protect
         (cl-letf (((symbol-function 'gptel--update-status) #'ignore)
                   ((symbol-function 'gptel-curl-get-response)
                    (lambda (request)
                      (let ((info (gptel-fsm-info request)))
                        (unless (plist-get info :callback)
                          (setf (gptel-fsm-info request)
                                (plist-put info :callback
                                           (if (plist-get info :stream)
                                               #'gptel-curl--stream-insert-response
                                             #'gptel--insert-response)))))
                      (push request requests)))
                   ((symbol-function 'gptel--url-get-response)
                    (lambda (&rest _) (ert-fail "Unexpected transport"))))
           (dolist (buffer (list sender recipient))
             (with-current-buffer buffer
               (setq-local gptel-backend
                           (or gptel-agent-workers-test--backend
                               (gptel-make-openai "main-chat-test" :key "unused" :models '(test)))
                           gptel-model 'test gptel-mode t gptel-use-tools t
                           gptel-use-curl t gptel-stream gptel-agent-workers-test--stream
                           gptel-agent-preset nil gptel-confirm-tool-calls nil
                           gptel-pre-tool-call-functions nil gptel-post-tool-call-functions nil
                           gptel-agent--agent-snapshot
                           '(("researcher" :system "Research" :task-timeout nil
                              :max-request-rounds nil))
                           gptel-prompt-transform-functions
                           '(gptel-agent--localize-skill-tool gptel-agent--localize-agent-tool
                             gptel-agent-discussion--localize gptel-agent-chat--localize)
                           gptel-tools
                           (append
                            (when (eq buffer sender)
                              (mapcar #'gptel-get-tool '("MainChat" "Discussion" "Agent")))
                            (list (gptel-make-tool
                                   :name "MainChatDeferred" :async t :include t
                                   :description "Deferred operation" :args nil
                                   :function (lambda (callback) (setq tool-callback callback))))))
               (insert "USER> Initial request")
               (gptel-send)
               (if (eq buffer sender) (setq sender-fsm (car requests))
                 (setq recipient-fsm (car requests)))))
           ,@body)
       (dolist (buffer (list sender recipient))
         (when (buffer-live-p buffer)
           (dolist (chat (buffer-local-value 'gptel-agent-discussion--children buffer))
             (setf (gptel-agent-discussion--chat-owner chat) nil)
             (when (buffer-live-p (gptel-agent-discussion--chat-buffer chat))
               (kill-buffer (gptel-agent-discussion--chat-buffer chat))))
           (kill-buffer buffer))))))

(defun gptel-agent-chat-test--call (fsm action &optional id text offset)
  "Call FSM's actual request-local MainChat tool."
  (funcall (gptel-tool-function (gptel-agent-skills-test--tool fsm "MainChat"))
           action id text offset))

(ert-deftest gptel-agent-chat-delivers-once-after-tools-across-backends ()
  (dolist (backend (list (gptel-make-openai "c-openai" :key "unused" :models '(test) :stream t)
                        (gptel-make-openai-responses "c-responses" :key "unused" :models '(test) :stream t)
                        (gptel-make-anthropic "c-anthropic" :key "unused" :models '(test) :stream t)
                        (gptel-make-gemini "c-gemini" :key "unused" :models '(test) :stream t)))
    (dolist (stream '(nil t))
      (let ((gptel-agent-workers-test--backend backend)
            (gptel-agent-workers-test--stream stream))
        (gptel-agent-chat-test--with-chats
          (gptel-agent-chat-test--call sender-fsm "list")
          (gptel-agent-workers-test--tool recipient-fsm "MainChatDeferred" nil)
          (let ((count (length requests))
                (timers (copy-sequence timer-list)))
            (should (string-prefix-p
                     "Message queued"
                     (gptel-agent-chat-test--call
                      sender-fsm "send" (gptel-agent-chat--id recipient) "PEER-MESSAGE")))
            (should (= count (length requests)))
            (should (equal timers timer-list))
            (should-not (string-match-p "PEER-MESSAGE" (gptel-agent-workers-test--data recipient-fsm)))
            (funcall tool-callback "DEFERRED-RESULT")
            (should (= (1+ count) (length requests))))
          (let ((data (gptel-agent-workers-test--data recipient-fsm)))
            (should (< (string-match "DEFERRED-RESULT" data) (string-match "PEER-MESSAGE" data)))
            (should (string-match-p "not a user instruction or authorization" data)))
          (should-not (buffer-local-value 'gptel-agent-chat--inbox recipient))
          (gptel-agent-workers-test--tool recipient-fsm "MainChatDeferred" nil)
          (funcall tool-callback "SECOND-RESULT")
          (with-current-buffer recipient
            (should (= 1 (how-many "PEER-MESSAGE" (point-min) (point-max))))
            (goto-char (point-min)) (search-forward "PEER-MESSAGE")
            (should-not (get-text-property (1- (point)) 'gptel))))))))

(ert-deftest gptel-agent-chat-idle-draft-abort-and-retry ()
  (gptel-agent-chat-test--with-chats
    (gptel-agent-workers-test--answer recipient-fsm "Finished")
    (with-current-buffer recipient (goto-char (point-max)) (insert "UNSENT-DRAFT"))
    (let ((before (with-current-buffer recipient (buffer-string)))
          (count (length requests)))
      (gptel-agent-chat-test--call sender-fsm "send" (gptel-agent-chat--id recipient) "IDLE-MESSAGE")
      (should (= count (length requests)))
      (should (equal before (with-current-buffer recipient (buffer-string)))))
    ;; Injection failure keeps the queue for retry and does not copy the payload.
    (cl-letf (((symbol-function 'gptel--inject-prompt) (lambda (&rest _) (error "Unavailable"))))
      (should-error (gptel-agent-chat--inject recipient-fsm)))
    (should (buffer-local-value 'gptel-agent-chat--inbox recipient))
    (with-current-buffer recipient (goto-char (point-max)) (gptel-send))
    (setq recipient-fsm (car requests))
    (let ((data (gptel-agent-workers-test--data recipient-fsm)))
      (should (string-match-p "UNSENT-DRAFT" data))
      (should (string-match-p "IDLE-MESSAGE" data)))
    (gptel--fsm-transition recipient-fsm 'ABRT)
    (let ((count (length requests)))
      (gptel-agent-chat-test--call sender-fsm "send" (gptel-agent-chat--id recipient) "AFTER-ABORT")
      (should (= count (length requests)))
      (should (buffer-local-value 'gptel-agent-chat--inbox recipient)))))

(ert-deftest gptel-agent-chat-running-draft-survives-delivery ()
  (gptel-agent-chat-test--with-chats
    (gptel-agent-workers-test--tool recipient-fsm "MainChatDeferred" nil)
    (with-current-buffer recipient (goto-char (point-max)) (insert "PRIVATE-DRAFT"))
    (gptel-agent-chat-test--call sender-fsm "send" (gptel-agent-chat--id recipient) "ACTIVE-MESSAGE")
    (funcall tool-callback "TOOL-RESULT")
    (should-not (string-match-p "PRIVATE-DRAFT" (gptel-agent-workers-test--data recipient-fsm)))
    (with-current-buffer recipient
      (should (string-suffix-p "PRIVATE-DRAFT" (buffer-string))))))

(ert-deftest gptel-agent-chat-long-message-is-delivered-in-full ()
  (gptel-agent-chat-test--with-chats
    (let ((text (concat "LONG-BEGIN" (make-string 100000 ?x) "LONG-END")))
      (gptel-agent-workers-test--tool recipient-fsm "MainChatDeferred" nil)
      (should (string-prefix-p "Message queued"
                               (gptel-agent-chat-test--call sender-fsm "send"
                                                           (gptel-agent-chat--id recipient) text)))
      (funcall tool-callback "RESULT")
      (should (string-search text (gptel-agent-workers-test--data recipient-fsm)))
      (with-current-buffer recipient
        (should (string-search text (buffer-string)))))))

(ert-deftest gptel-agent-chat-read-only-recipient-keeps-queued-message ()
  (gptel-agent-chat-test--with-chats
    (gptel-agent-chat-test--call sender-fsm "send" (gptel-agent-chat--id recipient) "QUEUED")
    (with-current-buffer recipient (setq buffer-read-only t))
    (gptel-agent-chat--inject recipient-fsm)
    (should (buffer-local-value 'gptel-agent-chat--inbox recipient))
    (should-not (string-match-p "QUEUED" (gptel-agent-workers-test--data recipient-fsm)))))

(ert-deftest gptel-agent-chat-limits-fifo-and-stale-ids ()
  (gptel-agent-chat-test--with-chats
    (let ((gptel-agent-chat-inbox-limit 2)
          (id (gptel-agent-chat--id recipient)))
      (dolist (message '(nil "" " \n"))
        (should (string-prefix-p "Error:" (gptel-agent-chat-test--call sender-fsm "send" id message))))
      (dolist (message '("first" "next"))
        (should (string-prefix-p "Message queued" (gptel-agent-chat-test--call sender-fsm "send" id message))))
      (should (string-match-p "inbox is full" (gptel-agent-chat-test--call sender-fsm "send" id "last")))
      (should (equal '("first" "next")
                     (mapcar (lambda (event) (plist-get event :text))
                             (buffer-local-value 'gptel-agent-chat--inbox recipient))))
      (with-current-buffer recipient (rename-buffer " *renamed-main-chat*" t))
      (should (equal id (gptel-agent-chat--id recipient)))
      (kill-buffer recipient)
      (setq recipient (generate-new-buffer " *renamed-main-chat*"))
      (with-current-buffer recipient (setq-local gptel-mode t))
      (should-not (equal id (gptel-agent-chat--id recipient)))
      (should (string-prefix-p "Error:" (gptel-agent-chat-test--call sender-fsm "send" id "lost"))))))

(ert-deftest gptel-agent-chat-rejects-self-stale-caller-and-invalid-actions ()
  (gptel-agent-chat-test--with-chats
    (dolist (args `(("send" ,(gptel-agent-chat--id sender) "hello")
                    ("send" "discussion-1" "hello") ("read") ("list" nil nil -1)))
      (should (string-prefix-p "Error:" (apply #'gptel-agent-chat-test--call sender-fsm args))))
    (let ((old sender-fsm))
      (gptel-agent-workers-test--answer sender-fsm "Done")
      (should (string-prefix-p "Error:" (gptel-agent-chat-test--call old "list")))
      (with-current-buffer sender (goto-char (point-max)) (insert "New turn") (gptel-send))
      ;; Even a retained closure whose old FSM is made active cannot act as
      ;; the newer turn.  Checking only terminal states would miss this.
      (setf (gptel-fsm-state old) 'TOOL)
      (should (string-prefix-p "Error:" (gptel-agent-chat-test--call old "list"))))))

(ert-deftest gptel-agent-chat-never-targets-or-enables-discussions ()
  (gptel-agent-chat-test--with-chats
    (let* ((chat (gptel-agent-discussion-test--spawn sender-fsm sender))
           (child (gptel-agent-discussion--chat-buffer chat))
           (child-fsm (gptel-agent-discussion--chat-fsm chat))
           (id (gptel-agent-chat--id child)))
      (should-not (gptel-agent-skills-test--tool child-fsm "MainChat"))
      (should-not (string-match-p (regexp-quote id) (gptel-agent-chat-test--call sender-fsm "list")))
      (should (string-prefix-p "Error:" (gptel-agent-chat-test--call sender-fsm "send" id "hidden")))
      (gptel-agent-discussion--close chat)
      (with-current-buffer child
        (fundamental-mode)
        (setq-local gptel-mode t))
      (should (buffer-local-value 'gptel-agent--child-chat-p child))
      (should-not (gptel-agent-chat--main-p child))
      (setf (gptel-agent-discussion--chat-owner chat) nil)
      (should (string-prefix-p "Error:" (gptel-agent-chat-test--call sender-fsm "send" id "hidden")))
      ;; A child cannot invoke the implementation even with a forged schema.
      (should (string-prefix-p "Error:" (gptel-agent-chat--dispatch child-fsm "list"))))))

(ert-deftest gptel-agent-chat-worker-cannot-inherit-sender-closure ()
  (gptel-agent-chat-test--with-chats
    (let* ((run (gptel-agent-workers-test--launch sender-fsm))
           (worker (gptel-agent--run-child-buffer run)))
      (should (buffer-local-value 'gptel-agent--child-chat-p worker))
      (should-not (gptel-agent-skills-test--tool (gptel-agent--run-fsm run) "MainChat"))
      (with-current-buffer worker
        (fundamental-mode)
        (setq-local gptel-mode t))
      (should-not (gptel-agent-chat--main-p worker)))))

(ert-deftest gptel-agent-chat-custom-discussion-transforms-cannot-inherit-tool ()
  (gptel-agent-chat-test--with-chats
    (let ((gptel--known-presets (copy-tree gptel--known-presets)))
      (gptel-make-preset 'chat-test-no-localizer
                        :eval '(setq-local gptel-prompt-transform-functions nil))
      (gptel-agent-workers-test--tool
       sender-fsm "Discussion"
       '(:title "Custom child" :prompt "Work" :preset "chat-test-no-localizer"))
      (let* ((child (car (buffer-local-value 'gptel-agent-discussion--children sender)))
             (fsm (gptel-agent-discussion--chat-fsm child)))
        (should-not (gptel-agent-skills-test--tool fsm "MainChat"))))))

(ert-deftest gptel-agent-chat-bounded-discovery-does-not-read-transcripts ()
  (let (buffers)
    (unwind-protect
        (progn
          (dotimes (_ 23)
            (let ((buffer (generate-new-buffer " *main-page*")))
              (push buffer buffers)
              (with-current-buffer buffer
                (setq-local gptel-mode t)
                (insert "SECRET-TRANSCRIPT"))))
          (cl-letf (((symbol-function 'buffer-list) (lambda (&rest _) buffers))
                    ((symbol-function 'buffer-substring-no-properties)
                     (lambda (&rest _) (ert-fail "Discovery read a transcript"))))
            (let ((page (gptel-agent-chat--list nil 0)))
              (should (string-match-p "next_offset: 20" page))
              (should-not (string-match-p "SECRET-TRANSCRIPT" page)))
            (should (string-match-p "next_offset: end" (gptel-agent-chat--list nil 20)))))
      (mapc #'kill-buffer buffers))))

(ert-deftest gptel-agent-chat-skill-activation-in-current-and-next-turn ()
  (gptel-agent-skills-test--with-chat "allowed-tools: MainChat\n"
    (should-not (gptel-agent-skills-test--tool fsm "MainChat"))
    (gptel-agent-skills-test--load fsm)
    (should (member "MainChat" (gptel-agent-skills-test--wire-names fsm)))
    (should-not (string-prefix-p "Error:" (gptel-agent-chat-test--call fsm "list")))
    (gptel-agent-workers-test--answer fsm "Ready")
    (with-current-buffer parent
      (add-hook 'gptel-prompt-transform-functions #'gptel-agent-chat--localize t t)
      (goto-char (point-max)) (insert "Continue") (gptel-send))
    (setq fsm (caar requests))
    (should-not (string-prefix-p "Error:" (gptel-agent-chat-test--call fsm "list")))
    (let* ((gate (gptel-agent--workers-for fsm))
           (handlers (if gate (gptel-agent--workers-handlers gate) (gptel-fsm-handlers fsm))))
      (should (= 1 (cl-count #'gptel-agent-chat--inject (alist-get 'WAIT handlers)))))))

(ert-deftest gptel-agent-chat-bundled-skill-loads-on-demand ()
  (gptel-agent-skills-test--with-chat ""
    (let ((gptel-agent-skill-dirs nil)) (gptel-agent--update-skills))
    (gptel-agent-workers-test--answer fsm "Ready")
    (with-current-buffer parent
      (goto-char (point-max)) (insert "List my other main chats") (gptel-send)
      (setq fsm (caar requests)))
    (should-not (member "MainChat" (gptel-agent-skills-test--wire-names fsm)))
    (gptel-agent-workers-test--tool fsm "Skill" '(:skill "main-chat-messages"))
    (should (member "MainChat" (gptel-agent-skills-test--wire-names fsm)))
    (should-not (string-prefix-p "Error:" (gptel-agent-chat-test--call fsm "list")))))

(ert-deftest gptel-agent-chat-child-cannot-activate-via-skill ()
  (gptel-agent-skills-test--with-chat "allowed-tools: MainChat\n"
    (with-current-buffer parent (setq-local gptel-agent--child-chat-p t))
    (gptel-agent-skills-test--load fsm)
    (should-not (gptel-agent-skills-test--tool fsm "MainChat"))
    (should-not (member "MainChat" (gptel-agent-skills-test--wire-names fsm)))))

(provide 'gptel-agent-chat-test)
;;; gptel-agent-chat-test.el ends here
