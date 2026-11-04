;;; gptel-agent-discussion-test.el --- Managed chat lifecycle -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent-tools-test)
(require 'gptel-agent-workers-test)
(require 'gptel-agent-discussion)

(defvar gptel-agent-discussion-test--mode #'text-mode)

(cl-defmacro gptel-agent-discussion-test--with-chat (&body body)
  "Run BODY through ordinary gptel-send with only network transport mocked."
  (declare (indent 0))
  `(let* ((parent (generate-new-buffer " *discussion-parent*"))
          (gptel-default-mode gptel-agent-discussion-test--mode)
          (gptel-post-response-functions nil)
          (gptel-post-request-hook nil)
          (gptel-pre-response-hook nil)
          (gptel-log-level nil)
          (gptel-prompt-prefix-alist '((fundamental-mode . "USER> ") (text-mode . "USER> ")))
          (gptel-response-prefix-alist '((fundamental-mode . "ASSISTANT> ") (text-mode . "ASSISTANT> ")))
          (gptel-agent--runs (make-hash-table :test #'equal))
          requests tool-callback (executions 0) fsm chat)
     (unwind-protect
         (cl-letf (((symbol-function 'gptel--update-status) #'ignore)
                   ((symbol-function 'gptel-curl-get-response)
                    (lambda (request) (push request requests)))
                   ((symbol-function 'gptel--url-get-response)
                    (lambda (request) (push request requests))))
           (with-current-buffer parent
             (fundamental-mode)
             (setq-local gptel-backend
                         (or gptel-agent-workers-test--backend
                             (gptel-make-openai "discussion-test" :key "unused"
                                                :host "example.invalid" :stream t :models '(test)))
                         gptel-model 'test gptel-stream gptel-agent-workers-test--stream
                         gptel-use-curl t gptel-mode t gptel-use-tools t
                         gptel-agent-preset nil gptel-confirm-tool-calls nil
                         gptel-pre-tool-call-functions nil gptel-post-tool-call-functions nil
                         gptel-prompt-transform-functions '(gptel-agent--localize-ask-tool
                                                            gptel-agent-discussion--localize)
                         gptel-tools
                         (list (gptel-get-tool "Discussion") (gptel-get-tool "DiscussionControl")
                               (gptel-get-tool "AskUserQuestion")
                               (gptel-make-tool :name "DiscussionDeferred" :async t :include t
                                                :description "Deferred operation" :args nil
                                                :function (lambda (callback) (setq tool-callback callback)))
                               (gptel-make-tool :name "DiscussionMutation" :include t :confirm t
                                                :description "Count executions" :args nil
                                                :function (lambda () (cl-incf executions) "changed"))))
             (insert "USER> Initial parent request")
             (gptel-send)
             (setq fsm gptel-agent-discussion--fsm))
           ,@body)
       ;; Detach owners before killing children so teardown never wakes a model.
       (when (buffer-live-p parent)
         (dolist (child (buffer-local-value 'gptel-agent-discussion--children parent))
           (setf (gptel-agent-discussion--chat-owner child) nil)
           (when (buffer-live-p (gptel-agent-discussion--chat-buffer child))
             (kill-buffer (gptel-agent-discussion--chat-buffer child))))
         (kill-buffer parent)))))

(defun gptel-agent-discussion-test--spawn (fsm parent &optional prompt)
  "Call Discussion through FSM's actual backend and tool dispatch in PARENT."
  (gptel-agent-workers-test--tool
   fsm "Discussion" (list :title "Independent chat" :prompt (or prompt "Child request")))
  (car (buffer-local-value 'gptel-agent-discussion--children parent)))

(defun gptel-agent-discussion-test--control (fsm action &optional chat message request-id ids offset)
  "Run the request-local management tool in FSM and return its synchronous result."
  (let ((tool (cl-find "DiscussionControl" (plist-get (gptel-fsm-info fsm) :tools)
                       :key #'gptel-tool-name :test #'equal)) result)
    (funcall (gptel-tool-function tool) (lambda (text) (setq result text))
             action (and chat (gptel-agent-discussion--chat-id chat)) message request-id ids offset)
    result))

(defun gptel-agent-discussion-test--buffer-text (buffer)
  "Return BUFFER's visible transcript."
  (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest gptel-agent-discussion-independent-completion-and-followup ()
  (dolist (backend (list (gptel-make-openai "d-openai" :key "unused" :models '(test) :stream t)
                         (gptel-make-openai-responses "d-responses" :key "unused" :models '(test) :stream t)
                         (gptel-make-anthropic "d-anthropic" :key "unused" :models '(test) :stream t)
                         (gptel-make-gemini "d-gemini" :key "unused" :models '(test) :stream t)))
    (dolist (stream '(nil t))
      (let ((gptel-agent-workers-test--backend backend)
            (gptel-agent-workers-test--stream stream))
        (gptel-agent-discussion-test--with-chat
         (setq chat (gptel-agent-discussion-test--spawn fsm parent))
         (let* ((child (gptel-agent-discussion--chat-buffer chat))
                (child-fsm (gptel-agent-discussion--chat-fsm chat)))
           (should (buffer-local-value 'gptel-mode child))
           (should-not (string-match-p "Initial parent request"
                                       (gptel-agent-workers-test--data child-fsm)))
           (gptel-agent-workers-test--answer fsm "Parent can finish now" stream)
           (should (eq (gptel-fsm-state fsm) 'DONE))
           (should (gptel-agent-discussion--live-p chat child-fsm))
           (gptel-agent-workers-test--answer child-fsm "CHILD-COMPLETION" stream)
           (should (eq (gptel-agent-discussion--chat-state chat) 'idle))
           (let ((count (length requests)))
             (gptel-agent-discussion--flush parent)
             (should (= count (length requests))))
           (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
           (should (eq (gptel-fsm-state fsm) 'DONE))
           (should-not (string-match-p "CHILD-COMPLETION" (gptel-agent-discussion-test--buffer-text parent)))
           (with-current-buffer parent
             (goto-char (point-max)) (insert "Ask the child for an update") (gptel-send)
             (setq fsm gptel-agent-discussion--fsm))
           (should (string-match-p "queued"
                                   (gptel-agent-discussion-test--control
                                    fsm "send" chat "FOLLOW-UP: please report your progress")))
           (gptel-agent-discussion--flush child)
           (should (= 2 (gptel-agent-discussion--chat-turn chat)))
           (setq child-fsm (gptel-agent-discussion--chat-fsm chat))
           (let ((data (gptel-agent-workers-test--data child-fsm)))
             (should (string-match-p "Child request" data))
             (should (string-match-p "CHILD-COMPLETION" data))
             (should (string-match-p "FOLLOW-UP" data)))
           (gptel-agent-workers-test--answer fsm "Parent can finish again" stream)
           (gptel-agent-workers-test--tool child-fsm "DiscussionReport" '(:message "CHOSEN-REPORT"))
           (gptel-agent-workers-test--answer child-fsm "LOCAL-FOLLOW-UP" stream)
           (gptel-agent-discussion--flush parent)
           (setq fsm (buffer-local-value 'gptel-agent-discussion--fsm parent))
           (should (gptel-agent-discussion--active-p fsm))
           (should (= 1 (gptel-agent-workers-test--count "CHOSEN-REPORT"
                                                        (gptel-agent-workers-test--data fsm))))
           (should (string-match-p "CHOSEN-REPORT" (gptel-agent-discussion-test--buffer-text parent)))
           (should-not (string-match-p "LOCAL-FOLLOW-UP" (gptel-agent-workers-test--data fsm)))))))))

(ert-deftest gptel-agent-discussion-steering-waits-for-tool-results ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let ((child-fsm (gptel-agent-discussion--chat-fsm chat)))
     (gptel-agent-workers-test--tool child-fsm "DiscussionDeferred" nil)
     (let ((count (length requests)))
       (gptel-agent-discussion-test--control fsm "send" chat "STEERING-CORRECTION")
       (gptel-agent-discussion--flush (gptel-agent-discussion--chat-buffer chat))
       (should (= count (length requests)))
       (funcall tool-callback "DEFERRED-RESULT")
       (should (= (1+ count) (length requests))))
     (let ((data (gptel-agent-workers-test--data child-fsm)))
       (should (< (string-match "DEFERRED-RESULT" data) (string-match "STEERING-CORRECTION" data))))
     (should (= 1 (gptel-agent-discussion--chat-turn chat))))))

(ert-deftest gptel-agent-discussion-parent-and-child-drafts-survive ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (gptel-agent-workers-test--answer fsm "Parent done")
   (with-current-buffer parent (goto-char (point-max)) (insert "UNSENT-DRAFT"))
   (gptel-agent-workers-test--tool (gptel-agent-discussion--chat-fsm chat)
                                 "DiscussionReport" '(:message "Chosen child report"))
   (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Child done")
   (let ((count (length requests)))
     (gptel-agent-discussion--flush parent)
     (should (= count (length requests)))
     (should (buffer-local-value 'gptel-agent-discussion--inbox parent)))
   (with-current-buffer parent (goto-char (point-max)) (gptel-send)
                        (setq fsm gptel-agent-discussion--fsm))
   (should (string-match-p "UNSENT-DRAFT" (gptel-agent-workers-test--data fsm)))
   (should (string-match-p "Chosen child report" (gptel-agent-workers-test--data fsm)))
   (should-not (string-match-p "Child done" (gptel-agent-workers-test--data fsm)))
   (with-current-buffer (gptel-agent-discussion--chat-buffer chat)
     (goto-char (point-max)) (insert "CHILD-DRAFT"))
   (gptel-agent-discussion-test--control fsm "send" chat "Parent followup")
   (let ((count (length requests)))
     (gptel-agent-discussion--flush (gptel-agent-discussion--chat-buffer chat))
     (should (= count (length requests))))))

(ert-deftest gptel-agent-discussion-user-sends-ordinary-followup ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (gptel-agent-workers-test--answer fsm "Parent done")
   (let ((child (gptel-agent-discussion--chat-buffer chat)))
     (with-current-buffer child (should-error (gptel-send) :type 'user-error))
     (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "First answer")
     (with-current-buffer child (goto-char (point-max)) (insert "Direct user followup") (gptel-send))
     (should (= 2 (gptel-agent-discussion--chat-turn chat)))
     (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Second answer")
     (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
     (let ((count (length requests)))
       (gptel-agent-discussion--flush parent)
       (should (= count (length requests))))
     (should (eq (gptel-fsm-state fsm) 'DONE)))))

(ert-deftest gptel-agent-discussion-direct-steering-identifies-the-user ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let ((child (gptel-agent-discussion--chat-buffer chat))
         (child-fsm (gptel-agent-discussion--chat-fsm chat)))
     (gptel-agent-workers-test--tool child-fsm "DiscussionDeferred" nil)
     (with-current-buffer child
       (gptel-agent-discussion-send "USER-STEERING"))
     (gptel-agent-discussion-test--control fsm "send" chat "MANAGER-STEERING")
     (funcall tool-callback "Deferred result")
     (let ((text (gptel-agent-discussion-test--buffer-text child)))
       (should (string-match-p "Message from the user in this discussion:\nUSER-STEERING" text))
       (should (string-match-p "Message from the managing conversation:\nMANAGER-STEERING" text)))
     (gptel-agent-workers-test--answer child-fsm "Local answer")
     (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent)))))

(ert-deftest gptel-agent-discussion-report-validates-message-and-request-lifetime ()
  (gptel-agent-discussion-test--with-chat
   (should-not (cl-find "DiscussionReport" (plist-get (gptel-fsm-info fsm) :tools)
                        :key #'gptel-tool-name :test #'equal))
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let* ((child (gptel-agent-discussion--chat-buffer chat))
          (child-fsm (gptel-agent-discussion--chat-fsm chat))
          (report (gptel-tool-function
                   (cl-find "DiscussionReport" (plist-get (gptel-fsm-info child-fsm) :tools)
                            :key #'gptel-tool-name :test #'equal))))
     (dolist (invalid '(nil "" " \n\t" 12))
       (should (string-prefix-p "Error:" (funcall report invalid))))
     (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
     (gptel-agent-workers-test--answer child-fsm "First answer")
     (should (string-prefix-p "Error:" (funcall report "Late report")))
     (with-current-buffer child (goto-char (point-max)) (insert "New user turn") (gptel-send))
     (should (string-prefix-p "Error:" (funcall report "Old request during new turn")))
     (setq report (gptel-tool-function
                   (cl-find "DiscussionReport"
                            (plist-get (gptel-fsm-info (gptel-agent-discussion--chat-fsm chat)) :tools)
                            :key #'gptel-tool-name :test #'equal)))
     (gptel-agent-discussion--close chat)
     (should (string-prefix-p "Error:" (funcall report "Closed report")))
     (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
     (with-current-buffer child (goto-char (point-max)) (insert "Ordinary chat now") (gptel-send))
     (should-not (cl-find "DiscussionReport"
                          (plist-get (gptel-fsm-info (car requests)) :tools)
                          :key #'gptel-tool-name :test #'equal)))))

(ert-deftest gptel-agent-discussion-nested-reports-go-only-to-immediate-owner ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let* ((child (gptel-agent-discussion--chat-buffer chat))
          (child-fsm (gptel-agent-discussion--chat-fsm chat))
          (nested (progn
                    (gptel-agent-discussion-test--spawn child-fsm child)
                    (when-let* ((pending (car (gptel-agent-discussion--chat-pending chat))))
                      (gptel--accept-tool-calls (gptel-agent-discussion--interaction-calls pending)))
                    (car (buffer-local-value 'gptel-agent-discussion--children child))))
          (nested-fsm (gptel-agent-discussion--chat-fsm nested)))
     (unwind-protect
         (progn
           (gptel-agent-workers-test--tool nested-fsm "DiscussionReport" '(:message "NESTED-REPORT"))
           (gptel-agent-workers-test--answer nested-fsm "Local nested answer")
           (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
           (let ((event (car (buffer-local-value 'gptel-agent-discussion--inbox child))))
             (should (equal (plist-get event :discussion-id) (gptel-agent-discussion--chat-id nested)))
             (should (equal (plist-get event :text) "NESTED-REPORT")))
           (should (string-match-p "NESTED-REPORT"
                                   (gptel-agent-discussion-test--control child-fsm "wait")))
           (gptel-agent-workers-test--answer child-fsm "Local child answer")
           (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent)))
       (kill-buffer (gptel-agent-discussion--chat-buffer nested))))))

(ert-deftest gptel-agent-discussion-reports-wait-for-parent-tool-results ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (gptel-agent-workers-test--tool fsm "DiscussionDeferred" nil)
   (gptel-agent-workers-test--tool (gptel-agent-discussion--chat-fsm chat)
                                 "DiscussionReport" '(:message "CHOSEN-UPDATE"))
   (let ((count (length requests)))
     (gptel-agent-discussion--flush parent)
     (should (= count (length requests))))
   (funcall tool-callback "PARENT-TOOL-RESULT")
   (let ((data (gptel-agent-workers-test--data fsm)))
     (should (< (string-match "PARENT-TOOL-RESULT" data) (string-match "CHOSEN-UPDATE" data)))
     (should (= 1 (gptel-agent-workers-test--count "CHOSEN-UPDATE" data))))))

(ert-deftest gptel-agent-discussion-silent-completion-does-not-wake-waiter ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (gptel-agent-workers-test--tool fsm "DiscussionControl" '(:action "wait"))
   (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "LOCAL-RESULT")
   (let ((count (length requests)))
     (gptel-agent-discussion--flush parent)
     (should (= count (length requests))))
   (should (eq (gptel-fsm-state fsm) 'TOOL))
   (let ((waiter (car (buffer-local-value 'gptel-agent-discussion--waiters parent))))
     (funcall (timer--function (nth 3 waiter))))
   (should (string-match-p "status: timeout" (gptel-agent-workers-test--data fsm)))
   (should-not (string-match-p "LOCAL-RESULT" (gptel-agent-workers-test--data fsm)))))

(ert-deftest gptel-agent-discussion-terminal-alerts-stay-local ()
  (dolist (outcome '(failed cancelled closed killed))
    (gptel-agent-discussion-test--with-chat
     (setq chat (gptel-agent-discussion-test--spawn fsm parent))
     (gptel-agent-workers-test--answer fsm "Parent finished")
     (gptel-agent-discussion--flush parent)
     (let ((child (gptel-agent-discussion--chat-buffer chat))
           (child-fsm (gptel-agent-discussion--chat-fsm chat))
           (count (length requests)))
       (pcase outcome
         ('failed
          (setf (plist-get (gptel-fsm-info child-fsm) :error) "LOCAL-FAILURE")
          (gptel--fsm-transition child-fsm 'ERRS)
          (should (string-match-p "LOCAL-FAILURE" (gptel-agent-discussion--read chat 0))))
         ('cancelled (gptel-abort child))
         ('closed (gptel-agent-discussion--close chat))
         ('killed (kill-buffer child)))
       (should (eq (gptel-agent-discussion--chat-state chat)
                   (if (eq outcome 'killed) 'closed outcome)))
       (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
       (should-not (buffer-local-value 'gptel-agent-discussion--timer parent))
       (gptel-agent-discussion--flush parent)
       (should (= count (length requests)))))))

(ert-deftest gptel-agent-discussion-question-one-winner-and-stale-answer ()
  (dolist (user-first '(nil t))
    (gptel-agent-discussion-test--with-chat
     (setq chat (gptel-agent-discussion-test--spawn fsm parent))
     (let ((child-fsm (gptel-agent-discussion--chat-fsm chat)))
       (gptel-agent-workers-test--tool
        child-fsm "AskUserQuestion"
        '(:questions [(:question "Which color?" :choices [(:value "Blue") (:value "Red")])]))
       (should-not (gptel-agent-discussion--chat-pending chat))
       (gptel-agent-workers-test--tool child-fsm "AskUserQuestion" '(:action "wait"))
       (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
              (id (gptel-agent-discussion--interaction-id pending))
              (ov (car (gptel-agent-discussion--interaction-overlays pending)))
              (ui-callback (overlay-get ov 'gptel-ask--callback))
              (count (length requests)))
         (should (eq (gptel-agent-discussion--chat-state chat) 'waiting-for-question))
         (gptel-agent-discussion--flush parent)
         (should (= count (length requests)))
         (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
         (should (string-match-p "Which color?"
                                 (gptel-agent-discussion-test--control fsm "read" chat)))
         (if user-first
             (progn (funcall ui-callback "Blue from user")
                    (should (string-prefix-p "Error:" (gptel-agent-discussion-test--control
                                                       fsm "answer" chat "Red" id))))
           (should (equal "Question answered." (gptel-agent-discussion-test--control
                                                fsm "answer" chat "Blue from parent" id)))
           (funcall ui-callback "Late user callback"))
         (should (= (1+ count) (length requests)))
         (should-not (overlay-buffer ov))
         (should-not (gptel-agent-discussion--chat-pending chat))
         (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent)))))))

(ert-deftest gptel-agent-discussion-approvals-stay-in-child-with-echo-notice ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let* ((child (gptel-agent-discussion--chat-buffer chat))
          (child-fsm (gptel-agent-discussion--chat-fsm chat))
          (parent-text (gptel-agent-discussion-test--buffer-text parent)) notices)
     (with-current-buffer child (setq-local gptel-confirm-tool-calls 'auto))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest args)
                  (push (apply #'format-message format-string args) notices))))
       (gptel-agent-workers-test--tool child-fsm "DiscussionMutation" nil))
     (should (cl-some (lambda (notice)
                       (string-match-p
                        (regexp-quote (concat "Tool approval waiting in " (buffer-name child))) notice))
                     notices))
     (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
            (id (gptel-agent-discussion--interaction-id pending))
            (calls (gptel-agent-discussion--interaction-calls pending)))
       (should (= 0 executions))
       (let ((count (length requests)))
         (gptel-agent-discussion--flush parent)
         (should (= count (length requests))))
       (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
       (should (= 1 (length (gptel-agent-discussion--interaction-overlays pending))))
       (dolist (action '("request_approval" "reject"))
         (should-not (member action (append (plist-get (car (gptel-tool-args
                                                            (gptel-get-tool "DiscussionControl"))) :enum) nil)))
         ;; Old request schemas must not move or resolve this approval either.
         (should (string-prefix-p "Error: Approve or reject tool calls in discussion buffer"
                                  (gptel-agent-discussion-test--control fsm action chat nil id))))
       (should (= 0 executions))
       (should (gptel-agent-discussion--pending pending))
       (should (= 1 (length (gptel-agent-discussion--interaction-overlays pending))))
       (should (equal parent-text (gptel-agent-discussion-test--buffer-text parent)))
       (with-current-buffer parent
         (should-not (cl-some (lambda (ov) (overlay-get ov 'gptel-tool))
                              (overlays-in (point-min) (point-max)))))
       (let ((overlays (copy-sequence (gptel-agent-discussion--interaction-overlays pending))))
         (should (eq (overlay-buffer (car overlays)) child))
         ;; Native user acceptance.  Stale callbacks cannot run the tool again.
         (with-current-buffer child (gptel--accept-tool-calls calls (car overlays)))
         (should (= 1 executions))
         (gptel--accept-tool-calls calls)
         (should (= 1 executions))
         (should (cl-every (lambda (ov) (not (overlay-buffer ov))) overlays)))
       (should (string-prefix-p "Error:" (gptel-agent-discussion-test--control
                                          fsm "request_approval" chat nil id)))))))

(ert-deftest gptel-agent-discussion-close-invalidates-approvals-and-late-callbacks ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let* ((child (gptel-agent-discussion--chat-buffer chat))
          (child-fsm (gptel-agent-discussion--chat-fsm chat)))
     (with-current-buffer child (setq-local gptel-confirm-tool-calls 'auto))
     (gptel-agent-workers-test--tool child-fsm "DiscussionMutation" nil)
     (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
            (calls (gptel-agent-discussion--interaction-calls pending)))
       (gptel-agent-discussion-test--control fsm "close" chat)
       (gptel--accept-tool-calls calls)
       (should (= 0 executions))
       (should-not (gptel-agent-discussion--chat-pending chat))))
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let ((child (gptel-agent-discussion--chat-buffer chat))
         (child-fsm (gptel-agent-discussion--chat-fsm chat)))
     (gptel-agent-workers-test--tool child-fsm "DiscussionDeferred" nil)
     (gptel-agent-discussion-test--control fsm "close" chat)
     (let ((count (length requests)))
       (funcall tool-callback "Late completion")
       (should (= count (length requests))))
     (should (eq (gptel-agent-discussion--chat-state chat) 'closed))
     (should (buffer-live-p child))
     (should-not (buffer-local-value 'gptel-agent-discussion--self child)))))

(ert-deftest gptel-agent-discussion-read-does-not-consume-and-wait-consumes-once ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (gptel-agent-workers-test--tool (gptel-agent-discussion--chat-fsm chat)
                                 "DiscussionReport" '(:message "WAIT-REPORT"))
   (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Local answer")
   (should (string-match-p "WAIT-REPORT" (gptel-agent-discussion-test--control fsm "read" chat)))
   (should (string-match-p "WAIT-REPORT" (gptel-agent-discussion-test--control fsm "wait")))
   (should (string-match-p "no_eligible_work" (gptel-agent-discussion-test--control fsm "wait")))
   (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
   (should (string-prefix-p "Error:" (gptel-agent-discussion-test--control fsm "wait" nil nil nil ["foreign"])))))

(ert-deftest gptel-agent-discussion-waiter-wakes-from-explicit-report ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (gptel-agent-workers-test--tool fsm "DiscussionControl" '(:action "wait"))
   (should (eq (gptel-fsm-state fsm) 'TOOL))
   (gptel-agent-workers-test--tool (gptel-agent-discussion--chat-fsm chat)
                                 "DiscussionReport" '(:message "WAITED-REPORT"))
   (gptel-agent-discussion--flush parent)
   (should (eq (gptel-fsm-state fsm) 'WAIT))
   (should (= 1 (gptel-agent-workers-test--count "WAITED-REPORT" (gptel-agent-workers-test--data fsm))))
   (should-not (buffer-local-value 'gptel-agent-discussion--waiters parent))))

(ert-deftest gptel-agent-discussion-owner-abort-pauses-notifications-child-survives ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (gptel-abort parent)
   (should (gptel-agent-discussion--live-p chat (gptel-agent-discussion--chat-fsm chat)))
   (gptel-agent-workers-test--tool (gptel-agent-discussion--chat-fsm chat)
                                 "DiscussionReport" '(:message "After parent abort"))
   (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "After parent abort")
   (let ((count (length requests)))
     (gptel-agent-discussion--flush parent)
     (should (= count (length requests))))
   (should (buffer-local-value 'gptel-agent-discussion--inbox parent))))

(ert-deftest gptel-agent-discussion-killed-owner-detaches-child ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let ((child (gptel-agent-discussion--chat-buffer chat)))
     (unwind-protect
         (progn
           (let* ((child-fsm (gptel-agent-discussion--chat-fsm chat))
                  (report (gptel-tool-function
                           (cl-find "DiscussionReport" (plist-get (gptel-fsm-info child-fsm) :tools)
                                    :key #'gptel-tool-name :test #'equal))))
             (kill-buffer parent)
             (should (string-prefix-p "Error:" (funcall report "Detached report"))))
           (should-not (gptel-agent-discussion--chat-owner chat))
           (should (gptel-agent-discussion--live-p chat (gptel-agent-discussion--chat-fsm chat)))
           (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Detached completion")
           (should (buffer-live-p child))
           (with-current-buffer child (goto-char (point-max)) (insert "Detached user turn") (gptel-send))
           (should-not (cl-find "DiscussionReport"
                                (plist-get (gptel-fsm-info (gptel-agent-discussion--chat-fsm chat)) :tools)
                                :key #'gptel-tool-name :test #'equal)))
       (kill-buffer child)))))

(ert-deftest gptel-agent-discussion-ownership-validation-and-pagination ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let ((child-fsm (gptel-agent-discussion--chat-fsm chat))
         (gptel-agent-discussion-report-limit 10))
     (should (string-prefix-p "Error:" (gptel-agent-discussion-test--control child-fsm "read" chat)))
     (should (string-match-p "next_offset: 10" (gptel-agent-discussion-test--control fsm "read" chat)))
     (should (string-match-p "transcript_offset: 10" (gptel-agent-discussion-test--control fsm "read" chat nil nil nil 10)))
     (should (string-prefix-p "Error:" (gptel-agent-discussion-test--control fsm "read" chat nil nil nil -1))))))

(ert-deftest gptel-agent-discussion-edited-approval-retains-guard-and-child-context ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let* ((child (gptel-agent-discussion--chat-buffer chat))
          (child-fsm (gptel-agent-discussion--chat-fsm chat)))
     (with-current-buffer child (setq-local gptel-confirm-tool-calls 'auto))
     (gptel-agent-workers-test--tool child-fsm "DiscussionMutation" nil)
     (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
            (call (car (gptel-agent-discussion--interaction-calls pending)))
            (edited (list (gptel-make-tool
                           :name "EditedMutation" :description "Edited by user"
                           :args nil :function (lambda ()
                                                 (should (eq (current-buffer) child))
                                                 (cl-incf executions) "edited"))
                          nil (nth 2 call))))
       ;; The inspector swaps even unchanged names back to the original tool
       ;; object. Simulate that boundary without relying on inspector UI text.
       (with-current-buffer child (gptel--accept-tool-calls (list edited)))
       (should (= executions 1))
       (gptel--accept-tool-calls (list edited))
       (gptel--accept-tool-calls (list call))
       (should (= executions 1))
        (should-not (gptel-agent-discussion--chat-pending chat))))))

(ert-deftest gptel-agent-discussion-native-cancel-stops-child-not-parent ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let* ((child (gptel-agent-discussion--chat-buffer chat))
          (child-fsm (gptel-agent-discussion--chat-fsm chat)))
     (with-current-buffer child (setq-local gptel-confirm-tool-calls 'auto))
     (gptel-agent-workers-test--tool child-fsm "DiscussionMutation" nil)
     (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
            (calls (gptel-agent-discussion--interaction-calls pending)))
       (let ((ov (car (gptel-agent-discussion--interaction-overlays pending))))
         (should (eq (overlay-buffer ov) child))
         (with-current-buffer child (gptel--reject-tool-calls calls ov)))
       (should (eq (gptel-fsm-state child-fsm) 'ABRT))
       (should (gptel-agent-discussion--active-p fsm))
       (should-not (gptel-agent-discussion--chat-pending chat))
       (gptel--accept-tool-calls calls)
       (should (= executions 0))
       (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
       (should (string-match-p "status: cancelled"
                               (gptel-agent-discussion-test--control fsm "read" chat)))))))

(ert-deftest gptel-agent-discussion-org-streaming-and-steering-history ()
  (let ((gptel-agent-discussion-test--mode #'org-mode)
        (gptel-agent-workers-test--stream t))
    (gptel-agent-discussion-test--with-chat
     (setq chat (gptel-agent-discussion-test--spawn fsm parent))
     (let* ((child (gptel-agent-discussion--chat-buffer chat))
            (child-fsm (gptel-agent-discussion--chat-fsm chat)))
       (gptel-agent-workers-test--tool child-fsm "DiscussionDeferred" nil)
       (gptel-agent-discussion-test--control fsm "send" chat "ORG-STEERING")
       (funcall tool-callback "Tool result")
       (with-current-buffer child
         (goto-char (point-min)) (search-forward "ORG-STEERING")
         (should-not (get-text-property (1- (point)) 'gptel)))
       (gptel-agent-workers-test--answer child-fsm "**Org response**" t)
       (gptel-agent-discussion-test--control fsm "send" chat "Org followup")
       (gptel-agent-discussion--flush child)
       (should (= 2 (gptel-agent-discussion--chat-turn chat)))
       (should (string-match-p "ORG-STEERING"
                               (gptel-agent-workers-test--data (gptel-agent-discussion--chat-fsm chat))))))))

(ert-deftest gptel-agent-discussion-wait-timeout-and-abort-cleanup ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (gptel-agent-workers-test--tool fsm "DiscussionControl" '(:action "wait"))
   (let* ((waiter (car (buffer-local-value 'gptel-agent-discussion--waiters parent)))
          (timer (nth 3 waiter)))
     (funcall (timer--function timer))
     (should (string-match-p "status: timeout" (gptel-agent-workers-test--data fsm)))
     (should-not (buffer-local-value 'gptel-agent-discussion--waiters parent)))
   (gptel-agent-workers-test--tool fsm "DiscussionControl" '(:action "wait"))
   (let* ((waiter (car (buffer-local-value 'gptel-agent-discussion--waiters parent)))
          (late-timeout (timer--function (nth 3 waiter))))
     (gptel-abort parent)
     (let ((count (length requests)))
       (funcall late-timeout)
       (should (= count (length requests))))
     (should-not (buffer-local-value 'gptel-agent-discussion--waiters parent)))))

(ert-deftest gptel-agent-discussion-multiple-approvals-remain-independent ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let ((second (gptel-agent-discussion-test--spawn fsm parent "Second request")) pending)
     (dolist (item (list chat second))
       (with-current-buffer (gptel-agent-discussion--chat-buffer item)
         (setq-local gptel-confirm-tool-calls 'auto))
       (gptel-agent-workers-test--tool (gptel-agent-discussion--chat-fsm item) "DiscussionMutation" nil)
       (let ((interaction (car (gptel-agent-discussion--chat-pending item))))
         (push interaction pending)))
     (let* ((first (car pending))
            (other (cadr pending))
            (ov (car (gptel-agent-discussion--interaction-overlays first))))
       (with-current-buffer (overlay-buffer ov)
         (gptel--accept-tool-calls (gptel-agent-discussion--interaction-calls first) ov))
       (should (= 1 executions))
       (should (gptel-agent-discussion--pending other))
       (let ((other-ov (car (gptel-agent-discussion--interaction-overlays other))))
         (should (eq (overlay-buffer other-ov)
                     (gptel-agent-discussion--chat-buffer
                      (gptel-agent-discussion--interaction-chat other))))
         (with-current-buffer (overlay-buffer other-ov)
           (gptel--accept-tool-calls (gptel-agent-discussion--interaction-calls other) other-ov)))
       (should (= 2 executions))))))

(ert-deftest gptel-agent-discussion-default-inherits-current-request-settings ()
  (let ((gptel-system-prompt "CURRENT-REQUEST-SYSTEM")
        (gptel-temperature 0.4)
        (gptel--known-presets nil))
    (gptel-make-preset 'discussion-parent :system "SAVED-PRESET-SYSTEM")
    (gptel-agent-discussion-test--with-chat
     ;; A saved preset and later buffer edits must not replace the settings
     ;; captured by the request that actually calls Discussion.
     (with-current-buffer parent
       (setq-local gptel--preset 'discussion-parent
                   gptel-system-prompt "LATER-BUFFER-SYSTEM"))
     (dolist (preset '(nil ""))
       (gptel-agent-workers-test--tool
        fsm "Discussion" (append '(:title "Inherited" :prompt "Fresh")
                                 (and preset (list :preset preset))))
       (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
       (let ((child (gptel-agent-discussion--chat-buffer chat)))
         (should (equal (buffer-local-value 'gptel-system-prompt child)
                        "CURRENT-REQUEST-SYSTEM"))
         (should (= (buffer-local-value 'gptel-temperature child) 0.4))
         (should (eq (buffer-local-value 'gptel-backend child)
                     (buffer-local-value 'gptel-backend parent)))
         (should (eq (buffer-local-value 'gptel-model child) 'test))
         (should (cl-find "DiscussionMutation" (buffer-local-value 'gptel-tools child)
                          :key #'gptel-tool-name :test #'equal)))))))

(ert-deftest gptel-agent-discussion-advertises-all-presets-per-request ()
  (let ((gptel--known-presets nil)
        (base-args (copy-tree (gptel-tool-args (gptel-get-tool "Discussion")))))
    (gptel-make-preset 'personal-coder :description "My implementation settings")
    (gptel-make-preset 'gptel-agent :description "Implementation agent")
    (gptel-agent-discussion-test--with-chat
     (let* ((tool (cl-find "Discussion" (plist-get (gptel-fsm-info fsm) :tools)
                          :key #'gptel-tool-name :test #'equal))
            (argument (cl-find "preset" (gptel-tool-args tool)
                               :key (lambda (arg) (plist-get arg :name)) :test #'equal))
            (description (plist-get argument :description)))
       (should (plist-get argument :optional))
       (should (string-match-p "personal-coder: My implementation settings" description))
       (should (string-match-p "gptel-agent: Implementation agent" description))
       (should (equal base-args (gptel-tool-args (gptel-get-tool "Discussion"))))
       (gptel-make-preset 'later-preset :description "Added after the request began")
       (setq chat (gptel-agent-discussion-test--spawn fsm parent))
       (let* ((child-fsm (gptel-agent-discussion--chat-fsm chat))
              (child-tool (cl-find "Discussion" (plist-get (gptel-fsm-info child-fsm) :tools)
                                   :key #'gptel-tool-name :test #'equal))
              (child-argument (cl-find "preset" (gptel-tool-args child-tool)
                                       :key (lambda (arg) (plist-get arg :name)) :test #'equal)))
         (should (string-match-p "later-preset: Added after the request began"
                                 (plist-get child-argument :description)))
         (should (= 1 (gptel-agent-workers-test--count
                       "Available gptel presets" (plist-get child-argument :description)))))
       (should-not (string-match-p "later-preset" (plist-get argument :description)))
       (should (equal base-args (gptel-tool-args (gptel-get-tool "Discussion"))))))))

(ert-deftest gptel-agent-discussion-planner-can-select-implementation-preset ()
  (gptel-agent-test--with-main-presets
    (gptel-agent-discussion-test--with-chat
     (gptel-agent-workers-test--answer fsm "Ready to plan")
     (with-current-buffer parent
       (gptel--apply-preset 'gptel-plan
                            (lambda (symbol value) (set (make-local-variable symbol) value)))
       (push (gptel-get-tool "Discussion") gptel-tools)
       (setq-local gptel-prompt-transform-functions
                   '(gptel-agent--localize-agent-tool gptel-agent-discussion--localize))
       (goto-char (point-max)) (insert "Hand this plan to an implementer") (gptel-send)
       (setq fsm gptel-agent-discussion--fsm))
     (gptel-agent-workers-test--tool
      fsm "Discussion" '(:title "Implement" :prompt "Self-contained plan" :preset "gptel-agent"))
     (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
     (let* ((child (gptel-agent-discussion--chat-buffer chat))
            (child-fsm (gptel-agent-discussion--chat-fsm chat))
            (child-tools (plist-get (gptel-fsm-info child-fsm) :tools))
            (agent (cl-find "Agent" child-tools :key #'gptel-tool-name :test #'equal)))
       (should (equal (buffer-local-value 'gptel-agent--current-agent child) "gptel-agent"))
       (should (equal (buffer-local-value 'gptel-system-prompt child)
                      (plist-get (gptel-get-preset 'gptel-agent) :system)))
       (should (cl-find "Write" child-tools :key #'gptel-tool-name :test #'equal))
       (should (member "executor" (append (plist-get (car (gptel-tool-args agent)) :enum) nil)))
       (should (equal (buffer-local-value 'gptel-agent--current-agent parent) "gptel-plan"))
       (should-not (cl-find "Write" (plist-get (gptel-fsm-info fsm) :tools)
                            :key #'gptel-tool-name :test #'equal))))))

(ert-deftest gptel-agent-discussion-preset-isolation-and-failed-creation ()
  (gptel-agent-discussion-test--with-chat
   (let ((gptel--known-presets (copy-tree gptel--known-presets)))
     (gptel-make-preset 'discussion-test-base :system "ISOLATED" :temperature 0.2)
     (gptel-make-preset 'discussion-test-preset
       :parents 'discussion-test-base :system '(:append "-SYSTEM") :tools '("AskUserQuestion"))
     (gptel-agent-workers-test--tool
      fsm "Discussion" '(:title "Custom" :prompt "Fresh" :preset "discussion-test-preset"))
     (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
     (let ((child (gptel-agent-discussion--chat-buffer chat)))
       (should (equal (buffer-local-value 'gptel-system-prompt child) "ISOLATED-SYSTEM"))
       (should (= (buffer-local-value 'gptel-temperature child) 0.2))
       (should (eq (buffer-local-value 'gptel-model child)
                   (buffer-local-value 'gptel-model parent)))
       (should-not (equal (buffer-local-value 'gptel-system-prompt parent) "ISOLATED-SYSTEM"))
       (should-not (buffer-local-value 'gptel-context child))
       (let ((child-fsm (gptel-agent-discussion--chat-fsm chat)))
         (should child-fsm)
         (gptel-agent-workers-test--tool child-fsm "DiscussionReport" '(:message "Custom preset report"))
         (should (string-match-p "Custom preset report"
                                 (gptel-agent-discussion-test--control fsm "wait"))))))
   (let ((count (length (buffer-local-value 'gptel-agent-discussion--children parent))))
     (gptel-agent-workers-test--tool
      fsm "Discussion" '(:title "Invalid" :prompt "Test" :preset "missing-preset"))
     (should (= count (length (buffer-local-value 'gptel-agent-discussion--children parent))))
     (should (string-match-p "Cannot find gptel preset" (gptel-agent-workers-test--data fsm))))))

(ert-deftest gptel-agent-discussion-cleanup-without-gptel-helper ()
  (with-temp-buffer
    (insert "Keep this transcript\nApproval prompt\n")
    (let* ((start (save-excursion (goto-char (point-min)) (forward-line) (point)))
           (prompt (make-overlay start (point-max)))
           (ov (make-overlay start start))
           (interaction (gptel-agent-discussion--make-interaction :overlays (list ov)))
           (cleanups 0))
      (overlay-put ov 'prompt (list prompt))
      (overlay-put ov 'previews (list (list (lambda () (cl-incf cleanups)))))
      (setq buffer-read-only t)
      (cl-letf (((symbol-function 'gptel--cleanup-tool-call-overlay) nil))
        (gptel-agent-discussion--cleanup-ui interaction)
        (gptel-agent-discussion--cleanup-ui interaction))
      (should (= cleanups 1))
      (should-not (overlay-buffer ov))
      (should-not (gptel-agent-discussion--interaction-overlays interaction))
      (should (equal (buffer-string) "Keep this transcript\n")))))

(ert-deftest gptel-agent-discussion-executor-approval-stays-local-and-settles ()
  (dolist (background '(nil t))
    (dolist (decision '(approve cancel))
      (gptel-agent-test--with-main-presets
        (gptel-agent-discussion-test--with-chat
         (gptel-make-preset 'discussion-test-executor
           :parents 'gptel-agent
           :prompt-transform-functions
           '(gptel-agent--localize-agent-tool gptel-agent-discussion--localize))
         (gptel-agent-workers-test--tool
          fsm "Discussion" '(:title "Executor work" :prompt "Implement" :preset "discussion-test-executor"))
         (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
         (let* ((child (gptel-agent-discussion--chat-buffer chat))
                (child-fsm (gptel-agent-discussion--chat-fsm chat)))
           (with-current-buffer child (setq-local gptel-confirm-tool-calls nil))
           (gptel-agent-workers-test--tool
            child-fsm "Agent" `(:subagent_type "executor" :description "Implement"
                                              :prompt "Bounded task" :background ,background))
           (let* ((run (car (hash-table-values gptel-agent--runs)))
                  (worker-fsm (gptel-agent--run-fsm run))
                  (worker (gptel-agent--run-child-buffer run)))
             (when background
               (gptel-agent-workers-test--answer child-fsm "Waiting for executor")
               (should (eq (gptel-fsm-state child-fsm) 'AGWT)))
             (gptel-agent-workers-test--tool
              worker-fsm "TodoWrite"
              '(:todos [(:content "Implement" :activeForm "Implementing" :status "in_progress")]))
             ;; An asynchronous tool must complete or cancel from the child
             ;; discussion's UI without leaving its worker gate waiting.
             (push (gptel-get-tool "DiscussionDeferred")
                   (plist-get (gptel-fsm-info worker-fsm) :tools))
             (with-current-buffer worker (setq-local gptel-confirm-tool-calls t))
             (gptel-agent-workers-test--tool worker-fsm "DiscussionDeferred" nil)
             (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
                    (calls (gptel-agent-discussion--interaction-calls pending))
                    (overlays (gptel-agent-discussion--interaction-overlays pending)))
               (should (eq (gptel-agent-discussion--interaction-source-fsm pending) worker-fsm))
               (should (= (length overlays) 1))
               (should (eq (overlay-buffer (car overlays)) child))
               (with-current-buffer parent
                 (should-not (cl-some (lambda (ov) (overlay-get ov 'gptel-tool))
                                      (overlays-in (point-min) (point-max)))))
               (with-current-buffer child
                 (if (eq decision 'approve)
                     (gptel--accept-tool-calls calls (car overlays))
                   (gptel--reject-tool-calls calls (car overlays))))
               (should-not (gptel-agent-discussion--chat-pending chat))
               (if (eq decision 'approve)
                   (progn
                     (should (functionp tool-callback))
                     (funcall tool-callback "APPROVED-OPERATION-FINISHED")
                     (should (eq (gptel-fsm-state worker-fsm) 'WAIT))
                     (with-current-buffer worker (setq-local gptel-confirm-tool-calls nil))
                     (gptel-agent-workers-test--tool
                      worker-fsm "TodoWrite"
                      '(:todos [(:content "Implement" :activeForm "Implementing" :status "completed")]))
                     (gptel-agent-workers-test--answer worker-fsm "EXECUTOR-FINISHED")
                     (should (eq (gptel-agent--run-state run) 'completed)))
                 (should-not tool-callback)
                 (should (eq (gptel-agent--run-state run) 'cancelled))))
             (gptel-agent-workers-test--wake child-fsm)
             (should (eq (gptel-fsm-state child-fsm) 'WAIT))
             (should (string-match-p (if (eq decision 'approve) "EXECUTOR-FINISHED" "aborted")
                                     (gptel-agent-workers-test--data child-fsm)))
             (gptel-agent-workers-test--answer child-fsm "Implementation complete")
             (should (eq (gptel-agent-discussion--chat-state chat) 'idle))
             (should-not (gptel-agent--workers-active (gptel-agent--workers-for child-fsm))))))))))

(ert-deftest gptel-agent-discussion-descendant-approval-runs-in-worker-buffer ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let* ((child (gptel-agent-discussion--chat-buffer chat))
          (child-fsm (gptel-agent-discussion--chat-fsm chat))
          (worker (generate-new-buffer " *discussion-test-worker*"))
          (worker-fsm (gptel-make-fsm :state 'TOOL))
          (run (gptel-agent--make-run :state 'waiting-for-tool :fsm worker-fsm))
          (tool (gptel-make-tool :name "DescendantMutation" :description "Worker action"
                                 :args nil :function (lambda ()
                                                       (should (eq (current-buffer) worker))
                                                       (cl-incf executions) "done")))
          (info (list :buffer worker :backend (plist-get (gptel-fsm-info fsm) :backend)
                      :position (with-current-buffer child (point-max-marker)) :context run)))
     (unwind-protect
         (progn
           (setf (gptel-fsm-info worker-fsm) info)
           (gptel--display-tool-calls (list (list tool nil #'ignore)) info)
           (let ((pending (car (gptel-agent-discussion--chat-pending chat))))
             (should pending)
             (should (eq worker-fsm (gptel-agent-discussion--interaction-source-fsm pending)))
             (let ((ov (car (gptel-agent-discussion--interaction-overlays pending))))
               (should (eq (overlay-buffer ov) child))
               (with-current-buffer child
                 (gptel--accept-tool-calls (gptel-agent-discussion--interaction-calls pending) ov)))
             (should (= executions 1))
             (should (gptel-agent-discussion--live-p chat child-fsm))))
       (kill-buffer worker)))))

(ert-deftest gptel-agent-discussion-reviews-stay-local-and-readable ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let* ((child (gptel-agent-discussion--chat-buffer chat))
          (file (list :path "/tmp/example.el" :id 1 :revision 1 :status 'pending))
          (queue (gptel-agent-review--make-queue :owner child :files (list file) :applied t)))
     (unwind-protect
         (progn
           (gptel-agent-review--refresh queue)
           (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
           (should (equal (gptel-agent-discussion--chat-review chat) '(("/tmp/example.el" 1))))
           (gptel-agent-review--refresh queue)
           (should (equal (gptel-agent-discussion--chat-review chat) '(("/tmp/example.el" 1))))
           (setf (plist-get file :revision) 2)
           (gptel-agent-review--refresh queue)
           (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
           (should (string-match-p "example.el.*2"
                                   (gptel-agent-discussion-test--control fsm "read" chat)))
           (gptel-agent-review--close queue)
           (should-not (gptel-agent-discussion--chat-review chat))
           (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent)))
       (when (buffer-live-p (gptel-agent-review--queue-buffer queue))
         (kill-buffer (gptel-agent-review--queue-buffer queue)))))))

(ert-deftest gptel-agent-discussion-monitoring-is-not-a-repetition-loop ()
  (with-temp-buffer
    (let ((gptel-agent-max-tool-repetitions 2))
      (dolist (action '("read" "wait" "list"))
        (dotimes (_ 4)
          (should-not (gptel-agent--detect-repetition
                       (list :name "DiscussionControl" :args (list :action action))))))
      (should-not gptel-agent--tool-call-history))))

(provide 'gptel-agent-discussion-test)
;;; gptel-agent-discussion-test.el ends here
