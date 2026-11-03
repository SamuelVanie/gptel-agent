;;; gptel-agent-discussion-test.el --- Managed chat lifecycle -*- lexical-binding: t; -*-

(require 'ert)
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
                         gptel-prompt-transform-functions '(gptel-agent-discussion--localize)
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
           (gptel-agent-discussion--flush parent)
           (setq fsm (buffer-local-value 'gptel-agent-discussion--fsm parent))
           (should (gptel-agent-discussion--active-p fsm))
           (should (= 1 (gptel-agent-workers-test--count "CHILD-COMPLETION"
                                                         (gptel-agent-workers-test--data fsm))))
           (should (string-match-p "CHILD-COMPLETION" (gptel-agent-discussion-test--buffer-text parent)))
           (should (string-match-p "queued"
                                   (gptel-agent-discussion-test--control fsm "send" chat "FOLLOW-UP")))
           (gptel-agent-discussion--flush child)
           (should (= 2 (gptel-agent-discussion--chat-turn chat)))
           (let ((data (gptel-agent-workers-test--data (gptel-agent-discussion--chat-fsm chat))))
             (should (string-match-p "Child request" data))
             (should (string-match-p "CHILD-COMPLETION" data))
             (should (string-match-p "FOLLOW-UP" data)))))))))

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
   (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Child done")
   (let ((count (length requests)))
     (gptel-agent-discussion--flush parent)
     (should (= count (length requests)))
     (should (buffer-local-value 'gptel-agent-discussion--inbox parent)))
   (with-current-buffer parent (goto-char (point-max)) (gptel-send)
                        (setq fsm gptel-agent-discussion--fsm))
   (should (string-match-p "UNSENT-DRAFT" (gptel-agent-workers-test--data fsm)))
   (should (string-match-p "Child done" (gptel-agent-workers-test--data fsm)))
   (with-current-buffer (gptel-agent-discussion--chat-buffer chat)
     (goto-char (point-max)) (insert "CHILD-DRAFT"))
   (gptel-agent-discussion-test--control fsm "send" chat "Parent followup")
   (let ((count (length requests)))
     (gptel-agent-discussion--flush (gptel-agent-discussion--chat-buffer chat))
     (should (= count (length requests))))))

(ert-deftest gptel-agent-discussion-user-sends-ordinary-followup ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let ((child (gptel-agent-discussion--chat-buffer chat)))
     (with-current-buffer child (should-error (gptel-send) :type 'user-error))
     (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "First answer")
     (with-current-buffer child (goto-char (point-max)) (insert "Direct user followup") (gptel-send))
     (should (= 2 (gptel-agent-discussion--chat-turn chat)))
     (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Second answer")
     (should (= 2 (length (buffer-local-value 'gptel-agent-discussion--inbox parent)))))))

(ert-deftest gptel-agent-discussion-question-one-winner-and-stale-answer ()
  (dolist (user-first '(nil t))
    (gptel-agent-discussion-test--with-chat
     (setq chat (gptel-agent-discussion-test--spawn fsm parent))
     (let ((child-fsm (gptel-agent-discussion--chat-fsm chat)))
       (gptel-agent-workers-test--tool
        child-fsm "AskUserQuestion"
        '(:questions [(:question "Which color?" :choices [(:value "Blue") (:value "Red")])]))
       (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
              (id (gptel-agent-discussion--interaction-id pending))
              (ov (car (gptel-agent-discussion--interaction-overlays pending)))
              (ui-callback (overlay-get ov 'gptel-ask--callback))
              (count (length requests)))
         (should (eq (gptel-agent-discussion--chat-state chat) 'waiting-for-question))
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

(ert-deftest gptel-agent-discussion-approval-forwarding-never-auto-executes ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let* ((child (gptel-agent-discussion--chat-buffer chat))
          (child-fsm (gptel-agent-discussion--chat-fsm chat)))
     (with-current-buffer child (setq-local gptel-confirm-tool-calls 'auto))
     (gptel-agent-workers-test--tool child-fsm "DiscussionMutation" nil)
     (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
            (id (gptel-agent-discussion--interaction-id pending))
            (calls (gptel-agent-discussion--interaction-calls pending)))
       (should (= 0 executions))
       (should (= 1 (length (gptel-agent-discussion--interaction-overlays pending))))
       (should (string-match-p "displayed" (gptel-agent-discussion-test--control
                                            fsm "request_approval" chat nil id)))
       (should (= 0 executions))
       (should (= 2 (length (gptel-agent-discussion--interaction-overlays pending))))
       (let ((overlays (copy-sequence (gptel-agent-discussion--interaction-overlays pending))))
         ;; Native user acceptance.  A stale copy of the other UI cannot run it again.
         (with-current-buffer parent (gptel--accept-tool-calls calls (car overlays)))
         (should (= 1 executions))
         (gptel--accept-tool-calls calls)
         (should (= 1 executions))
         (should (cl-every (lambda (ov) (not (overlay-buffer ov))) overlays)))
       (should (string-prefix-p "Error:" (gptel-agent-discussion-test--control
                                          fsm "request_approval" chat nil id)))))))

(ert-deftest gptel-agent-discussion-rejection-close-and-late-callbacks ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let* ((child (gptel-agent-discussion--chat-buffer chat))
          (child-fsm (gptel-agent-discussion--chat-fsm chat)))
     (with-current-buffer child (setq-local gptel-confirm-tool-calls 'auto))
     (gptel-agent-workers-test--tool child-fsm "DiscussionMutation" nil)
     (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
            (calls (gptel-agent-discussion--interaction-calls pending)))
       (gptel-agent-discussion-test--control
        fsm "reject" chat "Unneeded" (gptel-agent-discussion--interaction-id pending))
       (gptel--accept-tool-calls calls)
       (should (= 0 executions)))
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
   (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "WAIT-REPORT")
   (should (string-match-p "WAIT-REPORT" (gptel-agent-discussion-test--control fsm "read" chat)))
   (should (string-match-p "WAIT-REPORT" (gptel-agent-discussion-test--control fsm "wait")))
   (should (string-match-p "no_eligible_work" (gptel-agent-discussion-test--control fsm "wait")))
   (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))
   (should (string-prefix-p "Error:" (gptel-agent-discussion-test--control fsm "wait" nil nil nil ["foreign"])))))

(ert-deftest gptel-agent-discussion-waiter-wakes-from-completion ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (gptel-agent-workers-test--tool fsm "DiscussionControl" '(:action "wait"))
   (should (eq (gptel-fsm-state fsm) 'TOOL))
   (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "WAITED-COMPLETION")
   (gptel-agent-discussion--flush parent)
   (should (eq (gptel-fsm-state fsm) 'WAIT))
   (should (= 1 (gptel-agent-workers-test--count "WAITED-COMPLETION" (gptel-agent-workers-test--data fsm))))
   (should-not (buffer-local-value 'gptel-agent-discussion--waiters parent))))

(ert-deftest gptel-agent-discussion-owner-abort-pauses-notifications-child-survives ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (gptel-abort parent)
   (should (gptel-agent-discussion--live-p chat (gptel-agent-discussion--chat-fsm chat)))
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
           (kill-buffer parent)
           (should-not (gptel-agent-discussion--chat-owner chat))
           (should (gptel-agent-discussion--live-p chat (gptel-agent-discussion--chat-fsm chat)))
           (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Detached completion")
           (should (buffer-live-p child)))
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
       (with-current-buffer parent (gptel--accept-tool-calls (list edited)))
       (should (= executions 1))
       (gptel--accept-tool-calls (list edited))
       (gptel--accept-tool-calls (list call))
       (should (= executions 1))
        (should-not (gptel-agent-discussion--chat-pending chat))))))

(ert-deftest gptel-agent-discussion-forwarded-cancel-stops-child-not-parent ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let* ((child (gptel-agent-discussion--chat-buffer chat))
          (child-fsm (gptel-agent-discussion--chat-fsm chat)))
     (with-current-buffer child (setq-local gptel-confirm-tool-calls 'auto))
     (gptel-agent-workers-test--tool child-fsm "DiscussionMutation" nil)
     (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
            (calls (gptel-agent-discussion--interaction-calls pending)))
       (gptel-agent-discussion-test--control
        fsm "request_approval" chat nil (gptel-agent-discussion--interaction-id pending))
       (let ((ov (cl-find parent (gptel-agent-discussion--interaction-overlays pending) :key #'overlay-buffer)))
         (with-current-buffer parent (gptel--reject-tool-calls calls ov)))
       (should (eq (gptel-fsm-state child-fsm) 'ABRT))
       (should (gptel-agent-discussion--active-p fsm))
       (should-not (gptel-agent-discussion--chat-pending chat))
       (gptel--accept-tool-calls calls)
       (should (= executions 0))
       (should (eq (plist-get (car (buffer-local-value 'gptel-agent-discussion--inbox parent)) :kind) 'cancelled))))))

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
         (push interaction pending)
         (gptel-agent-discussion-test--control
          fsm "request_approval" item nil (gptel-agent-discussion--interaction-id interaction))))
     (let* ((first (car pending))
            (other (cadr pending))
            (ov (cl-find parent (gptel-agent-discussion--interaction-overlays first) :key #'overlay-buffer)))
       (gptel--accept-tool-calls (gptel-agent-discussion--interaction-calls first) ov)
       (should (= 1 executions))
       (should (gptel-agent-discussion--pending other))
       (should (cl-find parent (gptel-agent-discussion--interaction-overlays other) :key #'overlay-buffer))
       (gptel--accept-tool-calls (gptel-agent-discussion--interaction-calls other))
       (should (= 2 executions))))))

(ert-deftest gptel-agent-discussion-preset-isolation-and-failed-creation ()
  (gptel-agent-discussion-test--with-chat
   (let ((gptel--known-presets (copy-tree gptel--known-presets)))
     (gptel-make-preset 'discussion-test-preset :system "ISOLATED-SYSTEM" :tools '("AskUserQuestion"))
     (gptel-agent-workers-test--tool
      fsm "Discussion" '(:title "Custom" :prompt "Fresh" :preset "discussion-test-preset"))
     (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
     (let ((child (gptel-agent-discussion--chat-buffer chat)))
       (should (equal (buffer-local-value 'gptel-system-prompt child) "ISOLATED-SYSTEM"))
       (should-not (equal (buffer-local-value 'gptel-system-prompt parent) "ISOLATED-SYSTEM"))
       (should-not (buffer-local-value 'gptel-context child))
       (should (gptel-agent-discussion--chat-fsm chat))))
   (let ((count (length (buffer-local-value 'gptel-agent-discussion--children parent))))
     (gptel-agent-workers-test--tool
      fsm "Discussion" '(:title "Invalid" :prompt "Test" :preset "missing-preset"))
     (should (= count (length (buffer-local-value 'gptel-agent-discussion--children parent))))
     (should (string-match-p "Cannot find gptel preset" (gptel-agent-workers-test--data fsm))))))

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
             (gptel-agent-discussion-test--control
              fsm "request_approval" chat nil (gptel-agent-discussion--interaction-id pending))
             (gptel--accept-tool-calls (gptel-agent-discussion--interaction-calls pending))
             (should (= executions 1))
             (should (gptel-agent-discussion--live-p chat child-fsm))))
       (kill-buffer worker)))))

(ert-deftest gptel-agent-discussion-review-events-coalesce-and-expire ()
  (gptel-agent-discussion-test--with-chat
   (setq chat (gptel-agent-discussion-test--spawn fsm parent))
   (let* ((child (gptel-agent-discussion--chat-buffer chat))
          (file (list :path "/tmp/example.el" :id 1 :revision 1 :status 'pending))
          (queue (gptel-agent-review--make-queue :owner child :files (list file) :applied t)))
     (unwind-protect
         (progn
           (gptel-agent-review--refresh queue)
           (should (= 1 (length (buffer-local-value 'gptel-agent-discussion--inbox parent))))
           (gptel-agent-review--refresh queue)
           (should (= 1 (length (buffer-local-value 'gptel-agent-discussion--inbox parent))))
           (setf (plist-get file :revision) 2)
           (gptel-agent-review--refresh queue)
           (should (= 1 (length (buffer-local-value 'gptel-agent-discussion--inbox parent))))
           (should (string-match-p "revision 2" (plist-get (car (buffer-local-value 'gptel-agent-discussion--inbox parent)) :text)))
           (gptel-agent-review--close queue)
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
