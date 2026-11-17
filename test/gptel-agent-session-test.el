;;; gptel-agent-session-test.el --- Durable discussion sessions -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent-discussion-test)
(require 'gptel-agent-session)

(cl-defmacro gptel-agent-session-test--with-store (&body body)
  "Run BODY with an isolated session store, restoring global hooks afterward."
  (declare (indent 0))
  `(let ((gptel-agent-session-directory (make-temp-file "gptel-session-test-" t))
         (gptel-agent-session-mode nil)
         (gptel-agent-session-save-interval 0)
         (gptel-agent-session-idle-delay 3600)
         (gptel-agent-session-time-budget 10)
         (gptel-agent-session-batch-size 4)
         (gptel-agent-session--buffers (make-hash-table :test #'equal))
         (gptel-agent-session--dirty (make-hash-table :test #'eq))
         (gptel-agent-session--queue nil)
         (gptel-agent-session--queue-tail nil)
         (initial-buffers (buffer-list)))
     (unwind-protect
         (progn ,@body)
       (gptel-agent-session-mode -1)
       (let ((gptel-agent-session--restoring t))
         (dolist (buffer (buffer-list))
           (unless (memq buffer initial-buffers)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory gptel-agent-session-directory t))))

(defun gptel-agent-session-test--buffer (name text)
  "Make an ordinary gptel chat named NAME containing TEXT."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (text-mode)
      (setq-local gptel-backend
                  (gptel-make-openai "session-test" :key "unused" :models '(test))
                  gptel-model 'test)
      (gptel-mode 1)
      (insert text))
    buffer))

(defun gptel-agent-session-test--forget (&rest buffers)
  "Simulate leaving Emacs after a checkpoint of BUFFERS."
  (gptel-agent-session-mode -1)
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)))
  (clrhash gptel-agent-session--buffers)
  (clrhash gptel-agent-session--dirty)
  (setq gptel-agent-session--queue nil gptel-agent-session--queue-tail nil)
  (gptel-agent-session-mode 1))

(ert-deftest gptel-agent-session-roundtrip-nested-discussions-and-followup ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (gptel-agent-session-mode 1)
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (let* ((child (gptel-agent-discussion--chat-buffer chat))
             (child-fsm (gptel-agent-discussion--chat-fsm chat))
             (grandchat
              (progn
                (gptel-agent-discussion-test--spawn child-fsm child "Nested task")
                (when-let* ((pending (car (gptel-agent-discussion--chat-pending chat))))
                  (gptel--accept-tool-calls (gptel-agent-discussion--interaction-calls pending)))
                (car (buffer-local-value 'gptel-agent-discussion--children child))))
             (grandchild (gptel-agent-discussion--chat-buffer grandchat))
             (id (buffer-local-value 'gptel-agent-session--id parent))
             (discussion-id (gptel-agent-discussion--chat-id chat))
             (grand-id (gptel-agent-discussion--chat-id grandchat)))
        (gptel-agent-workers-test--answer fsm "Parent answer")
        (gptel-agent-workers-test--answer child-fsm "Child answer")
        (gptel-agent-workers-test--answer
         (gptel-agent-discussion--chat-fsm grandchat) "Grandchild answer")
        (gptel-agent-session-save)
        (gptel-agent-session-test--forget grandchild child parent)
        (let ((count (length requests)))
          (setq parent (gptel-agent-session-restore id))
          (should (= count (length requests))))
        (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent))
              child (gptel-agent-discussion--chat-buffer chat)
              grandchat (car (buffer-local-value 'gptel-agent-discussion--children child))
              grandchild (gptel-agent-discussion--chat-buffer grandchat))
        (should (equal discussion-id (gptel-agent-discussion--chat-id chat)))
        (should (equal grand-id (gptel-agent-discussion--chat-id grandchat)))
        (should (eq (gptel-agent-discussion--chat-owner chat) parent))
        (should (eq (gptel-agent-discussion--chat-owner grandchat) child))
        (should (eq (buffer-local-value 'gptel-agent-discussion--self child) chat))
        (should (eq (buffer-local-value 'gptel-agent-discussion--self grandchild) grandchat))
        (should (buffer-local-value 'gptel-mode child))
        (should-not (buffer-local-value 'gptel-agent-discussion--fsm child))
        (should (string-match-p "Child answer" (gptel-agent-discussion--read chat 0)))
        (should (string-match-p "Grandchild answer" (gptel-agent-discussion--read grandchat 0)))
        ;; The actual request-local DiscussionControl dispatcher must work again.
        (with-current-buffer parent
          (goto-char (point-max)) (insert "Continue this project") (gptel-send)
          (setq fsm gptel-agent-discussion--fsm))
        (should (string-match-p discussion-id
                                (gptel-agent-discussion-test--control fsm "list")))
        (should (string-match-p "queued"
                                (gptel-agent-discussion-test--control
                                 fsm "send" chat "PERSISTED-FOLLOWUP")))
        (gptel-agent-discussion--flush child)
        (setq child-fsm (gptel-agent-discussion--chat-fsm chat))
        (should (gptel-agent-discussion--active-p child-fsm))
        (should (= 2 (gptel-agent-discussion--chat-turn chat)))
        (should (string-match-p "Child answer" (gptel-agent-workers-test--data child-fsm)))
        (should (string-match-p "PERSISTED-FOLLOWUP" (gptel-agent-workers-test--data child-fsm)))
        (gptel-agent-workers-test--tool
         child-fsm "DiscussionReport" '(:message "Restored report"))
        (should (equal "Restored report"
                       (plist-get (car (buffer-local-value
                                        'gptel-agent-discussion--inbox parent)) :text)))))))

(ert-deftest gptel-agent-session-preserves-transcript-properties-drafts-and-inboxes ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (gptel-agent-session-mode 1)
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (let* ((child (gptel-agent-discussion--chat-buffer chat))
             (id (buffer-local-value 'gptel-agent-session--id parent))
             child-text child-bounds draft-point)
        (gptel-agent-workers-test--answer fsm "Parent complete")
        (gptel-agent-workers-test--answer
         (gptel-agent-discussion--chat-fsm chat) "Assistant response")
        (with-current-buffer child
          (setq draft-point (marker-position gptel-agent-discussion--draft-start))
          (goto-char (point-max)) (insert "UNSENT CHILD DRAFT")
          (setq child-text (buffer-string)
                child-bounds (gptel--get-buffer-bounds))
          (setq-local gptel-system-prompt "Saved system prompt"
                      gptel-temperature 0.42
                      gptel-agent-discussion--inbox
                      '((:kind message :text "Queued steering" :source user))))
        (with-current-buffer parent
          (setq-local gptel-agent-chat--inbox
                      '((:sender-id "chat-peer" :sender-name "Peer" :text "Peer message"))
                      gptel-agent-discussion--inbox
                      (list (list :id "event-saved" :discussion-id
                                  (gptel-agent-discussion--chat-id chat)
                                  :kind 'report :status "idle" :text "Queued report"))))
        (gptel-agent-session-save)
        (gptel-agent-session-test--forget child parent)
        (setq parent (gptel-agent-session-restore id)
              chat (car (buffer-local-value 'gptel-agent-discussion--children parent))
              child (gptel-agent-discussion--chat-buffer chat))
        (with-current-buffer child
          (should (equal (substring-no-properties child-text)
                         (buffer-substring-no-properties (point-min) (point-max))))
          (should (equal child-bounds (gptel--get-buffer-bounds)))
          (should (= draft-point (marker-position gptel-agent-discussion--draft-start)))
          (should (gptel-agent-discussion--draft-p))
          (should gptel-agent-discussion--paused)
          (should (equal "Saved system prompt" gptel-system-prompt))
          (should (= 0.42 gptel-temperature))
          (should (equal "Queued steering" (plist-get (car gptel-agent-discussion--inbox) :text))))
        (should (equal "Queued report"
                       (plist-get (car (buffer-local-value
                                        'gptel-agent-discussion--inbox parent)) :text)))
        (should (equal "Peer message"
                       (plist-get (car (buffer-local-value 'gptel-agent-chat--inbox parent)) :text)))
        (let ((count (length requests)))
          (gptel-agent-discussion--flush parent)
          (gptel-agent-discussion--flush child)
          (should (= count (length requests))))))))

(ert-deftest gptel-agent-session-restores-running-discussions-without-runtime ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (gptel-agent-session-mode 1)
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (let ((child (gptel-agent-discussion--chat-buffer chat))
            (id (buffer-local-value 'gptel-agent-session--id parent)))
        (should (eq 'running (gptel-agent-discussion--chat-state chat)))
        (gptel-agent-session-save)
        (gptel-agent-session-test--forget child parent)
        (let ((count (length requests)))
          (setq parent (gptel-agent-session-restore id))
          (should (= count (length requests))))
        (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
        (should (eq 'idle (gptel-agent-discussion--chat-state chat)))
        (should-not (gptel-agent-discussion--chat-fsm chat))
        (should-not (gptel-agent-discussion--chat-pending chat))
        (with-current-buffer (gptel-agent-discussion--chat-buffer chat)
          (should gptel-agent-discussion--paused)
          (should-not gptel-agent-discussion--fsm)
          (should-not gptel-agent-discussion--waiters)
          (should-not gptel-agent-discussion--timer)
          (should-not gptel-agent--supervised-run)
          (should-not gptel-agent--sessions))))))

(ert-deftest gptel-agent-session-keeps-closed-discussion-transcripts ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (gptel-agent-session-mode 1)
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (let ((child (gptel-agent-discussion--chat-buffer chat))
            (id (buffer-local-value 'gptel-agent-session--id parent)))
        (gptel-agent-workers-test--answer
         (gptel-agent-discussion--chat-fsm chat) "Closed transcript")
        (gptel-agent-discussion--close chat)
        (gptel-agent-session-save)
        (gptel-agent-session-test--forget child parent)
        (should-not (gptel-agent-discussion--chat-fsm chat))
        (should-not (gptel-agent-discussion--chat-result chat))
        (setq parent (gptel-agent-session-restore id)
              chat (car (buffer-local-value 'gptel-agent-discussion--children parent))
              child (gptel-agent-discussion--chat-buffer chat))
        (should (eq 'closed (gptel-agent-discussion--chat-state chat)))
        (should (buffer-live-p child))
        (should (buffer-local-value 'gptel-mode child))
        (should-not (buffer-local-value 'gptel-agent-discussion--self child))
        (should-not (gptel-agent-chat--main-p child))
        (should (string-match-p "Closed transcript" (gptel-agent-discussion--read chat 0)))))))

(ert-deftest gptel-agent-session-reopens-killed-child-without-duplicate-ownership ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (gptel-agent-session-mode 1)
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (let* ((child (gptel-agent-discussion--chat-buffer chat))
             (id (buffer-local-value 'gptel-agent-session--id parent))
             (child-id (buffer-local-value 'gptel-agent-session--id child))
             (discussion-id (gptel-agent-discussion--chat-id chat)))
        (gptel-agent-workers-test--answer fsm "Parent idle")
        (gptel-agent-workers-test--answer
         (gptel-agent-discussion--chat-fsm chat) "Archived child text")
        (gptel-agent-session-save)
        (kill-buffer child)
        (should-not (gptel-agent-discussion--chat-fsm chat))
        (should-not (gptel-agent-discussion--chat-result chat))
        (should-not (gethash child-id gptel-agent-session--buffers))
        (should (eq parent (gptel-agent-session-restore id)))
        (let ((children (buffer-local-value 'gptel-agent-discussion--children parent)))
          (should (= 1 (length children)))
          (setq chat (car children)))
        (should (equal discussion-id (gptel-agent-discussion--chat-id chat)))
        (should (eq parent (gptel-agent-discussion--chat-owner chat)))
        (should (buffer-live-p (gptel-agent-discussion--chat-buffer chat)))
        (should-not (eq child (gptel-agent-discussion--chat-buffer chat)))
        (should (= 1 (gptel-agent-discussion--chat-turn chat)))
        (should (eq 'idle (gptel-agent-discussion--chat-state chat)))
        (should (string-match-p "Archived child text" (gptel-agent-discussion--read chat 0)))
        (with-current-buffer parent
          (goto-char (point-max)) (insert "Continue the restored child") (gptel-send)
          (setq fsm gptel-agent-discussion--fsm))
        (should (string-match-p
                 "queued" (gptel-agent-discussion-test--control
                           fsm "send" chat "REOPENED-CHILD-FOLLOWUP")))
        (gptel-agent-discussion--flush (gptel-agent-discussion--chat-buffer chat))
        (let ((child-fsm (gptel-agent-discussion--chat-fsm chat)))
          (should (gptel-agent-discussion--active-p child-fsm))
          (should (= 2 (gptel-agent-discussion--chat-turn chat)))
          (should (string-match-p "Archived child text"
                                  (gptel-agent-workers-test--data child-fsm)))
          (should (string-match-p "REOPENED-CHILD-FOLLOWUP"
                                  (gptel-agent-workers-test--data child-fsm))))))))

(ert-deftest gptel-agent-session-kill-saves-diagnostics-before-releasing-request ()
  (dolist (outcome '(failed cancelled))
    (ert-info ((format "Outcome: %s" outcome))
      (gptel-agent-session-test--with-store
        (gptel-agent-discussion-test--with-chat
          (gptel-agent-session-mode 1)
          (setq chat (gptel-agent-discussion-test--spawn fsm parent))
          (let* ((child (gptel-agent-discussion--chat-buffer chat))
                 (child-fsm (gptel-agent-discussion--chat-fsm chat))
                 (root-id (buffer-local-value 'gptel-agent-session--id parent))
                 (child-id (buffer-local-value 'gptel-agent-session--id child))
                 (discussion-id (gptel-agent-discussion--chat-id chat))
                 (diagnostic (if (eq outcome 'failed)
                                 "Failure to preserve across kill" "Request cancelled.")))
            (gptel-agent-workers-test--answer fsm "Parent idle")
            (if (eq outcome 'failed)
                (progn
                  (setf (plist-get (gptel-fsm-info child-fsm) :error) diagnostic)
                  (gptel--fsm-transition child-fsm 'ERRS))
              (gptel-abort child))
            (should (eq outcome (gptel-agent-discussion--chat-state chat)))
            (should (equal diagnostic (gptel-agent-discussion--chat-result chat)))
            ;; There is no earlier checkpoint to mask a teardown ordering bug.
            (kill-buffer child)
            (should-not (gptel-agent-discussion--chat-fsm chat))
            (should-not (gptel-agent-discussion--chat-result chat))
            (gptel-agent-session-save)
            (let ((record (gptel-agent-session--read child-id)))
              (should (eq outcome (plist-get (plist-get record :discussion) :state)))
              (should (equal diagnostic (plist-get (plist-get record :discussion) :result))))
            (let ((count (length requests)))
              (should (eq parent (gptel-agent-session-restore root-id)))
              (should (= count (length requests))))
            (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
            (should (equal discussion-id (gptel-agent-discussion--chat-id chat)))
            (should (eq outcome (gptel-agent-discussion--chat-state chat)))
            (should-not (gptel-agent-discussion--chat-fsm chat))
            (should (string-match-p diagnostic (gptel-agent-discussion--read chat 0)))))))))

(ert-deftest gptel-agent-session-reconnects-live-child-when-parent-reopens ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (gptel-agent-session-mode 1)
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (let ((child (gptel-agent-discussion--chat-buffer chat))
            (id (buffer-local-value 'gptel-agent-session--id parent)))
        (gptel-agent-workers-test--answer fsm "Parent idle")
        (gptel-agent-workers-test--answer
         (gptel-agent-discussion--chat-fsm chat) "Child remains open")
        (gptel-agent-session-save)
        (kill-buffer parent)
        (should (buffer-live-p child))
        (should-not (gptel-agent-discussion--chat-owner chat))
        (setq parent (gptel-agent-session-restore id))
        (should (eq parent (gptel-agent-discussion--chat-owner chat)))
        (should (equal (list chat) (buffer-local-value 'gptel-agent-discussion--children parent)))
        (should (eq child (gptel-agent-discussion--chat-buffer chat)))
        (with-current-buffer parent
          (goto-char (point-max)) (insert "Ask the existing child") (gptel-send)
          (setq fsm gptel-agent-discussion--fsm))
        (should (string-match-p "queued"
                                (gptel-agent-discussion-test--control
                                 fsm "send" chat "RECONNECTED-FOLLOWUP")))
        (gptel-agent-discussion--flush child)
        (should (string-match-p "RECONNECTED-FOLLOWUP"
                                (gptel-agent-workers-test--data
                                 (gptel-agent-discussion--chat-fsm chat))))))))

(ert-deftest gptel-agent-session-cancelled-kill-keeps-registration-and-saving ()
  (gptel-agent-session-test--with-store
    (let ((buffer (gptel-agent-session-test--buffer " *cancelled-kill*" "Original"))
          id)
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (setq id (buffer-local-value 'gptel-agent-session--id buffer))
      (with-current-buffer buffer
        ;; The session query succeeds, then a later owner refuses this kill.
        (let ((kill-buffer-query-functions
               (list #'gptel-agent-session--before-kill (lambda () nil))))
          (should-not (kill-buffer buffer)))
        (should (eq buffer (gethash id gptel-agent-session--buffers)))
        (goto-char (point-max)) (insert " still editing"))
      (should (gethash buffer gptel-agent-session--dirty))
      (gptel-agent-session--drain t)
      (should (equal "Original still editing"
                     (plist-get (gptel-agent-session--read id) :text))))))

(ert-deftest gptel-agent-session-excludes-workers-and-review-asides ()
  (gptel-agent-session-test--with-store
    (let ((ordinary (gptel-agent-session-test--buffer " *saved-chat*" "Ordinary"))
          (worker (gptel-agent-session-test--buffer " *worker*" "Temporary worker"))
          (aside (gptel-agent-session-test--buffer " *aside*" "Review commentary")))
      (with-current-buffer worker
        (setq-local gptel-agent--supervised-run 'worker
                    gptel-agent--child-chat-p t))
      (with-current-buffer aside
        (setq-local gptel-agent-review-aside-mode t))
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (should (buffer-local-value 'gptel-agent-session--id ordinary))
      (should-not (buffer-local-value 'gptel-agent-session--id worker))
      (should-not (buffer-local-value 'gptel-agent-session--id aside))
      (should (= 1 (hash-table-count gptel-agent-session--buffers))))))

(ert-deftest gptel-agent-session-stores-names-without-live-capabilities ()
  (gptel-agent-session-test--with-store
    (let ((buffer (gptel-agent-session-test--buffer " *settings-chat*" "Transcript"))
          id)
      (with-current-buffer buffer
        (setq-local gptel-backend
                    (gptel-make-openai "session-private-backend"
                                       :key "SESSION-PRIVATE-API-KEY" :models '(test))
                    gptel-agent-access--grant '(:write t :commands t)
                    gptel-tools
                    (list (let ((tool (gptel--copy-tool (gptel-get-tool "Discussion"))))
                            (setf (gptel-tool-function tool)
                                  (lambda (&rest _) (buffer-name buffer)))
                            tool))))
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (setq id (buffer-local-value 'gptel-agent-session--id buffer))
      (let ((serialized
             (with-temp-buffer
               (insert-file-contents (gptel-agent-session--file id))
               (buffer-string))))
        (should-not (string-match-p "SESSION-PRIVATE-API-KEY" serialized))
        (should-not (string-match-p "#<" serialized)))
      (gptel-agent-session-test--forget buffer)
      (setq buffer (gptel-agent-session-restore id))
      (with-current-buffer buffer
        (should (equal "session-private-backend" (gptel-backend-name gptel-backend)))
        (should (equal '("Discussion") (mapcar #'gptel-tool-name gptel-tools)))
        (should (eq (gptel-tool-function (car gptel-tools))
                    (gptel-tool-function (gptel-get-tool "Discussion"))))
        (should-not gptel-agent-access--grant)))))

(ert-deftest gptel-agent-session-restores-preset-without-reapplying-it ()
  (gptel-agent-session-test--with-store
    (let ((buffer (gptel-agent-session-test--buffer " *preset-chat*" "Transcript"))
          (gptel--known-presets (copy-tree gptel--known-presets))
          (activations 0)
          id)
      (gptel-make-preset 'session-test-preset
        :system "Preset prompt" :temperature 0.2 :tools '("Read")
        :post (lambda () (cl-incf activations)))
      (with-current-buffer buffer
        (gptel--apply-preset
         'session-test-preset (lambda (symbol value) (set (make-local-variable symbol) value)))
        ;; Changes made after activation must survive alongside the preset name.
        (setq-local gptel-system-prompt "Customized prompt"
                    gptel-temperature 0.7
                    gptel-tools (list (gptel-get-tool "Discussion"))))
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (setq id (buffer-local-value 'gptel-agent-session--id buffer))
      (gptel-agent-session-test--forget buffer)
      (setq buffer (gptel-agent-session-restore id))
      (with-current-buffer buffer
        (should (local-variable-p 'gptel--preset))
        (should (eq gptel--preset 'session-test-preset))
        (should (equal gptel-system-prompt "Customized prompt"))
        (should (= gptel-temperature 0.7))
        (should (equal '("Discussion") (mapcar #'gptel-tool-name gptel-tools))))
      (should (= activations 1)))))

(ert-deftest gptel-agent-session-preset-only-changes-are-persisted ()
  (gptel-agent-session-test--with-store
    (let ((buffer (gptel-agent-session-test--buffer " *preset-change-chat*" "Transcript"))
          id)
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (setq id (buffer-local-value 'gptel-agent-session--id buffer))
      ;; A name need not be registered to be retained.  Clearing it is saved too.
      (dolist (preset '(session-first-preset session-second-preset nil))
        (with-current-buffer buffer (setq-local gptel--preset preset))
        (should (gethash buffer gptel-agent-session--dirty))
        (gptel-agent-session-save)
        (gptel-agent-session-test--forget buffer)
        (setq buffer (gptel-agent-session-restore id))
        (with-current-buffer buffer
          (should (local-variable-p 'gptel--preset))
          (should (eq gptel--preset preset)))))))

(ert-deftest gptel-agent-session-unchanged-chats-do-not-rewrite ()
  (gptel-agent-session-test--with-store
    (let ((buffer (gptel-agent-session-test--buffer " *quiet-chat*" "Saved once"))
          (write (symbol-function 'gptel-agent-session--write))
          (writes 0))
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (cl-letf (((symbol-function 'gptel-agent-session--write)
                 (lambda (record) (cl-incf writes) (funcall write record))))
        (gptel-agent-session--drain)
        (gptel-agent-session-save)
        (should (= 0 writes))
        (with-current-buffer buffer (goto-char (point-max)) (insert " updated"))
        (gptel-agent-session-save)
        (should (= 1 writes))
        (gptel-agent-session-save)
        (should (= 1 writes))))))

(ert-deftest gptel-agent-session-idle-saves-are-bounded-and-deduplicated ()
  (gptel-agent-session-test--with-store
    (let ((gptel-agent-session-batch-size 2)
          buffers)
      (dotimes (index 6)
        (push (gptel-agent-session-test--buffer (format " *batch-%d*" index) "Text") buffers))
      (gptel-agent-session-mode 1)
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (dotimes (_ 20) (gptel-agent-session--mark-dirty))))
      (should (= 6 (hash-table-count gptel-agent-session--dirty)))
      (gptel-agent-session--drain)
      (should (= 4 (hash-table-count gptel-agent-session--dirty)))
      (gptel-agent-session--drain)
      (should (= 2 (hash-table-count gptel-agent-session--dirty)))
      (gptel-agent-session--drain)
      (should (= 0 (hash-table-count gptel-agent-session--dirty)))
      (dolist (buffer buffers)
        (should (file-exists-p
                 (gptel-agent-session--file
                  (buffer-local-value 'gptel-agent-session--id buffer))))))))

(ert-deftest gptel-agent-session-idle-gate-throttle-and-streaming-fairness ()
  (gptel-agent-session-test--with-store
    (let ((gptel-agent-session-batch-size 2)
          (gptel-agent-session-idle-delay 5)
          (gptel-agent-session-save-interval 30)
          (now 100.0) (idle 0) (writes 0)
          (clock (symbol-function 'float-time))
          (write (symbol-function 'gptel-agent-session--write))
          buffers)
      (dotimes (index 3)
        (push (gptel-agent-session-test--buffer (format " *fair-%d*" index) "Saved") buffers))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional time) (if time (funcall clock time) now)))
                ((symbol-function 'current-idle-time) (lambda () (seconds-to-time idle)))
                ((symbol-function 'active-minibuffer-window) (lambda () nil))
                ((symbol-function 'gptel-agent-session--write)
                 (lambda (record) (cl-incf writes) (funcall write record))))
        (gptel-agent-session-mode 1)
        (gptel-agent-session-save)
        (setq writes 0 now 101.0)
        (dolist (buffer buffers)
          (with-current-buffer buffer (goto-char (point-max)) (insert " changed")))
        ;; Typing prevents background writes even though all chats are dirty.
        (gptel-agent-session--tick)
        (should (= 0 writes))
        (should (= 3 (hash-table-count gptel-agent-session--dirty)))
        ;; An idle user still does not cause writes inside the per-chat interval.
        (setq idle 10)
        (gptel-agent-session--tick)
        (should (= 0 writes))
        (setq now 131.0)
        (gptel-agent-session--tick)
        (should (= 2 writes))
        ;; Streaming into a just-saved chat must not put it ahead of the oldest.
        (let ((streaming (cl-find-if
                          (lambda (buffer) (not (gethash buffer gptel-agent-session--dirty)))
                          buffers))
              (waiting (cl-find-if
                        (lambda (buffer) (gethash buffer gptel-agent-session--dirty)) buffers)))
          (with-current-buffer streaming (goto-char (point-max)) (insert " stream chunk"))
          (gptel-agent-session--tick)
          (should (= 3 writes))
          (should-not (gethash waiting gptel-agent-session--dirty))
          (should (gethash streaming gptel-agent-session--dirty))
          ;; New chunks do not reset the due date, so a long response progresses.
          (setq now 161.0)
          (with-current-buffer streaming (goto-char (point-max)) (insert " next chunk"))
          (gptel-agent-session--tick)
          (should (= 4 writes))
          (should (= 0 (hash-table-count gptel-agent-session--dirty))))
        (gptel-agent-session--tick)
        (should (= 4 writes))))))

(ert-deftest gptel-agent-session-org-unicode-and-structured-bounds-roundtrip ()
  (gptel-agent-session-test--with-store
    (let ((buffer (generate-new-buffer " *org-session*"))
          id text bounds narrow)
      (with-current-buffer buffer
        (org-mode)
        (setq-local gptel-backend
                    (gptel-make-openai "session-org" :key "unused" :models '(test))
                    gptel-model 'test)
        (gptel-mode 1)
        (insert "* Discussion\nUtilisateur: café 漢字 🌲\n")
        (let ((start (point)))
          (insert "Réponse de l’assistant.\n")
          (put-text-property start (point) 'gptel 'response))
        (let ((start (point)))
          (insert "Analyse enregistrée.\n")
          (put-text-property start (point) 'gptel '(reasoning . "saved-signature")))
        (let ((start (point)))
          (insert "Résultat d’outil: ✓\n")
          (put-text-property start (point) 'gptel '(tool . "saved-tool-call")))
        (setq text (buffer-substring-no-properties (point-min) (point-max))
              bounds (gptel--get-buffer-bounds)
              narrow (cons 5 (- (point-max) 2)))
        (narrow-to-region (car narrow) (cdr narrow)))
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (setq id (buffer-local-value 'gptel-agent-session--id buffer))
      (gptel-agent-session-test--forget buffer)
      (setq buffer (gptel-agent-session-restore id))
      (with-current-buffer buffer
        (should (eq major-mode 'org-mode))
        (should (equal narrow (cons (point-min) (point-max))))
        (widen)
        (should (equal text (buffer-substring-no-properties (point-min) (point-max))))
        (should (equal bounds (gptel--get-buffer-bounds)))))))

(ert-deftest gptel-agent-session-failed-atomic-save-keeps-checkpoint-and-retries ()
  (gptel-agent-session-test--with-store
    (let* ((buffer (gptel-agent-session-test--buffer " *retry-chat*" "Before failure"))
           id)
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (setq id (buffer-local-value 'gptel-agent-session--id buffer))
      (with-current-buffer buffer (goto-char (point-max)) (insert " after edit"))
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (&rest _) (signal 'file-error '("Simulated disk failure")))))
        (ignore-errors (gptel-agent-session--drain t)))
      (should (equal "Before failure" (plist-get (gptel-agent-session--read id) :text)))
      (should (gethash buffer gptel-agent-session--dirty))
      (gptel-agent-session--drain t)
      (should (equal "Before failure after edit"
                     (plist-get (gptel-agent-session--read id) :text)))
      (should-not (gethash buffer gptel-agent-session--dirty)))))

(ert-deftest gptel-agent-session-clones-have-independent-identities ()
  (gptel-agent-session-test--with-store
    (let ((original (gptel-agent-session-test--buffer " *session-original*" "Original text"))
          clone original-id original-main-id)
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (setq original-id (buffer-local-value 'gptel-agent-session--id original)
            original-main-id (buffer-local-value 'gptel-agent-chat--id original)
            clone (with-current-buffer original (clone-buffer " *session-clone*")))
      (gptel-agent-session--track clone)
      (with-current-buffer clone
        (erase-buffer)
        (insert "Independent cloned text"))
      (gptel-agent-session-save)
      (should-not (equal original-id (buffer-local-value 'gptel-agent-session--id clone)))
      (should-not (equal original-main-id (buffer-local-value 'gptel-agent-chat--id clone)))
      (should (eq original (gethash original-id gptel-agent-session--buffers)))
      (should (equal "Original text" (plist-get (gptel-agent-session--read original-id) :text)))
      (should (equal "Independent cloned text"
                     (plist-get (gptel-agent-session--read
                                 (buffer-local-value 'gptel-agent-session--id clone)) :text))))))

(ert-deftest gptel-agent-session-locks-when-ordinary-file-locking-disabled ()
  (gptel-agent-session-test--with-store
    (let ((create-lockfiles nil)
          (buffer (gptel-agent-session-test--buffer " *session-no-file-locks*" "Durable")))
      (gptel-agent-session-mode 1)
      (should (eq t (file-locked-p gptel-agent-session--lock)))
      (should (= 1 (gptel-agent-session-save)))
      (should (file-exists-p (gptel-agent-session--file
                             (buffer-local-value 'gptel-agent-session--id buffer))))
      (let ((lock gptel-agent-session--lock))
        (gptel-agent-session-mode -1)
        (should-not (file-locked-p lock))))))

(ert-deftest gptel-agent-session-corrupt-child-does-not-alter-live-owner ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (gptel-agent-session-mode 1)
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (let* ((child (gptel-agent-discussion--chat-buffer chat))
             (root-id (buffer-local-value 'gptel-agent-session--id parent))
             (child-id (buffer-local-value 'gptel-agent-session--id child))
             (list-buffer (generate-new-buffer " *session-corrupt-list*")))
        (gptel-agent-workers-test--answer fsm "Parent done")
        (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Child done")
        (gptel-agent-session-save)
        (kill-buffer child)
        (with-current-buffer list-buffer
          (gptel-agent-discussions-mode)
          (setq gptel-agent-discussion--list-owner parent))
        (with-current-buffer parent
          (setq gptel-agent-discussion--list-buffer list-buffer))
        (let ((record (gptel-agent-session--read child-id))
              (before (copy-sequence (buffer-local-value 'gptel-agent-discussion--children parent)))
              (registered (hash-table-count gptel-agent-session--buffers)))
          (setf (plist-get (plist-get record :discussion) :turn) "invalid-turn")
          (gptel-agent-session--write record)
          (should-error (gptel-agent-session-restore root-id))
          (should (equal before (buffer-local-value 'gptel-agent-discussion--children parent)))
          (should (= registered (hash-table-count gptel-agent-session--buffers)))
          (should (eq parent (gptel-agent-discussion--chat-owner chat)))
          (should-not (buffer-live-p (gethash child-id gptel-agent-session--buffers))))))))

(ert-deftest gptel-agent-session-mode-switch-and-rename-keep-autosaving ()
  (gptel-agent-session-test--with-store
    (let ((buffer (gptel-agent-session-test--buffer " *mode-switch*" "Original")) id)
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (with-current-buffer buffer
        (setq id gptel-agent-session--id)
        (org-mode)
        (gptel-mode 1)
        (rename-buffer " *renamed-session*")
        (gptel-agent-session--drain t)
        (should (equal " *renamed-session*" (plist-get (gptel-agent-session--read id) :name)))
        (should (eq 'org-mode (plist-get (gptel-agent-session--read id) :mode)))
        (goto-char (point-max)) (insert " after mode change")
        (should (gethash buffer gptel-agent-session--dirty))
        (gptel-agent-session--drain t)
        (should (equal "Original after mode change"
                       (plist-get (gptel-agent-session--read id) :text)))))))

(provide 'gptel-agent-session-test)
;;; gptel-agent-session-test.el ends here
