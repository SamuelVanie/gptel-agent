;;; gptel-agent-supervision-test.el --- Host API contract -*- lexical-binding: t; -*-

(require 'gptel-agent-discussion-test)

(defmacro gptel-agent-supervision-test--with-chat (&rest body)
  "Run BODY with a supervised ordinary chat and real lifecycle handlers."
  (declare (indent 0))
  `(gptel-agent-discussion-test--with-chat
     (gptel-agent-workers-test--answer fsm "Initial turn complete")
     (let* (events approvals
            (scope (gptel-agent-supervise
                    parent
                    :on-state-change
                    (lambda (handle request event) (push (list handle request event) events))
                    :on-tool-approval
                    (lambda (handle request calls) (push (list handle request calls) approvals) t))))
       (unwind-protect
           (progn
             (with-current-buffer parent
               (goto-char (point-max)) (insert "Supervised task") (gptel-send)
               (setq fsm gptel-agent-discussion--fsm))
             ,@body)
         (gptel-agent-supervision-stop scope)))))

(ert-deftest gptel-agent-supervision-silent-discussion-releases-held-answer ()
  (gptel-agent-supervision-test--with-chat
    (setq chat (gptel-agent-discussion-test--spawn fsm parent))
    (gptel-agent-workers-test--answer fsm "Final answer")
    (should (eq (gptel-fsm-state fsm) 'AGWT))
    (should (member (list scope fsm 'waiting-discussions) events))
    (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Evidence")
    (accept-process-output nil 0.02)
    (should (eq (gptel-fsm-state fsm) 'DONE))
    (should (string-match-p "Final answer" (gptel-agent-discussion-test--buffer-text parent)))))

(ert-deftest gptel-agent-supervision-report-resumes-same-request ()
  (gptel-agent-supervision-test--with-chat
    (setq chat (gptel-agent-discussion-test--spawn fsm parent))
    (let ((child (gptel-agent-discussion--chat-fsm chat)))
      (gptel-agent-workers-test--answer fsm "Provisional answer")
      (gptel-agent-workers-test--tool child "DiscussionReport" '(:message "New evidence"))
      (accept-process-output nil 0.02)
      (should (eq (gptel-fsm-state fsm) 'WAIT))
      (should (string-match-p "New evidence" (gptel-agent-workers-test--data fsm)))
      (gptel-agent-workers-test--answer child "Finished")
      (gptel-agent-workers-test--answer fsm "Revised answer")
      (should (eq (gptel-fsm-state fsm) 'DONE)))))

(ert-deftest gptel-agent-supervision-narrowed-report-preserves-history ()
  (dolist (delivery '(flush settle))
    (dolist (stream '(nil t))
      (ert-info ((format "Delivery: %s, streaming: %s" delivery stream))
        (let ((gptel-agent-workers-test--stream stream))
          (gptel-agent-supervision-test--with-chat
           (setq chat (gptel-agent-discussion-test--spawn fsm parent))
           (gptel-agent-workers-test--answer fsm "Provisional answer" stream)
           (should (eq (gptel-fsm-state fsm) 'AGWT))
           (with-current-buffer parent
             (let ((before (buffer-string))
                   (end (point-max))
                   (count (length requests)))
               (narrow-to-region 2 12)
               (goto-char 4)
               (gptel-agent-discussion--emit chat 'report "NARROWED-ACTIVE-REPORT")
               (pcase delivery
                 ('flush (gptel-agent-discussion--flush parent))
                 ('settle (gptel-agent--supervision-settle scope)))
               (should (eq fsm gptel-agent-discussion--fsm))
               (should (eq (gptel-fsm-state fsm) 'WAIT))
               (should (= (1+ count) (length requests)))
               (should-not gptel-agent-discussion--inbox)
               (should (equal (list (point-min) (point-max) (point)) '(2 12 4)))
               (save-restriction
                 (widen)
                 (should (equal before (buffer-substring (point-min) end)))
                 (save-excursion
                   (goto-char end)
                   (search-forward "NARROWED-ACTIVE-REPORT")
                   (should-not (get-text-property (1- (point)) 'gptel))))
               (let ((data (gptel-agent-workers-test--data fsm)))
                 (should (string-match-p "Initial parent request" data))
                 (should (string-match-p "Provisional answer" data))
                 (should (= 1 (gptel-agent-workers-test--count "NARROWED-ACTIVE-REPORT" data))))
               (gptel-agent-discussion--flush parent)
               (gptel-agent--supervision-settle scope)
               (should (= (1+ count) (length requests)))))))))))

(ert-deftest gptel-agent-supervision-approval-is-owned-and-stop-is-idempotent ()
  (gptel-agent-supervision-test--with-chat
    (should-error (gptel-agent-supervise parent) :type 'user-error)
    (setq chat (gptel-agent-discussion-test--spawn fsm parent))
    (let ((child (gptel-agent-discussion--chat-fsm chat)))
      (with-current-buffer (gptel-agent-discussion--chat-buffer chat)
        (setq-local gptel-confirm-tool-calls t))
      (gptel-agent-workers-test--tool child "DiscussionMutation" nil)
      (should (eq (caar approvals) scope))
      (should (eq (cadar approvals) child))
      (should (gptel-agent-supervision-request-live-p scope child))
      (should (= executions 0))
      (let ((calls (nth 2 (car approvals))))
        (gptel-agent-supervision-stop scope)
        (gptel-agent-supervision-stop scope)
        (should-not (gptel-agent-discussion--active-p child))
        (should-not (gptel-agent-supervision-request-live-p scope child))
        ;; The discussion guard also rejects an already retained approval.
        (with-current-buffer (plist-get (gptel-fsm-info child) :buffer)
          (gptel--accept-tool-calls calls))
        (should (= executions 0))
        (setq approvals nil)
        (gptel--display-tool-calls calls (gptel-fsm-info child))
        (should-not approvals))
      (let ((new (gptel-agent-supervise parent)))
        (should-not (eq new scope))
        (gptel-agent-supervision-stop new)))))

(ert-deftest gptel-agent-supervision-retry-restores-report-delivery ()
  (gptel-agent-supervision-test--with-chat
    (setf (plist-get (gptel-fsm-info fsm) :error) "Unavailable"
          (plist-get (gptel-fsm-info fsm) :http-status) "503")
    (gptel--fsm-transition fsm 'ERRS)
    (should (eq (gptel-agent-supervision-retry scope fsm) fsm))
    (setf (plist-get (gptel-fsm-info fsm) :error) nil)
    (gptel--fsm-transition fsm 'WAIT)
    (setq chat (gptel-agent-discussion-test--spawn fsm parent))
    (let ((child (gptel-agent-discussion--chat-fsm chat)))
      (gptel-agent-workers-test--answer fsm "Provisional answer")
      (gptel-agent-workers-test--tool child "DiscussionReport" '(:message "Evidence after retry"))
      (accept-process-output nil 0.02)
      (should (eq (gptel-fsm-state fsm) 'WAIT))
      (should (string-match-p "Evidence after retry" (gptel-agent-workers-test--data fsm)))
      (gptel-agent-workers-test--answer child "Finished")
      (gptel-agent-workers-test--answer fsm "Recovered answer")
      (should (eq (gptel-fsm-state fsm) 'DONE)))
    (should-error (gptel-agent-supervision-retry scope fsm) :type 'user-error)))

(ert-deftest gptel-agent-supervision-unrelated-chat-keeps-independent-discussions ()
  (gptel-agent-supervision-test--with-chat
    (let ((supervised-fsm fsm) (supervised-parent parent))
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (gptel-agent-workers-test--answer fsm "Held answer")
      (gptel-agent-discussion-test--with-chat
        (let ((independent (gptel-agent-discussion-test--spawn fsm parent)))
          (gptel-agent-workers-test--answer fsm "Independent answer")
          (should (eq (gptel-fsm-state fsm) 'DONE))
          (should (eq (gptel-fsm-state supervised-fsm) 'AGWT))
          (gptel-agent-supervision-stop scope)
          (should (gptel-agent-discussion--active-p
                   (gptel-agent-discussion--chat-fsm independent)))
          (should (buffer-live-p supervised-parent)))))))

(ert-deftest gptel-agent-supervision-killing-owner-stops-descendants ()
  (gptel-agent-supervision-test--with-chat
    (setq chat (gptel-agent-discussion-test--spawn fsm parent))
    (let ((child (gptel-agent-discussion--chat-fsm chat))
          (buffer (gptel-agent-discussion--chat-buffer chat)))
      (unwind-protect
          (progn
            (kill-buffer parent)
            (should-not (gptel-agent-discussion--active-p child))
        (should-not (gptel-agent-supervision-request-live-p scope child))
            (should-error (gptel-agent-supervision-retry scope fsm) :type 'user-error))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest gptel-agent-supervision-raw-worker-reports-work-without-host ()
  (gptel-agent-workers-test--with-chat
    (with-current-buffer parent
      (setq-local gptel-agent-preset
                  '(:tools ("Discussion" "DiscussionControl"))
                  gptel-prompt-transform-functions
                  '(gptel-agent--localize-agent-tool gptel-agent-discussion--localize)))
    (let* ((run (gptel-agent-workers-test--launch fsm "test-worker"))
           (child (gptel-agent--run-fsm run))
           (buffer (gptel-agent--run-child-buffer run))
           (chat (gptel-agent-discussion-test--spawn child buffer))
           (discussion (gptel-agent-discussion--chat-fsm chat)))
      (unwind-protect
          (progn
            (gptel-agent-workers-test--tool
             discussion "DiscussionReport" '(:message "Raw evidence for worker"))
            (gptel-agent-workers-test--tool child "DiscussionControl" '(:action "list"))
            (should (gptel-agent--run-active-p run))
            (should (string-match-p "Raw evidence for worker" (gptel-agent-workers-test--data child)))
            (should (string-match-p "Raw evidence for worker" (gptel-agent-discussion-test--buffer-text buffer)))
            (should-not (string-match-p "Raw evidence for worker" (gptel-agent--run-response run)))
            (gptel-agent-workers-test--answer child "Final evidence")
            (should (eq (gptel-agent--run-state run) 'completed)))
        (when (buffer-live-p (gptel-agent-discussion--chat-buffer chat))
          (kill-buffer (gptel-agent-discussion--chat-buffer chat)))))))

(ert-deftest gptel-agent-supervision-narrowed-worker-report-keeps-buffer-ownership ()
  (gptel-agent-workers-test--with-chat
    (with-current-buffer parent
      (setq-local gptel-agent-preset '(:tools ("Discussion" "DiscussionControl"))
                  gptel-prompt-transform-functions
                  '(gptel-agent--localize-agent-tool gptel-agent-discussion--localize)))
    (let* ((run (gptel-agent-workers-test--launch fsm "test-worker"))
           (child (gptel-agent--run-fsm run))
           (buffer (gptel-agent--run-child-buffer run))
           (chat (gptel-agent-discussion-test--spawn child buffer)))
      (unwind-protect
          (progn
            (should (eq (marker-buffer (plist-get (gptel-fsm-info child) :position)) parent))
            (with-current-buffer parent (narrow-to-region 2 12) (goto-char 4))
            (with-current-buffer buffer
              (let ((before (buffer-string)) (end (point-max)))
                (narrow-to-region 2 12)
                (goto-char 4)
                (gptel-agent-discussion--emit chat 'report "NARROWED-WORKER-REPORT")
                ;; Deliver at the request boundary with the worker's real callback.
                (gptel-agent-discussion--before-wait child)
                (should (equal (list (point-min) (point-max) (point)) '(2 12 4)))
                (save-restriction
                  (widen)
                  (should (equal before (buffer-substring (point-min) end)))
                  (should (string-match-p "NARROWED-WORKER-REPORT"
                                          (buffer-substring end (point-max)))))))
            (with-current-buffer parent
              (should (equal (list (point-min) (point-max) (point)) '(2 12 4)))
              (save-restriction
                (widen)
                (should-not (string-match-p "NARROWED-WORKER-REPORT" (buffer-string)))))
            (should (string-match-p "NARROWED-WORKER-REPORT" (gptel-agent-workers-test--data child)))
            (should-not (string-match-p "NARROWED-WORKER-REPORT" (gptel-agent--run-response run))))
        (when (buffer-live-p (gptel-agent-discussion--chat-buffer chat))
          (kill-buffer (gptel-agent-discussion--chat-buffer chat)))))))

(provide 'gptel-agent-supervision-test)
;;; gptel-agent-supervision-test.el ends here
