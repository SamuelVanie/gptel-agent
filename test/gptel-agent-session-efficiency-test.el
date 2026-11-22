;;; gptel-agent-session-efficiency-test.el --- Session save overhead -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent-session-test)

(ert-deftest gptel-agent-session-efficiency-child-steering-keeps-parent-clean ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (gptel-agent-session-mode 1)
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (let ((child (gptel-agent-discussion--chat-buffer chat)))
        (gptel-agent-workers-test--answer fsm "Parent finished")
        (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Child finished")
        (gptel-agent-session-save)
        (gptel-agent-discussion--send chat "Continue in the child" 'user)
        (should (gethash child gptel-agent-session--dirty))
        (should-not (gethash parent gptel-agent-session--dirty))
        (gptel-agent-session-save)
        ;; Attention events are stored with the child, not in the parent inbox.
        (gptel-agent-discussion--emit chat 'approval "Local approval notice")
        (should-not (gethash parent gptel-agent-session--dirty))
        (should-not (buffer-local-value 'gptel-agent-discussion--inbox parent))))))

(ert-deftest gptel-agent-session-efficiency-reports-still-save-owner-inbox ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (gptel-agent-session-mode 1)
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (gptel-agent-workers-test--answer fsm "Parent finished")
      (gptel-agent-session-save)
      ;; Exercise the real live child's reporting tool and request dispatcher.
      (gptel-agent-workers-test--tool
       (gptel-agent-discussion--chat-fsm chat) "DiscussionReport"
       '(:message "Report worth keeping"))
      (should (gethash parent gptel-agent-session--dirty))
      (gptel-agent-session-save)
      (should (equal "Report worth keeping"
                     (plist-get (car (plist-get (gptel-agent-session--read
                                                (buffer-local-value 'gptel-agent-session--id parent))
                                               :inbox))
                                :text))))))

(ert-deftest gptel-agent-session-efficiency-empty-inbox-operations-do-not-save ()
  (gptel-agent-session-test--with-store
    (let* ((buffer (gptel-agent-session-test--buffer " *empty-inbox*" "Unchanged"))
           (fsm (gptel-make-fsm :info (list :buffer buffer) :state 'WAIT))
           (write (symbol-function 'gptel-agent-session--write))
           (writes 0))
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (cl-letf (((symbol-function 'gptel-agent-session--write)
                 (lambda (record) (cl-incf writes) (funcall write record))))
        (with-current-buffer buffer
          (dotimes (_ 10)
            (should-not (gptel-agent-discussion--take))
            (gptel-agent-discussion--inject fsm)
            (gptel-agent-chat--inject fsm)))
        (should-not (gethash buffer gptel-agent-session--dirty))
        (gptel-agent-session-save)
        (should (= writes 0))))))

(ert-deftest gptel-agent-session-efficiency-sending-saves-new-turn-metadata ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (gptel-agent-session-mode 1)
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (let ((child (gptel-agent-discussion--chat-buffer chat)))
        (gptel-agent-workers-test--answer fsm "Parent finished")
        (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Child finished")
        (with-current-buffer child (goto-char (point-max)) (insert "New turn"))
        (gptel-agent-session-save)
        (should-not (gethash child gptel-agent-session--dirty))
        (with-current-buffer child (gptel-send))
        (should (= 2 (gptel-agent-discussion--chat-turn chat)))
        (should (gethash child gptel-agent-session--dirty))
        (gptel-agent-session-save)
        (let ((data (plist-get (gptel-agent-session--read
                               (buffer-local-value 'gptel-agent-session--id child)) :discussion)))
          (should (= 2 (plist-get data :turn)))
          (should (eq 'running (plist-get data :state))))))))

(ert-deftest gptel-agent-session-efficiency-identical-local-settings-stay-clean ()
  (gptel-agent-session-test--with-store
    (let ((buffer (gptel-agent-session-test--buffer " *same-setting*" "Transcript")))
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (with-current-buffer buffer
        (dotimes (_ 20)
          (setq-local gptel-model gptel-model)
          (setq default-directory default-directory)))
      (should-not (gethash buffer gptel-agent-session--dirty))
      (with-current-buffer buffer (setq-local gptel-model 'changed-model))
      (should (gethash buffer gptel-agent-session--dirty)))))

(ert-deftest gptel-agent-session-efficiency-global-settings-only-dirty-inheritors ()
  (gptel-agent-session-test--with-store
    (let ((inherited (gptel-agent-session-test--buffer " *inherited-setting*" "Inherit"))
          (overridden (gptel-agent-session-test--buffer " *local-setting*" "Override"))
          (original (default-value 'gptel-temperature)))
      (unwind-protect
          (progn
            (set-default 'gptel-temperature 0.2)
            (with-current-buffer inherited (kill-local-variable 'gptel-temperature))
            (with-current-buffer overridden (setq-local gptel-temperature 0.8))
            (gptel-agent-session-mode 1)
            (gptel-agent-session-save)
            (set-default 'gptel-temperature 0.4)
            (should (gethash inherited gptel-agent-session--dirty))
            (should-not (gethash overridden gptel-agent-session--dirty))
            (should (= 0.8 (buffer-local-value 'gptel-temperature overridden)))
            (gptel-agent-session-save)
            (set-default 'gptel-temperature (default-value 'gptel-temperature))
            (should (= 0 (hash-table-count gptel-agent-session--dirty))))
        (set-default 'gptel-temperature original)))))

(ert-deftest gptel-agent-session-efficiency-timer-only-runs-with-pending-work ()
  (gptel-agent-session-test--with-store
    (let ((buffer (gptel-agent-session-test--buffer " *timer-chat*" "Initial")))
      (gptel-agent-session-mode 1)
      (should (timerp gptel-agent-session--timer))
      (gptel-agent-session-save)
      (should-not gptel-agent-session--timer)
      (with-current-buffer buffer (goto-char (point-max)) (insert " edit"))
      (let ((timer gptel-agent-session--timer))
        (should (timerp timer))
        (with-current-buffer buffer
          (dotimes (_ 50) (gptel-agent-session--mark-dirty)))
        (should (eq timer gptel-agent-session--timer))
        (gptel-agent-session--drain t)
        (should-not gptel-agent-session--timer)
        (should-not (memq timer timer-list)))
      (gptel-agent-session--tick)
      (should-not gptel-agent-session--timer))))

(ert-deftest gptel-agent-session-efficiency-failed-autosaves-are-throttled ()
  (gptel-agent-session-test--with-store
    (let ((buffer (gptel-agent-session-test--buffer " *failure-backoff*" "Original"))
          (gptel-agent-session-save-interval 30)
          (now 100.0) (attempts 0) (fail t)
          (clock (symbol-function 'float-time))
          (write (symbol-function 'gptel-agent-session--write)))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional time) (if time (funcall clock time) now))))
        (gptel-agent-session-mode 1)
        (gptel-agent-session-save)
        (with-current-buffer buffer (goto-char (point-max)) (insert " edited"))
        (cl-letf (((symbol-function 'gptel-agent-session--write)
                   (lambda (record)
                     (cl-incf attempts)
                     (if fail (signal 'file-error '("Temporary storage failure"))
                       (funcall write record)))))
          (setq now 131.0)
          (gptel-agent-session--drain)
          (should (= attempts 1))
          (should (gethash buffer gptel-agent-session--dirty))
          (setq now 132.0)
          (dotimes (_ 20) (gptel-agent-session--drain))
          (should (= attempts 1))
          (setq now 161.0)
          (gptel-agent-session--drain)
          (should (= attempts 2))
          ;; An explicit save bypasses retry throttling after storage recovers.
          (setq now 162.0 fail nil)
          (gptel-agent-session--drain t)
          (should (= attempts 3))
          (should-not (gethash buffer gptel-agent-session--dirty))
          (should (equal "Original edited"
                         (plist-get (gptel-agent-session--read
                                     (buffer-local-value 'gptel-agent-session--id buffer)) :text))))))))

(provide 'gptel-agent-session-efficiency-test)
;;; gptel-agent-session-efficiency-test.el ends here
