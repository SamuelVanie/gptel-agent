;;; gptel-agent-indirect-chat-test.el --- Shared chat views -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent-chat-test)
(require 'gptel-agent-session-test)

(ert-deftest gptel-agent-indirect-chat-closing-view-keeps-owned-work ()
  (gptel-agent-chat-test--with-chats
    (let* ((run (gptel-agent-workers-test--launch sender-fsm))
           (gate (gptel-agent--workers-for sender-fsm))
           (scope (gptel-agent-supervise sender))
           (chat (gptel-agent-discussion-test--spawn sender-fsm sender))
           (sessions (buffer-local-value 'gptel-agent--sessions sender))
           (view (with-current-buffer sender
                   (clone-indirect-buffer " *indirect-owned-work*" nil))))
      (unwind-protect
          (progn
            (gptel-agent--supervision-register sender-fsm)
            (kill-buffer view)
            (should-not (gptel-agent--workers-closed gate))
            (should-not (gptel-agent--supervision-stopped scope))
            (should (eq sessions (buffer-local-value 'gptel-agent--sessions sender)))
            (should (gptel-agent--run-active-p run))
            (should (eq sender (gptel-agent-discussion--chat-owner chat)))
            (should (gptel-agent-discussion--active-p (gptel-agent-discussion--chat-fsm chat)))
            (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "Worker survived")
            (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat)
                                              "Discussion survived")
            (should (eq (gptel-agent--run-state run) 'completed))
            (should (gptel-agent-discussion--active-p sender-fsm)))
        (when (buffer-live-p view) (kill-buffer view))))))

(ert-deftest gptel-agent-indirect-chat-closing-view-keeps-process-actions ()
  (let ((source (generate-new-buffer " *indirect-process-owner*"))
        (directory (make-temp-file "indirect-process-gate-" t))
        view foreground background result send-result eof-result wait-result)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq-local gptel-mode t)
            (setq foreground
                  (gptel-agent--execute-bash
                   (lambda (value) (setq result value))
                   (format "while [ ! -f %s ]; do sleep 0.01; done; printf 'survived'"
                           (shell-quote-argument (expand-file-name "release" directory)))
                   "Read reply")
                  background (gptel-agent--execute-bash #'ignore "cat" "Read reply" t))
            (let ((job (process-get background 'gptel-agent-job)))
              (gptel-agent--process-send-input
               (lambda (value) (setq send-result value)) job "survived\n" 5000 nil)
              (gptel-agent--process-close-input
               (lambda (value) (setq eof-result value)) job 5000 nil)
              (gptel-agent--process-wait
               (lambda (value) (setq wait-result value)) job nil nil nil nil 5000 nil))
            (setq view (clone-indirect-buffer " *indirect-process-view*" nil)))
          (kill-buffer view)
          (should (process-live-p foreground))
          (should (process-live-p background))
          (should-not result)
          (with-temp-file (expand-file-name "release" directory))
          (let ((deadline (+ (float-time) 5)))
            (while (and (< (float-time) deadline) (or (not result) (not wait-result)))
              (accept-process-output nil 0.02)))
          (should (string-match-p "survived" result))
          (should (string-match-p "send_reason: sent" send-result))
          (should (string-match-p "eof_reason: closed" eof-result))
          (should (string-match-p "survived" wait-result)))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p source) (kill-buffer source))
      (delete-directory directory t))))

(ert-deftest gptel-agent-indirect-chat-cannot-send-a-second-shared-transcript ()
  (gptel-agent-chat-test--with-chats
    (let ((view (with-current-buffer sender
                  (clone-indirect-buffer " *indirect-request-view*" nil)))
          (count (length requests)))
      (unwind-protect
          (progn
            (with-current-buffer view
              (should (string-match-p
                       (regexp-quote (buffer-name sender))
                       (error-message-string (should-error (gptel-send) :type 'user-error))))
              (should-error (gptel-request "Another request") :type 'user-error))
            (with-current-buffer sender
              (should-error (gptel-request "Another request" :buffer view) :type 'user-error)
              (should-error (gptel-request nil :buffer (buffer-name view))
                            :type 'user-error)
              (should-error (gptel-request "Another request"
                              :position (with-current-buffer view (point-marker)))
                            :type 'user-error))
            (should (= count (length requests)))
            (should-not (gptel-agent-chat--main-p view))
            (should-not (string-match-p "indirect-request-view"
                                        (gptel-agent-chat--list recipient nil))))
        (when (buffer-live-p view) (kill-buffer view))))))

(ert-deftest gptel-agent-indirect-chat-guard-keeps-optional-prompt-contract ()
  (gptel-agent-chat-test--with-chats
    (gptel-agent-workers-test--answer sender-fsm "Finished")
    (with-current-buffer sender
      (goto-char (point-max))
      (insert "A new prompt")
      (should (gptel-fsm-p (gptel-request))))))

(ert-deftest gptel-agent-indirect-chat-ordinary-source-buffer-remains-usable ()
  (with-temp-buffer
    (insert "A source excerpt")
    (let ((view (clone-indirect-buffer " *indirect-source-excerpt*" nil)))
      (unwind-protect
          (with-current-buffer view
            (should-not buffer-read-only)
            (should-not (gptel-agent-chat--guard-indirect-request)))
        (kill-buffer view)))))

(ert-deftest gptel-agent-indirect-chat-inline-edits-only-writable-chats ()
  (skip-unless (require 'gptel-inline nil t))
  (gptel-agent-session-test--with-store
    (let* ((gptel-default-mode 'text-mode)
           (gptel-use-header-line t)
           (chat (gptel-agent-session-test--buffer " *inline-chat*" "Prior turn"))
           (gptel-backend (buffer-local-value 'gptel-backend chat))
           (gptel-model (buffer-local-value 'gptel-model chat)))
      (save-window-excursion
        (switch-to-buffer chat)
        (let ((prompt (gptel-inline nil (buffer-name chat))))
          (should (eq (buffer-base-buffer prompt) chat))
          (with-current-buffer prompt
            (insert "Inline question")
            (should-error (gptel-request) :type 'user-error)
            (gptel-inline-append))
          (should-not (buffer-live-p prompt))
          (should (string-suffix-p "Inline question" (with-current-buffer chat (buffer-string)))))
        (with-current-buffer chat (setq buffer-read-only t))
        (with-current-buffer (gptel-inline nil (buffer-name chat))
          (should-error (insert "A locked edit") :type 'buffer-read-only))))))

(ert-deftest gptel-agent-indirect-chat-preserves-base-session-and-title-request ()
  (gptel-agent-session-test--with-store
    (let* ((source (gptel-agent-session-test--buffer " *indirect-session-owner*" "Original"))
           (title-request (generate-new-buffer " *indirect-title-request*"))
           view id)
      (unwind-protect
          (progn
            (gptel-agent-session-mode 1)
            (gptel-agent-session-save)
            (with-current-buffer source
              (setq id gptel-agent-session--id
                    gptel-agent-session--title-request title-request)
              (setq view (clone-indirect-buffer " *indirect-session-view*" nil)))
            (with-current-buffer view
              (should buffer-read-only)
              (should-not gptel-agent-session--id)
              (should-not gptel-agent-session--title-request)
              (should-error (insert "Unowned edit") :type 'buffer-read-only))
            (should-not (gptel-agent-session--eligible-p view))
            (gptel-agent-session--track view)
            (should (= 1 (hash-table-count gptel-agent-session--buffers)))
            (with-current-buffer source (goto-char (point-max)) (insert " updated"))
            (should (equal (with-current-buffer view (buffer-string)) "Original updated"))
            (gptel-agent-session-save)
            ;; Explicitly unlocking the view still edits the base transcript.  Only
            ;; its base session should be queued and saved, never a second archive.
            (with-current-buffer view (org-mode))
            (gptel-agent-session-save)
            (should-not (gethash source gptel-agent-session--dirty))
            (with-current-buffer view
              (let ((inhibit-read-only t)) (goto-char (point-max)) (insert " through view")))
            (should (gethash source gptel-agent-session--dirty))
            (should-not (gethash view gptel-agent-session--dirty))
            (kill-buffer view)
            (should (eq source (gethash id gptel-agent-session--buffers)))
            (should (buffer-live-p title-request))
            (should (eq title-request (buffer-local-value 'gptel-agent-session--title-request source)))
            (gptel-agent-session-save)
            (should (= 1 (length (gptel-agent-session--catalog))))
            (should (equal "Original updated through view"
                           (plist-get (gptel-agent-session--read id) :text))))
        (when (buffer-live-p source)
          (with-current-buffer source (setq gptel-agent-session--title-request nil)))
        (when (buffer-live-p view) (kill-buffer view))
        (when (buffer-live-p title-request) (kill-buffer title-request))))))

(ert-deftest gptel-agent-indirect-chat-enabling-sessions-does-not-adopt-view ()
  (gptel-agent-session-test--with-store
    (let ((source (gptel-agent-session-test--buffer " *indirect-disabled-owner*" "Original"))
          view id)
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (setq id (buffer-local-value 'gptel-agent-session--id source))
      (gptel-agent-session-mode -1)
      (setq view (with-current-buffer source
                   (clone-indirect-buffer " *indirect-disabled-view*" nil)))
      (gptel-agent-session-mode 1)
      (should (= 1 (hash-table-count gptel-agent-session--buffers)))
      (kill-buffer view)
      (should (eq source (gethash id gptel-agent-session--buffers))))))

(provide 'gptel-agent-indirect-chat-test)
;;; gptel-agent-indirect-chat-test.el ends here
