;;; gptel-agent-buffer-cleanup-test.el --- Owned worker buffers -*- lexical-binding: t; -*-

(require 'gptel-agent-session-test)

(ert-deftest gptel-agent-buffer-cleanup-direct-worker-kill-after-mode-change ()
  (gptel-agent-workers-test--with-chat
    (let* ((branch (gptel-agent-workers-test--launch fsm))
           (buffer (gptel-agent--run-child-buffer branch))
           (leaf (gptel-agent-workers-test--launch
                  (gptel-agent--run-fsm branch) "test-worker"))
           (sibling (gptel-agent-workers-test--launch fsm)))
      (with-current-buffer buffer (text-mode))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (should-not (kill-buffer buffer)))
      (should (gptel-agent--run-active-p branch))
      (should (gptel-agent--run-active-p leaf))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (should (kill-buffer buffer)))
      (should (eq 'cancelled (gptel-agent--run-state branch)))
      (should (eq 'cancelled (gptel-agent--run-state leaf)))
      (should-not (buffer-live-p (gptel-agent--run-child-buffer leaf)))
      (should (equal (list sibling) (hash-table-values gptel-agent--runs)))
      (should (equal (list (gptel-agent--run-session sibling))
                     (mapcar #'cdr (buffer-local-value 'gptel-agent--sessions parent)))))))

(ert-deftest gptel-agent-buffer-cleanup-runs-child-hooks-without-child-queries ()
  (gptel-agent-workers-test--with-chat
    (let* ((gptel-agent-keep-failed-run-buffers nil)
           (branch (gptel-agent-workers-test--launch fsm))
           (leaf (gptel-agent-workers-test--launch
                  (gptel-agent--run-fsm branch) "test-worker"))
           (buffers (mapcar #'gptel-agent--run-child-buffer (list branch leaf)))
           queried killed)
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (add-hook 'kill-buffer-query-functions
                    (lambda () (push (current-buffer) queried) nil) -90 t)
          ;; Hosts like gptel-runner archive the transcript in this hook.
          (add-hook 'kill-buffer-hook (lambda () (push (current-buffer) killed)) 90 t)))
      (should (kill-buffer parent))
      (should-not queried)
      (should (= (length killed) (length buffers)))
      (dolist (buffer buffers)
        (should (memq buffer killed))
        (should-not (buffer-live-p buffer)))
      (should (= 0 (hash-table-count gptel-agent--runs))))))

(ert-deftest gptel-agent-buffer-cleanup-review-restores-owner-for-later-kill-hooks ()
  (gptel-agent-workers-test--with-chat
    (let* ((run (gptel-agent-workers-test--launch fsm))
           (child (gptel-agent--run-child-buffer run))
           (review (generate-new-buffer " *cleanup review*"))
           (queue (gptel-agent-review--make-queue :owner parent :buffer review))
           observed)
      (unwind-protect
          (save-window-excursion
            (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "Completed work")
            ;; Opening Ediff from the review list saves its window configuration.
            (setf (gptel-agent-review--queue-view queue)
                  (smv/gptel-ediff--make-view
                   :queue queue :config (with-current-buffer review (current-window-configuration))))
            (with-current-buffer parent
              (setq-local gptel-agent-review--queue queue)
              (add-hook 'kill-buffer-hook #'gptel-agent-review--cleanup nil t)
              (add-hook 'kill-buffer-hook (lambda () (setq observed (current-buffer))) 90 t))
            (should (kill-buffer parent))
            (should (eq observed parent))
            (should-not (buffer-live-p review))
            (should-not (buffer-live-p child)))
        (when (buffer-live-p review) (kill-buffer review))
        (when (buffer-live-p child) (kill-buffer child))))))

(ert-deftest gptel-agent-buffer-cleanup-retries-after-a-child-kill-hook-errors ()
  (dolist (first-kill '(parent child))
    (ert-info ((format "First kill targets the %s" first-kill))
      (gptel-agent-workers-test--with-chat
        (let* ((completed (gptel-agent-workers-test--launch fsm))
               (retained (gptel-agent--run-child-buffer completed))
               (active (gptel-agent-workers-test--launch fsm))
               (child (gptel-agent--run-child-buffer active))
               (fail-once t))
          (gptel-agent-workers-test--answer (gptel-agent--run-fsm completed) "Archived answer")
          (with-current-buffer child
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (when fail-once
                          (setq fail-once nil)
                          (error "Archive interrupted")))
                      90 t))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (should (equal (should-error (kill-buffer (if (eq first-kill 'parent) parent child)))
                           '(error "Archive interrupted"))))
          (should (buffer-live-p parent))
          (should (buffer-live-p child))
          (should (kill-buffer parent))
          (should-not (buffer-live-p child))
          (should-not (buffer-live-p retained)))))))

(ert-deftest gptel-agent-buffer-cleanup-respects-discussion-and-session-ownership ()
  (gptel-agent-session-test--with-store
    (gptel-agent-discussion-test--with-chat
      (gptel-agent-session-mode 1)
      (setq chat (gptel-agent-discussion-test--spawn fsm parent "Saved discussion prompt"))
      (let* ((discussion (gptel-agent-discussion--chat-buffer chat))
             (other (gptel-agent-session-test--buffer "*separate chat*" "Unrelated transcript"))
             (other-fsm (gptel-make-fsm :info (list :buffer other :position 1)))
             (snapshot '(("researcher" :system "Research" :task-timeout nil))))
        (cl-flet ((spawn (owner-fsm)
                    (let ((previous (car requests)))
                      (gptel-agent--task #'ignore "researcher" "Research" "Find evidence"
                                         owner-fsm snapshot nil nil nil t)
                      (should-not (eq previous (car requests)))
                      (plist-get (gptel-fsm-info (car requests)) :context))))
          (unwind-protect
              (let* ((completed (spawn fsm))
                     (owned (spawn fsm))
                     (independent (spawn (gptel-agent-discussion--chat-fsm chat)))
                     (unrelated (spawn other-fsm))
                     (discussion-id (buffer-local-value 'gptel-agent-session--id discussion))
                     (other-id (buffer-local-value 'gptel-agent-session--id other))
                     (other-text (with-current-buffer other (buffer-string))))
                (gptel-agent-workers-test--answer (gptel-agent--run-fsm completed) "Done")
                (should (eq (gptel-agent--run-state completed) 'completed))
                (should (buffer-live-p (gptel-agent--run-child-buffer completed)))
                (gptel-agent-session-save)
                (should (kill-buffer parent))
                (should-not (buffer-live-p (gptel-agent--run-child-buffer completed)))
                (should-not (buffer-live-p (gptel-agent--run-child-buffer owned)))
                (should (buffer-live-p discussion))
                (should-not (gptel-agent-discussion--chat-owner chat))
                (should (gptel-agent--run-active-p independent))
                (should (buffer-live-p (gptel-agent--run-child-buffer independent)))
                (let ((new-worker (spawn (gptel-agent-discussion--chat-fsm chat))))
                  (should (gptel-agent--run-active-p new-worker))
                  (should (eq discussion (gptel-agent--run-parent-buffer new-worker)))
                  (should (kill-buffer discussion))
                  (should-not (buffer-live-p (gptel-agent--run-child-buffer new-worker))))
                (should-not (buffer-live-p (gptel-agent--run-child-buffer independent)))
                (should (buffer-live-p other))
                (should (gptel-agent--run-active-p unrelated))
                (should (buffer-live-p (gptel-agent--run-child-buffer unrelated)))
                (should (string-match-p "Saved discussion prompt"
                                        (plist-get (gptel-agent-session--read discussion-id) :text)))
                (should (equal other-text
                               (plist-get (gptel-agent-session--read other-id) :text))))
            (when (buffer-live-p other) (kill-buffer other))))))))

(provide 'gptel-agent-buffer-cleanup-test)
;;; gptel-agent-buffer-cleanup-test.el ends here
