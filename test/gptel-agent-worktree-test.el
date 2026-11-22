;;; gptel-agent-worktree-test.el --- Discussion Git worktrees -*- lexical-binding: t; -*-

(require 'gptel-agent-discussion-test)

(defun gptel-agent-worktree-test--git (directory &rest args)
  "Run real Git with ARGS in DIRECTORY, checking its exit status."
  (with-temp-buffer
    (let ((default-directory directory))
      (should (eq 0 (apply #'process-file "git" nil '(t t) nil
                           "-c" "user.name=Discussion Test"
                           "-c" "user.email=discussion@example.invalid"
                           "-c" "commit.gpgsign=false" args))))
    (string-trim (buffer-string))))

(cl-defmacro gptel-agent-worktree-test--with-repo (&body body)
  "Run BODY with a committed REPO inside temporary ROOT."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (file-truename (make-temp-file "discussion-git-" t))))
          (repo (expand-file-name "source repo/" root))
          (process-environment (copy-sequence process-environment))
          (gptel-agent-use-project-instructions t))
     (unwind-protect
         (progn
           (setenv "GIT_CONFIG_GLOBAL" null-device)
           (setenv "GIT_CONFIG_NOSYSTEM" "1")
           (make-directory (expand-file-name "nested" repo) t)
           (with-temp-file (expand-file-name "nested/tracked.txt" repo) (insert "committed"))
           (with-temp-file (expand-file-name "AGENTS.md" repo) (insert "WORKTREE-INSTRUCTION"))
           (gptel-agent-worktree-test--git repo "init" "--quiet")
           (gptel-agent-worktree-test--git repo "add" ".")
           (gptel-agent-worktree-test--git repo "commit" "--quiet" "-m" "Initial")
           ,@body)
       (delete-directory root t))))

(defun gptel-agent-worktree-test--dispatch (fsm args)
  "Call FSM's Discussion dispatcher with schema-mapped ARGS from another buffer."
  (let ((tool (cl-find "Discussion" (plist-get (gptel-fsm-info fsm) :tools)
                       :key #'gptel-tool-name :test #'equal))
        result)
    (with-temp-buffer
      (setq default-directory temporary-file-directory)
      (apply (gptel-tool-function tool) (lambda (text) (setq result text))
             (gptel--map-tool-args tool (append '(:title "Isolated" :prompt "Work here") args))))
    result))

(ert-deftest gptel-agent-worktree-inherits-or-selects-repository-and-reports-metadata ()
  (gptel-agent-worktree-test--with-repo
    (let ((commit (gptel-agent-worktree-test--git repo "rev-parse" "HEAD"))
          (source-branch (gptel-agent-worktree-test--git repo "branch" "--show-current"))
          paths branches)
      (with-temp-file (expand-file-name "nested/tracked.txt" repo) (insert "uncommitted"))
      (with-temp-file (expand-file-name "AGENTS.md" repo) (insert "DIRTY-INSTRUCTION"))
      (with-temp-file (expand-file-name "untracked.txt" repo) (insert "private"))
      (dolist (directory (list nil "" :null "source repo/nested" repo))
        (gptel-agent-discussion-test--with-chat
          (let* ((caller (if (member directory '(nil "" :null))
                             (expand-file-name "nested/" repo) root))
                 (source (if (equal directory repo) repo (expand-file-name "nested/" repo))))
            (with-current-buffer parent (setq default-directory caller))
            ;; Exercise actual backend dispatch, including optional argument order.
            (gptel-agent-workers-test--tool
             fsm "Discussion" (list :title "Isolated" :prompt "Work here"
                                     :directory directory :worktree t))
            (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
            (should chat)
            (let* ((metadata (gptel-agent-discussion--chat-worktree chat))
                   (path (plist-get metadata :path))
                   (branch (plist-get metadata :branch))
                   (child (gptel-agent-discussion--chat-buffer chat))
                   (child-fsm (gptel-agent-discussion--chat-fsm chat))
                   (payload (gptel-agent-workers-test--data child-fsm))
                   (reply (gptel-agent-workers-test--data fsm)))
              (should (eq (gptel-agent-discussion--chat-state chat) 'running))
              (should (equal (buffer-local-value 'default-directory child) path))
              (should (equal (buffer-local-value 'default-directory parent) caller))
              (should (equal (plist-get metadata :repository) repo))
              (should (equal (plist-get metadata :source-directory) source))
              (should (equal (plist-get metadata :base-commit) commit))
              (should (equal (file-name-directory (directory-file-name path)) root))
              (should-not (member path paths))
              (should-not (member branch branches))
              (push path paths) (push branch branches)
              (should (equal (gptel-agent-worktree-test--git path "branch" "--show-current") branch))
              (should (equal (gptel-agent-worktree-test--git path "rev-parse" "HEAD") commit))
              (should (equal (gptel-agent-worktree-test--git path "status" "--porcelain") ""))
              (should (equal (with-temp-buffer
                               (insert-file-contents (expand-file-name "nested/tracked.txt" path))
                               (buffer-string)) "committed"))
              (should-not (file-exists-p (expand-file-name "untracked.txt" path)))
              (should (string-match-p "WORKTREE-INSTRUCTION" payload))
              (should-not (string-match-p "DIRTY-INSTRUCTION" payload))
              (dolist (value (list path branch repo source commit))
                (should (string-match-p (regexp-quote value) reply))
                (should (string-match-p (regexp-quote value) payload))
                (should (string-match-p (regexp-quote value)
                                        (gptel-agent-discussion-test--control fsm "read" chat))))
              ;; A follow-up and later close retain the workspace and its edits.
              (gptel-agent-workers-test--answer child-fsm "First answer")
              (gptel-agent-discussion-test--control fsm "send" chat "Continue")
              (gptel-agent-discussion--flush child)
              (should (equal (buffer-local-value 'default-directory child) path))
              (with-current-buffer child
                (with-temp-file "child.txt" (insert "child work")))
              (should-not (file-exists-p (expand-file-name "child.txt" repo)))
              (gptel-agent-discussion-test--control fsm "close" chat)
              (kill-buffer child)
              (should (file-exists-p (expand-file-name "child.txt" path)))
              (should (string-match-p (regexp-quote branch) (gptel-agent-discussion--status chat)))))))
      (should (equal (gptel-agent-worktree-test--git repo "branch" "--show-current") source-branch))
      (should (equal (with-temp-buffer
                       (insert-file-contents (expand-file-name "nested/tracked.txt" repo))
                       (buffer-string)) "uncommitted")))))

(ert-deftest gptel-agent-worktree-uses-linked-worktree-head-and-caller-directory ()
  (gptel-agent-worktree-test--with-repo
    (let ((linked (expand-file-name "linked/" root)))
      (gptel-agent-worktree-test--git repo "worktree" "add" "--quiet" "--detach" linked "HEAD")
      (with-temp-file (expand-file-name "linked-only.txt" linked) (insert "linked commit"))
      (gptel-agent-worktree-test--git linked "add" ".")
      (gptel-agent-worktree-test--git linked "commit" "--quiet" "-m" "Linked change")
      (gptel-agent-discussion-test--with-chat
        (with-current-buffer parent (setq default-directory linked))
        (let ((result (gptel-agent-worktree-test--dispatch fsm '(:worktree t))))
          (should-not (string-prefix-p "Error:" result))
          (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
          (let* ((metadata (gptel-agent-discussion--chat-worktree chat))
                 (path (plist-get metadata :path)))
            (should (equal (plist-get metadata :repository) linked))
            (should (equal (plist-get metadata :base-commit)
                           (gptel-agent-worktree-test--git linked "rev-parse" "HEAD")))
            (should (file-exists-p (expand-file-name "linked-only.txt" path)))))))))

(ert-deftest gptel-agent-worktree-disabled-does-not-require-git ()
  (dolist (value '(nil :json-false :null))
    (gptel-agent-discussion-test--with-chat
      (cl-letf (((symbol-function 'gptel-agent-discussion--git)
                 (lambda (&rest _) (ert-fail "Git must not run without worktree=true"))))
        (let ((result (gptel-agent-worktree-test--dispatch fsm (list :worktree value))))
          (should-not (string-prefix-p "Error:" result))
          (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
          (should-not (gptel-agent-discussion--chat-worktree chat))
          (should (equal (buffer-local-value 'default-directory (gptel-agent-discussion--chat-buffer chat))
                         (expand-file-name (buffer-local-value 'default-directory parent)))))))))

(ert-deftest gptel-agent-worktree-invalid-source-and-arguments-create-nothing ()
  (gptel-agent-worktree-test--with-repo
    (let ((empty (expand-file-name "empty/" root))
          (bare (expand-file-name "bare/" root)))
      (make-directory empty)
      (make-directory bare)
      (gptel-agent-worktree-test--git empty "init" "--quiet")
      (gptel-agent-worktree-test--git bare "init" "--quiet" "--bare")
      (gptel-agent-discussion-test--with-chat
        (with-current-buffer parent (setq default-directory repo))
        (let ((before (directory-files root))
              (branches (gptel-agent-worktree-test--git repo "branch" "--list")))
          (dolist (args (list (list :directory root :worktree t)
                             (list :directory empty :worktree t)
                             (list :directory bare :worktree t)
                             '(:directory "missing" :worktree t)
                             '(:directory "AGENTS.md" :worktree t)
                             '(:worktree "true")
                             '(:preset "missing-preset" :worktree t)))
            (should (string-prefix-p "Error:" (gptel-agent-worktree-test--dispatch fsm args)))
            (should-not (buffer-local-value 'gptel-agent-discussion--children parent))
            (should (equal (directory-files root) before))
            (should (equal (gptel-agent-worktree-test--git repo "branch" "--list") branches))))))))

(ert-deftest gptel-agent-worktree-git-failure-reports-path-and-cleans-empty-reservation ()
  (gptel-agent-worktree-test--with-repo
    (gptel-agent-discussion-test--with-chat
      (with-current-buffer parent (setq default-directory repo))
      (let ((git (symbol-function 'gptel-agent-discussion--git))
            (before (directory-files root)))
        (cl-letf (((symbol-function 'gptel-agent-discussion--git)
                   (lambda (directory &rest args)
                     (if (equal (car args) "worktree")
                         (error "Checkout failed")
                       (apply git directory args)))))
          (let ((result (gptel-agent-worktree-test--dispatch fsm '(:worktree t))))
            (should (string-prefix-p "Error:" result))
            (should (string-match-p "Checkout failed" result))
            (should (string-match-p "gptel-discussion/" result))
            (should (string-match-p (regexp-quote (directory-file-name repo)) result))
            (should-not (buffer-local-value 'gptel-agent-discussion--children parent))
            (should (equal (directory-files root) before))))))))

(ert-deftest gptel-agent-worktree-startup-failure-retains-inspectable-worktree ()
  (gptel-agent-worktree-test--with-repo
    (gptel-agent-discussion-test--with-chat
      (with-current-buffer parent (setq default-directory repo))
      (cl-letf (((symbol-function 'gptel-send) (lambda (&rest _) (error "Startup failed"))))
        (let ((result (gptel-agent-worktree-test--dispatch fsm '(:worktree t))))
          (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
          (should (string-match-p "status: failed" result))
          (should (string-match-p "Startup failed" (gptel-agent-discussion-test--control fsm "read" chat)))
          (let ((path (plist-get (gptel-agent-discussion--chat-worktree chat) :path)))
            (should (file-directory-p path))
            (should (string-match-p (regexp-quote path) result))))))))

(ert-deftest gptel-agent-worktree-checkout-hook-failure-preserves-created-files ()
  (gptel-agent-worktree-test--with-repo
    (let ((hook (expand-file-name ".git/hooks/post-checkout" repo)))
      (with-temp-file hook (insert "#!/bin/sh\necho checkout-hook-failed >&2\nexit 1\n"))
      (set-file-modes hook #o755))
    (gptel-agent-discussion-test--with-chat
      (with-current-buffer parent (setq default-directory repo))
      (let* ((result (gptel-agent-worktree-test--dispatch fsm '(:worktree t)))
             (path (car (directory-files root t "^source repo-discussion-"))))
        (should (string-prefix-p "Error:" result))
        (should (string-match-p "checkout-hook-failed" result))
        (should-not (buffer-local-value 'gptel-agent-discussion--children parent))
        (should path)
        (should (string-match-p (regexp-quote path) result))
        (should (file-exists-p (expand-file-name "nested/tracked.txt" path)))
        (should (string-match-p "gptel-discussion/"
                                (gptel-agent-worktree-test--git
                                 (file-name-as-directory path) "branch" "--show-current")))))))

(ert-deftest gptel-agent-worktree-loads-plan-from-committed-checkout ()
  (gptel-agent-worktree-test--with-repo
    (let ((gptel-agent-dirs '("agents")) (gptel-agent-skill-dirs nil)
          gptel--known-presets gptel-agent--agents gptel-agent--agent-files
          gptel-agent--registered-agents gptel-agent--file-cache
          (file (expand-file-name "agents/gptel-plan.md" repo)))
      (make-directory (file-name-directory file))
      (with-temp-file file (insert "---\ndescription: Plan\n---\nCommitted Plan"))
      (gptel-agent-worktree-test--git repo "add" "agents")
      (gptel-agent-worktree-test--git repo "commit" "--quiet" "-m" "Add plan")
      (with-temp-file file (insert "---\ndescription: Plan\n---\nUncommitted Plan"))
      (gptel-agent-discussion-test--with-chat
        (with-current-buffer parent
          (setq default-directory repo)
          (gptel-agent-update))
        (let ((result (gptel-agent-worktree-test--dispatch fsm '(:preset "gptel-plan" :worktree t))))
          (should-not (string-prefix-p "Error:" result)))
        (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
        (should (equal (buffer-local-value 'gptel-system-prompt
                                           (gptel-agent-discussion--chat-buffer chat))
                       "Committed Plan"))))))

(ert-deftest gptel-agent-worktree-overrides-preset-directory-before-sending ()
  (gptel-agent-worktree-test--with-repo
    (let ((gptel--known-presets (copy-tree gptel--known-presets)))
      (gptel-make-preset 'worktree-directory-test :post (lambda () (setq default-directory repo)))
      (gptel-agent-discussion-test--with-chat
        (with-current-buffer parent (setq default-directory repo))
        (let ((result (gptel-agent-worktree-test--dispatch
                       fsm '(:preset "worktree-directory-test" :worktree t))))
          (should-not (string-prefix-p "Error:" result))
          (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
          (should (eq (gptel-agent-discussion--chat-state chat) 'running))
          (should (equal (buffer-local-value 'default-directory (gptel-agent-discussion--chat-buffer chat))
                         (plist-get (gptel-agent-discussion--chat-worktree chat) :path))))))))

(defun gptel-agent-worktree-test--cleanup (fsm chat &optional delete-branch)
  "Call FSM's cleanup dispatcher for CHAT with schema-mapped DELETE-BRANCH."
  (let ((tool (cl-find "DiscussionControl" (plist-get (gptel-fsm-info fsm) :tools)
                       :key #'gptel-tool-name :test #'equal)) result)
    (with-temp-buffer
      (setq default-directory temporary-file-directory)
      (apply (gptel-tool-function tool) (lambda (text) (setq result text))
             (gptel--map-tool-args
              tool (list :action "cleanup" :discussion_id (gptel-agent-discussion--chat-id chat)
                         :delete_branch delete-branch))))
    result))

(cl-defmacro gptel-agent-worktree-test--with-finished-chat (&body body)
  "Run BODY with a finished CHAT in a generated worktree PATH on BRANCH."
  (declare (indent 0))
  `(gptel-agent-worktree-test--with-repo
     (gptel-agent-discussion-test--with-chat
       (with-current-buffer parent (setq default-directory repo))
       (gptel-agent-worktree-test--dispatch fsm '(:worktree t))
       (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
       (let* ((path (plist-get (gptel-agent-discussion--chat-worktree chat) :path))
              (branch (plist-get (gptel-agent-discussion--chat-worktree chat) :branch))
              (child (gptel-agent-discussion--chat-buffer chat)))
         (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Finished work")
         ,@body))))

(ert-deftest gptel-agent-worktree-cleanup-keeps-branch-and-transcript-by-default ()
  (dolist (delete-branch '(nil :json-false :null))
    (gptel-agent-worktree-test--with-finished-chat
      (let ((transcript (gptel-agent-discussion-test--buffer-text child))
            (source-head (gptel-agent-worktree-test--git repo "rev-parse" "HEAD")))
        (should (string-match-p "worktree_status: removed"
                                (gptel-agent-worktree-test--cleanup fsm chat delete-branch)))
        (should-not (file-exists-p path))
        (should (eq (gptel-agent-discussion--chat-state chat) 'closed))
        (should (equal (buffer-local-value 'default-directory child) repo))
        (should (equal (gptel-agent-discussion-test--buffer-text child) transcript))
        (should (equal (gptel-agent-worktree-test--git repo "rev-parse" branch) source-head))
        (should (equal (gptel-agent-worktree-test--git repo "rev-parse" "HEAD") source-head))
        (should-not (string-match-p (regexp-quote (directory-file-name path))
                                    (gptel-agent-worktree-test--git repo "worktree" "list" "--porcelain")))
        (dolist (action '("list" "read"))
          (let ((result (gptel-agent-discussion-test--control fsm action chat)))
            (should (string-match-p "worktree_status: removed" result))
            (should (string-match-p "worktree_branch_status: retained" result))
            (should (string-match-p (regexp-quote path) result))))
        (should (string-prefix-p "Error:" (gptel-agent-discussion-test--control fsm "send" chat "Resume")))
        ;; Retrying removal does not touch the filesystem again.
        (cl-letf (((symbol-function 'gptel-agent-discussion--git)
                   (lambda (&rest _) (ert-fail "Cleanup should already be complete"))))
          (should-not (string-prefix-p "Error:" (gptel-agent-worktree-test--cleanup fsm chat))))))))

(ert-deftest gptel-agent-worktree-cleanup-deletes-branch-through-backend-dispatch ()
  (gptel-agent-worktree-test--with-finished-chat
    (gptel-agent-workers-test--tool
     fsm "DiscussionControl" (list :action "cleanup" :discussion_id (gptel-agent-discussion--chat-id chat)
                                    :delete_branch t))
    (should-not (file-exists-p path))
    (should (equal (gptel-agent-worktree-test--git repo "branch" "--list" branch) ""))
    (should (string-match-p "worktree_branch_status: deleted" (gptel-agent-workers-test--data fsm)))
    (cl-letf (((symbol-function 'gptel-agent-discussion--git)
               (lambda (&rest _) (ert-fail "Cleanup should already be complete"))))
      (should-not (string-prefix-p "Error:" (gptel-agent-worktree-test--cleanup fsm chat t))))))

(ert-deftest gptel-agent-worktree-cleanup-retries-unmerged-branch-after-partial-success ()
  (gptel-agent-worktree-test--with-finished-chat
    (with-temp-file (expand-file-name "done.txt" path) (insert "Finished work"))
    (gptel-agent-worktree-test--git path "add" ".")
    (gptel-agent-worktree-test--git path "commit" "--quiet" "-m" "Finished work")
    (let ((result (gptel-agent-worktree-test--cleanup fsm chat t)))
      (should (string-prefix-p "Error:" result))
      (should (string-match-p "worktree_status: removed" result))
      (should (string-match-p "worktree_branch_status: retained" result))
      (should-not (file-exists-p path))
      (should (eq (gptel-agent-discussion--chat-state chat) 'closed))
      (should-not (equal (gptel-agent-worktree-test--git repo "branch" "--list" branch) "")))
    (gptel-agent-worktree-test--git repo "merge" "--ff-only" branch)
    (should (string-match-p "worktree_branch_status: deleted"
                            (gptel-agent-worktree-test--cleanup fsm chat t)))
    (should (equal (gptel-agent-worktree-test--git repo "branch" "--list" branch) ""))))

(ert-deftest gptel-agent-worktree-cleanup-rejects-foreign-and-ordinary-discussions ()
  (gptel-agent-worktree-test--with-finished-chat
    (gptel-agent-discussion-test--control fsm "send" chat "Continue")
    (gptel-agent-discussion--flush child)
    (should (string-match-p "Unknown discussion_id"
                            (gptel-agent-worktree-test--cleanup (gptel-agent-discussion--chat-fsm chat) chat t)))
    (should (file-directory-p path)))
  (gptel-agent-discussion-test--with-chat
    (setq chat (gptel-agent-discussion-test--spawn fsm parent))
    (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Finished")
    (cl-letf (((symbol-function 'gptel-agent-discussion--git)
               (lambda (&rest _) (ert-fail "Ordinary discussions have no worktree to clean"))))
      (should (string-match-p "no generated worktree" (gptel-agent-worktree-test--cleanup fsm chat))))))

(ert-deftest gptel-agent-worktree-cleanup-rejects-invalid-flag-and-unfinished-work ()
  (gptel-agent-worktree-test--with-finished-chat
    (should (string-match-p "delete_branch must be a boolean"
                            (gptel-agent-worktree-test--cleanup fsm chat "true")))
    (gptel-agent-discussion-test--control fsm "send" chat "Continue")
    (should (string-match-p "queued discussion work" (gptel-agent-worktree-test--cleanup fsm chat)))
    (gptel-agent-discussion--flush child)
    (should (string-match-p "still running" (gptel-agent-worktree-test--cleanup fsm chat)))
    (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm chat) "Finished again")
    (setf (gptel-agent-discussion--chat-review chat) '(("file" 1)))
    (should (string-match-p "pending file reviews" (gptel-agent-worktree-test--cleanup fsm chat)))
    (setf (gptel-agent-discussion--chat-review chat) nil)
    ;; Closing monitoring does not prevent direct user requests in that buffer.
    (gptel-agent-discussion-test--control fsm "close" chat)
    (with-current-buffer child (goto-char (point-max)) (insert "New user request") (gptel-send))
    (should (string-match-p "Active or queued" (gptel-agent-worktree-test--cleanup fsm chat)))
    (should (file-directory-p path))
    (should-not (equal (gptel-agent-worktree-test--git repo "branch" "--list" branch) ""))))

(ert-deftest gptel-agent-worktree-cleanup-refuses-dirty-locked-and-switched-worktrees ()
  (dolist (kind '(tracked untracked locked switched detached))
    (gptel-agent-worktree-test--with-finished-chat
      (pcase kind
        ('tracked (with-temp-file (expand-file-name "nested/tracked.txt" path) (insert "Uncommitted")))
        ('untracked (with-temp-file (expand-file-name "untracked.txt" path) (insert "Untracked")))
        ('locked (gptel-agent-worktree-test--git repo "worktree" "lock" path))
        ('switched (gptel-agent-worktree-test--git path "checkout" "-b" "other-branch"))
        ('detached (gptel-agent-worktree-test--git path "checkout" "--detach")))
      (let ((result (gptel-agent-worktree-test--cleanup fsm chat t)))
        (should (string-prefix-p "Error:" result))
        (should (string-match-p "worktree_status: retained" result))
        (should (string-match-p "worktree_branch_status: retained" result)))
      (should (file-directory-p path))
      (should (eq (gptel-agent-discussion--chat-state chat) 'idle))
      (should-not (equal (gptel-agent-worktree-test--git repo "branch" "--list" branch) "")))))

(ert-deftest gptel-agent-worktree-cleanup-refuses-unsaved-files-and-background-processes ()
  (gptel-agent-worktree-test--with-finished-chat
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "nested/tracked.txt" path))
      (insert "Unsaved work")
      (should (string-match-p "unsaved edits" (gptel-agent-worktree-test--cleanup fsm chat))))
    (with-current-buffer child (gptel-agent--start-background-process "cat"))
    (should (string-match-p "Stop background process" (gptel-agent-worktree-test--cleanup fsm chat)))
    (with-current-buffer child (gptel-agent--process-cleanup))
    (should (file-directory-p path))
    (should-not (string-prefix-p "Error:" (gptel-agent-worktree-test--cleanup fsm chat t)))))

(ert-deftest gptel-agent-worktree-cleanup-refuses-other-discussions-sharing-workspace ()
  (gptel-agent-worktree-test--with-finished-chat
    (gptel-agent-worktree-test--dispatch fsm (list :directory path))
    (let ((other (car (buffer-local-value 'gptel-agent-discussion--children parent))))
      (gptel-agent-workers-test--answer (gptel-agent-discussion--chat-fsm other) "Other discussion done")
      (should (string-match-p "Close the other discussion" (gptel-agent-worktree-test--cleanup fsm chat)))
      (gptel-agent-discussion-test--control fsm "close" other)
      (should-not (string-prefix-p "Error:" (gptel-agent-worktree-test--cleanup fsm chat t))))))

(ert-deftest gptel-agent-worktree-cleanup-refuses-workers-after-directory-change ()
  (dolist (phase '(requesting retry resumed))
    (gptel-agent-worktree-test--with-finished-chat
      (let ((snapshot '(("researcher" :system "Read only" :tools ("Read")
                         :task-timeout nil :max-request-rounds nil)))
            (gptel-agent-retry-delay 60))
        (with-current-buffer parent
          (gptel-agent--task #'ignore "researcher" "Inspect tree" "Read this workspace"
                             fsm snapshot nil nil nil t (expand-file-name "nested/" path)))
        (let* ((run (car (hash-table-values gptel-agent--runs)))
               (worker (gptel-agent--run-child-buffer run)))
          (when (eq phase 'resumed)
            (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "First pass")
            (should-not (gptel-agent--run-active-p run))
            (with-current-buffer parent
              (gptel-agent--task #'ignore "researcher" "Continue" "Read more"
                                 fsm snapshot nil nil
                                 (gptel-agent--session-id (gptel-agent--run-session run)) t))
            (setq run (gptel-agent--session-run (gptel-agent--run-session run))))
          (should (gptel-agent--run-active-p run))
          (should-not (buffer-local-value 'gptel-agent-discussion--fsm worker))
          (with-current-buffer worker (setq default-directory repo))
          (when (eq phase 'retry)
            (setf (plist-get (gptel-fsm-info (gptel-agent--run-fsm run)) :http-status) 503)
            (gptel--fsm-transition (gptel-agent--run-fsm run) 'ERRS)
            (should (eq (gptel-agent--run-state run) 'retry-wait)))
          (should (string-match-p "Active Agent worker"
                                  (gptel-agent-worktree-test--cleanup fsm chat)))
          (should (file-directory-p path))
          (if (eq phase 'retry)
              (gptel-abort worker)
            (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "Done"))
          (should-not (gptel-agent--run-active-p run))
          (should-not (string-prefix-p "Error:" (gptel-agent-worktree-test--cleanup fsm chat)))
          (should-not (file-exists-p path)))))))

(defun gptel-agent-worktree-test--request-buffer (parent path)
  "Create an ordinary request buffer in PATH with PARENT's test backend."
  (let ((buffer (generate-new-buffer " *worktree-request*")))
    (with-current-buffer buffer
      (setq default-directory path)
      (setq-local gptel-backend (buffer-local-value 'gptel-backend parent)
                  gptel-model 'test gptel-use-curl t gptel-stream nil
                  gptel-prompt-transform-functions nil gptel-tools nil))
    buffer))

(ert-deftest gptel-agent-worktree-cleanup-refuses-ordinary-request-phases ()
  (dolist (phase '(requesting preparing tool approval))
    (gptel-agent-worktree-test--with-finished-chat
      (let ((owner (gptel-agent-worktree-test--request-buffer
                    parent (expand-file-name "nested/" path)))
            request continue)
        (unwind-protect
            (progn
              (with-current-buffer owner
                (setq-local
                 gptel-use-tools t gptel-confirm-tool-calls (eq phase 'approval)
                 gptel-tools
                 (list (gptel-make-tool
                        :name "WorktreeDeferred" :async t :include t :confirm t
                        :description "Wait for the test" :args nil
                        :function (lambda (callback) (setq continue callback))))))
              ;; The explicit request owner, not the dispatching buffer, owns PATH.
              (with-current-buffer parent
                (setq request
                      (if (eq phase 'approval)
                          (gptel-request "Inspect workspace" :buffer owner :callback #'ignore
                            :fsm (gptel-make-fsm :table gptel-send--transitions
                                                :handlers gptel-send--handlers))
                        (gptel-request "Inspect workspace" :buffer owner :callback #'ignore
                          :transforms (when (eq phase 'preparing)
                                        (list (lambda (callback _fsm) (setq continue callback))))))))
              (should-not (buffer-local-value 'gptel-agent-discussion--fsm owner))
              (when (memq phase '(tool approval))
                (cl-letf (((symbol-function 'gptel--display-tool-calls) #'ignore))
                  (gptel-agent-workers-test--tool request "WorktreeDeferred" nil)))
              (should (eq (gptel-fsm-state request)
                          (pcase phase ('preparing 'INIT) ((or 'tool 'approval) 'TOOL)
                                 (_ 'WAIT))))
              (when (eq phase 'approval) (should-not continue))
              (with-current-buffer owner (setq default-directory repo))
              (should (string-match-p "Active gptel request"
                                      (gptel-agent-worktree-test--cleanup fsm chat)))
              (should (file-directory-p path))
              (pcase phase
                ('preparing (funcall continue))
                ('tool (funcall continue "Finished tool")))
              (if (eq phase 'approval)
                  (gptel--fsm-transition request 'ABRT)
                (gptel-agent-workers-test--answer request "Finished request"))
              (should-not (string-prefix-p "Error:" (gptel-agent-worktree-test--cleanup fsm chat)))
              (should-not (file-exists-p path)))
          (when (buffer-live-p owner) (kill-buffer owner)))))))

(ert-deftest gptel-agent-worktree-cleanup-refuses-request-waiting-for-workers ()
  (gptel-agent-worktree-test--with-finished-chat
    (let ((owner (gptel-agent-worktree-test--request-buffer parent path)))
      (unwind-protect
          (let ((request (with-current-buffer owner
                           (gptel-request "Wait for work" :callback #'ignore))))
            (with-current-buffer owner
              ;; The worker itself uses a different checkout.  Its waiting
              ;; request still owns PATH after the chat changes directory.
              (gptel-agent--task #'ignore "researcher" "Inspect source" "Read"
                                 request '(("researcher" :system "Read" :tools ("Read")
                                            :task-timeout nil :max-request-rounds nil))
                                 nil nil nil t repo))
            (gptel-agent-workers-test--answer request "Awaiting worker")
            (should (eq (gptel-fsm-state request) 'AGWT))
            (with-current-buffer owner (setq default-directory repo))
            (should (string-match-p "Active gptel request"
                                    (gptel-agent-worktree-test--cleanup fsm chat)))
            (should (file-directory-p path))
            (gptel-abort owner)
            (should-not (string-prefix-p "Error:" (gptel-agent-worktree-test--cleanup fsm chat)))
            (should-not (file-exists-p path)))
        (when (buffer-live-p owner) (kill-buffer owner))))))

(ert-deftest gptel-agent-worktree-cleanup-checks-every-request-and-ignores-nonowners ()
  (gptel-agent-worktree-test--with-finished-chat
    (let ((owner (gptel-agent-worktree-test--request-buffer parent path)))
      (unwind-protect
          (with-current-buffer owner
            (let ((first (gptel-request "First" :callback #'ignore))
                  (second (gptel-request "Second" :callback #'ignore)))
              (gptel-agent-workers-test--answer second "Done")
              (setq default-directory repo)
              (should (string-match-p "Active gptel request"
                                      (gptel-agent-worktree-test--cleanup fsm chat)))
              (gptel--fsm-transition first 'ABRT))
            (setq default-directory path)
            ;; Preview-only and failed startup requests must not hold the tree.
            (gptel-request "Preview" :dry-run t :callback #'ignore)
            (should-error
             (gptel-request "Failed" :callback #'ignore
               :transforms (list (lambda (_fsm) (error "Preparation failed")))))
            (setq default-directory repo)
            (should-not (string-prefix-p "Error:" (gptel-agent-worktree-test--cleanup fsm chat)))
            (should-not (file-exists-p path)))
        (when (buffer-live-p owner) (kill-buffer owner))))))

(ert-deftest gptel-agent-worktree-cleanup-refuses-foreground-bash-after-directory-change ()
  (gptel-agent-worktree-test--with-finished-chat
    (with-temp-buffer
      (setq default-directory (expand-file-name "nested/" path))
      (let ((process (gptel-agent--start-foreground-bash #'ignore "sleep 30" "Wait" nil)))
        (unwind-protect
            (progn
              (should (process-live-p process))
              (setq default-directory repo)
              (should (string-match-p "Stop foreground Bash"
                                      (gptel-agent-worktree-test--cleanup fsm chat)))
              (should (file-directory-p path))
              (gptel-agent--bash-cleanup)
              (should-not (process-live-p process))
              (should-not (string-prefix-p "Error:" (gptel-agent-worktree-test--cleanup fsm chat)))
              (should-not (file-exists-p path)))
          (gptel-agent--bash-cleanup))))))

(ert-deftest gptel-agent-worktree-cleanup-retains-requests-after-post-launch-errors ()
  (dolist (failure '(transport hook))
    (gptel-agent-worktree-test--with-finished-chat
      (let ((owner (gptel-agent-worktree-test--request-buffer parent path))
            (request (gptel-make-fsm))
            (gptel--request-alist nil))
        (unwind-protect
            (progn
              (with-current-buffer owner
                (setq-local gptel-post-request-hook
                            (list (lambda () (error "Post-request hook failed"))))
                (cl-letf (((symbol-function 'gptel-curl-get-response)
                           (lambda (fsm)
                             (if (eq failure 'transport)
                                 (error "Transport failed before launch")
                               ;; Model transport ownership without any network.
                               (push (cons owner (cons fsm #'ignore)) gptel--request-alist)))))
                  (should-error (gptel-request "Inspect workspace" :fsm request :callback #'ignore)))
                (setq default-directory repo))
              (when (eq failure 'hook)
                (should (eq (gptel-fsm-state request) 'WAIT))
                (should (string-match-p "Active gptel request"
                                        (gptel-agent-worktree-test--cleanup fsm chat)))
                (should (file-directory-p path))
                (gptel-abort owner)
                (should (eq (gptel-fsm-state request) 'ABRT)))
              (should-not (string-prefix-p "Error:" (gptel-agent-worktree-test--cleanup fsm chat)))
              (should-not (file-exists-p path)))
          (when (buffer-live-p owner) (kill-buffer owner)))))))

(ert-deftest gptel-agent-worktree-cleanup-supports-closed-killed-and-failed-discussions ()
  (dolist (state '(closed killed failed cancelled))
    (gptel-agent-worktree-test--with-finished-chat
      (pcase state
        ('closed (gptel-agent-discussion-test--control fsm "close" chat))
        ('killed (kill-buffer child))
        (_ (setf (gptel-agent-discussion--chat-state chat) state)))
      (should-not (string-prefix-p "Error:" (gptel-agent-worktree-test--cleanup fsm chat t)))
      (should-not (file-exists-p path))
      (should (equal (gptel-agent-worktree-test--git repo "branch" "--list" branch) "")))))

(provide 'gptel-agent-worktree-test)
;;; gptel-agent-worktree-test.el ends here
