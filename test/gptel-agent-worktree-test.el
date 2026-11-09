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

(provide 'gptel-agent-worktree-test)
;;; gptel-agent-worktree-test.el ends here
