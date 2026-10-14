;;; gptel-agent-edit-test.el --- File edit and Ediff regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent-tools)
(require 'gptel-ediff-prehook)

(defmacro gptel-edit-test--with-files (files &rest body)
  "Create FILES in a temporary directory and run BODY there."
  (declare (indent 1))
  `(let* ((directory (make-temp-file "gptel-edit-test-" t))
          (default-directory (file-name-as-directory directory)))
     (unwind-protect
         (progn
           (dolist (file ,files)
             (make-directory (file-name-directory (expand-file-name (car file))) t)
             (with-temp-file (car file) (insert (cdr file))))
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((path (buffer-file-name buffer)))
           (when (string-prefix-p default-directory path)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory directory t))))

(defun gptel-edit-test--read (path)
  (with-temp-buffer (insert-file-contents path) (buffer-string)))

(defconst gptel-edit-test--multi-diff
  "diff --git a/one b/one
--- a/one
+++ b/one
@@ -1 +1 @@
-first
+FIRST
@@ -5 +5 @@
-last
+LAST
diff --git a/sub/two b/sub/two
--- a/sub/two
+++ b/sub/two
@@ -1 +1 @@
-second
+SECOND
")

(ert-deftest gptel-edit-multiple-files-and-hunks-preview-equals-execution ()
  (gptel-edit-test--with-files '(("one" . "first\na\nb\nc\nlast\n") ("sub/two" . "second\n"))
    ;; A directory without a trailing slash was previously run from its parent.
    (let* ((args (list :path directory :diff t :old_str "ignored" :new_str gptel-edit-test--multi-diff))
           (changes (gptel-agent-edit-prepare "Edit" args)))
      (should (= (length changes) 2))
      (should (equal (gptel-edit-test--read "one") "first\na\nb\nc\nlast\n"))
      (should (equal (plist-get (car changes) :after) "FIRST\na\nb\nc\nLAST\n"))
      (gptel-agent--edit-files directory "ignored" gptel-edit-test--multi-diff t)
      (dolist (change changes)
        (should (equal (gptel-edit-test--read (plist-get change :path))
                       (plist-get change :after)))))))

(ert-deftest gptel-edit-invalid-later-hunk-writes-nothing ()
  (gptel-edit-test--with-files '(("one" . "first\na\nb\nc\nlast\n") ("sub/two" . "does not match\n"))
    (should-error (gptel-agent--edit-files directory nil gptel-edit-test--multi-diff t))
    (should (equal (gptel-edit-test--read "one") "first\na\nb\nc\nlast\n"))
    (should (equal (gptel-edit-test--read "sub/two") "does not match\n"))
    (should-not (file-exists-p "one.rej"))))

(ert-deftest gptel-edit-corrects-counts-with-empty-and-sign-prefixed-lines ()
  (gptel-edit-test--with-files '(("one" . "\n--old\n++old\nend\n"))
    (gptel-agent--edit-files
     "one" nil "```patch\n--- arbitrary\n+++ arbitrary\n@@ -1,99 +1,99 @@\n-\n---old\n-++old\n+\n+--new\n+++new\n end\n```" t)
    (should (equal (gptel-edit-test--read "one") "\n--new\n++new\nend\n"))))

(ert-deftest gptel-edit-preserves-whitespace-and-no-final-newline ()
  (gptel-edit-test--with-files '(("one" . "old"))
    (gptel-agent--edit-files
     "one" nil "--- one\n+++ one\n@@ -1 +1 @@\n-old\n\\ No newline at end of file\n+new  \n\\ No newline at end of file\n" t)
    (should (equal (gptel-edit-test--read "one") "new  "))))

(ert-deftest gptel-edit-plain-paths-spaces-and-timestamps ()
  (gptel-edit-test--with-files '(("a file" . "old\n"))
    (gptel-agent--edit-files directory nil
                             "--- a file\t2026-01-01\n+++ a file\t2026-01-01\n@@ -1 +1 @@\n-old\n+new\n" t)
    (should (equal (gptel-edit-test--read "a file") "new\n"))))

(ert-deftest gptel-edit-creation-and-deletion ()
  (gptel-edit-test--with-files '(("old" . "old\n"))
    (let ((changes (gptel-agent-edit-prepare
                    "Edit" (list :path directory :diff t :new_str
                                 "--- a/old\n+++ /dev/null\n@@ -1 +0,0 @@\n-old\n--- /dev/null\n+++ b/nested/new\n@@ -0,0 +1 @@\n+new\n"))))
      (should (null (plist-get (car changes) :after)))
      (should (null (plist-get (cadr changes) :before)))
      (gptel-agent-edit-apply changes)
      (should-not (file-exists-p "old"))
      (should (equal (gptel-edit-test--read "nested/new") "new\n")))))

(ert-deftest gptel-edit-rejects-unsafe-paths-and-renames ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (dolist (headers '("--- ../one\n+++ ../one" "--- /tmp/one\n+++ /tmp/one"
                       "--- a/one\n+++ b/two"))
      (should-error (gptel-agent-edit-prepare
                     "Edit" (list :path directory :diff t :new_str
                                  (concat headers "\n@@ -1 +1 @@\n-old\n+new\n")))))))

(ert-deftest gptel-edit-rejects-a-symlink-escape ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (let ((outside (make-temp-file "gptel-outside-")))
      (unwind-protect
          (progn
            (make-symbolic-link outside "link")
            (should-error
             (gptel-agent--edit-files directory nil
                                      "--- a/link\n+++ b/link\n@@ -0,0 +1 @@\n+new\n" t)))
        (delete-file outside)))))

(ert-deftest gptel-edit-literal-case-sensitive-and-unique-replacement ()
  (gptel-edit-test--with-files '(("one" . "FOO foo aaa"))
    (gptel-agent--edit-files "one" "foo" "bar\\1\\&" :json-false)
    (should (equal (gptel-edit-test--read "one") "FOO bar\\1\\& aaa"))
    (should-error (gptel-agent--edit-files "one" "aa" "x" :json-false))
    (should-error (gptel-agent--edit-files "one" "" "x" :json-false))
    (should-error (gptel-agent--edit-files "one" "missing" "x" :json-false))))

(ert-deftest gptel-edit-nil-diff-means-string-replacement ()
  (gptel-edit-test--with-files '(("one" . "old"))
    (gptel-agent--edit-files "one" "old" "new")
    (should (equal (gptel-edit-test--read "one") "new"))
    (should-error (gptel-agent--edit-files "one" nil "new"))))

(ert-deftest gptel-edit-noop-is-an-error ()
  (gptel-edit-test--with-files '(("one" . "old"))
    (should-error (gptel-agent--edit-files "one" "old" "old" :json-false))
    (should-error (gptel-agent--write-file directory "one" "old"))))

(ert-deftest gptel-edit-insert-preview-and-execution-agree ()
  (dolist (line '(0 1 2 -1))
    (gptel-edit-test--with-files '(("one" . "first\nsecond\n"))
      (let ((change (car (gptel-agent-edit-prepare
                         "Insert" (list :path "one" :line_number line :new_str "new")))))
        (gptel-agent--insert-in-file "one" line "new")
        (should (equal (gptel-edit-test--read "one") (plist-get change :after)))))))

(ert-deftest gptel-edit-write-empty-new-file-is-a-change ()
  (gptel-edit-test--with-files nil
    (gptel-agent--write-file directory "empty" "")
    (should (file-exists-p "empty"))
    (should (equal (gptel-edit-test--read "empty") ""))))

(ert-deftest gptel-edit-stale-review-checks-all-files-before-writing ()
  (gptel-edit-test--with-files '(("one" . "first\na\nb\nc\nlast\n") ("sub/two" . "second\n"))
    (let ((changes (gptel-agent-edit-prepare
                    "Edit" (list :path directory :diff t :new_str gptel-edit-test--multi-diff))))
      (with-temp-file "sub/two" (insert "user edit\n"))
      (should-error (gptel-agent-edit-apply changes))
      (should (equal (gptel-edit-test--read "one") "first\na\nb\nc\nlast\n"))
      (should (equal (gptel-edit-test--read "sub/two") "user edit\n")))))

(ert-deftest gptel-edit-unsaved-buffers-block-and-clean-buffers-refresh ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (let ((buffer (find-file-noselect "one")))
      (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved\n"))
      (should-error (gptel-agent--edit-files "one" "old" "new" :json-false))
      (should (equal (gptel-edit-test--read "one") "old\n"))
      (with-current-buffer buffer (revert-buffer t t))
      (gptel-agent--edit-files "one" "old" "new" :json-false)
      (should (equal (with-current-buffer buffer (buffer-string)) "new\n")))))

(ert-deftest gptel-edit-write-failure-restores-earlier-files ()
  (gptel-edit-test--with-files '(("one" . "first\na\nb\nc\nlast\n") ("sub/two" . "second\n"))
    (let ((write (symbol-function 'gptel-agent-edit--write))
          (changes (gptel-agent-edit-prepare
                    "Edit" (list :path directory :diff t :new_str gptel-edit-test--multi-diff))))
      (cl-letf (((symbol-function 'gptel-agent-edit--write)
                 (lambda (change content)
                   (if (equal content "SECOND\n") (error "Simulated write failure")
                     (funcall write change content)))))
        (should-error (gptel-agent-edit-apply changes)))
      (should (equal (gptel-edit-test--read "one") "first\na\nb\nc\nlast\n"))
      (should (equal (gptel-edit-test--read "sub/two") "second\n")))))

(ert-deftest gptel-ediff-invalid-preview-is-blocked ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (let ((gptel-confirm-tool-calls t))
      (cl-letf (((symbol-function 'smv/gptel-ediff--review)
                 (lambda (_) (ert-fail "Invalid proposal reached review"))))
        (should (plist-get
                 (smv/gptel-ediff-tool-call
                  '(:name "Edit" :args (:path "one" :diff t :new_str "invalid patch")))
                 :block))))))

(ert-deftest gptel-ediff-accept-applies-the-reviewed-snapshot-once ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (let ((gptel-confirm-tool-calls t))
      (cl-letf (((symbol-function 'smv/gptel-ediff--review) (lambda (_) nil)))
        (let ((result (smv/gptel-ediff-tool-call
                       '(:name "Edit" :args (:path "one" :old_str "old" :new_str "new" :diff :json-false)))))
          (should (plist-get result :result))
          (should-not (plist-get result :confirm))
          (should (equal (gptel-edit-test--read "one") "new\n")))))))

(ert-deftest gptel-ediff-rejection-and-abort-leave-files-untouched ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (let ((gptel-confirm-tool-calls t))
      (dolist (review (list (lambda (_) "Keep the original") (lambda (_) (signal 'quit nil))))
        (cl-letf (((symbol-function 'smv/gptel-ediff--review) review))
          (should (plist-get
                   (smv/gptel-ediff-tool-call
                    '(:name "Edit" :args (:path "one" :old_str "old" :new_str "new" :diff :json-false)))
                   :block))
          (should (equal (gptel-edit-test--read "one") "old\n")))))))

(defun gptel-edit-test--control ()
  (or (cl-find-if (lambda (b) (with-current-buffer b (eq major-mode 'ediff-mode)))
                  (buffer-list))
      (ert-fail "No Ediff control buffer")))

(defun gptel-edit-test--key (key)
  (with-current-buffer (gptel-edit-test--control)
    (call-interactively (key-binding (kbd key)))))

(ert-deftest gptel-ediff-real-navigation-and-differences ()
  (gptel-edit-test--with-files '(("one" . "first\na\nb\nc\nlast\n") ("sub/two" . "second\n"))
    (let ((changes (gptel-agent-edit-prepare
                    "Edit" (list :path directory :diff t :new_str gptel-edit-test--multi-diff)))
          (ediff-window-setup-function #'ediff-setup-windows-plain)
          (steps '("N" "P" "q")) paths)
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda ()
                   (with-current-buffer (gptel-edit-test--control)
                     (should (> ediff-number-of-differences 0))
                     (push (buffer-name ediff-buffer-A) paths))
                   (should steps)
                   (gptel-edit-test--key (pop steps)))))
        (should-not (smv/gptel-ediff--review changes)))
      (should (equal (car paths) (nth 2 paths)))
      (should-not (equal (car paths) (nth 1 paths)))
      (should-not steps)
      (should-not (cl-find-if (lambda (b) (string-prefix-p "*gptel-original:" (buffer-name b))) (buffer-list)))
      (should (equal (gptel-edit-test--read "one") "first\na\nb\nc\nlast\n")))))

(ert-deftest gptel-ediff-q-visits-unseen-files-before-accepting ()
  (gptel-edit-test--with-files '(("one" . "first\na\nb\nc\nlast\n") ("sub/two" . "second\n"))
    (let ((changes (gptel-agent-edit-prepare
                    "Edit" (list :path directory :diff t :new_str gptel-edit-test--multi-diff)))
          (ediff-window-setup-function #'ediff-setup-windows-plain)
          (visits 0))
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda () (cl-incf visits) (should (<= visits 2)) (gptel-edit-test--key "q"))))
        (should-not (smv/gptel-ediff--review changes)))
      (should (= visits 2)))))

(ert-deftest gptel-ediff-feedback-retains-file-and-range ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (let ((changes (gptel-agent-edit-prepare "Edit" '(:path "one" :old_str "old" :new_str "new")))
          (ediff-window-setup-function #'ediff-setup-windows-plain))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Explain this change"))
                ((symbol-function 'recursive-edit)
                 (lambda ()
                   (let ((proposed (buffer-local-value 'ediff-buffer-B (gptel-edit-test--control))))
                     (with-current-buffer proposed
                       (goto-char (point-min))
                       (call-interactively (key-binding (kbd "H")))))
                   (should-error (gptel-edit-test--key "q") :type 'user-error)
                   (gptel-edit-test--key "R"))))
        (let ((reason (smv/gptel-ediff--review changes)))
          (should (string-match-p "one (proposed changes), line 1" reason))
          (should (string-match-p "Explain this change" reason)))))))

(ert-deftest gptel-edit-malformed-later-section-is-not-silently-skipped ()
  (gptel-edit-test--with-files '(("one" . "old\n") ("two" . "old\n"))
    (dolist (tail '("diff --git a/two b/two\n--- a/two\n+++ b/two\ninvalid\n"
                    "@@ broken header\n-old\n+new\n"
                    "diff --git a/two b/two\nGIT binary patch\n"))
      (should-error
       (gptel-agent--edit-files directory nil
                                (concat "--- a/one\n+++ b/one\n@@ -1 +1 @@\n-old\n+new\n" tail) t))
      (should (equal (gptel-edit-test--read "one") "old\n")))))

(ert-deftest gptel-edit-already-applied-patch-fails-without-reversing ()
  (gptel-edit-test--with-files '(("one" . "new\n"))
    (should-error (gptel-agent--edit-files "one" nil
                                          "--- one\n+++ one\n@@ -1 +1 @@\n-old\n+new\n" t))
    (should (equal (gptel-edit-test--read "one") "new\n"))))

(ert-deftest gptel-edit-preserves-crlf-and-unicode ()
  (gptel-edit-test--with-files nil
    (let ((coding-system-for-write 'utf-8-dos))
      (with-temp-file "one" (insert "café\n")))
    (gptel-agent--edit-files "one" nil "--- one\n+++ one\n@@ -1 +1 @@\n-café\n+été\n" t)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally "one")
      (should (equal (buffer-string) (encode-coding-string "été\n" 'utf-8-dos))))))

(ert-deftest gptel-ediff-abort-cleans-up-real-session ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (let ((gptel-confirm-tool-calls t)
          (ediff-window-setup-function #'ediff-setup-windows-plain)
          (registry (copy-sequence ediff-session-registry)))
      (cl-letf (((symbol-function 'recursive-edit) (lambda () (signal 'quit nil))))
        (should (plist-get
                 (smv/gptel-ediff-tool-call '(:name "Edit" :args (:path "one" :old_str "old" :new_str "new")))
                 :block)))
      (should (equal ediff-session-registry registry))
      (should-not (cl-find-if (lambda (b) (string-prefix-p "*gptel-" (buffer-name b))) (buffer-list)))
      (should (equal (gptel-edit-test--read "one") "old\n")))))

(ert-deftest gptel-ediff-picker-preserves-hunk-position ()
  (gptel-edit-test--with-files '(("one" . "first\na\nb\nc\nlast\n") ("sub/two" . "second\n"))
    (let ((changes (gptel-agent-edit-prepare
                    "Edit" (list :path directory :diff t :new_str gptel-edit-test--multi-diff)))
          (ediff-window-setup-function #'ediff-setup-windows-plain)
          (step 0))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (car (nth 1 choices))))
                ((symbol-function 'recursive-edit)
                 (lambda ()
                   (pcase (cl-incf step)
                     (1 (with-current-buffer (gptel-edit-test--control) (ediff-jump-to-difference 2))
                        (gptel-edit-test--key "f"))
                     (2 (gptel-edit-test--key "P"))
                     (3 (should (= 1 (buffer-local-value 'ediff-current-difference (gptel-edit-test--control))))
                        (gptel-edit-test--key "q"))
                     (_ (ert-fail "Review did not exit"))))))
        (should-not (smv/gptel-ediff--review changes)))
      (should (= step 3)))))

(ert-deftest gptel-ediff-whitespace-change-is-visible-despite-global-options ()
  (gptel-edit-test--with-files '(("one" . "text\n"))
    (let ((changes (gptel-agent-edit-prepare "Edit" '(:path "one" :old_str "text" :new_str "text  ")))
          (ediff-diff-options "-w")
          (ediff-window-setup-function #'ediff-setup-windows-plain))
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda ()
                   (should (= 1 (buffer-local-value 'ediff-number-of-differences (gptel-edit-test--control))))
                   (gptel-edit-test--key "q"))))
        (should-not (smv/gptel-ediff--review changes)))
      (should (equal ediff-diff-options "-w")))))

(ert-deftest gptel-ediff-modified-preview-is-not-applied ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (let ((gptel-confirm-tool-calls t)
          (ediff-window-setup-function #'ediff-setup-windows-plain))
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda ()
                   (with-current-buffer (buffer-local-value 'ediff-buffer-B (gptel-edit-test--control))
                     (let ((inhibit-read-only t)) (goto-char (point-max)) (insert "manual change\n")))
                   (gptel-edit-test--key "q"))))
        (should (plist-get (smv/gptel-ediff-tool-call
                            '(:name "Edit" :args (:path "one" :old_str "old" :new_str "new")))
                           :block)))
      (should (equal (gptel-edit-test--read "one") "old\n")))))

(provide 'gptel-agent-edit-test)
;;; gptel-agent-edit-test.el ends here
