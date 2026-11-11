;;; gptel-agent-path-test.el --- File tool contracts -*- lexical-binding: t; -*-

(require 'gptel-agent-edit-test)

(ert-deftest gptel-path-dotted-directories-and-extensionless-files ()
  (gptel-edit-test--with-files nil
    (gptel-agent--make-directory "a.b/nested/.config")
    (should (file-directory-p "a.b/nested/.config"))
    (gptel-agent--make-directory "a.b/nested/.config")
    (gptel-agent--write-file "a.b/nested/.config/README" "first")
    (gptel-agent--edit-files "replace" "a.b/nested/.config/README" nil "first" "second")
    (should (equal (gptel-edit-test--read "a.b/nested/.config/README") "second"))))

(ert-deftest gptel-path-invalid-creation-has-no-side-effects-through-dispatch ()
  (dolist (confirm '(nil t))
    (dolist (immediate '(nil t))
      (gptel-edit-test--with-files nil
        (let ((gptel-agent-review-apply-immediately immediate))
          (dolist (call '(("Write" (:file_path "missing/Poulet.java/" :content "class Poulet {}"))
                          ("Edit" (:mode "patch" :root_directory "." :new_str "--- /dev/null\n+++ b/missing/Poulet.java/\n@@ -0,0 +1 @@\n+class Poulet {}\n"))
                          ("Edit" (:mode "batch" :root_directory "." :operations [(:operation "write" :file_path "missing/Poulet.java/" :content "class Poulet {}")]))))
            (let ((result (gptel-edit-test--dispatch (car call) (cadr call) confirm)))
              (should-not (string-match-p "Applied changes" (plist-get result :result)))
              (should (string-match-p "expected:" (plist-get result :result))))
            (should-not (file-exists-p "missing"))
            (should-not (file-exists-p gptel-agent-history-directory))
            (should-not gptel-agent-review--queue)))))))

(ert-deftest gptel-path-directory-errors-precede-text-errors ()
  (gptel-edit-test--with-files nil
    (gptel-agent--make-directory "Poulet.java")
    (dolist (call '(("Edit" (:mode "replace" :file_path "Poulet.java/" :old_str "" :new_str "text"))
                    ("Insert" (:file_path "Poulet.java" :line_number 0 :new_str "text"))
                    ("Write" (:file_path "Poulet.java" :content "text"))
                    ("Read" (:file_path "Poulet.java"))))
      (let ((result (gptel-edit-test--dispatch (car call) (cadr call) nil)))
        (should-not (string-match-p "Applied changes" (plist-get result :result)))
        (should (string-match-p "actual: directory" (plist-get result :result)))
        (should (string-match-p "PathInfo" (plist-get result :result)))))
    (should (file-directory-p "Poulet.java"))))

(ert-deftest gptel-path-creation-rejects-empty-dot-and-parent-obstruction ()
  (gptel-edit-test--with-files '(("blocker" . "keep"))
    (dolist (path '("" "." ".." "missing/." "missing/.." "blocker/child/file"))
      (should-error (gptel-agent--write-file path "wrong")))
    (should-not (file-exists-p "missing"))
    (should (equal (gptel-edit-test--read "blocker") "keep"))))

(ert-deftest gptel-path-batch-file-parent-conflict-is-rejected-before-writing ()
  (gptel-edit-test--with-files nil
    (should-error
     (gptel-agent--edit-files
      "batch" nil "." nil nil
      '[(:operation "write" :file_path "new" :content "file")
        (:operation "write" :file_path "new/child" :content "child")]))
    (should-not (file-exists-p "new"))
    (should-not (file-exists-p gptel-agent-history-directory))))

(ert-deftest gptel-path-write-creates-missing-parents-without-mkdir ()
  (gptel-edit-test--with-files nil
    (should-not (plist-get (gptel-edit-test--dispatch
                           "Write" '(:file_path "src/main/Poulet.java" :content "class Poulet {}") nil) :error))
    (should (file-regular-p "src/main/Poulet.java"))
    (should (equal (gptel-edit-test--read "src/main/Poulet.java") "class Poulet {}"))))

(ert-deftest gptel-path-edit-requires-explicit-mode-and-target-kind ()
  (gptel-edit-test--with-files '(("one" . "old"))
    (dolist (args '((:file_path "one" :old_str "old" :new_str "new")
                    (:mode "replace" :root_directory "." :old_str "old" :new_str "new")
                    (:mode "patch" :file_path "one" :root_directory "." :new_str "diff")
                    (:mode "batch" :file_path "one" :operations [(:operation "write" :file_path "x" :content "x")])
                    (:mode "replace" :file_path "one" :new_str "--- one\n+++ one\n@@ -1 +1 @@\n-old\n+new\n")))
      (should-error (gptel-agent-edit-prepare "Edit" args)))
    (should (equal (gptel-edit-test--read "one") "old"))))

(ert-deftest gptel-path-patch-root-can-be-a-dotted-directory ()
  (gptel-edit-test--with-files nil
    (gptel-agent--make-directory "package.java")
    (gptel-agent--edit-files "patch" nil "package.java" nil
                             "--- /dev/null\n+++ b/sub/File.java\n@@ -0,0 +1 @@\n+class File {}\n")
    (should (file-regular-p "package.java/sub/File.java"))))

(ert-deftest gptel-path-mkdir-errors-are-signalled ()
  (gptel-edit-test--with-files '(("existing" . "keep"))
    (should-error (gptel-agent--make-directory "existing") :type 'gptel-agent-path-error)
    (should-error (gptel-agent--make-directory "existing/sub") :type 'gptel-agent-path-error)
    (should (equal (gptel-edit-test--read "existing") "keep"))))

(ert-deftest gptel-path-inspection-and-empty-directory-recovery ()
  (gptel-edit-test--with-files nil
    (should (string-match-p "Type on disk: missing" (gptel-agent--path-info "Poulet.java")))
    (gptel-agent--make-directory "Poulet.java")
    (should (string-match-p "Type on disk: directory" (gptel-agent--path-info "Poulet.java/")))
    (gptel-agent--remove-directory "Poulet.java/")
    (gptel-agent--write-file "Poulet.java" "class Poulet {}")
    (should (string-match-p "Type on disk: file" (gptel-agent--path-info "Poulet.java")))
    (make-symbolic-link "missing" "broken")
    (should (string-match-p "broken symlink" (gptel-agent--path-info "broken")))
    (should-error (gptel-agent--write-file "broken" "text"))))

(ert-deftest gptel-path-rmdir-never-removes-contents-or-follows-links ()
  (gptel-edit-test--with-files '(("nonempty/keep" . "safe"))
    (gptel-agent--make-directory "empty")
    (make-symbolic-link "empty" "alias")
    (dolist (path '("nonempty" "alias/" "." ".." "/" "nonempty/keep" "absent"))
      (should-error (gptel-agent--remove-directory path)))
    (should (file-directory-p "empty"))
    (should (file-symlink-p "alias"))
    (should (equal (gptel-edit-test--read "nonempty/keep") "safe"))))

(defun gptel-path-test--glob (&rest args)
  (let ((deadline (+ (float-time) 10)) result)
    (apply #'gptel-agent--glob (lambda (value) (setq result value)) args)
    (while (and (not result) (< (float-time) deadline)) (accept-process-output nil 0.01))
    (should result)
    result))

(ert-deftest gptel-path-glob-is-typed-with-isolated-fallback ()
  (gptel-edit-test--with-files '(("File.java" . "text") ("plain" . "text")
                                (".hidden" . "hidden") ("a.b/deep/child" . "child")
                                (".git/config" . "excluded"))
    (make-symbolic-link "." "loop")
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (let ((all (gptel-path-test--glob "*" "." nil "all"))
            (files (gptel-path-test--glob "*" "." 1))
            (dirs (gptel-path-test--glob "*" "." 1 "directories")))
        (should (string-match-p (regexp-quote "[directory]") all))
        (should (string-match-p (regexp-quote "[directory symlink]") all))
        (should (string-match-p "child" all))
        (should (string-match-p "\\.hidden" files))
        (should-not (string-match-p "\\.git" all))
        (should-not (string-match-p "loop/" all))
        (should-not (string-match-p "child\\|\\[directory" files))
        (should-not (string-match-p "\\[file\\]" dirs))
        (should (string-match-p "File.java" (gptel-path-test--glob "*.JAVA" "." 1)))))
    (should-error (gptel-path-test--glob "*" "File.java"))
    (should-error (gptel-path-test--glob "*" "." 0))))

(ert-deftest gptel-path-new-schemas-expose-complete-targets ()
  (dolist (entry '(("Write" "file_path" "content") ("Mkdir" "directory_path")
                   ("Rmdir" "directory_path") ("PathInfo" "path")
                   ("Insert" "file_path" "line_number" "new_str")
                   ("Glob" "pattern" "root_directory" "depth" "type" "max_results" "max_entries")
                   ("Edit" "mode" "file_path" "root_directory" "old_str" "new_str" "operations")))
    (should (equal (mapcar (lambda (arg) (plist-get arg :name)) (gptel-tool-args (gptel-get-tool (car entry))))
                   (cdr entry)))))

(ert-deftest gptel-path-pending-review-cannot-use-a-staged-file-as-a-parent ()
  (gptel-edit-test--with-files nil
    (let ((gptel-agent-review-apply-immediately nil))
      (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
        (should-not (plist-get (gptel-edit-test--dispatch "Write" '(:file_path "new" :content "file") t) :error))
        (let ((result (gptel-edit-test--dispatch "Write" '(:file_path "new/child" :content "child") t)))
          (should (plist-get result :error))
          (should (string-match-p "file in pending changes" (plist-get result :result))))
        (should-not (file-exists-p "new"))))))

(ert-deftest gptel-path-batch-rejects-remote-targets-before-filesystem-access ()
  (cl-letf (((symbol-function 'file-truename) (lambda (&rest _) (ert-fail "Unexpected filesystem access"))))
    (should-error (gptel-agent-edit--batch-path "/ssh:example:/tmp/file" "/tmp/"))))

(provide 'gptel-agent-path-test)
;;; gptel-agent-path-test.el ends here
