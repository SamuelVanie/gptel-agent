;;; gptel-agent-edit-test.el --- File edit and Ediff regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent-tools)

(ert-deftest gptel-ediff-tools-load-registers-review ()
  ;; Do not require the hook in this test suite: normal package loading must
  ;; enable it, just as it does in a user's Emacs session.
  (should (featurep 'gptel-ediff-prehook))
  (should (memq #'smv/gptel-ediff-tool-call
                (default-value 'gptel-pre-tool-call-functions))))

(defmacro gptel-edit-test--with-files (files &rest body)
  "Create FILES in a temporary directory and run BODY there."
  (declare (indent 1))
  `(with-temp-buffer
     (let* ((directory (make-temp-file "gptel-edit-test-" t))
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
         (delete-directory directory t)))))

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
      (cl-letf (((symbol-function 'smv/gptel-ediff-open)
                 (lambda (_) (ert-fail "Invalid proposal reached review"))))
        (should (plist-get
                 (smv/gptel-ediff-tool-call
                  '(:name "Edit" :args (:path "one" :diff t :new_str "invalid patch")))
                 :block))))))

(defun gptel-edit-test--control ()
  (or (cl-find-if (lambda (b) (with-current-buffer b (eq major-mode 'ediff-mode)))
                  (buffer-list))
      (ert-fail "No Ediff control buffer")))

(defun gptel-edit-test--key (key)
  (with-current-buffer (gptel-edit-test--control)
    (call-interactively (key-binding (kbd key)))))

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

(defconst gptel-edit-test--mixed-operations
  '[(:operation "replace" :path "one" :old_str "old" :new_str "new")
    (:operation "insert" :path "./one" :line_number 1 :new_str "inserted")
    (:operation "write" :path "nested/created" :content "draft\n")
    (:operation "patch" :path "nested/created" :new_str "--- created\n+++ created\n@@ -1 +1 @@\n-draft\n+final\n")
    (:operation "write" :path "empty" :content "")
    (:operation "write" :path "overwrite" :content "replacement\n")
    (:operation "delete" :path "obsolete")])

(ert-deftest gptel-edit-mixed-batch-stages-in-order-and-applies-final-results ()
  (gptel-edit-test--with-files '(("one" . "old\ntail\n") ("overwrite" . "original\n") ("obsolete" . ""))
                               (let ((changes (gptel-agent-edit-prepare
                                               "Edit" (list :path directory :operations gptel-edit-test--mixed-operations))))
                                 (should (= 5 (length changes)))
                                 (should (equal (plist-get (car changes) :before) "old\ntail\n"))
                                 (should (equal (plist-get (car changes) :after) "new\ninserted\ntail\n"))
                                 (should (equal (plist-get (cadr changes) :after) "final\n"))
                                 (should (equal (gptel-edit-test--read "one") "old\ntail\n"))
                                 (should-not (file-exists-p "nested"))
                                 ;; Exercise the same registered function called with confirmation off.
                                 (gptel-agent--edit-files directory nil nil nil gptel-edit-test--mixed-operations)
                                 (dolist (change changes)
                                   (let ((path (plist-get change :path)) (after (plist-get change :after)))
                                     (if after (should (equal (gptel-edit-test--read path) after))
                                       (should-not (file-exists-p path)))))
                                 (should (file-exists-p "empty")))))

(ert-deftest gptel-edit-batch-directory-patch-sees-created-and-inserted-content ()
  (gptel-edit-test--with-files '(("existing" . "first\n"))
                               (gptel-agent--edit-files
                                directory nil nil nil
                                '[(:operation "write" :path "new" :content "hello\n")
                                  (:operation "insert" :path "existing" :line_number -1 :new_str "second\n")
                                  (:operation "patch" :path "." :new_str "--- a/new\n+++ b/new\n@@ -1 +1 @@\n-hello\n+HELLO\n--- a/existing\n+++ b/existing\n@@ -2 +2 @@\n-second\n+SECOND\n")])
                               (should (equal (gptel-edit-test--read "new") "HELLO\n"))
                               (should (equal (gptel-edit-test--read "existing") "first\nSECOND\n"))))

(ert-deftest gptel-edit-batch-invalid-later-operation-writes-nothing ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
                               (let ((failure
                                      (should-error
                                       (gptel-agent--edit-files
                                        directory nil nil nil
                                        '[(:operation "write" :path "nested/new" :content "new\n")
                                          (:operation "delete" :path "one")
                                          (:operation "insert" :path "one" :line_number 0 :new_str "oops")]))))
                                 (should (string-match-p "Operation 3: File was deleted" (error-message-string failure)))
                                 (should (equal (gptel-edit-test--read "one") "old\n"))
                                 (should-not (file-exists-p "nested")))))

(ert-deftest gptel-edit-batch-retains-first-snapshot-across-recreation ()
  (gptel-edit-test--with-files '(("one" . "original"))
                               (let ((changes (gptel-agent-edit-prepare
                                               "Edit" (list :path directory :operations
                                                            '[(:operation "delete" :path "one")
                                                              (:operation "write" :path "./one" :content "replacement")]))))
                                 (should (= 1 (length changes)))
                                 (should (equal (plist-get (car changes) :before) "original"))
                                 (should (equal (plist-get (car changes) :after) "replacement"))
                                 (with-temp-file "one" (insert "user change"))
                                 (should-error (gptel-agent-edit-apply changes))
                                 (should (equal (gptel-edit-test--read "one") "user change")))))

(ert-deftest gptel-edit-batch-discards-transient-files-and-rejects-net-noop ()
  (gptel-edit-test--with-files nil
                               (should-error
                                (gptel-agent--edit-files directory nil nil nil
                                                         '[(:operation "write" :path "transient" :content "")
                                                           (:operation "delete" :path "transient")]))
                               (gptel-agent--edit-files directory nil nil nil
                                                        '[(:operation "write" :path "transient" :content "")
                                                          (:operation "delete" :path "transient")
                                                          (:operation "write" :path "kept" :content "")])
                               (should-not (file-exists-p "transient"))
                               (should (file-exists-p "kept"))))

(ert-deftest gptel-edit-batch-rejects-invalid-arguments-and-paths ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
                               (dolist (operations
                                        (list [] "invalid" '[(:operation "unknown" :path "one")]
                                              '[(:operation "write" :path "../outside" :content "x")]
                                              '[(:operation "write" :path "/tmp/outside" :content "x")]
                                              '[(:operation "replace" :path "one" :new_str "x")]
                                              '[(:operation "insert" :path "one" :new_str "x")]
                                              '[(:operation "write" :path "new")]))
                                 (should-error (gptel-agent-edit-prepare "Edit" (list :path directory :operations operations))))
                               (should-error
                                (gptel-agent-edit-prepare
                                 "Edit" (list :path directory :new_str "extra" :operations '[(:operation "delete" :path "one")])))))

(ert-deftest gptel-edit-batch-blocks-unsaved-files-and-rolls-back-write-failures ()
  (gptel-edit-test--with-files '(("one" . "original"))
                               (let ((buffer (find-file-noselect "one"))
                                     (operations '[(:operation "write" :path "new" :content "created")
                                                   (:operation "write" :path "one" :content "replaced")]))
                                 (with-current-buffer buffer (insert "unsaved"))
                                 (should-error (gptel-agent--edit-files directory nil nil nil operations))
                                 (should-not (file-exists-p "new"))
                                 (with-current-buffer buffer (revert-buffer t t))
                                 (let ((write (symbol-function 'gptel-agent-edit--write)))
                                   (cl-letf (((symbol-function 'gptel-agent-edit--write)
                                              (lambda (change content)
                                                (if (equal content "replaced") (error "Simulated failure")
                                                  (funcall write change content)))))
                                     (should-error (gptel-agent--edit-files directory nil nil nil operations))))
                                 (should (equal (gptel-edit-test--read "one") "original"))
                                 (should-not (file-exists-p "new")))))

(ert-deftest gptel-ediff-confirmation-off-skips-all-file-reviews ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
                               (let ((gptel-confirm-tool-calls nil)
                                     (calls '((:name "Insert" :args (:path "one" :line_number 0 :new_str "inserted"))
                                              (:name "Write" :args (:path "." :filename "written" :content "created"))
                                              (:name "Edit" :args (:path "one" :old_str "old" :new_str "new" :diff :json-false))
                                              (:name "Edit" :args (:path "." :operations [(:operation "delete" :path "written")
                                                                                          (:operation "write" :path "empty" :content "")])))))
                                 (cl-letf (((symbol-function 'smv/gptel-ediff-open)
                                            (lambda (_) (ert-fail "Confirmation off opened Ediff"))))
                                   (dolist (call calls)
                                     (should-not (smv/gptel-ediff-tool-call call))
                                     (let ((tool (gptel-get-tool (plist-get call :name))))
                                       (apply (gptel-tool-function tool)
                                              (mapcar (lambda (arg)
                                                        (plist-get (plist-get call :args) (intern (concat ":" (plist-get arg :name)))))
                                                      (gptel-tool-args tool))))))
                                 (should (equal (gptel-edit-test--read "one") "inserted\nnew\n"))
                                 (should-not (file-exists-p "written"))
                                 (should (file-exists-p "empty")))))

(ert-deftest gptel-edit-batch-schema-and-json-dispatch ()
  (gptel-edit-test--with-files '(("old" . "old"))
                               (let* ((tool (gptel-get-tool "Edit"))
                                      ;; Round-trip the outgoing schema: gptel uses uninterned property keys.
                                      (schema (json-parse-string
                                               (gptel--json-encode
                                                (plist-get (plist-get (aref (gptel--parse-tools nil (list tool)) 0) :function) :parameters))
                                               :object-type 'plist :false-object :json-false))
                                      (args (json-parse-string
                                             "{\"path\":\".\",\"operations\":[{\"operation\":\"replace\",\"path\":\"old\",\"old_str\":\"old\",\"new_str\":\"new\"},{\"operation\":\"write\",\"path\":\"created\",\"content\":\"\"}]}"
                                             :object-type 'plist :false-object :json-false)))
                                 (should (equal (plist-get schema :required) ["path"]))
                                 (should (equal (plist-get (plist-get (plist-get (plist-get schema :properties) :operations) :items) :required)
                                                ["operation" "path"]))
                                 (apply (gptel-tool-function tool)
                                        (mapcar (lambda (arg) (plist-get args (intern (concat ":" (plist-get arg :name))))) (gptel-tool-args tool))))
                               (should (equal (gptel-edit-test--read "old") "new"))
                               (should (file-exists-p "created"))))

(ert-deftest gptel-edit-batch-fallback-preview-includes-all-changes ()
  (gptel-edit-test--with-files '(("one" . "old\ntail\n") ("overwrite" . "original\n") ("obsolete" . ""))
                               (with-temp-buffer
                                 (let ((overlays 0))
                                   (cl-letf (((symbol-function 'gptel-agent--confirm-overlay)
                                              (lambda (&rest _) (cl-incf overlays))))
                                     (gptel-agent--edit-files-preview-setup (list directory nil nil nil gptel-edit-test--mixed-operations) nil))
                                   (should (= overlays 1)))
                                 (should (string-match-p "BatchEdit" (buffer-string)))
                                 (should (string-match-p "final" (buffer-string)))
                                 (should (string-match-p (regexp-quote "[delete file]") (buffer-string))))))

(ert-deftest gptel-edit-batch-coalesces-symlink-aliases-within-a-patch ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (make-symbolic-link "one" "alias")
    (let ((changes (gptel-agent-edit-prepare
                    "Edit" (list :path directory :operations
                                 '[(:operation "replace" :path "alias" :old_str "old" :new_str "staged")
                                   (:operation "patch" :path "." :new_str "--- a/one\n+++ b/one\n@@ -1 +1 @@\n-staged\n+next\n--- a/alias\n+++ b/alias\n@@ -1 +1 @@\n-next\n+final\n")]))))
      (should (= 1 (length changes)))
      (should (equal (plist-get (car changes) :before) "old\n"))
      (should (equal (plist-get (car changes) :after) "final\n"))
      (gptel-agent-edit-apply changes)
      (should (file-symlink-p "alias"))
      (should (equal (gptel-edit-test--read "one") "final\n")))))

(defun gptel-edit-test--dispatch (name args confirm)
  "Dispatch NAME with ARGS through gptel's hooks and tool runner.
Use CONFIRM in the current conversation and stop before any network request.
Queued review integration is tested in `gptel-agent-review-test'."
  (let ((gptel-confirm-tool-calls confirm))
    (let* ((call (list :name name :args args))
           (info (list :backend t :buffer (current-buffer)
                       :tools (list (gptel-get-tool name))
                       :tool-use (list call)
                       :callback (lambda (&rest _)
                                   (ert-fail "Unexpected ordinary confirmation prompt"))))
           (fsm (gptel-make-fsm :state 'TPRE
                               :table '((TPRE . ((t . DONE))))
                               :handlers nil :info info)))
      (gptel--handle-pre-tool fsm)
      (gptel--handle-tool-use fsm)
      (should (eq (gptel-fsm-state fsm) 'DONE))
      (should (= 1 (length (plist-get info :tool-result))))
      (should (stringp (plist-get call :result)))
      call)))

(ert-deftest gptel-ediff-dispatch-confirmation-off-applies-without-review ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (cl-letf (((symbol-function 'smv/gptel-ediff-open)
               (lambda (_) (ert-fail "Confirmation off opened Ediff"))))
      (pcase-dolist
          (`(,name ,args)
           '(("Write" (:path "." :filename "created" :content "new"))
             ("Insert" (:path "one" :line_number 0 :new_str "first"))
             ("Edit" (:path "one" :old_str "old" :new_str "new"))
             ("Edit" (:path "." :operations [(:operation "delete" :path "created")
                                              (:operation "write" :path "empty" :content "")]))))
        (should-not (plist-get (gptel-edit-test--dispatch name args nil) :error))))
    (should (equal (gptel-edit-test--read "one") "first\nnew\n"))
    (should-not (file-exists-p "created"))
    (should (file-exists-p "empty"))))

(ert-deftest gptel-ediff-dispatch-invalid-edit-reports-error-without-running-tool ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (cl-letf (((symbol-function 'smv/gptel-ediff-open)
               (lambda (_) (ert-fail "Invalid edit reached Ediff"))))
      (let ((call (gptel-edit-test--dispatch
                   "Edit" '(:path "one" :old_str "missing" :new_str "new") t)))
        (should (plist-get call :error))
        (should (string-match-p "File review could not prepare proposal" (plist-get call :result)))))
    (should (equal (gptel-edit-test--read "one") "old\n"))))

(ert-deftest gptel-edit-replacement-recovers-indentation-and-trailing-whitespace ()
  (gptel-edit-test--with-files '(("one" . "head\n\tif ready:  \n\t    run()\n\ttail()\nend\n"))
    (gptel-agent--edit-files
     "one" "  if ready:\n      run()  \n  tail()\n"
     "  if ready:\n      changed()\n      extra()\n  tail()\n")
    (should (equal (gptel-edit-test--read "one")
                   "head\n\tif ready:\n\t    changed()\n\t    extra()\n\ttail()\nend\n"))))

(ert-deftest gptel-edit-replacement-fallback-keeps-blank-lines-and-literal-text ()
  (gptel-edit-test--with-files '(("one" . "head\n    first\n \t\n    last  \nend\n"))
    (gptel-agent--edit-files "one" "first\n\nlast\n" "new \\1\\&\n\nlast\n")
    (should (equal (gptel-edit-test--read "one") "head\n    new \\1\\&\n\n    last\nend\n"))))

(ert-deftest gptel-edit-replacement-normalizes-crlf-arguments ()
  (gptel-edit-test--with-files '(("one" . "first\nlast\n"))
    (gptel-agent--edit-files "one" "first\r\nlast\r\n" "new\r\nlast\r\n")
    (should (equal (gptel-edit-test--read "one") "new\nlast\n"))))

(ert-deftest gptel-edit-replacement-exact-match-wins-over-whitespace-candidates ()
  (gptel-edit-test--with-files '(("one" . "  first\n  last\n    first\n    last\n"))
    (gptel-agent--edit-files "one" "  first\n  last\n" "EXACT\n")
    (should (equal (gptel-edit-test--read "one") "EXACT\n    first\n    last\n"))))

(ert-deftest gptel-edit-replacement-fallback-rejects-ambiguous-and-changed-text ()
  (gptel-edit-test--with-files '(("one" . "  first\n  last\n    first\n    last\n")
                               ("two" . "    title = \"hello  world\"\n"))
    (should-error (gptel-agent--edit-files "one" "first\nlast\n" "wrong\n"))
    (dolist (old '("TITLE = \"hello  world\"\n" "title = \"hello world\"\n"
                   "title = \"hello  world!\"\n"))
      (should-error (gptel-agent--edit-files "two" old "wrong\n")))
    (should (equal (gptel-edit-test--read "one") "  first\n  last\n    first\n    last\n"))
    (should (equal (gptel-edit-test--read "two") "    title = \"hello  world\"\n"))))

(ert-deftest gptel-edit-patch-recovers-context-drift-and-whitespace ()
  (gptel-edit-test--with-files '(("one" . "changed above\n    old\nchanged below\n"))
    (gptel-agent--edit-files
     "one" nil "--- one\n+++ one\n@@ -1,3 +1,3 @@\n original above\n-  old\n+    new\n original below\n" t)
    (should (equal (gptel-edit-test--read "one") "changed above\n    new\nchanged below\n"))))

(ert-deftest gptel-edit-patch-retry-starts-from-original-after-partial-failure ()
  (gptel-edit-test--with-files '(("one" . "old\nseparator\n  second\n"))
    (gptel-agent--edit-files
     "one" nil "--- one\n+++ one\n@@ -1 +1 @@\n-old\n+new\n@@ -3 +3 @@\n-    second\n+  SECOND\n" t)
    (should (equal (gptel-edit-test--read "one") "new\nseparator\n  SECOND\n"))))

(ert-deftest gptel-edit-patch-tolerance-still-rejects-changed-removed-text ()
  (gptel-edit-test--with-files '(("one" . "above\nchanged\nbelow\n"))
    (should-error
     (gptel-agent--edit-files
      "one" nil "--- one\n+++ one\n@@ -1,3 +1,3 @@\n above\n-old\n+new\n below\n" t))
    (should (equal (gptel-edit-test--read "one") "above\nchanged\nbelow\n"))))

(ert-deftest gptel-ediff-preparation-error-is-visible-and-actionable ()
  (gptel-edit-test--with-files '(("one" . "old\n"))
    (let (messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (push (apply #'format format args) messages))))
        (let ((call (gptel-edit-test--dispatch
                     "Edit" '(:path "one" :old_str "missing" :new_str "new") t)))
          (should (plist-get call :error))
          (should (string-match-p "Read the current file" (plist-get call :result)))))
      (should (cl-some (lambda (text) (string-match-p "Could not find old_str" text)) messages)))))

(provide 'gptel-agent-edit-test)
;;; gptel-agent-edit-test.el ends here
