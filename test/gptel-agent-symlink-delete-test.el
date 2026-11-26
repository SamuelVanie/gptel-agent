;;; gptel-agent-symlink-delete-test.el --- Structured link deletion -*- lexical-binding: t; -*-

(require 'gptel-agent-edit-test)
(require 'gptel-agent-history)

(defun gptel-agent-symlink-test--batch (&rest operations)
  "Prepare ordered OPERATIONS against the current directory."
  (gptel-agent-edit-prepare "Edit" (list :mode "batch" :root_directory "."
                                         :operations (vconcat operations))))

(defun gptel-agent-symlink-test--patch (path payload)
  "Return a deletion patch for a symlink at PATH containing PAYLOAD."
  (concat "diff --git a/" path " b/" path "\ndeleted file mode 120000\n"
          "--- a/" path "\n+++ /dev/null\n@@ -1 +0,0 @@\n-"
          payload "\n\\ No newline at end of file\n"))

(ert-deftest gptel-edit-symlink-dangling-deletion-history-round-trip ()
  (dolist (mode '(batch file-patch directory-patch nested-patch))
    (gptel-edit-test--with-files nil
      (make-symbolic-link "./missing" "link")
      (let* ((patch (gptel-agent-symlink-test--patch "link" "./missing"))
             (args (pcase mode
                     ('batch '(:mode "batch" :root_directory "."
                               :operations [(:operation "delete" :file_path "link")]))
                     ('file-patch (list :mode "patch" :file_path "link" :new_str patch))
                     ('directory-patch (list :mode "patch" :root_directory "." :new_str patch))
                     ('nested-patch
                      (list :mode "batch" :root_directory "."
                            :operations (vector (list :operation "patch" :file_path "link"
                                                      :new_str patch)))))))
        (should-not (plist-get (gptel-edit-test--dispatch "Edit" args nil) :error)))
      (should-not (file-symlink-p "link"))
      (gptel-agent-history-restore (car (gptel-agent-history--entries)))
      (should (equal (file-symlink-p "link") "./missing"))
      (should-not (file-exists-p "missing"))
      (gptel-agent-history-restore (car (gptel-agent-history--entries)))
      (should-not (file-symlink-p "link"))
      (should-not (file-exists-p "link"))
      (should-not (file-exists-p "missing")))))

(ert-deftest gptel-edit-symlink-deletion-patch-requires-link-payload ()
  (gptel-edit-test--with-files '(("rules" . "shared text"))
    (make-symbolic-link "./rules" "AGENTS.md")
    (dolist (wrong '("shared text" "rules" "./ rules"))
      (should-error (gptel-agent--edit-files
                     "patch" nil "." nil
                     (gptel-agent-symlink-test--patch "AGENTS.md" wrong) nil)))
    (should (equal (file-symlink-p "AGENTS.md") "./rules"))
    (should (equal (gptel-edit-test--read "rules") "shared text"))))

(ert-deftest gptel-edit-symlink-deletion-requires-git-entry-type ()
  (gptel-edit-test--with-files '(("regular" . "rules") ("rules" . "shared"))
    (make-symbolic-link "rules" "link")
    ;; The hunk would match either entry.  The Git metadata distinguishes them.
    (dolist (case '(("regular" "120000") ("link" "100644")))
      (let ((patch (string-replace "deleted file mode 120000"
                                   (concat "deleted file mode " (cadr case))
                                   (gptel-agent-symlink-test--patch (car case) "rules"))))
        (should-error (gptel-agent--edit-files "patch" nil "." nil patch nil))))
    (should (equal (file-symlink-p "link") "rules"))
    (should (equal (gptel-edit-test--read "regular") "rules"))
    (should (equal (gptel-edit-test--read "rules") "shared"))))

(ert-deftest gptel-edit-symlink-delete-then-write-replaces-entry-only ()
  (gptel-edit-test--with-files '(("shared" . "shared text"))
    (make-symbolic-link "shared" "AGENTS.md")
    (make-symbolic-link "shared" "second")
    ;; Use the literal link payload as replacement text.  The type change
    ;; alone is significant and must survive coalescing and undo.
    (let ((changes (gptel-agent-symlink-test--batch
                    '(:operation "delete" :file_path "AGENTS.md")
                    '(:operation "write" :file_path "./AGENTS.md" :content "shared"))))
      (gptel-agent-edit-apply changes)
      (should-not (file-symlink-p "AGENTS.md"))
      (should (equal (gptel-edit-test--read "AGENTS.md") "shared"))
      (should (equal (gptel-edit-test--read "second") "shared text"))
      (gptel-agent-history-restore (car (gptel-agent-history--entries)))
      (should (equal (file-symlink-p "AGENTS.md") "shared"))
      (should (equal (gptel-edit-test--read "shared") "shared text")))))

(ert-deftest gptel-edit-symlink-text-edit-and-delete-remain-distinct ()
  (gptel-edit-test--with-files '(("rules" . "old"))
    (make-symbolic-link "rules" "link")
    (make-symbolic-link "rules" "second")
    (gptel-agent-edit-apply
     (gptel-agent-symlink-test--batch
      '(:operation "replace" :file_path "link" :old_str "old" :new_str "new")
      '(:operation "delete" :file_path "link")))
    (should-not (file-symlink-p "link"))
    (should (equal (gptel-edit-test--read "second") "new"))
    (gptel-agent-history-restore (car (gptel-agent-history--entries)))
    (should (equal (file-symlink-p "link") "rules"))
    (should (equal (gptel-edit-test--read "second") "old"))))

(ert-deftest gptel-edit-symlink-deletion-rejects-entry-type-change ()
  (gptel-edit-test--with-files '(("target" . "target"))
    (make-symbolic-link "target" "link")
    (let ((changes (gptel-agent-symlink-test--batch '(:operation "delete" :file_path "link"))))
      (delete-file "link")
      (with-temp-file "link" (insert "target"))
      (should-error (gptel-agent-edit-apply changes))
      (should-not (file-symlink-p "link"))
      (should (equal (gptel-edit-test--read "link") "target"))
      (should (equal (gptel-edit-test--read "target") "target")))))

(ert-deftest gptel-edit-symlink-history-rejects-occupied-dangling-entry ()
  (gptel-edit-test--with-files '(("target" . "data"))
    (make-symbolic-link "target" "link")
    (gptel-agent-edit-apply
     (gptel-agent-symlink-test--batch '(:operation "delete" :file_path "link")))
    (make-symbolic-link "unrelated-missing" "link")
    (let ((snapshots (gptel-agent-history--entries)))
      (should-error (gptel-agent-history-restore (car snapshots)))
      (should (equal (gptel-agent-history--entries) snapshots))
      (should (equal (file-symlink-p "link") "unrelated-missing"))
      (should-not (file-exists-p "unrelated-missing"))
      (should (equal (gptel-edit-test--read "target") "data")))))

(ert-deftest gptel-edit-symlink-rollback-recreates-exact-link ()
  (gptel-edit-test--with-files '(("rules" . "data") ("later" . "initial"))
    (make-symbolic-link "./rules" "link")
    (let ((changes (gptel-agent-symlink-test--batch
                    '(:operation "delete" :file_path "link")
                    '(:operation "write" :file_path "later" :content "new")))
          (later (file-truename "later"))
          (link (expand-file-name "link"))
          (write-region-fn (symbol-function 'write-region))
          failed-after-unlink)
      (cl-letf (((symbol-function 'write-region)
                 (lambda (start end filename &rest args)
                   (if (and (equal filename later) (equal (buffer-string) "new"))
                       (progn
                         (setq failed-after-unlink (not (file-symlink-p link)))
                         (error "Simulated second write failure"))
                     (apply write-region-fn start end filename args)))))
        (should-error (gptel-agent-edit-apply changes)))
      (should failed-after-unlink)
      (should (equal (file-symlink-p "link") "./rules"))
      (should (equal (gptel-edit-test--read "rules") "data"))
      (should (equal (gptel-edit-test--read "later") "initial"))
      (should-not (gptel-agent-history--entries)))))

(ert-deftest gptel-edit-symlink-retarget-during-snapshot-preserves-user-entry ()
  (gptel-edit-test--with-files '(("rules" . "shared") ("other" . "shared") ("first" . "initial"))
    (make-symbolic-link "rules" "link")
    (let ((changes (gptel-agent-symlink-test--batch
                    '(:operation "write" :file_path "first" :content "proposed")
                    '(:operation "delete" :file_path "link")))
          (record (symbol-function 'gptel-agent-history--record)))
      (cl-letf (((symbol-function 'gptel-agent-history--record)
                 (lambda (batch)
                   (prog1 (funcall record batch)
                     (delete-file "link")
                     (make-symbolic-link "other" "link")))))
        (should-error (gptel-agent-edit-apply changes)))
      (should (equal (gptel-edit-test--read "first") "initial"))
      (should (equal (file-symlink-p "link") "other"))
      (should (equal (gptel-edit-test--read "rules") "shared"))
      (should (equal (gptel-edit-test--read "other") "shared")))))

(ert-deftest gptel-edit-symlink-delete-enforces-canonical-parent-containment ()
  (gptel-edit-test--with-files '(("outside/rules" . "data"))
    (make-directory "inside")
    (make-symbolic-link "../outside" "inside/parent")
    (make-symbolic-link "rules" "outside/link")
    (should-error
     (gptel-agent--edit-files
      "batch" nil "inside" nil nil [(:operation "delete" :file_path "parent/link")]))
    (should-error
     (gptel-agent--edit-files
      "patch" nil "inside" nil (gptel-agent-symlink-test--patch "parent/link" "rules") nil))
    (should (equal (file-symlink-p "outside/link") "rules"))
    (should (equal (gptel-edit-test--read "outside/rules") "data"))))

(ert-deftest gptel-edit-symlink-history-diff-identifies-a-link ()
  (gptel-edit-test--with-files nil
    (let ((destination "./a\r\nb"))
      (make-symbolic-link destination "link")
      (let* ((changes (gptel-agent-symlink-test--batch '(:operation "delete" :file_path "link")))
             (snapshot (gptel-agent-edit-apply changes t))
             before after)
        (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () snapshot))
                  ((symbol-function 'completing-read) (lambda (_prompt choices &rest _) (car choices)))
                  ((symbol-function 'diff)
                   (lambda (old new &rest _)
                     (setq before (with-current-buffer old (buffer-string))
                           after (with-current-buffer new (buffer-string))))))
          (gptel-agent-history-diff))
        (should (string-match-p "Symbolic link" before))
        (should (equal after ""))
        (gptel-agent-history-restore snapshot)
        (should (equal (file-symlink-p "link") destination))))))

(ert-deftest gptel-edit-symlink-deletion-policy-checks-the-entry ()
  (gptel-edit-test--with-files '(("allowed/rules" . "in") ("other/rules" . "out"))
    (make-symbolic-link "../other/rules" "allowed/link")
    (make-symbolic-link "../allowed/rules" "other/link")
    (let* ((gptel-agent-policy--definitions (make-hash-table))
           (ceiling (list (cons 'local-edits (expand-file-name "allowed" directory))))
           (tool (gptel-get-tool "Edit"))
           (third-party (gptel--copy-tool tool))
           (patch (gptel-agent-symlink-test--patch "link" "../other/rules")))
      (setf (gptel-tool-category third-party) "example")
      (gptel-agent-define-tool-policy
       'local-edits :allow '((:tool "Edit" :args (:mode "patch" :file_path (under workspace)))
                             (:tool "Edit" :args (:mode "batch" :operations
                                                       (every (object :file_path (under workspace)))))))
      (dolist (root '("allowed" "other"))
        (let* ((inside (equal root "allowed"))
               (args (list :mode "patch" :file_path (concat root "/link") :new_str patch))
               (batch (list :mode "batch" :root_directory root :operations
                            [(:operation "delete" :file_path "link")])))
          (should (eq (not (null (gptel-agent-policy--allows-p ceiling tool args directory)))
                      inside))
          (should (eq (not (null (gptel-agent-policy--allows-p ceiling tool batch directory)))
                      inside))
          ;; A different package's Edit tool can interpret its arguments differently.
          (should (eq (not (null (gptel-agent-policy--allows-p ceiling third-party args directory)))
                      (not inside)))))
      ;; A literal still matches the supplied spelling.
      (gptel-agent-define-tool-policy
       'local-edits :allow '((:tool "Edit" :args (:file_path "allowed/link"))))
      (should (gptel-agent-policy--allows-p
               ceiling tool (list :mode "patch" :file_path "allowed/link" :new_str patch) directory))
      (should-not (gptel-agent-policy--allows-p
                   ceiling tool (list :mode "patch" :file_path "./allowed/link" :new_str patch)
                   directory)))))

(ert-deftest gptel-edit-symlink-policy-resolves-ordered-batch ()
  (gptel-edit-test--with-files '(("local/rules" . "inside") ("outside" . "shared"))
    (make-symbolic-link "../outside" "local/link")
    (let* ((gptel-agent-policy--definitions (make-hash-table))
           (ceiling (list (cons 'review (expand-file-name "local" directory))))
           (tool (gptel-get-tool "Edit"))
           (args '(:mode "batch" :root_directory "local"
                   :operations [(:operation "delete" :file_path "link")
                                (:operation "write" :file_path "link" :content "draft")])))
      (gptel-agent-define-tool-policy
       'review :allow '((:tool "Edit" :args (:operations (every (object :file_path (under workspace)))))))
      (should (gptel-agent-policy--allows-p ceiling tool args directory))
      (should-not (gptel-agent-policy--allows-p
                   ceiling tool '(:mode "batch" :root_directory "local"
                                  :operations [(:operation "write" :file_path "link" :content "draft")])
                   directory))
      (gptel-agent-define-tool-policy
       'review :allow '((:tool "Edit"))
       :deny '((:tool "Edit" :args (:operations (every (object :file_path (under workspace)))))))
      (should-not (gptel-agent-policy--allows-p ceiling tool args directory)))))

(ert-deftest gptel-edit-exclusive-create-conflict-preserves-other-writer ()
  (dolist (kind '(create-text create-link replace-link changed-earlier))
    (gptel-edit-test--with-files '(("first" . "original") ("rules" . "shared"))
      (when (eq kind 'replace-link) (make-symbolic-link "./rules" "new"))
      (let* ((path (expand-file-name "new" (file-truename directory)))
             (first (gptel-agent-edit-prepare "Write" '(:file_path "first" :content "updated")))
             (change (if (eq kind 'create-link)
                         ;; History restore of a deleted link.
                         (list :path path :before nil :after '(symlink . "./rules"))
                       (list :path path :before (and (eq kind 'replace-link) '(symlink . "./rules"))
                             :after "proposed")))
             (write (symbol-function 'write-region))
             (symlink (symbol-function 'make-symbolic-link))
             raced err)
        (cl-letf (((symbol-function 'write-region)
                   (lambda (start end file &rest args)
                     (when (and (equal file path) (eq (nth 3 args) 'excl) (not raced))
                       (setq raced t)
                       (funcall write "competing text" nil path nil 'silent)
                       (when (eq kind 'changed-earlier)
                         (funcall write "later change" nil (plist-get (car first) :path) nil 'silent)))
                     (apply write start end file args)))
                  ((symbol-function 'make-symbolic-link)
                   (lambda (destination file &optional overwrite)
                     (when (and (eq kind 'create-link) (equal file path) (not raced))
                       (setq raced t)
                       ;; Even an identical destination belongs to the winner.
                       (funcall symlink destination file))
                     (funcall symlink destination file overwrite))))
          (setq err (should-error (gptel-agent-edit-apply (append first (list change))))))
        (should raced)
        (should (equal (gptel-edit-test--read "first")
                       (if (eq kind 'changed-earlier) "later change" "original")))
        (if (eq kind 'create-link)
            (should (equal (file-symlink-p path) "./rules"))
          (should-not (file-symlink-p path))
          (should (equal (gptel-edit-test--read path) "competing text")))
        (when (memq kind '(replace-link changed-earlier))
          (should (string-match-p "recover from snapshots" (error-message-string err))))
        (should (equal (gptel-edit-test--read "rules") "shared"))
        (should-not (gptel-agent-history--entries))))))

(ert-deftest gptel-edit-exclusive-partial-write-restores-removed-link ()
  (gptel-edit-test--with-files '(("rules" . "shared"))
    (make-symbolic-link "./rules" "link")
    (let* ((changes (gptel-agent-symlink-test--batch
                     '(:operation "delete" :file_path "link")
                     '(:operation "write" :file_path "link" :content "replacement")))
           (path (plist-get (car changes) :path))
           (write (symbol-function 'write-region)) partial)
      (cl-letf (((symbol-function 'write-region)
                 (lambda (start end file &rest args)
                   (if (and (equal file path) (eq (nth 3 args) 'excl))
                       (progn
                         (funcall write "partial" nil file nil 'silent nil 'excl)
                         (setq partial t)
                         (signal 'file-error '("Simulated partial write")))
                     (apply write start end file args)))))
        (should-error (gptel-agent-edit-apply changes) :type 'file-error))
      (should partial)
      (should (equal (file-symlink-p path) "./rules"))
      (should (equal (gptel-edit-test--read "rules") "shared"))
      (should-not (gptel-agent-history--entries)))))

(provide 'gptel-agent-symlink-delete-test)
;;; gptel-agent-symlink-delete-test.el ends here
