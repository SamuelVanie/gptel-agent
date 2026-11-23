;;; gptel-agent-introspection-test.el --- Info tool result encoding -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent-tools-introspection)

(ert-deftest gptel-agent-introspection-info-names-round-trip-through-json ()
  (let* ((directory (make-temp-file "gptel-agent-info-" t))
         (Info-directory-list (list directory))
         (Info-additional-directory-list nil)
         (Info-file-completions nil)
         (file (expand-file-name "example.info" directory))
         (names (gptel-tool-function (gptel-get-tool "manual_names")))
         (nodes (gptel-tool-function (gptel-get-tool "manual_nodes")))
         (prefix "café \"雪\" ")
         (invalid-name (concat prefix (string #x110000)))
         (expected (concat prefix "\uFFFD")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file file
              (insert "-*- coding: utf-8 -*-\n\037\nFile: example.info,  Node: Top\n"
                      "Top node.\n\037\nFile: example.info,  Node: " invalid-name
                      ",  Up: Top\nSecond node.\n")))
          (with-temp-buffer
            ;; Info includes visited manuals from visible-named Info buffers.
            (rename-buffer (generate-new-buffer-name "gptel-agent-info-test"))
            (Info-mode)
            (setq-local Info-current-file (expand-file-name (concat invalid-name ".info")
                                                            directory))
            (dolist (response (list (funcall names) (funcall nodes "example")))
              ;; Each tool's JSON array is itself a string in the next request.
              (let ((text (json-parse-string (json-serialize response))))
                (should (member expected (json-parse-string text :array-type 'list)))))))
      (delete-directory directory t))))

(ert-deftest gptel-agent-introspection-empty-names-use-json-arrays ()
  (cl-letf (((symbol-function 'info--manual-names) (lambda (_) nil))
            ((symbol-function 'Info-build-node-completions) (lambda (_) nil)))
    (should (equal (funcall (gptel-tool-function (gptel-get-tool "manual_names"))) "[]"))
    (should (equal (funcall (gptel-tool-function (gptel-get-tool "manual_nodes")) "missing")
                   "[]"))))

(provide 'gptel-agent-introspection-test)
;;; gptel-agent-introspection-test.el ends here
