;;; gptel-agent-instructions-context-test.el --- Request context scopes -*- lexical-binding: t; -*-

(require 'gptel-agent-instructions-test)
(require 'gptel-context)

(ert-deftest gptel-agent-instructions-context-sources-in-outgoing-request ()
  (gptel-agent-instructions-test--with-project
    (let ((file (expand-file-name "module.txt" front))
          (buffer (generate-new-buffer " *instruction context source*"))
          (gptel-context-restrict-to-project-files nil))
      (unwind-protect
          (progn
            (gptel-agent-instructions-test--write file "FILE-LINE-ONE\nFILE-LINE-TWO\n")
            (gptel-agent-instructions-test--write (expand-file-name "data.txt" back)
                                                   "DIRECTORY-CONTEXT")
            (with-current-buffer buffer
              (setq buffer-file-name (expand-file-name "unsaved.txt" front))
              (insert "BUFFER-LINE-ONE\nBUFFER-LINE-TWO\n"))
            (with-current-buffer parent
              (setq gptel-backend (gptel-make-openai-responses
                                   "context-forms" :key "unused" :models '(test))
                    gptel-use-context 'user))
            (dolist (enabled '(t nil))
              (dolist (case
                       (list (list file front "FILE-LINE-ONE")
                             (list (directory-file-name back) back "DIRECTORY-CONTEXT")
                             (list buffer front "BUFFER-LINE-ONE")
                             (list (list file :lines '(2 . 2))
                                   front "FILE-LINE-TWO" "FILE-LINE-ONE")
                             (list (list buffer :lines '(2 . 2))
                                   front "BUFFER-LINE-TWO" "BUFFER-LINE-ONE")))
                (pcase-let ((`(,entry ,scope ,included . ,excluded) case))
                  (with-current-buffer parent
                    (setq gptel-context (list entry)
                          gptel-agent-use-project-instructions enabled)
                    (setq requests nil)
                    (setq fsm
                          (gptel-request "User task" :buffer parent :callback #'ignore
                                         :transforms '(gptel--transform-add-context
                                                       gptel-agent-instructions--transform))))
                  (should (= (length requests) 1))
                  (let ((records (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
                        (input (plist-get (car requests) :input))
                        (system (plist-get (car requests) :instructions)))
                    (should (equal (mapcar (lambda (r) (plist-get r :scope)) records)
                                   (and enabled (list root scope))))
                    (should (eq enabled (and (string-match-p
                                              (if (equal scope front)
                                                  "FRONT-INSTRUCTION" "BACK-INSTRUCTION")
                                              system)
                                             t)))
                    (let ((user (plist-get (aref input 0) :content)))
                      (should (string-match-p included user))
                      (when excluded (should-not (string-match-p (car excluded) user)))))))))
        (kill-buffer buffer)))))

(ert-deftest gptel-agent-instructions-context-uses-collected-directory-files ()
  (gptel-agent-instructions-test--with-project
    (let* ((leaf (expand-file-name "nested/" front))
           (notes (expand-file-name "notes.txt" leaf))
           (rules (expand-file-name "AGENTS.md" leaf))
           (missing (expand-file-name "absent.txt" back))
           (dead (generate-new-buffer " *dead context*"))
           (gptel-context-restrict-to-project-files t)
           (gptel-context--project-files nil)
           (scans 0)
           (scan (symbol-function 'directory-files-recursively)))
      (kill-buffer dead)
      (gptel-agent-instructions-test--write notes "VISIBLE-NOTE")
      (make-symbolic-link (concat back "AGENTS.md") rules)
      (gptel-agent-instructions-test--write (expand-file-name "ignored/AGENTS.md" front)
                                            "IGNORED-INSTRUCTION")
      (with-current-buffer parent
        (setq gptel-context (list front missing dead)
              gptel-use-context 'user))
      (cl-letf (((symbol-function 'project-files) (lambda (&rest _) (list notes rules)))
                ((symbol-function 'directory-files-recursively)
                 (lambda (dir &rest args)
                   (when (equal (directory-file-name dir) (directory-file-name front))
                     (cl-incf scans))
                   (apply scan dir args))))
        (dolist (defer '(nil t))
          (let* (finish
                 (gptel-context-string-function
                  (if defer
                      (lambda (done sources)
                        (setq finish (lambda () (funcall done (gptel-context--string sources)))))
                    #'gptel-context--string)))
            (setq scans 0 requests nil)
            (with-current-buffer parent
              (setq gptel-agent-use-project-instructions (not defer))
              (setq fsm (gptel-request "Review" :callback #'ignore
                                       :transforms '(gptel--transform-add-context))))
            (when defer (should-not requests) (funcall finish))
            (should (= 1 (length requests)))
            (when defer
              (should-not (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
              (with-current-buffer parent (setq gptel-agent-use-project-instructions t))
              (gptel--handle-wait fsm))
            (let ((records (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
                  (payload (gptel-agent-instructions-test--payload fsm)))
              (should (string-search "VISIBLE-NOTE" payload))
              (should-not (string-search "IGNORED-INSTRUCTION" payload))
              (should (equal (mapcar (lambda (r) (plist-get r :scope)) records)
                             (list root front leaf)))
              (should (= scans 1)))))))))

(ert-deftest gptel-agent-instructions-context-custom-transform-skips-missing-files ()
  (gptel-agent-instructions-test--with-project
    (let ((missing (expand-file-name "missing.txt" back)))
      (with-current-buffer parent
        (setq gptel-use-context t
              gptel-context (list front missing (list missing :lines '(1 . 2)))))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (should (equal (mapcar (lambda (record) (plist-get record :scope))
                             (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
                     (list root front))))))

(provide 'gptel-agent-instructions-context-test)
;;; gptel-agent-instructions-context-test.el ends here
