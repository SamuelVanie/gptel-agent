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

(provide 'gptel-agent-instructions-context-test)
;;; gptel-agent-instructions-context-test.el ends here
