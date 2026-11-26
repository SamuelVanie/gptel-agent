;;; gptel-agent-search-access-test.el --- Recursive Grep access -*- lexical-binding: t; -*-

(require 'gptel-agent-access-test)
(require 'gptel-agent-read-test)

(defun gptel-agent-search-access-test--result (fsm)
  "Wait for FSM's single Grep call and return its result."
  (let ((call (car (plist-get (gptel-fsm-info fsm) :tool-use))))
    (gptel-agent-read-test--wait (lambda () (plist-get call :result)))
    (plist-get call :result)))

(ert-deftest gptel-agent-repo-recursive-grep-uses-owner-boundary ()
  (gptel-agent-access-test--with-project
    (let ((nested (expand-file-name "nested/" front)))
      (make-directory nested)
      (gptel-agent-instructions-test--write (concat front "other") "fixture: sibling")
      (gptel-agent-instructions-test--write (concat nested "regular") "fixture: regular")
      (make-symbolic-link "../other" (concat nested "inside"))
      (make-symbolic-link (concat back "data.txt") (concat nested "outside"))
      (with-current-buffer parent
        (setq-local gptel-agent-use-project-instructions nil
                    gptel-confirm-tool-calls t))
      (setq fsm (gptel-agent-instructions-test--request parent))
      ;; Ordinary confirmation can be displayed in a parent or another buffer.
      (let ((pending (gptel-agent-access-test--ask fsm "Grep" '(:path "nested" :regex "."))))
        (should pending)
        (with-temp-buffer
          (setq default-directory back)
          (gptel--accept-tool-calls pending)
          (let ((result (gptel-agent-search-access-test--result fsm)))
            (should (string-match-p "fixture: sibling" result))
            (should (string-match-p "fixture: regular" result))
            (should (string-match-p "incomplete: paths require outside approval" result))
            (should-not (string-match-p "OUTSIDE-CONTENT" result))))))))

(ert-deftest gptel-agent-repo-recursive-grep-once-allows-only-approved-subtree ()
  (gptel-agent-access-test--with-project
    (make-symbolic-link (concat root "old.txt") (concat back "escape"))
    (setq fsm (gptel-agent-instructions-test--request parent))
    (let ((pending (gptel-agent-access-test--ask fsm "Grep" (list :path back :regex "."))))
      (should pending)
      (gptel--accept-tool-calls pending)
      (let ((result (gptel-agent-search-access-test--result fsm)))
        (should (string-match-p "OUTSIDE-CONTENT" result))
        (should-not (string-match-p "old.txt\\|:old" result))
        (should (string-match-p "incomplete: paths require outside approval" result))))))

(ert-deftest gptel-agent-repo-recursive-grep-with-grant-or-supervision ()
  (dolist (authority '(grant supervision))
    (gptel-agent-access-test--with-project
      (make-symbolic-link (concat back "data.txt") (concat front "alias"))
      (with-current-buffer parent
        (if (eq authority 'grant)
            (progn
              (setq-local gptel-agent-access--grant
                          (cons default-directory (file-truename default-directory)))
              (setq fsm (gptel-agent-instructions-test--request parent)))
          (let ((scope (gptel-agent-supervise parent)))
            (setq fsm (gptel-request "Search" :buffer parent :callback #'ignore
                        :transforms (list #'gptel-agent--localize-repository-search
                                          (lambda (request)
                                            (gptel-agent-supervision-allow-outside-reads scope request)))
                        :fsm (gptel-make-fsm :table gptel-send--transitions
                                             :handlers gptel-send--handlers))))))
      (should-not (gptel-agent-access-test--ask fsm "Grep" '(:path "." :regex "OUTSIDE")))
      (let ((result (gptel-agent-search-access-test--result fsm)))
        (should (string-match-p "OUTSIDE-CONTENT" result))
        (should (string-match-p "Grep complete" result))))))

(ert-deftest gptel-agent-repo-recursive-grep-invalidates-pending-results ()
  (dolist (change '(revoke cd))
    (gptel-agent-access-test--with-project
      (make-symbolic-link (concat back "data.txt") (concat front "alias"))
      (with-current-buffer parent
        (setq-local gptel-agent-access--grant
                    (cons default-directory (file-truename default-directory))))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (let ((parse (symbol-function 'gptel-agent--repo-grep-record)) saw-match)
        (cl-letf (((symbol-function 'gptel-agent--repo-grep-record)
                   (lambda (job record)
                     (funcall parse job record)
                     (when (string-match-p "OUTSIDE-CONTENT" record)
                       (setq saw-match t)
                       (with-current-buffer parent
                         (if (eq change 'cd) (cd back)
                           (gptel-agent-revoke-outside-access)))))))
          (should-not (gptel-agent-access-test--ask fsm "Grep" '(:path "." :regex "OUTSIDE")))
          (let ((result (gptel-agent-search-access-test--result fsm)))
            (should saw-match)
            (should (string-match-p "incomplete: conversation access changed" result))
            (should (string-match-p "results=0" result))
            (should-not (string-match-p "OUTSIDE-CONTENT" result))))))))

(ert-deftest gptel-agent-repo-recursive-grep-passes-resolved-target-to-matcher ()
  (gptel-agent-access-test--with-project
    (let ((alias (concat front "alias")) (launch (symbol-function 'gptel-agent--repo-launch)))
      (gptel-agent-instructions-test--write (concat front "permitted") "fixture: permitted")
      (make-symbolic-link "permitted" alias)
      (setq fsm (gptel-agent-instructions-test--request parent))
      (cl-letf (((symbol-function 'gptel-agent--repo-launch)
                 (lambda (job command)
                   (when (eq (gptel-agent--repo-stage job) 'grep)
                     (delete-file alias)
                     (make-symbolic-link (concat back "data.txt") alias))
                   (funcall launch job command))))
        (should-not (gptel-agent-access-test--ask fsm "Grep" '(:path "." :glob "alias" :regex ".")))
        (let ((result (gptel-agent-search-access-test--result fsm)))
          (should (string-match-p "fixture: permitted" result))
          (should-not (string-match-p "OUTSIDE-CONTENT" result)))))))

(ert-deftest gptel-agent-repo-recursive-grep-skips-type-changes-before-matching ()
  (gptel-agent-access-test--with-project
    (let ((file (concat front "candidate"))
          (batch (symbol-function 'gptel-agent--repo-grep-batch)))
      (gptel-agent-instructions-test--write file "original")
      (setq fsm (gptel-agent-instructions-test--request parent))
      (cl-letf (((symbol-function 'gptel-agent--repo-grep-batch)
                 (lambda (job)
                   (delete-file file)
                   (gptel-agent-instructions-test--write (concat file "/child") "directory child")
                   (funcall batch job))))
        (should-not (gptel-agent-access-test--ask fsm "Grep" '(:path "." :glob "candidate" :regex ".")))
        (let ((result (gptel-agent-search-access-test--result fsm)))
          (should (string-match-p "incomplete: candidate changed file type" result))
          (should-not (string-match-p "directory child" result)))))))

(ert-deftest gptel-agent-repo-recursive-grep-discards-redirected-matcher-results ()
  (gptel-agent-access-test--with-project
    (let ((file (concat front "candidate"))
          (launch (symbol-function 'gptel-agent--repo-launch)))
      (gptel-agent-instructions-test--write file "original")
      (setq fsm (gptel-agent-instructions-test--request parent))
      (cl-letf (((symbol-function 'gptel-agent--repo-launch)
                 (lambda (job command)
                   (when (eq (gptel-agent--repo-stage job) 'grep)
                     (delete-file file)
                     (make-symbolic-link (concat back "data.txt") file))
                   (funcall launch job command))))
        (should-not (gptel-agent-access-test--ask fsm "Grep" '(:path "." :glob "candidate" :regex ".")))
        (let ((result (gptel-agent-search-access-test--result fsm)))
          (should (string-match-p "incomplete: search targets changed" result))
          (should (string-match-p "results=0" result))
          (should-not (string-match-p "OUTSIDE-CONTENT" result)))))))

(provide 'gptel-agent-search-access-test)
;;; gptel-agent-search-access-test.el ends here
