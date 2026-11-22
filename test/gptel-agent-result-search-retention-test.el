;;; gptel-agent-result-search-retention-test.el --- Search leases -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)

(cl-defmacro gptel-agent-result-search-test--with-file
    ((file owner explorer text limit) &body body)
  "Run BODY with a real private saved FILE and separate OWNER and EXPLORER."
  (declare (indent 1))
  `(let* ((scratch (make-temp-file "gptel-search-retention-" t))
          (temporary-file-directory (file-name-as-directory scratch))
          (gptel-agent--tool-results-storage nil)
          (gptel-agent--saved-tool-results (make-hash-table :test #'equal))
          (gptel-agent--result-resources (make-hash-table :test #'equal))
          (gptel-agent--results-timer nil)
          (,owner (generate-new-buffer " *result-owner*"))
          (,explorer (generate-new-buffer " *result-explorer*"))
          (,file nil))
     (unwind-protect
         (progn
           (pcase-let ((`(,path . ,identity) (gptel-agent--create-tool-result "search")))
             (setq ,file path)
             (gptel-agent--result-register path identity ,owner ,limit)
             (gptel-agent--result-append path ,text)
             (gptel-agent--result-publish path "Find evidence" "test" (length ,text) 1))
           (with-current-buffer ,explorer
             (setq-local gptel-agent--current-agent "result-explorer"
                         gptel-agent--result-owner ,owner
                         gptel-agent--result-file-scope ,file)
             ,@body))
       (when (buffer-live-p ,explorer) (kill-buffer ,explorer))
       (when (buffer-live-p ,owner) (kill-buffer ,owner))
       (gptel-agent--results-exit-cleanup)
       (when (timerp gptel-agent--results-timer)
         (cancel-timer gptel-agent--results-timer))
       (delete-directory scratch t))))

(defun gptel-agent-result-search-test--wait (predicate)
  "Wait at most five seconds for PREDICATE while servicing search timers."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(ert-deftest gptel-agent-result-search-leases-survive-concurrent-disposal ()
  (gptel-agent-result-search-test--with-file (file owner explorer "evidence\n" 100)
    (let (results first second first-pin second-pin)
      ;; The explorer run and each asynchronous search need independent pins.
      (gptel-agent--result-pin file explorer)
      (gptel-agent--grep-scoped-result
       (lambda (value) (push value results)) "evidence" t nil nil nil '(:name emacs))
      (setq first (car gptel-agent--searches)
            first-pin (gptel-agent--search-pin-buffer first))
      (gptel-agent--grep-scoped-result
       (lambda (value) (push value results)) "evidence" t nil nil nil '(:name emacs))
      (setq second (car gptel-agent--searches)
            second-pin (gptel-agent--search-pin-buffer second))
      (should-not (eq first-pin second-pin))
      (gptel-agent--search-dispose first)
      (should-not (buffer-live-p first-pin))
      ;; Even aggressive quota pressure must leave the other reader's file.
      (let ((gptel-agent-result-count-limit 0))
        (gptel-agent--results-prune))
      (should (file-exists-p file))
      (should (memq second-pin (plist-get (gptel-agent--result-resource file) :pins)))
      (gptel-agent-result-search-test--wait (lambda () results))
      (should (= (length results) 1))
      (should (string-match-p "evidence" (car results)))
      (should-not (buffer-live-p second-pin))
      (should (equal (plist-get (gptel-agent--result-resource file) :pins)
                     (list explorer)))
      (gptel-agent--result-unpin file explorer)
      (let ((gptel-agent-result-count-limit 0))
        (gptel-agent--results-prune))
      (should-not (file-exists-p file))
      (should-not (gethash file gptel-agent--saved-tool-results)))))

(ert-deftest gptel-agent-result-search-owner-close-defers-deletion-until-read-finishes ()
  (gptel-agent-result-search-test--with-file (file owner explorer "evidence\n" 100)
    (let (result)
      (gptel-agent--grep-scoped-result
       (lambda (value) (setq result value)) "evidence" t nil nil nil '(:name emacs))
      (let ((pin (gptel-agent--search-pin-buffer (car gptel-agent--searches))))
        (kill-buffer owner)
        (gptel-agent--results-prune)
        (should (file-exists-p file))
        (gptel-agent-result-search-test--wait (lambda () result))
        (should (string-match-p ":1:1: evidence" result))
        (should-not (buffer-live-p pin))
        (should-not (file-exists-p file))
        (should-not (gptel-agent--result-resource file))))))

(ert-deftest gptel-agent-result-search-truncated-log-never-reports-full-coverage ()
  (gptel-agent-result-search-test--with-file (file owner explorer "first\nmissing evidence\n" 6)
    (dolist (backend (append '((:name emacs))
                             (when-let* ((program (executable-find "rg")))
                               (list (list :name 'rg :program program)))
                             (when-let* ((program (executable-find "grep")))
                               (list (list :name 'grep :program program)))))
      (let (result)
        (gptel-agent--grep-scoped-result
         (lambda (value) (setq result value)) "missing" t nil nil nil backend)
        (gptel-agent-result-search-test--wait (lambda () result))
        (should (string-match-p "ResultGrep incomplete: saved output truncated" result))
        (should (string-match-p "only first 6 UTF-8 bytes retained" result))
        (should-not (string-match-p "ResultGrep complete" result))
        (should-not (string-match-p "No nonempty matches" result))
        (should-not (plist-get (gptel-agent--result-resource file) :pins))))))

(ert-deftest gptel-agent-result-search-timeout-capture-never-reports-full-coverage ()
  (gptel-agent-result-search-test--with-file (file owner explorer "evidence\n" 100)
    (gptel-agent--result-publish file "Find evidence" "bash" 9 2 nil
                               "Command timed out; execution is partial.")
    (let (result)
      (gptel-agent--grep-scoped-result
       (lambda (value) (setq result value)) "missing" t nil nil nil '(:name emacs))
      (gptel-agent-result-search-test--wait (lambda () result))
      (should (string-match-p "ResultGrep incomplete: partial command execution" result))
      (should (string-match-p "timed out" result))
      (should-not (string-match-p "ResultGrep complete" result)))))

(ert-deftest gptel-agent-result-search-start-error-and-cancellation-release-leases ()
  (gptel-agent-result-search-test--with-file (file owner explorer "evidence\n" 100)
    (let (result)
      (gptel-agent--grep-scoped-result
       (lambda (value) (setq result value)) "[" t nil nil nil '(:name emacs))
      (should (string-match-p "incomplete:" result))
      (should-not gptel-agent--searches)
      (should-not (plist-get (gptel-agent--result-resource file) :pins)))
    (let ((fsm (gptel-make-fsm :state 'TOOL :info (list :buffer explorer))) result)
      (gptel-agent--grep-scoped-result
       (lambda (value) (setq result value)) "evidence" t nil nil fsm '(:name emacs))
      (let* ((job (car gptel-agent--searches))
             (pin (gptel-agent--search-pin-buffer job)))
        (funcall (gptel-agent--search-cleanup job))
        (gptel-agent--search-finish job "complete")
        (should-not (buffer-live-p pin))
        (should-not result)
        (should-not (plist-get (gptel-fsm-info fsm) :post))
        (should-not (plist-get (gptel-agent--result-resource file) :pins))))))

(provide 'gptel-agent-result-search-retention-test)
;;; gptel-agent-result-search-retention-test.el ends here
