;;; gptel-agent-bash-timeout-test.el --- Foreground execution deadlines -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)

(defun gptel-agent-bash-timeout-test--wait (predicate)
  "Service subprocesses and timers for at most five seconds until PREDICATE."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(cl-defmacro gptel-agent-bash-timeout-test--with-session ((source directory) &body body)
  "Run BODY in disposable SOURCE with saved results under DIRECTORY."
  (declare (indent 1))
  `(let ((,source (generate-new-buffer " *bash-timeout-test*"))
         (,directory (make-temp-file "gptel-bash-timeout-test-" t))
         (gptel-agent--saved-tool-results (make-hash-table :test #'equal))
         (gptel-agent--result-resources (make-hash-table :test #'equal))
         (gptel-agent--results-timer nil))
     (unwind-protect
         (cl-letf (((symbol-function 'gptel-agent--tool-results-directory)
                    (lambda () ,directory)))
           (with-current-buffer ,source ,@body))
       (when (buffer-live-p ,source) (kill-buffer ,source))
       (delete-directory ,directory t))))

(defun gptel-agent-bash-timeout-test--read (file)
  "Read FILE as decoded UTF-8."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (buffer-string)))

(ert-deftest gptel-agent-bash-timeout-returns-partial-output-once ()
  (gptel-agent-bash-timeout-test--with-session (source directory)
    (let* ((gptel-agent-bash-timeout 0.3)
           results callback-buffer callback-cleaned
           (process (gptel-agent--execute-bash
                     (lambda (result)
                       (setq callback-buffer (current-buffer)
                             callback-cleaned (null gptel-agent--bash-jobs))
                       (push result results))
                     "printf 'partial stdout\n'; printf 'partial stderr\n' >&2; sleep 60"
                     "Find the last completed step"))
           (job (process-get process 'gptel-agent-bash))
           (output (process-buffer process))
           (timer (gptel-agent--bash-timeout-timer job)))
      (should (timerp timer))
      (should (= (gptel-agent--bash-timeout job) 0.3))
      ;; Later configuration changes must not extend an already running call.
      (setq gptel-agent-bash-timeout 1000)
      (gptel-agent-bash-timeout-test--wait (lambda () results))
      (should (= (length results) 1))
      (should (string-match-p "[Tt]imed out" (car results)))
      (should (string-match-p "0\\.3" (car results)))
      (should (string-match-p "background" (car results)))
      (should (string-match-p "Process" (car results)))
      (should (string-match-p "partial stdout\npartial stderr\n" (car results)))
      (should-not (string-match-p "failed with exit code" (car results)))
      (should (eq callback-buffer source))
      (should callback-cleaned)
      (should-not (process-live-p process))
      (should-not (buffer-live-p output))
      (should-not (memq timer timer-list))
      (should-not (gptel-agent--bash-timeout-timer job))
      (should-not (directory-files directory nil directory-files-no-dot-files-regexp))
      ;; Both an already queued expiry and a late sentinel must be harmless.
      (gptel-agent--bash-expire job)
      (gptel-agent--bash-finish job process)
      (should (= (length results) 1)))))

(ert-deftest gptel-agent-bash-timeout-preserves-oversized-partial-log ()
  (gptel-agent-bash-timeout-test--with-session (source directory)
    (let* ((gptel-agent-bash-timeout 0.5)
           (gptel-agent-tool-result-max-chars 256)
           (gptel-agent-tool-result-preview-chars 64)
           (gptel-agent-tool-result-preview-lines 2)
           results
           (process (gptel-agent--execute-bash
                     (lambda (result) (push result results))
                     "printf '%4096s' x; printf '\n'; sleep 60"
                     "Find the last completed step"))
           (job (process-get process 'gptel-agent-bash))
           (output (process-buffer process)))
      (gptel-agent-bash-timeout-test--wait
       (lambda () (= (gptel-agent--bash-chars job) 4097)))
      (should (<= (buffer-size output) 256))
      (should (file-exists-p (gptel-agent--bash-file job)))
      (gptel-agent-bash-timeout-test--wait (lambda () results))
      (let ((file (gptel-agent--bash-file job)))
        (should (= (length results) 1))
        (should (string-match-p "[Tt]imed out" (car results)))
        (should (string-match-p "result-explorer" (car results)))
        (should (string-match-p "Partial result:" (car results)))
        (should (string-match-p "timed out" (gptel-agent--result-explorer-task-prompt
                                             file "Find the last completed step")))
        (should (string-match-p (regexp-quote file) (car results)))
        (should (< (length (car results)) 1600))
        (should (equal (gptel-agent-bash-timeout-test--read file)
                       (concat (make-string 4095 ?\s) "x\n")))
        (let ((metadata (gethash file gptel-agent--saved-tool-results)))
          (should (equal (plist-get metadata :query) "Find the last completed step"))
          (should (= (plist-get metadata :chars) 4097))
          (should (= (plist-get metadata :lines) 2))
          (should (eq (plist-get metadata :owner) source)))
        (should-not (buffer-live-p output))
        (should-not (process-live-p process))
        (kill-buffer source)
        (should-not (file-exists-p file))))))

(ert-deftest gptel-agent-bash-timeout-kills-ordinary-shell-descendants ()
  (skip-unless (memq system-type '(darwin gnu/linux berkeley-unix)))
  (gptel-agent-bash-timeout-test--with-session (source directory)
    (let* ((gptel-agent-bash-timeout 0.5)
           results
           (process (gptel-agent--execute-bash
                     (lambda (result) (push result results))
                     "sleep 60 & child=$!; printf '%s\n' \"$child\"; wait"
                     "Read child status"))
           (job (process-get process 'gptel-agent-bash)))
      (gptel-agent-bash-timeout-test--wait
       (lambda () (> (gptel-agent--bash-chars job) 0)))
      (let ((child (with-current-buffer (process-buffer process)
                     (string-to-number (buffer-string)))))
        (should (> child 0))
        (should (process-attributes child))
        (gptel-agent-bash-timeout-test--wait (lambda () results))
        (should (string-match-p "[Tt]imed out" (car results)))
        (gptel-agent-bash-timeout-test--wait
         (lambda ()
           (and (not (process-live-p process))
                (let ((attrs (process-attributes child)))
                  (or (null attrs) (equal (alist-get 'state attrs) "Z"))))))))))

(ert-deftest gptel-agent-bash-normal-completion-cancels-deadline ()
  (dolist (case '(("printf finished" "finished")
                  ("printf failed; exit 7"
                   "Command failed with exit code 7:\nSTDOUT+STDERR:\nfailed")))
    (gptel-agent-bash-timeout-test--with-session (source directory)
      (let* ((gptel-agent-bash-timeout 5)
             results
             (process (gptel-agent--execute-bash
                       (lambda (result) (push result results))
                       (car case) "Read output"))
             (job (process-get process 'gptel-agent-bash))
             (timer (gptel-agent--bash-timeout-timer job)))
        (should (timerp timer))
        (gptel-agent-bash-timeout-test--wait (lambda () results))
        (should (equal results (list (cadr case))))
        (should-not (memq timer timer-list))
        (should-not (gptel-agent--bash-timeout-timer job))
        (should-not gptel-agent--bash-jobs)
        (gptel-agent--bash-expire job)
        (gptel-agent--bash-finish job process)
        (should (equal results (list (cadr case))))))))

(ert-deftest gptel-agent-bash-deadline-preserves-exit-before-sentinel-delivery ()
  (gptel-agent-bash-timeout-test--with-session (source directory)
    (let* ((gptel-agent-bash-timeout 5)
           results
           (process (gptel-agent--execute-bash
                     (lambda (result) (push result results))
                     "printf completed" "Read output"))
           (job (process-get process 'gptel-agent-bash))
           (timer (gptel-agent--bash-timeout-timer job)))
      ;; An exit notification can update process status while Emacs is busy,
      ;; before either the output filter or completion sentinel is serviced.
      ;; Do not use accept-process-output here: it hides the lost-output race.
      (let ((deadline (+ (float-time) 1)))
        (while (and (not (eq (process-status process) 'exit))
                    (< (float-time) deadline))))
      (should (eq (process-status process) 'exit))
      (should (zerop (gptel-agent--bash-chars job)))
      (should-not results)
      (gptel-agent--bash-expire job)
      (gptel-agent-bash-timeout-test--wait (lambda () results))
      (should (equal results '("completed")))
      (should-not (memq timer timer-list))
      (gptel-agent--bash-finish job process)
      (should (equal results '("completed"))))))

(ert-deftest gptel-agent-bash-cancellation-cancels-deadline-without-result ()
  (dolist (cancel '(request owner abort))
    (gptel-agent-bash-timeout-test--with-session (source directory)
      (let* ((gptel-agent-bash-timeout 5)
             (gptel-agent-tool-result-max-chars 5)
             (gptel--request-alist nil)
             (fsm (gptel-make-fsm :state 'TOOL :info (list :buffer source)))
             results
             ;; Preserve the internal positional FSM calling convention.
             (process (gptel-agent--execute-bash
                       (lambda (result) (push result results))
                       "printf 'partial output'; sleep 60" "Read output" nil fsm))
             (job (process-get process 'gptel-agent-bash))
             (timer (gptel-agent--bash-timeout-timer job))
             (output (process-buffer process)))
        (gptel-agent-bash-timeout-test--wait
         (lambda () (gptel-agent--bash-file job)))
        (let ((file (gptel-agent--bash-file job)))
          (should (file-exists-p file))
          (should (memq timer timer-list))
          (pcase cancel
            ('request
             (setf (gptel-fsm-state fsm) 'ABRT)
             (gptel--handle-post fsm))
            ('owner (kill-buffer source))
            ('abort (gptel-abort source)))
          (should-not (process-live-p process))
          (should-not (buffer-live-p output))
          (should-not (memq timer timer-list))
          (should-not (gptel-agent--bash-timeout-timer job))
          (should-not (plist-get (gptel-fsm-info fsm) :post))
          (should-not (file-exists-p file))
          (should (zerop (hash-table-count gptel-agent--saved-tool-results)))
          (gptel-agent--bash-expire job)
          (gptel-agent--bash-finish job process)
          (should-not results))))))

(ert-deftest gptel-agent-bash-background-job-survives-foreground-deadline ()
  (gptel-agent-bash-timeout-test--with-session (source directory)
    (let* ((gptel-agent-bash-timeout 0.3)
           foreground-results background-results
           (background (gptel-agent--execute-bash
                        (lambda (result) (push result background-results))
                        "read -r answer; printf 'reply:%s' \"$answer\""
                        "Read replies" t))
           (background-job (process-get background 'gptel-agent-job))
           (foreground (gptel-agent--execute-bash
                        (lambda (result) (push result foreground-results))
                        "sleep 60" "Read output")))
      (should (= (length background-results) 1))
      (gptel-agent-bash-timeout-test--wait (lambda () foreground-results))
      (should (string-match-p "[Tt]imed out" (car foreground-results)))
      (should-not (process-live-p foreground))
      (should (process-live-p background))
      (should-not (process-get background 'gptel-agent-bash))
      (gptel-agent--manage-process
       "send" (gptel-agent--job-id background-job) "still running\n")
      (gptel-agent-bash-timeout-test--wait
       (lambda () (eq (process-status background) 'exit)))
      (should (= (length background-results) 1))
      (should (string-match-p
               "reply:still running"
               (gptel-agent--manage-process "read" (gptel-agent--job-id background-job)))))))

(ert-deftest gptel-agent-bash-timeout-callback-error-does-not-retry-or-leak ()
  (gptel-agent-bash-timeout-test--with-session (source directory)
    (let* ((gptel-agent-bash-timeout 5)
           (fsm (gptel-make-fsm :state 'TOOL :info (list :buffer source)))
           (calls 0)
           (process (gptel-agent--execute-bash
                     (lambda (_result)
                       (cl-incf calls)
                       (should-not gptel-agent--bash-jobs)
                       (should-not (plist-get (gptel-fsm-info fsm) :post))
                       (error "Timeout callback failed"))
                     "printf ready; sleep 60" "Read output" nil fsm))
           (job (process-get process 'gptel-agent-bash))
           (timer (gptel-agent--bash-timeout-timer job))
           (output (process-buffer process)))
      (gptel-agent-bash-timeout-test--wait
       (lambda () (= (gptel-agent--bash-chars job) 5)))
      ;; Invoke expiry directly so ERT sees the callback's escaping error.
      (let ((err (should-error (gptel-agent--bash-expire job))))
        (should (equal (error-message-string err) "Timeout callback failed")))
      (should (= calls 1))
      (should-not (process-live-p process))
      (should-not (buffer-live-p output))
      (should-not (memq timer timer-list))
      (should-not (gptel-agent--bash-timeout-timer job))
      (gptel-agent--bash-expire job)
      (gptel-agent--bash-finish job process)
      (should (= calls 1)))))

;;; gptel-agent-bash-timeout-test.el ends here
