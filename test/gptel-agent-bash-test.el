;;; gptel-agent-bash-test.el --- Bounded foreground output -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)
(require 'gptel-openai-responses)

(defun gptel-agent-bash-test--wait (predicate)
  "Service subprocesses for up to ten seconds until PREDICATE succeeds."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(cl-defmacro gptel-agent-bash-test--with-session ((source directory) &body body)
  "Run BODY with disposable SOURCE and result DIRECTORY."
  (declare (indent 1))
  `(let ((,source (generate-new-buffer " *bash-test*"))
         (,directory (make-temp-file "gptel-bash-test-" t))
         (gptel-agent--saved-tool-results (make-hash-table :test #'equal)))
     (unwind-protect
         (cl-letf (((symbol-function 'gptel-agent--tool-results-directory)
                    (lambda () ,directory)))
           (with-current-buffer ,source ,@body))
       (when (buffer-live-p ,source) (kill-buffer ,source))
       (delete-directory ,directory t))))

(defun gptel-agent-bash-test--read (file)
  "Read FILE as UTF-8, including its final newline if present."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents file))
    (buffer-string)))

(defun gptel-agent-bash-test--gate (directory name)
  "Return a shell command waiting for file NAME in DIRECTORY."
  (format "while [ ! -f %s ]; do sleep 0.01; done"
          (shell-quote-argument (expand-file-name name directory))))

(defun gptel-agent-bash-test--release (directory name)
  "Release the shell gate named NAME in DIRECTORY."
  (with-temp-file (expand-file-name name directory)))

(ert-deftest gptel-agent-bash-spools-large-output-before-exit ()
  (gptel-agent-bash-test--with-session (source directory)
    (let* ((gptel-agent-tool-result-max-chars 20000)
           (gptel-agent-tool-result-preview-chars 12000)
           (gptel-agent-tool-result-preview-lines 50)
           results callback-buffer
           (process (gptel-agent--execute-bash
                     (lambda (result)
                       (push result results) (setq callback-buffer (current-buffer)))
                     (concat "printf '%1048576s' x; printf '%1048576s' y; "
                             (gptel-agent-bash-test--gate directory "continue")
                             "; printf '\nfinished\n'")
                     "Find the completion marker"))
           (job (process-get process 'gptel-agent-bash))
           (output (process-buffer process)))
      (gptel-agent-bash-test--wait
       (lambda () (= (gptel-agent--bash-chars job) (* 2 1024 1024))))
      (should (process-live-p process))
      (should-not results)
      (should (= (buffer-size output) 20000))
      (should (eq (buffer-local-value 'buffer-undo-list output) t))
      (let ((file (gptel-agent--bash-file job)))
        (should (= (file-attribute-size (file-attributes file)) (* 2 1024 1024)))
        (should-not (gethash file gptel-agent--saved-tool-results))
        (gptel-agent-bash-test--release directory "continue")
        (gptel-agent-bash-test--wait (lambda () results))
        (should (= (length results) 1))
        (should (eq callback-buffer source))
        (should (< (length (car results)) 14000))
        (should (string-match-p (regexp-quote file) (car results)))
        (should (string-match-p "result-explorer" (car results)))
        (should (equal (gptel-agent-bash-test--read file)
                       (concat (make-string (1- 1048576) ?\s) "x"
                               (make-string (1- 1048576) ?\s) "y\nfinished\n")))
        (should (equal (gethash file gptel-agent--saved-tool-results)
                       (list :query "Find the completion marker" :tool "bash"
                             :chars (+ (* 2 1024 1024) 10) :lines 3)))
        (should-not (buffer-live-p output))
        (should-not gptel-agent--bash-jobs)
        (gptel-agent--bash-finish job process)
        (should (= (length results) 1))
        (kill-buffer source)
        (should (file-exists-p file))))))

(ert-deftest gptel-agent-bash-small-output-preserves-success-and-failure ()
  (dolist (case '(("true" "")
                  ("printf 'stdout\n'; printf 'stderr\n' >&2" "stdout\nstderr\n")
                  ("printf 'failed\n' >&2; exit 7"
                   "Command failed with exit code 7:\nSTDOUT+STDERR:\nfailed\n")))
    (gptel-agent-bash-test--with-session (source directory)
      (let* (results
             (process (gptel-agent--execute-bash
                       (lambda (result) (push result results)) (car case) "Read output"))
             (output (process-buffer process)))
        (gptel-agent-bash-test--wait (lambda () results))
        (should (equal results (list (cadr case))))
        (should-not (directory-files directory nil directory-files-no-dot-files-regexp))
        (should-not (buffer-live-p output))
        (should-not gptel-agent--bash-jobs)))))

(ert-deftest gptel-agent-bash-foreground-stdin-readers-complete ()
  ;; Foreground jobs cannot receive Process send input.  An inherited open
  ;; pipe used to leave each of these commands and its tool request waiting.
  (dolist (background '(nil :json-false))
    (dolist (case '(("cat" "")
                    ("grep needle"
                     "Command failed with exit code 1:\nSTDOUT+STDERR:\n")
                    ("read -r answer; printf 'finished\n'" "finished\n")))
      (gptel-agent-bash-test--with-session (source directory)
        (let* ((gptel-confirm-tool-calls nil)
               (gptel-agent-use-project-instructions nil)
               (backend (gptel-make-openai-responses
                         "bash-test" :key "unused" :host "example.invalid"
                         :models '(test)))
               (call (list :name "Bash" :args
                           (append (list :command (car case) :query "Read output")
                                   (when background (list :background background)))))
               (fsm (gptel-make-fsm
                     :table gptel-send--transitions
                     :handlers '((TOOL gptel--handle-tool-use))
                     :info (list :buffer source :backend backend
                                 :callback #'ignore :tool-use (list call)))))
          (setq-local gptel-tools (list (gptel-get-tool "Bash")))
          (gptel-agent--localize-agent-tool fsm)
          (setf (plist-get (gptel-fsm-info fsm) :tools) gptel-tools)
          (gptel--fsm-transition fsm 'TOOL)
          (gptel-agent-bash-test--wait
           (lambda () (plist-get call :result)))
          (should (equal (plist-get call :result) (cadr case)))
          (should (eq (gptel-fsm-state fsm) 'TRET))
          (should-not gptel-agent--bash-jobs))))))

(ert-deftest gptel-agent-bash-foreground-preserves-explicit-input ()
  (gptel-agent-bash-test--with-session (source directory)
    (let ((input-file (expand-file-name "input.txt" directory)))
      (with-temp-file input-file (insert "file input\n"))
      (dolist (case (list '("printf 'pipe input\n' | cat" "pipe input\n")
                         '("cat <<'EOF'\nhere document\nEOF" "here document\n")
                         '("cat <<< 'here string'" "here string\n")
                         (list (concat "cat < " (shell-quote-argument input-file))
                               "file input\n")))
        (let (results)
          (gptel-agent--execute-bash
           (lambda (result) (push result results)) (car case) "Read output")
          (gptel-agent-bash-test--wait (lambda () results))
          (should (equal results (list (cadr case))))
          (should-not gptel-agent--bash-jobs))))))

(ert-deftest gptel-agent-bash-ripgrep-without-path-searches-directory ()
  (skip-unless (executable-find "rg"))
  (gptel-agent-bash-test--with-session (source directory)
    (let ((default-directory (file-name-as-directory directory)) results)
      (with-temp-file (expand-file-name "input.txt" directory)
        (insert "needle\n"))
      ;; Merely sending EOF to a pipe still makes ripgrep search that pipe,
      ;; giving an empty result instead of searching the working directory.
      (gptel-agent--execute-bash
       (lambda (result) (push result results))
       "rg --no-config --no-heading --color never needle" "Find the matching line")
      (gptel-agent-bash-test--wait (lambda () results))
      (should (equal results '("input.txt:needle\n")))
      (should-not gptel-agent--bash-jobs))))

(ert-deftest gptel-agent-bash-spill-preserves-unicode-across-chunks-and-limits ()
  (gptel-agent-bash-test--with-session (source directory)
    (let* ((gptel-agent-tool-result-max-chars 8)
           (gptel-agent-tool-result-preview-chars 12)
           (gptel-agent-tool-result-preview-lines 1)
           (coding-system-for-read 'utf-8-unix)
           results
           (process (gptel-agent--execute-bash
                     (lambda (result) (push result results))
                     (concat "printf 'é猫\n'; "
                             (gptel-agent-bash-test--gate directory "next")
                             "; printf 'café世界\nlast'; "
                             (gptel-agent-bash-test--gate directory "done")
                             "; exit 9")
                     "Find the error"))
           (job (process-get process 'gptel-agent-bash)))
      (gptel-agent-bash-test--wait (lambda () (= (gptel-agent--bash-chars job) 3)))
      (should-not (gptel-agent--bash-file job))
      (gptel-agent-bash-test--release directory "next")
      (gptel-agent-bash-test--wait (lambda () (= (gptel-agent--bash-chars job) 14)))
      (should (= (buffer-size (process-buffer process)) 12))
      ;; Async completion uses the limits captured at launch, not later settings.
      (setq gptel-agent-tool-result-max-chars 1000
            gptel-agent-tool-result-preview-lines 10)
      (gptel-agent-bash-test--release directory "done")
      (gptel-agent-bash-test--wait (lambda () results))
      (let ((result (car results)) (file (gptel-agent--bash-file job)))
        (should (string-prefix-p "Command failed with exit code 9:" result))
        (should (string-match-p "14 chars, 3 lines" result))
        (should (string-match-p "Preview (up to 1 lines and 12 chars):\n\né猫\n" result))
        (should-not (string-match-p "café" result))
        (should (equal (gptel-agent-bash-test--read file) "é猫\ncafé世界\nlast"))))))

(ert-deftest gptel-agent-bash-failure-prefix-can-trigger-spill ()
  (gptel-agent-bash-test--with-session (source directory)
    (let* ((gptel-agent-tool-result-max-chars 10)
           results
           (process (gptel-agent--execute-bash
                     (lambda (result) (push result results)) "printf short; exit 2" "Read failure"))
           (job (process-get process 'gptel-agent-bash)))
      (gptel-agent-bash-test--wait (lambda () results))
      (should (string-prefix-p "Command failed with exit code 2:" (car results)))
      (should (string-match-p "result-explorer" (car results)))
      (should (equal (gptel-agent-bash-test--read (gptel-agent--bash-file job)) "short")))))

(ert-deftest gptel-agent-bash-owner-close-stops-process-and-removes-partial-log ()
  (gptel-agent-bash-test--with-session (source directory)
    (let* ((gptel-agent-tool-result-max-chars 5)
           results
           (process (gptel-agent--execute-bash
                     (lambda (result) (push result results))
                     (concat "printf 'long output'; "
                             (gptel-agent-bash-test--gate directory "continue"))
                     "Read output"))
           (job (process-get process 'gptel-agent-bash))
           (output (process-buffer process)))
      (gptel-agent-bash-test--wait (lambda () (gptel-agent--bash-file job)))
      ;; Changing modes must not abandon the command or its cleanup hook.
      (text-mode)
      (kill-buffer source)
      (should-not (process-live-p process))
      (should-not (buffer-live-p output))
      (should-not (file-exists-p (gptel-agent--bash-file job)))
      (should-not results)
      (should (zerop (hash-table-count gptel-agent--saved-tool-results))))))

(ert-deftest gptel-agent-bash-request-cancellation-keeps-background-job ()
  (gptel-agent-bash-test--with-session (source directory)
    (let* ((fsm (gptel-make-fsm :state 'TOOL :info (list :buffer source)))
           (base-tool (gptel-get-tool "Bash")) results)
      (setq-local gptel-tools (list base-tool))
      (gptel-agent--localize-agent-tool fsm)
      (let* ((foreground (funcall (gptel-tool-function (car gptel-tools))
                                  (lambda (result) (push result results))
                                  (gptel-agent-bash-test--gate directory "continue")
                                  "Read output"))
             (background (gptel-agent--execute-bash #'ignore "read -r answer" "Read output" t))
             (output (process-buffer foreground)))
        (should (eq (gptel-tool-function base-tool) #'gptel-agent--execute-bash))
        (should (plist-get (gptel-fsm-info fsm) :post))
        (setf (gptel-fsm-state fsm) 'ABRT)
        (gptel--handle-post fsm)
        (should-not (process-live-p foreground))
        (should-not (buffer-live-p output))
        (should (process-live-p background))
        (should-not (plist-get (gptel-fsm-info fsm) :post))
        (should-not results)))))

(ert-deftest gptel-agent-bash-abort-without-transport-stops-foreground ()
  (gptel-agent-bash-test--with-session (source directory)
    (let* ((gptel--request-alist nil) results
           (process (gptel-agent--execute-bash
                     (lambda (result) (push result results))
                     (gptel-agent-bash-test--gate directory "continue")
                     "Read output")))
      (gptel-abort source)
      (should-not (process-live-p process))
      (should-not gptel-agent--bash-jobs)
      (should-not results))))

(ert-deftest gptel-agent-bash-disk-errors-finish-once-and-clean-up ()
  (dolist (fail-on-append '(nil t))
    (gptel-agent-bash-test--with-session (source directory)
      (let* ((gptel-agent-tool-result-max-chars 5)
             (write (symbol-function 'write-region)) results
             (process (gptel-agent--execute-bash
                       (lambda (result) (push result results))
                       (concat "printf 'long output'; "
                               (gptel-agent-bash-test--gate directory "continue"))
                       "Read output"))
             (output (process-buffer process)))
        (cl-letf (((symbol-function 'write-region)
                   (lambda (start end filename &optional append &rest args)
                     (if (and (string-prefix-p directory filename)
                              (eq (not (null append)) fail-on-append))
                         (signal 'file-error '("Simulated full disk"))
                       (apply write start end filename append args)))))
          (gptel-agent-bash-test--wait (lambda () results)))
        (should (= (length results) 1))
        (should (string-match-p "Could not capture Bash output.*Simulated full disk" (car results)))
        (should-not (process-live-p process))
        (should-not (buffer-live-p output))
        (should-not gptel-agent--bash-jobs)
        (should-not (directory-files directory nil directory-files-no-dot-files-regexp))
        (should (zerop (hash-table-count gptel-agent--saved-tool-results)))))))

(ert-deftest gptel-agent-bash-launch-failure-does-not-leak ()
  (gptel-agent-bash-test--with-session (source directory)
    (let ((buffers (buffer-list)) results)
      (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (error "Launch failed"))))
        (gptel-agent--execute-bash
         (lambda (result) (push result results)) "true" "Read output"))
      (should (= (length results) 1))
      (should (string-prefix-p "Error: " (car results)))
      (should (string-match-p "Launch failed" (car results)))
      (should-not gptel-agent--bash-jobs)
      (should (equal buffers (buffer-list)))
      (should-not (directory-files directory nil directory-files-no-dot-files-regexp)))))

;;; gptel-agent-bash-test.el ends here
