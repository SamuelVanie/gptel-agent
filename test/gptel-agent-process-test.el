;;; gptel-agent-process-test.el --- Managed shell job tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)

(defun gptel-agent-process-test--wait (predicate)
  "Wait at most five seconds for PREDICATE while servicing subprocesses."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(defun gptel-agent-process-test--output (process)
  "Return retained output for PROCESS."
  (with-current-buffer (process-buffer process) (buffer-string)))

(defmacro gptel-agent-process-test--with-session (&rest body)
  "Run BODY in a conversation whose destruction runs kill hooks."
  (declare (indent 0))
  `(let ((session (generate-new-buffer " *process-test-session*")))
     (unwind-protect
         (with-current-buffer session ,@body)
       (when (buffer-live-p session) (kill-buffer session)))))

(cl-defmacro gptel-agent-process-test--with-job ((process command) &body body)
  "Start background PROCESS running COMMAND in a disposable conversation."
  (declare (indent 1))
  `(gptel-agent-process-test--with-session
     (let ((,process (gptel-agent--execute-bash #'ignore ,command "Test output" t)))
       ,@body)))

(ert-deftest gptel-agent-process-start-returns-once-and-does-not-block-other-bash ()
  (gptel-agent-process-test--with-session
    (let* ((source (current-buffer)) results callback-buffer foreground
           (process (gptel-agent--execute-bash
                     (lambda (result) (push result results))
                     "printf 'ready\n'; read -r answer; printf 'answer:%s\n' \"$answer\""
                     "Read replies" t))
           (id (gptel-agent--job-id (process-get process 'gptel-agent-job))))
      (should (= (length results) 1))
      (should (string-match-p (regexp-quote id) (car results)))
      (should (process-live-p process))
      (gptel-agent-process-test--wait
       (lambda () (string-match-p "ready" (gptel-agent-process-test--output process))))
      (should (string-match-p "ready" (gptel-agent--manage-process "read" id)))
      (should (string-match-p "no new output" (gptel-agent--manage-process "read" id)))
      (gptel-agent--execute-bash
       (lambda (result) (setq foreground result callback-buffer (current-buffer)))
       "printf 'parallel'" "Read result")
      (gptel-agent-process-test--wait (lambda () foreground))
      (should (equal foreground "parallel"))
      (should (eq source callback-buffer))
      (should (process-live-p process))
      (gptel-agent--manage-process "send" id "hello\n")
      (gptel-agent-process-test--wait (lambda () (not (process-live-p process))))
      (should (string-match-p "answer:hello" (gptel-agent--manage-process "read" id)))
      (should (= (length results) 1)))))

(ert-deftest gptel-agent-process-isolation-rejects-every-operation-from-other-chat ()
  (gptel-agent-process-test--with-job (process "read -r answer")
    (let ((id (gptel-agent--job-id (process-get process 'gptel-agent-job))))
      (gptel-agent-process-test--with-session
        (should (equal (gptel-agent--manage-process "list")
                       "No background processes in this session."))
        (dolist (action '("read" "send" "interrupt" "kill" "remove"))
          (should-error (gptel-agent--manage-process action id "oops\n")))
        (let* ((other (gptel-agent--execute-bash #'ignore "read -r answer" "Test" t))
               (other-id (gptel-agent--job-id (process-get other 'gptel-agent-job))))
          (should-not (equal id other-id))
          (should-not (string-match-p (concat "process_id: " id "\n")
                                     (gptel-agent--manage-process "list")))))
      (should (process-live-p process))
      (should (string-match-p id (gptel-agent--manage-process "list"))))))

(ert-deftest gptel-agent-process-subagent-job-survives-child-and-dies-with-parent ()
  (let ((parent (generate-new-buffer " *job-parent*"))
        (child (generate-new-buffer " *job-child*"))
        process job)
    (unwind-protect
        (progn
          (with-current-buffer child
            (setq-local gptel-agent--supervised-run
                        (gptel-agent--make-run :parent-buffer parent))
            (setq process (gptel-agent--execute-bash #'ignore "read -r answer" "Test" t)
                  job (process-get process 'gptel-agent-job)))
          (kill-buffer child)
          (should (process-live-p process))
          (with-current-buffer parent
            ;; Changing modes must not abandon the registry or cleanup hook.
            (text-mode)
            (should (string-match-p (gptel-agent--job-id job)
                                    (gptel-agent--manage-process "list"))))
          (kill-buffer parent)
          (should-not (process-live-p process))
          (should-not (buffer-live-p (gptel-agent--job-buffer job))))
      (when (buffer-live-p child) (kill-buffer child))
      (when (buffer-live-p parent) (kill-buffer parent)))))

(ert-deftest gptel-agent-process-retains-failure-output-and-can-forget-completed-job ()
  (gptel-agent-process-test--with-job
      (process "printf 'stdout\n'; printf 'stderr\n' >&2; exit 7")
    (let ((id (gptel-agent--job-id (process-get process 'gptel-agent-job))))
      (gptel-agent-process-test--wait (lambda () (not (process-live-p process))))
      (let ((result (gptel-agent--manage-process "read" id)))
        (should (string-match-p "status: exit\nexit_code: 7" result))
        (should (string-match-p "stdout\nstderr\n" result)))
      (should (string-match-p "exit_code: 7" (gptel-agent--manage-process "list")))
      (should-error (gptel-agent--manage-process "send" id "input"))
      (gptel-agent--manage-process "remove" id)
      (should-not (buffer-live-p (process-buffer process)))
      (should-error (gptel-agent--manage-process "read" id)))))

(ert-deftest gptel-agent-process-output-is-bounded-and-offsets-survive-eviction ()
  (let ((gptel-agent-process-output-limit 10))
    (gptel-agent-process-test--with-job
        (process "printf '0123456789abcdef'; read -r answer; printf 'GHIJ'")
      (let* ((job (process-get process 'gptel-agent-job))
             (id (gptel-agent--job-id job)))
        (gptel-agent-process-test--wait (lambda () (= (gptel-agent--job-base job) 6)))
        (should (= (with-current-buffer (process-buffer process) (buffer-size)) 10))
        (let ((result (gptel-agent--manage-process "read" id nil nil 4)))
          (should (string-match-p "Output truncated: characters 0..6" result))
          (should (string-match-p "next_offset: 10\noutput_end: 16" result))
          (should (string-suffix-p "6789" result)))
        (should (string-suffix-p "6789" (gptel-agent--manage-process "read" id nil 6 4)))
        (should (= (gptel-agent--job-cursor job) 10))
        (gptel-agent--manage-process "send" id "go\n")
        (gptel-agent-process-test--wait (lambda () (not (process-live-p process))))
        (should (string-suffix-p "abcdefGHIJ" (gptel-agent--manage-process "read" id)))
        (should (= (gptel-agent--job-base job) 10))
        (should (= (gptel-agent--job-cursor job) 20))
        (should-error (gptel-agent--manage-process "read" id nil -1))
        (should-error (gptel-agent--manage-process "read" id nil 21))
        (should-error (gptel-agent--manage-process "read" id nil nil 20001))
        (should-error (gptel-agent--manage-process "read" id nil nil 0))))))

(ert-deftest gptel-agent-process-interrupt-allows-graceful-exit ()
  (gptel-agent-process-test--with-job
      (process "trap 'printf stopped; exit 0' INT; printf ready; while :; do read -r answer; done")
    (let ((id (gptel-agent--job-id (process-get process 'gptel-agent-job))))
      (gptel-agent-process-test--wait
       (lambda () (equal (gptel-agent-process-test--output process) "ready")))
      (should-error (gptel-agent--manage-process "remove" id))
      (gptel-agent--manage-process "interrupt" id)
      (gptel-agent-process-test--wait (lambda () (not (process-live-p process))))
      (should (string-match-p "stopped" (gptel-agent--manage-process "read" id))))))

(ert-deftest gptel-agent-process-kill-terminates-shell-child-group ()
  (skip-unless (memq system-type '(darwin gnu/linux berkeley-unix)))
  (gptel-agent-process-test--with-job
      (process "sleep 60 & child=$!; printf '%s\n' \"$child\"; wait")
    (gptel-agent-process-test--wait
     (lambda () (string-match-p "[0-9]+\n" (gptel-agent-process-test--output process))))
    (let* ((child (string-to-number (gptel-agent-process-test--output process)))
           (id (gptel-agent--job-id (process-get process 'gptel-agent-job))))
      (should (process-attributes child))
      (gptel-agent--manage-process "kill" id)
      (gptel-agent-process-test--wait
       (lambda ()
         (and (not (process-live-p process))
              (let ((attrs (process-attributes child)))
                (or (null attrs) (equal (alist-get 'state attrs) "Z"))))))
      ;; A wrapper can reap its killed child and exit before it is signalled.
      (should (memq (process-status process) '(signal exit))))))

(ert-deftest gptel-agent-process-launch-failure-releases-buffer-and-registry ()
  (with-temp-buffer
    (let ((before (buffer-list)))
      (dolist (failure '(nil error))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest _) (when failure (error "Cannot start")))))
          (should-error (gptel-agent--execute-bash #'ignore "ignored" "Test" t)))
        (should-not gptel-agent--processes)
        (should-not
         (cl-find-if
          (lambda (buffer)
            (string-prefix-p " *gptel-agent-process*" (buffer-name buffer)))
          (cl-set-difference (buffer-list) before)))))))

(ert-deftest gptel-agent-process-bash-json-false-keeps-foreground-semantics ()
  (with-temp-buffer
    (let (result)
      (apply #'gptel-agent--execute-bash (lambda (value) (setq result value))
             (gptel--map-tool-args
              (gptel-get-tool "Bash")
              (gptel--json-read-string
               "{\"command\":\"printf foreground\",\"query\":\"Test\",\"background\":false}")))
      (gptel-agent-process-test--wait (lambda () result))
      (should (equal result "foreground"))
      (should-not gptel-agent--processes))))

(ert-deftest gptel-agent-process-polling-is-not-a-repetition-violation ()
  (with-temp-buffer
    (gptel-agent--reset-tool-call-counts)
    (let ((gptel-agent-max-tool-repetitions 2)
          (gptel-agent-max-similar-tool-calls 2))
      (dotimes (_ 10)
        (should-not (gptel-agent--detect-repetition
                     '(:name "Process" :args (:action "read" :process_id "bash-1")))))
      (should-not gptel-agent--tool-call-history))))

(ert-deftest gptel-agent-process-tool-is-enabled-and-only-input-needs-confirmation ()
  (let ((tool (gptel-get-tool "Process")))
    (should-not (gptel-tool-async tool))
    (dolist (action '("list" "read" "interrupt" "kill" "remove"))
      (should-not (funcall (gptel-tool-confirm tool) action)))
    (should (funcall (gptel-tool-confirm tool) "send")))
  (dolist (name '("gptel-agent" "executor"))
    (let* ((file (expand-file-name
                  (format "agents/%s.md" name)
                  (file-name-directory (locate-library "gptel-agent"))))
           (tools (plist-get (gptel-agent-parse-markdown-frontmatter file nil nil t) :tools)))
      (should (member "Process" tools)))))

;;; gptel-agent-process-test.el ends here
