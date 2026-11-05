;;; gptel-agent-process-test.el --- Managed shell job tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)
(require 'gptel-openai-responses)

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
        (let (result)
          (should-not (gptel-agent--process-tool
                       (lambda (value) (setq result value)) "wait" id))
          (should (string-prefix-p "Error: Unknown process_id" result)))
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
      (dolist (action '("read" "wait"))
        (dotimes (_ 10)
          (should-not (gptel-agent--detect-repetition
                       `(:name "Process" :args (:action ,action :process_id "bash-1"))))))
      (should-not gptel-agent--tool-call-history))))

(ert-deftest gptel-agent-process-tool-is-enabled-and-only-input-needs-confirmation ()
  (let ((tool (gptel-get-tool "Process")))
    (should (gptel-tool-async tool))
    (dolist (action '("list" "read" "wait" "interrupt" "kill" "remove"))
      (should-not (funcall (gptel-tool-confirm tool) action)))
    (should (funcall (gptel-tool-confirm tool) "send")))
  (let* ((file (expand-file-name
                "agents/gptel-agent.md"
                (file-name-directory (locate-library "gptel-agent"))))
         (tools (plist-get (gptel-agent-parse-markdown-frontmatter file nil nil t) :tools)))
    (should (member "Process" tools))))

(ert-deftest gptel-agent-process-tail-preserves-cursor-and-reports-retained-ranges ()
  (let ((gptel-agent-process-output-limit 10))
    (gptel-agent-process-test--with-job
        (process "printf '0123456789abcdef'; read -r answer")
      (let* ((job (process-get process 'gptel-agent-job))
             (id (gptel-agent--job-id job)))
        (gptel-agent-process-test--wait (lambda () (= (gptel-agent--job-base job) 6)))
        (let ((tail (gptel-agent--manage-process "read" id nil nil 4 "tail")))
          (should (string-suffix-p "cdef" tail))
          (should (string-match-p "retained_start: 6\noutput_start: 12\nnext_offset: 16\noutput_end: 16" tail))
          (should (string-match-p "characters 0..6 were discarded" tail))
          (should (string-match-p "omitted retained characters 6..12" tail))
          (should-not (string-match-p "read again" tail)))
        (should (= (gptel-agent--job-cursor job) 0))
        (should (string-suffix-p "6789" (gptel-agent--manage-process "read" id nil nil 4)))
        (should (string-suffix-p "cdef" (gptel-agent--manage-process "read" id nil nil 4 "tail")))
        (should (= (gptel-agent--job-cursor job) 10))
        (should (string-suffix-p "ab" (gptel-agent--manage-process "read" id nil 10 2)))
        (should (string-suffix-p "abcdef" (gptel-agent--manage-process "read" id)))
        (should-error (gptel-agent--manage-process "read" id nil 0 4 "tail"))
        (should-error (gptel-agent--manage-process "read" id nil nil 4 "unknown"))))))

(ert-deftest gptel-agent-process-tail-handles-empty-short-and-multibyte-output ()
  (gptel-agent-process-test--with-job (process "read -r answer")
    (let ((job (process-get process 'gptel-agent-job)))
      (should (string-match-p "output_start: 0\nnext_offset: 0\noutput_end: 0"
                              (gptel-agent--process-read job nil 4 "tail")))
      (gptel-agent--process-filter job process "aλ🙂z")
      (should (string-suffix-p "aλ🙂z" (gptel-agent--process-read job nil 20 "tail")))
      (should (string-suffix-p "🙂z" (gptel-agent--process-read job nil 2 "tail")))
      (should (= (gptel-agent--job-cursor job) 0)))))

(ert-deftest gptel-agent-process-wait-ignores-progress-and-delivers-final-tail-once ()
  (gptel-agent-process-test--with-job
      (process "printf started; read -r answer; printf finished; exit 7")
    (let* ((job (process-get process 'gptel-agent-job))
           (id (gptel-agent--job-id job))
           (source (current-buffer)) results callback-buffers)
      (dotimes (_ 2)
        (should-not
         (gptel-agent--process-tool
          (lambda (result) (push result results) (push (current-buffer) callback-buffers))
          "wait" id nil nil 8 nil "exit")))
      (gptel-agent-process-test--wait
       (lambda () (equal (gptel-agent-process-test--output process) "started")))
      (should-not results)
      (should (= (length (gptel-agent--job-waiters job)) 2))
      (let* ((waiters (copy-sequence (gptel-agent--job-waiters job)))
             (timers (mapcar #'gptel-agent--process-waiter-timer waiters)))
        (gptel-agent--manage-process "send" id "go\n")
        (gptel-agent-process-test--wait (lambda () (= (length results) 2)))
        (dolist (result results)
          (should (string-match-p "wait_reason: exit" result))
          (should (string-match-p "status: exit\nexit_code: 7" result))
          (should (string-suffix-p "finished" result)))
        (should (equal callback-buffers (list source source)))
        (should-not (gptel-agent--job-waiters job))
        (dolist (timer timers) (should-not (memq timer timer-list)))
        ;; Simulate a late timeout/duplicate sentinel after completion.
        (dolist (waiter waiters) (gptel-agent--process-finish-wait waiter "timeout"))
        (gptel-agent--process-sentinel job process "finished\n")
        (should (= (length results) 2)))
      (should (= (gptel-agent--job-cursor job) 0)))))

(ert-deftest gptel-agent-process-wait-timeout-keeps-job-and-completed-wait-is-immediate ()
  (gptel-agent-process-test--with-job
      (process "printf ready; read -r answer; printf done")
    (let* ((job (process-get process 'gptel-agent-job))
           (id (gptel-agent--job-id job)) results)
      (gptel-agent-process-test--wait
       (lambda () (equal (gptel-agent-process-test--output process) "ready")))
      (gptel-agent--process-tool (lambda (result) (push result results))
                                 "wait" id nil nil nil nil nil 20)
      (should-not results)
      (gptel-agent-process-test--wait (lambda () results))
      (should (string-match-p "wait_reason: timeout\n.*\nstatus: run" (car results)))
      (should (string-suffix-p "ready" (car results)))
      (should (process-live-p process))
      (should-not (gptel-agent--job-waiters job))
      (gptel-agent--process-tool (lambda (result) (push result results))
                                 "wait" id nil nil nil nil nil 0)
      (should (= (length results) 2))
      (should (string-match-p "wait_reason: timeout" (car results)))
      (gptel-agent--manage-process "send" id "go\n")
      (gptel-agent-process-test--wait (lambda () (not (process-live-p process))))
      (should (= (length results) 2))
      ;; Explicit offsets replay output even though wait defaults to tail.
      (gptel-agent--process-tool (lambda (result) (push result results))
                                 "wait" id nil 5 nil nil nil 0)
      (should (= (length results) 3))
      (should (string-match-p "wait_reason: exit" (car results)))
      (should (string-suffix-p "done" (car results)))
      (should (= (gptel-agent--job-cursor job) 0))
      (gptel-agent--process-tool (lambda (result) (push result results))
                                 "wait" id nil nil 5 "unread")
      (should (string-suffix-p "ready" (car results)))
      (should (= (gptel-agent--job-cursor job) 5)))))

(ert-deftest gptel-agent-process-wait-validates-before-parking ()
  (gptel-agent-process-test--with-job (process "read -r answer")
    (let* ((job (process-get process 'gptel-agent-job))
           (id (gptel-agent--job-id job)))
      (dolist (args '((nil nil nil nil -1) (nil nil nil nil 60001)
                      (nil nil nil nil 1.5) (nil nil nil "idle" nil)
                      (nil 0 nil nil nil) (nil nil "bad" nil nil)
                      (0 nil "tail" nil nil) (1 nil nil nil nil)))
        (let (results)
          (should-not
           (apply #'gptel-agent--process-tool
                  (lambda (result) (push result results)) "wait" id nil args))
          (should (= (length results) 1))
          (should (string-prefix-p "Error: " (car results))))
        (should-not (gptel-agent--job-waiters job))
        (should (= (gptel-agent--job-cursor job) 0))))))

(ert-deftest gptel-agent-process-tool-does-not-retry-a-failed-callback ()
  (gptel-agent-process-test--with-session
    ;; Neither a successful operation nor an operation error should cause a
    ;; callback's own failure to be caught and delivered as a second result.
    (dolist (action '("list" "wait"))
      (let ((calls 0))
        (should-error
         (gptel-agent--process-tool
          (lambda (_result) (cl-incf calls) (error "Callback failed")) action))
        (should (= calls 1))))))

(ert-deftest gptel-agent-process-invalid-output-options-remain-tool-results ()
  ;; Exercise the real async dispatcher and worker error handler.  An escaping
  ;; validation error used to become ERRS with the successful HTTP status intact.
  (dolist (localized '(nil t))
    (gptel-agent-process-test--with-job
        (process "printf ready; read -r answer; printf done")
      (let* ((job (process-get process 'gptel-agent-job))
             (id (gptel-agent--job-id job))
             (gptel-confirm-tool-calls nil)
             (gptel-agent-use-project-instructions nil)
             (backend (gptel-make-openai-responses
                       "process-test" :key "unused" :host "example.invalid"
                       :models '(test)))
             (gptel-tools (list (gptel-get-tool "Process")))
             (fsm (gptel-make-fsm
                   :table gptel-send--transitions
                   :handlers '((TOOL gptel--handle-tool-use))
                   :info (list :buffer (current-buffer) :backend backend
                               :callback #'ignore :http-status "200"
                               :status "HTTP/2 200"))))
        (when localized (gptel-agent--localize-agent-tool fsm))
        (setf (plist-get (gptel-fsm-info fsm) :tools) gptel-tools)
        (gptel-agent--workers-install fsm)
        (dolist (action '("read" "wait"))
          (let ((call (list :name "Process" :args
                            (list :action action :process_id id
                                  :offset 0 :view "tail"))))
            (setf (plist-get (gptel-fsm-info fsm) :tool-use) (list call))
            (gptel--fsm-transition fsm 'TOOL)
            (should (eq (gptel-fsm-state fsm) 'TRET))
            (should-not (plist-get (gptel-fsm-info fsm) :error))
            (should (string-prefix-p "Error: " (plist-get call :result)))
            (should (string-match-p "offset cannot be combined with view=tail"
                                    (plist-get call :result)))
            (should-not (gptel-agent--job-waiters job))
            (should (= (gptel-agent--job-cursor job) 0))
            (should (process-live-p process))))
        ;; The model can correct the arguments and wait in the same request.
        (let ((call (list :name "Process" :args
                          (list :action "wait" :process_id id :until "exit"
                                :view "tail" :timeout_ms 60000))))
          (setf (plist-get (gptel-fsm-info fsm) :tool-use) (list call))
          (gptel--fsm-transition fsm 'TOOL)
          (should (eq (gptel-fsm-state fsm) 'TOOL))
          (should-not (plist-get call :result))
          (gptel-agent--manage-process "send" id "go\n")
          (gptel-agent-process-test--wait (lambda () (plist-get call :result)))
          (should (eq (gptel-fsm-state fsm) 'TRET))
          (should (string-match-p "wait_reason: exit" (plist-get call :result)))
          (should (string-suffix-p "readydone" (plist-get call :result)))
          (should-not (plist-get (gptel-fsm-info fsm) :error))
          (should-not (gptel-agent--job-waiters job)))))))

(ert-deftest gptel-agent-process-tool-schema-dispatches-read-wait-and-send ()
  (gptel-agent-process-test--with-job
      (process "printf ready; read -r answer; printf done")
    (let* ((job (process-get process 'gptel-agent-job))
           (id (gptel-agent--job-id job))
           (tool (gptel-get-tool "Process")) results completed)
      (gptel-agent-process-test--wait
       (lambda () (equal (gptel-agent-process-test--output process) "ready")))
      (dolist (args (list (list :action "list")
                          (list :action "read" :process_id id :view "tail" :max_chars 3)))
        (should-not (apply (gptel-tool-function tool)
                           (lambda (result) (push result results))
                           (gptel--map-tool-args tool args))))
      (should (= (length results) 2))
      (should (string-suffix-p "ady" (car results)))
      (should (= (gptel-agent--job-cursor job) 0))
      (should-not
       (apply (gptel-tool-function tool)
              (lambda (result) (setq completed result))
              (gptel--map-tool-args
               tool (list :action "wait" :process_id id :until "exit"
                          :timeout_ms 60000 :view "tail" :max_chars 4))))
      (should-not completed)
      (apply (gptel-tool-function tool) (lambda (result) (push result results))
             (gptel--map-tool-args tool (list :action "send" :process_id id :input "go\n")))
      (should (= (length results) 3))
      (gptel-agent-process-test--wait (lambda () completed))
      (should (string-suffix-p "done" completed))
      (should (string-match-p "wait_reason: exit" completed)))))

(ert-deftest gptel-agent-process-removal-resolves-pending-wait-before-disposal ()
  (gptel-agent-process-test--with-job (process "read -r answer; printf done")
    (let* ((job (process-get process 'gptel-agent-job))
           (id (gptel-agent--job-id job)) results)
      (gptel-agent--process-tool (lambda (result) (push result results)) "wait" id)
      (let* ((waiter (car (gptel-agent--job-waiters job)))
             (timer (gptel-agent--process-waiter-timer waiter)))
        ;; Hold the completion notification to exercise removal racing delivery.
        (set-process-sentinel process #'ignore)
        (gptel-agent--manage-process "send" id "go\n")
        (gptel-agent-process-test--wait (lambda () (not (process-live-p process))))
        (gptel-agent--manage-process "remove" id)
        (should (= (length results) 1))
        (should (string-match-p "wait_reason: removed" (car results)))
        (should (string-suffix-p "done" (car results)))
        (should-not (buffer-live-p (gptel-agent--job-buffer job)))
        (should-not (memq timer timer-list))
        (gptel-agent--process-finish-wait waiter "timeout")
        (should (= (length results) 1))))))

(ert-deftest gptel-agent-process-request-cancellation-clears-wait-without-stopping-job ()
  (gptel-agent-process-test--with-job (process "read -r answer")
    (let* ((job (process-get process 'gptel-agent-job))
           (id (gptel-agent--job-id job))
           (fsm (gptel-make-fsm :state 'TOOL :info (list :buffer (current-buffer))))
           (global (gptel-get-tool "Process"))
           (gptel-tools (list global)) results)
      (gptel-agent--localize-agent-tool fsm)
      (should-not (eq (car gptel-tools) global))
      (apply (gptel-tool-function (car gptel-tools))
             (lambda (result) (push result results))
             (gptel--map-tool-args (car gptel-tools)
                                  (list :action "wait" :process_id id)))
      (let* ((waiter (car (gptel-agent--job-waiters job)))
             (timer (gptel-agent--process-waiter-timer waiter)))
        (should (plist-get (gptel-fsm-info fsm) :post))
        (setf (gptel-fsm-state fsm) 'ABRT)
        (mapc (lambda (fn) (funcall fn (gptel-fsm-info fsm)))
              (plist-get (gptel-fsm-info fsm) :post))
        (should-not (gptel-agent--job-waiters job))
        (should-not (memq timer timer-list))
        (should-not (plist-get (gptel-fsm-info fsm) :post))
        (should (process-live-p process))
        (gptel-agent--process-finish-wait waiter "timeout")
        (gptel-agent--manage-process "send" id "go\n")
        (gptel-agent-process-test--wait (lambda () (not (process-live-p process))))
        (should-not results)))))

(ert-deftest gptel-agent-process-killing-caller-cancels-only-its-wait ()
  (gptel-agent-process-test--with-job (process "read -r answer")
    (let* ((parent (current-buffer))
           (child (generate-new-buffer " *process-wait-child*"))
           (job (process-get process 'gptel-agent-job))
           (id (gptel-agent--job-id job)) parent-result child-result)
      (unwind-protect
          (progn
            (gptel-agent--process-tool (lambda (result) (setq parent-result result)) "wait" id)
            (with-current-buffer child
              (setq-local gptel-agent--supervised-run (gptel-agent--make-run :parent-buffer parent))
              (gptel-agent--process-tool (lambda (result) (setq child-result result)) "wait" id))
            (let ((timer (gptel-agent--process-waiter-timer (car (gptel-agent--job-waiters job)))))
              (kill-buffer child)
              (should-not (memq timer timer-list)))
            (should (process-live-p process))
            (should (= (length (gptel-agent--job-waiters job)) 1))
            (gptel-agent--manage-process "kill" id)
            (gptel-agent-process-test--wait (lambda () parent-result))
            (should (string-match-p "wait_reason: exit\n.*\nstatus: signal\nsignal:" parent-result))
            (should-not child-result)
            (should-not (gptel-agent--job-waiters job)))
        (when (buffer-live-p child) (kill-buffer child))))))

(ert-deftest gptel-agent-process-owner-cleanup-cancels-wait-and-timer ()
  (let (job process timer results)
    (gptel-agent-process-test--with-session
      (setq process (gptel-agent--start-background-process "read -r answer")
            job (process-get process 'gptel-agent-job))
      (gptel-agent--process-tool (lambda (result) (push result results))
                                 "wait" (gptel-agent--job-id job))
      (setq timer (gptel-agent--process-waiter-timer (car (gptel-agent--job-waiters job)))))
    (should-not results)
    (should-not (process-live-p process))
    (should-not (gptel-agent--job-waiters job))
    (should-not (memq timer timer-list))))

;;; gptel-agent-process-test.el ends here
