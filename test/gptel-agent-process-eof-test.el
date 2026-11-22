;;; gptel-agent-process-eof-test.el --- Managed pipe EOF tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)
(require 'gptel-openai-responses)

(defun gptel-agent-process-eof-test--wait (predicate)
  "Service timers and subprocesses for at most five seconds until PREDICATE."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.005))
    (should (funcall predicate))))

(defmacro gptel-agent-process-eof-test--with-session (&rest body)
  "Run BODY in a disposable conversation."
  (declare (indent 0))
  `(let ((session (generate-new-buffer " *process-eof-test*")))
     (unwind-protect
         (with-current-buffer session ,@body)
       (when (buffer-live-p session) (kill-buffer session)))))

(defmacro gptel-agent-process-eof-test--with-job (command &rest body)
  "Run BODY with PROCESS, JOB and ID for COMMAND."
  (declare (indent 1))
  `(gptel-agent-process-eof-test--with-session
     (let* ((process (gptel-agent--start-background-process ,command))
            (job (process-get process 'gptel-agent-job))
            (id (gptel-agent--job-id job)))
       ,@body)))

(defun gptel-agent-process-eof-test--cancel (fsm)
  "Cancel FSM through its real request cleanup callbacks."
  (setf (gptel-fsm-state fsm) 'ABRT)
  (mapc (lambda (fn) (funcall fn (gptel-fsm-info fsm)))
        (copy-sequence (plist-get (gptel-fsm-info fsm) :post))))

(ert-deftest gptel-agent-process-eof-finishes-wc-and-wait-after-literal-control-d ()
  (gptel-agent-process-eof-test--with-job "wc -c"
    (let (sent closed waited)
      (gptel-agent--process-tool (lambda (result) (setq waited result)) "wait" id)
      (gptel-agent--process-tool (lambda (result) (setq sent result))
                                 "send" id "abc\n\4")
      (gptel-agent-process-eof-test--wait (lambda () sent))
      (should (string-match-p "^send_reason: sent$" sent))
      (accept-process-output nil 0.05)
      (should (process-live-p process))
      (should-not waited)
      (should-not (gptel-agent--process-tool
                   (lambda (result) (setq closed result)) "eof" id))
      (gptel-agent-process-eof-test--wait (lambda () (and closed waited)))
      (should (string-match-p "^eof_reason: closed$" closed))
      (should (string-match-p "^wait_reason: exit$" waited))
      (should (string-match-p "^exit_code: 0$" waited))
      (should (string-match-p "STDOUT\\+STDERR:\n[[:space:]]*5\n\\'" waited))
      (should (gptel-agent--job-input-closed job))
      (should-not (gptel-agent--job-eof job))
      (should-not (gptel-agent--job-waiters job)))))

(ert-deftest gptel-agent-process-eof-queued-after-large-send-preserves-exact-input ()
  (let* ((directory (make-temp-file "gptel-eof-order-" t))
         (file (expand-file-name "received" directory))
         (input (concat (make-string 65535 ?x) "é\nTAIL"))
         (expected (encode-coding-string input 'utf-8-unix)))
    (unwind-protect
        (gptel-agent-process-eof-test--with-job
            (format "sleep 0.1; cat > %s; wc -c < %s"
                    (shell-quote-argument file) (shell-quote-argument file))
          (let ((native-eof (symbol-function 'process-send-eof))
                events waited eof-calls)
            (cl-letf (((symbol-function 'process-send-eof)
                       (lambda (target)
                         (should (eq target process))
                         (should-not (gptel-agent--job-send job))
                         (should (string-match-p "^send_reason: sent$"
                                                 (gptel-agent--job-last-send job)))
                         (push 'native eof-calls)
                         (funcall native-eof target))))
              (gptel-agent--process-tool (lambda (result) (push (cons 'send result) events))
                                         "send" id input)
              (gptel-agent--process-tool (lambda (result) (push (cons 'eof result) events))
                                         "eof" id)
              (should (gptel-agent--job-send job))
              (should (gptel-agent--job-eof job))
              (let ((pending (gptel-agent--job-eof job)) result)
                (dolist (action '("send" "eof"))
                  (gptel-agent--process-tool (lambda (value) (setq result value))
                                             action id (and (equal action "send") "late"))
                  (should (string-prefix-p "Error: " result))
                  (should (eq pending (gptel-agent--job-eof job)))))
              (gptel-agent--process-tool (lambda (result) (setq waited result)) "wait" id)
              (gptel-agent-process-eof-test--wait (lambda () (and waited (= (length events) 2))))
              (should (equal (mapcar #'car (reverse events)) '(send eof)))
              (should (equal eof-calls '(native)))
              (should (string-match-p "^eof_reason: closed$" (cdar events)))
              (should (string-match-p "^exit_code: 0$" waited))
              (should (equal (with-temp-buffer
                               (set-buffer-multibyte nil)
                               (insert-file-contents-literally file)
                               (buffer-string))
                             expected)))))
      (delete-directory directory t))))

(ert-deftest gptel-agent-process-eof-repeats-idempotently-and-rejects-late-input ()
  (gptel-agent-process-eof-test--with-job "cat >/dev/null; printf closed; sleep 60"
    (let ((native-eof (symbol-function 'process-send-eof)) (calls 0) result)
      (cl-letf (((symbol-function 'process-send-eof)
                 (lambda (target) (cl-incf calls) (funcall native-eof target))))
        (gptel-agent--process-tool (lambda (value) (setq result value)) "eof" id)
        (gptel-agent-process-eof-test--wait (lambda () result))
        (should (string-match-p "^eof_reason: closed$" result))
        (gptel-agent-process-eof-test--wait
         (lambda () (with-current-buffer (process-buffer process)
                      (string-match-p "closed" (buffer-string)))))
        (should (process-live-p process))
        (gptel-agent--process-tool (lambda (value) (setq result value)) "send" id "late")
        (should (string-prefix-p "Error: " result))
        (should (string-match-p "[Cc]losed" result))
        (dotimes (i 2)
          (when (= i 1) (delete-process process))
          (setq result nil)
          (gptel-agent--process-tool (lambda (value) (setq result value)) "eof" id)
          (gptel-agent-process-eof-test--wait (lambda () result))
          (should (string-match-p "^eof_reason: already_closed$" result)))
        (should (= calls 1))))))

(ert-deftest gptel-agent-process-eof-rejects-completed-or-deleted-open-jobs ()
  (dolist (command '("true" "sleep 60"))
    (gptel-agent-process-eof-test--with-job command
      (if (equal command "true")
          (gptel-agent-process-eof-test--wait (lambda () (not (process-live-p process))))
        (delete-process process))
      (let (results)
        (cl-letf (((symbol-function 'process-send-eof)
                   (lambda (&rest _) (ert-fail "EOF attempted on a dead job"))))
          (gptel-agent--process-tool (lambda (result) (push result results)) "eof" id))
        (should (= (length results) 1))
        (should (string-prefix-p "Error: " (car results)))
        (should-not (gptel-agent--job-input-closed job))
        (should-not (gptel-agent--job-eof job))))))

(ert-deftest gptel-agent-process-eof-zero-deadline-leaves-input-open ()
  (gptel-agent-process-eof-test--with-job "cat"
    (let (results sent)
      (cl-letf (((symbol-function 'process-send-eof)
                 (lambda (&rest _) (ert-fail "Zero deadline closed stdin"))))
        (gptel-agent--process-tool (lambda (result) (push result results))
                                   "eof" id nil nil nil nil nil 0)
        (gptel-agent-process-eof-test--wait (lambda () results)))
      (should (= (length results) 1))
      (should (string-match-p "^eof_reason: timeout$" (car results)))
      (should-not (gptel-agent--job-eof job))
      (should-not (gptel-agent--job-input-closed job))
      (gptel-agent--process-tool (lambda (result) (setq sent result)) "send" id "still open\n")
      (gptel-agent-process-eof-test--wait (lambda () sent))
      (should (string-match-p "^send_reason: sent$" sent))
      (should (process-live-p process)))))

(ert-deftest gptel-agent-process-eof-timeout-does-not-cancel-accepted-send ()
  (gptel-agent-process-eof-test--with-job "wc -c"
    (let ((gptel-agent--process-writing t) sent closed)
      (gptel-agent--process-tool (lambda (result) (setq sent result)) "send" id "abc")
      (gptel-agent--process-tool (lambda (result) (setq closed result))
                                 "eof" id nil nil nil nil nil 10)
      ;; Model another job holding the write guard past this EOF's deadline.
      (accept-process-output nil 0.03)
      (should-not (gptel-agent--job-input-closed job))
      (should (gptel-agent--job-send job))
      (setq gptel-agent--process-writing nil)
      (gptel-agent-process-eof-test--wait (lambda () (and sent closed)))
      (should (string-match-p "^send_reason: sent$" sent))
      (should (string-match-p "^eof_reason: timeout$" closed))
      (should-not (gptel-agent--job-input-closed job))
      (should (process-live-p process))
      (setq closed nil)
      (gptel-agent--process-tool (lambda (result) (setq closed result)) "eof" id)
      (gptel-agent-process-eof-test--wait (lambda () closed))
      (should (string-match-p "^eof_reason: closed$" closed)))))

(ert-deftest gptel-agent-process-eof-cancelled-preceding-send-never-closes ()
  (gptel-agent-process-eof-test--with-job "cat"
    (let ((fsm (gptel-make-fsm :state 'TOOL :info (list :buffer (current-buffer))))
          sent closed)
      (gptel-agent--process-tool (lambda (result) (setq sent result))
                                 "send" id "discarded" nil nil nil nil nil fsm)
      (gptel-agent--process-tool (lambda (result) (setq closed result)) "eof" id)
      (gptel-agent-process-eof-test--cancel fsm)
      (cl-letf (((symbol-function 'process-send-eof)
                 (lambda (&rest _) (ert-fail "EOF executed after send cancellation"))))
        (gptel-agent-process-eof-test--wait (lambda () closed)))
      (should-not sent)
      (should (string-match-p "^eof_reason: input_failed$" closed))
      (should-not (gptel-agent--job-input-disabled job))
      (should-not (gptel-agent--job-input-closed job))
      (should-not (gptel-agent--job-eof job))
      (should (process-live-p process)))))

(ert-deftest gptel-agent-process-eof-cancellation-preserves-an-independent-send ()
  (gptel-agent-process-eof-test--with-job "cat"
    (let ((fsm (gptel-make-fsm :state 'TOOL :info (list :buffer (current-buffer))))
          sent closed)
      (gptel-agent--process-tool (lambda (result) (setq sent result)) "send" id "kept\n")
      (let ((pending (gptel-agent--job-send job)))
        (gptel-agent--process-tool (lambda (result) (setq closed result))
                                   "eof" id nil nil nil nil nil nil fsm)
        (should (plist-get (gptel-fsm-info fsm) :post))
        (gptel-agent-process-eof-test--cancel fsm)
        (should (eq pending (gptel-agent--job-send job)))
        (should-not (plist-get (gptel-fsm-info fsm) :post))
        (should-not (gptel-agent--job-eof job)))
      (cl-letf (((symbol-function 'process-send-eof)
                 (lambda (&rest _) (ert-fail "Cancelled EOF closed stdin"))))
        (gptel-agent-process-eof-test--wait (lambda () sent))
        (accept-process-output nil 0.03))
      (should-not closed)
      (should (string-match-p "^send_reason: sent$" sent))
      (should-not (gptel-agent--job-input-closed job))
      (should (process-live-p process)))))

(ert-deftest gptel-agent-process-eof-reservation-rejects-reentrant-callback-sends ()
  (gptel-agent-process-eof-test--with-job "cat >/dev/null; sleep 60"
    (let (sent closed before-close after-close)
      (gptel-agent--process-tool
       (lambda (result)
         (setq sent result)
         (gptel-agent--process-tool (lambda (value) (setq before-close value))
                                    "send" id "too late"))
       "send" id "abc")
      (gptel-agent--process-tool
       (lambda (result)
         (setq closed result)
         (gptel-agent--process-tool (lambda (value) (setq after-close value))
                                    "send" id "also too late"))
       "eof" id)
      (gptel-agent-process-eof-test--wait (lambda () (and sent closed)))
      (should (string-match-p "^send_reason: sent$" sent))
      (should (string-match-p "^eof_reason: closed$" closed))
      (should (string-prefix-p "Error: " before-close))
      (should (string-prefix-p "Error: " after-close))
      (should (string-match-p "[Cc]losed" after-close))
      (should-not (gptel-agent--job-send job))
      (should (process-live-p process)))))

(ert-deftest gptel-agent-process-eof-defers-past-another-jobs-real-blocked-write ()
  (dolist (timeout '(10 2000))
    (gptel-agent-process-eof-test--with-job "cat"
      (let* ((source (current-buffer))
             (blocked (gptel-agent--start-background-process "sleep 60"))
             (blocked-id (gptel-agent--job-id (process-get blocked 'gptel-agent-job)))
             (native-send (symbol-function 'process-send-string))
             (native-eof (symbol-function 'process-send-eof))
             sent closed queued callback-during-write close-during-write (close-count 0)
             (safety (run-at-time 3 nil
                                  (lambda () (when (process-live-p blocked)
                                               (delete-process blocked))))))
        (unwind-protect
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (target input)
                         (let ((probe
                                (run-at-time
                                 0.02 nil
                                 (lambda ()
                                   (unless queued
                                     (setq queued t)
                                     (with-current-buffer source
                                       (gptel-agent--process-tool
                                        (lambda (result)
                                          (setq closed result
                                                callback-during-write
                                                gptel-agent--process-writing))
                                        "eof" id nil nil nil nil nil timeout)))))))
                           (unwind-protect (funcall native-send target input)
                             (cancel-timer probe)))))
                      ((symbol-function 'process-send-eof)
                       (lambda (target)
                         ;; EOF's own native close holds the write guard.  The
                         ;; blocked job must have fully unwound before that.
                         (setq close-during-write
                               (gptel-agent--job-send (process-get blocked 'gptel-agent-job)))
                         (cl-incf close-count)
                         (funcall native-eof target))))
              (gptel-agent--process-tool (lambda (result) (setq sent result))
                                         "send" blocked-id (make-string 1048576 ?x)
                                         nil nil nil nil 1000)
              (gptel-agent-process-eof-test--wait (lambda () (and sent closed)))
              (should queued)
              (should (string-match-p "^send_reason: timeout$" sent))
              (should-not callback-during-write)
              (should-not close-during-write)
              (if (= timeout 10)
                  (progn
                    (should (string-match-p "^eof_reason: timeout$" closed))
                    (should (= close-count 0))
                    (should-not (gptel-agent--job-input-closed job))
                    (should (process-live-p process)))
                (should (string-match-p "^eof_reason: closed$" closed))
                (should (= close-count 1))
                (should (gptel-agent--job-input-closed job))))
          (cancel-timer safety))))))

(ert-deftest gptel-agent-process-eof-source-kill-cancels-child-close-only ()
  (gptel-agent-process-eof-test--with-job "cat"
    (let ((parent (current-buffer))
          (child (generate-new-buffer " *process-eof-child*")) closed)
      (unwind-protect
          (progn
            (with-current-buffer child
              (setq-local gptel-agent--supervised-run
                          (gptel-agent--make-run :parent-buffer parent))
              (gptel-agent--process-tool (lambda (result) (setq closed result)) "eof" id))
            (should (gptel-agent--job-eof job))
            (kill-buffer child)
            (should-not (gptel-agent--job-eof job))
            (should-not (gptel-agent--job-input-closed job))
            (accept-process-output nil 0.03)
            (should-not closed)
            (should (process-live-p process))
            ;; The surviving conversation can independently close its own job.
            (gptel-agent--manage-process "eof" id)
            (gptel-agent-process-eof-test--wait
             (lambda () (not (process-live-p process))))
            (should (gptel-agent--job-input-closed job)))
        (when (buffer-live-p child) (kill-buffer child))))))

(ert-deftest gptel-agent-process-eof-owner-kill-cleans-pending-operation ()
  (let (process job closed)
    (gptel-agent-process-eof-test--with-session
      (setq process (gptel-agent--start-background-process "cat")
            job (process-get process 'gptel-agent-job))
      (gptel-agent--process-tool (lambda (result) (setq closed result))
                                 "eof" (gptel-agent--job-id job)))
    (should-not (process-live-p process))
    (should-not (gptel-agent--job-eof job))
    (accept-process-output nil 0.03)
    (should-not closed)))

(ert-deftest gptel-agent-process-eof-exit-before-close-resolves-once ()
  (gptel-agent-process-eof-test--with-job "sleep 60"
    (let (results)
      (cl-letf (((symbol-function 'process-send-eof)
                 (lambda (&rest _) (ert-fail "EOF attempted after exit"))))
        (gptel-agent--process-tool (lambda (result) (push result results)) "eof" id)
        (delete-process process)
        (gptel-agent-process-eof-test--wait (lambda () results))
        (should (string-match-p "^eof_reason: exit$" (car results)))
        (should-not (gptel-agent--job-eof job))
        (should-not (gptel-agent--job-input-closed job))
        (gptel-agent--process-sentinel job process "killed\n")
        (accept-process-output nil 0.03)
        (should (= (length results) 1))))))

(ert-deftest gptel-agent-process-eof-removal-resolves-pending-close-once ()
  (gptel-agent-process-eof-test--with-job "sleep 60"
    (let ((fsm (gptel-make-fsm :state 'TOOL :info (list :buffer (current-buffer))))
          results)
      (gptel-agent--process-tool (lambda (result) (push result results))
                                 "eof" id nil nil nil nil nil nil fsm)
      ;; Hold exit notification to exercise removal racing the queued close.
      (set-process-sentinel process #'ignore)
      (delete-process process)
      (gptel-agent--manage-process "remove" id)
      (cl-letf (((symbol-function 'process-send-eof)
                 (lambda (&rest _) (ert-fail "Removed EOF called the native close"))))
        (gptel-agent-process-eof-test--wait (lambda () results))
        (should (string-match-p "^eof_reason: removed$" (car results)))
        (should-not (gptel-agent--job-eof job))
        (should-not (plist-get (gptel-fsm-info fsm) :post))
        (should-not (buffer-live-p (gptel-agent--job-buffer job)))
        (accept-process-output nil 0.03)
        (should (= (length results) 1))))))

(ert-deftest gptel-agent-process-eof-never-flushes-a-real-interrupted-write ()
  (skip-unless (executable-find "python3"))
  (let* ((directory (make-temp-file "gptel-eof-disabled-" t))
         (gate (expand-file-name "read-now" directory))
         (script (concat
                  "import array,fcntl,os,select,sys,termios,time\n"
                  "print('ready',flush=True)\n"
                  "while not os.path.exists(sys.argv[1]): time.sleep(.01)\n"
                  "queued=array.array('i',[0])\n"
                  "fcntl.ioctl(0,termios.FIONREAD,queued,True)\n"
                  "print('queued:'+str(queued[0]),flush=True)\n"
                  "total=0; end=time.monotonic()+.2\n"
                  "while time.monotonic()<end:\n"
                  " if select.select([0],[],[],max(0,end-time.monotonic()))[0]:\n"
                  "  part=os.read(0,2097152)\n"
                  "  if not part: print('unexpected-eof',flush=True); break\n"
                  "  total+=len(part)\n"
                  "print('received:'+str(total),flush=True)\n"
                  "time.sleep(60)\n")))
    (unwind-protect
        (gptel-agent-process-eof-test--with-job
            (mapconcat #'shell-quote-argument
                       (list (executable-find "python3") "-c" script gate) " ")
          (let (sent closed rejected
                (safety (run-at-time 3 nil
                                     (lambda () (when (process-live-p process)
                                                  (delete-process process))))))
            (unwind-protect
                (progn
                  (gptel-agent-process-eof-test--wait
                   (lambda () (with-current-buffer (process-buffer process)
                                (string-match-p "ready" (buffer-string)))))
                  (cl-letf (((symbol-function 'process-send-eof)
                             (lambda (&rest _) (ert-fail "EOF flushed an uncertain suffix"))))
                    (gptel-agent--process-tool (lambda (result) (setq sent result))
                                               "send" id (make-string 1048576 ?x)
                                               nil nil nil nil 1000)
                    (gptel-agent--process-tool (lambda (result) (setq closed result)) "eof" id)
                    (gptel-agent-process-eof-test--wait (lambda () (and sent closed)))
                    (should (string-match-p "^send_reason: timeout$" sent))
                    (should (string-match-p "^eof_reason: input_failed$" closed))
                    (should (gptel-agent--job-input-disabled job))
                    (gptel-agent--process-tool (lambda (result) (setq rejected result)) "eof" id)
                    (should (string-prefix-p "Error: " rejected))
                    (should (string-match-p "[Dd]isabled\\|[Ii]nterrupted\\|unusable" rejected))
                    (with-temp-file gate)
                    (gptel-agent-process-eof-test--wait
                     (lambda () (with-current-buffer (process-buffer process)
                                  (string-match-p "received:" (buffer-string)))))
                    (with-current-buffer (process-buffer process)
                      (let ((output (buffer-string)) queued)
                        (should (string-match "queued:\\([0-9]+\\)" output))
                        (setq queued (string-to-number (match-string 1 output)))
                        (should (string-match "received:\\([0-9]+\\)" output))
                        (should (= queued (string-to-number (match-string 1 output))))
                        (should-not (string-match-p "unexpected-eof" output))))
                    (should (process-live-p process))
                    (should-not (gptel-agent--job-input-closed job))))
              (cancel-timer safety))))
      (delete-directory directory t))))

(ert-deftest gptel-agent-process-eof-validation-precedes-queueing ()
  (gptel-agent-process-eof-test--with-job "cat"
    (dolist (args '(("unexpected") ("") (nil 0) (nil nil 1)
                    (nil nil nil "tail") (nil nil nil nil "exit")
                    (nil nil nil nil nil -1) (nil nil nil nil nil 60001)
                    (nil nil nil nil nil 0.5) (nil nil nil nil nil "10")))
      (let (results)
        (apply #'gptel-agent--process-tool (lambda (result) (push result results))
               "eof" id args)
        (should (= (length results) 1))
        (should (string-prefix-p "Error: " (car results)))
        (should-not (gptel-agent--job-eof job))
        (should-not (gptel-agent--job-input-closed job))))))

(ert-deftest gptel-agent-process-eof-confirmation-and-owner-scoping ()
  (let* ((tool (gptel-get-tool "Process"))
         (actions (plist-get (car (gptel-tool-args tool)) :enum)))
    (should (member "eof" (append actions nil)))
    (should (funcall (gptel-tool-confirm tool) "eof")))
  (gptel-agent-process-eof-test--with-job "cat"
    (gptel-agent-process-eof-test--with-session
      (let (result)
        (gptel-agent--process-tool (lambda (value) (setq result value)) "eof" id)
        (should (string-prefix-p "Error: Unknown process_id" result))
        (should-error (gptel-agent--manage-process "eof" id))))
    (should-not (gptel-agent--job-eof job))
    (should-not (gptel-agent--job-input-closed job))
    (should (process-live-p process))))

(ert-deftest gptel-agent-process-eof-rejects-unsupported-transports ()
  (gptel-agent-process-eof-test--with-job "cat"
    (let (result)
      (let ((system-type 'windows-nt))
        (gptel-agent--process-tool (lambda (value) (setq result value)) "eof" id)
        (should (string-prefix-p "Error: " result)))
      (let ((directory (gptel-agent--job-directory job)))
        (unwind-protect
            (progn
              (setf (gptel-agent--job-directory job) "/ssh:example.invalid:/tmp/")
              (gptel-agent--process-tool (lambda (value) (setq result value)) "eof" id)
              (should (string-prefix-p "Error: " result)))
          (setf (gptel-agent--job-directory job) directory)))
      (should-not (gptel-agent--job-eof job))
      (should-not (gptel-agent--job-input-closed job)))))

(ert-deftest gptel-agent-process-eof-real-dispatch-orders-send-eof-and-wait ()
  (dolist (localized '(nil t))
    (gptel-agent-process-eof-test--with-job "wc -c"
      (let* ((gptel-confirm-tool-calls nil)
             (gptel-agent-use-project-instructions nil)
             (backend (gptel-make-openai-responses
                       "process-eof-dispatch" :key "unused" :host "example.invalid"
                       :models '(test)))
             (send-call (list :name "Process" :args
                              (list :action "send" :process_id id :input "abc\n")))
             (eof-call (list :name "Process" :args
                             (list :action "eof" :process_id id :timeout_ms 5000)))
             (wait-call (list :name "Process" :args
                              (list :action "wait" :process_id id :until "exit")))
             (fsm (gptel-make-fsm
                   :table gptel-send--transitions
                   :handlers '((TOOL gptel--handle-tool-use))
                   :info (list :buffer (current-buffer) :backend backend
                               :callback #'ignore
                               :tool-use (list send-call eof-call wait-call)))))
        (setq-local gptel-tools (list (gptel-get-tool "Process")))
        (when localized (gptel-agent--localize-agent-tool fsm))
        (setf (plist-get (gptel-fsm-info fsm) :tools) gptel-tools)
        (gptel--fsm-transition fsm 'TOOL)
        (gptel-agent-process-eof-test--wait (lambda () (eq (gptel-fsm-state fsm) 'TRET)))
        (should-not (plist-get (gptel-fsm-info fsm) :error))
        (should (= (length (plist-get (gptel-fsm-info fsm) :tool-result)) 3))
        (should (string-match-p "^send_reason: sent$" (plist-get send-call :result)))
        (should (string-match-p "^eof_reason: closed$" (plist-get eof-call :result)))
        (should (string-match-p "^wait_reason: exit$" (plist-get wait-call :result)))
        (should (string-match-p "STDOUT\\+STDERR:\n[[:space:]]*4\n\\'"
                                (plist-get wait-call :result)))
        (should-not (plist-get (gptel-fsm-info fsm) :post))))))

(ert-deftest gptel-agent-process-eof-localized-call-uses-requesting-child-context ()
  (gptel-agent-process-eof-test--with-job "cat"
    (let ((parent (current-buffer))
          (child (generate-new-buffer " *process-eof-requester*"))
          (ui (generate-new-buffer " *process-eof-approval-ui*"))
          callback-buffer result)
      (unwind-protect
          (let (tool fsm)
            (with-current-buffer child
              (setq-local gptel-agent--supervised-run
                          (gptel-agent--make-run :parent-buffer parent))
              (setq fsm (gptel-make-fsm :state 'TOOL :info (list :buffer child)))
              (setq-local gptel-tools (list (gptel-get-tool "Process")))
              (gptel-agent--localize-agent-tool fsm)
              (setq tool (car gptel-tools)))
            (with-current-buffer ui
              (apply (gptel-tool-function tool)
                     (lambda (value) (setq result value callback-buffer (current-buffer)))
                     (gptel--map-tool-args tool (list :action "eof" :process_id id))))
            (kill-buffer ui)
            (gptel-agent-process-eof-test--wait (lambda () result))
            (should (eq callback-buffer child))
            (should (string-match-p "^eof_reason: closed$" result))
            (should-not (plist-get (gptel-fsm-info fsm) :post)))
        (dolist (buffer (list child ui))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(provide 'gptel-agent-process-eof-test)
;;; gptel-agent-process-eof-test.el ends here
