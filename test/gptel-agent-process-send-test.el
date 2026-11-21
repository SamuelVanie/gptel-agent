;;; gptel-agent-process-send-test.el --- Bounded process input tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)
(require 'gptel-openai-responses)

(defun gptel-agent-process-send-test--wait (predicate)
  "Service timers and processes for at most five seconds until PREDICATE."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.005))
    (should (funcall predicate))))

(defmacro gptel-agent-process-send-test--with-session (&rest body)
  "Run BODY in a disposable conversation buffer."
  (declare (indent 0))
  `(let ((session (generate-new-buffer " *process-send-test*")))
     (unwind-protect
         (with-current-buffer session ,@body)
       (when (buffer-live-p session) (kill-buffer session)))))

(defmacro gptel-agent-process-send-test--with-job (command &rest body)
  "Run BODY with PROCESS, JOB and ID for COMMAND in a disposable session."
  (declare (indent 1))
  `(gptel-agent-process-send-test--with-session
     (let* ((process (gptel-agent--start-background-process ,command))
            (job (process-get process 'gptel-agent-job))
            (id (gptel-agent--job-id job)))
       ,@body)))

(defun gptel-agent-process-send-test--field (result name)
  "Return the numeric NAME field in RESULT, failing if it is absent."
  (should (string-match (concat "^" (regexp-quote name) ": \\([0-9]+\\)$") result))
  (string-to-number (match-string 1 result)))

(defun gptel-agent-process-send-test--bytes (file)
  "Return FILE's contents without decoding."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun gptel-agent-process-send-test--cancel (fsm)
  "Cancel FSM using its real request cleanup callbacks."
  (setf (gptel-fsm-state fsm) 'ABRT)
  (mapc (lambda (fn) (funcall fn (gptel-fsm-info fsm)))
        (copy-sequence (plist-get (gptel-fsm-info fsm) :post))))

(ert-deftest gptel-agent-process-send-preserves-unicode-order-and-exact-input ()
  (let* ((directory (make-temp-file "gptel-send-exact-" t))
         (file (expand-file-name "input" directory))
         ;; UTF-8's multibyte sequence straddles the 4096-byte pump boundary.
         (first (concat (make-string 4095 ?x) "é\0"))
         (second "β\nTAIL")
         (expected (encode-coding-string (concat first second) 'utf-8-unix)))
    (unwind-protect
        (gptel-agent-process-send-test--with-job
            (format "head -c %d > %s; printf done; sleep 60"
                    (length expected) (shell-quote-argument file))
          (let ((source (current-buffer)) results callback-buffers)
            (dolist (input (list first second))
              (let ((previous (length results)))
                (should-not
                 (gptel-agent--process-tool
                  (lambda (result)
                    (push result results)
                    (push (current-buffer) callback-buffers))
                  "send" id input))
                (gptel-agent-process-send-test--wait
                 (lambda () (> (length results) previous)))
                (should (string-match-p "^send_reason: sent$" (car results)))
                (let ((size (length (encode-coding-string input 'utf-8-unix))))
                  (dolist (field '("input_bytes" "written_bytes_min" "written_bytes_max"))
                    (should (= (gptel-agent-process-send-test--field (car results) field)
                               size))))))
            (gptel-agent-process-send-test--wait
             (lambda () (with-current-buffer (process-buffer process)
                          (string-match-p "done" (buffer-string)))))
            (should (equal (gptel-agent-process-send-test--bytes file) expected))
            (should (= (length results) 2))
            (should (equal callback-buffers (list source source)))
            (should-not (gptel-agent--job-send job))
            (should-not (gptel-agent--job-input-disabled job))))
      (delete-directory directory t))))

(defun gptel-agent-process-send-test--large-reader (delay)
  "Verify exact delivery of one MiB to a reader starting after DELAY seconds."
  (let* ((directory (make-temp-file "gptel-send-large-" t))
         (file (expand-file-name "input" directory))
         (block (apply #'unibyte-string (number-sequence 0 255)))
         (input (apply #'concat (make-list 4096 block))))
    (unwind-protect
        (gptel-agent-process-send-test--with-job
            (format "sleep %s; head -c %d > %s; printf done; sleep 60"
                    delay (length input) (shell-quote-argument file))
          (let ((start (float-time)) results)
            (should-not (gptel-agent--process-tool
                         (lambda (result) (push result results)) "send" id input
                         nil nil nil nil 4000))
            (gptel-agent-process-send-test--wait (lambda () results))
            (should (string-match-p "^send_reason: sent$" (car results)))
            (when (> delay 0)
              (should (> (- (float-time) start) delay)))
            (should (= (gptel-agent-process-send-test--field
                        (car results) "written_bytes_min") 1048576))
            (should (= (gptel-agent-process-send-test--field
                        (car results) "written_bytes_max") 1048576))
            (gptel-agent-process-send-test--wait
             (lambda () (with-current-buffer (process-buffer process)
                          (string-match-p "done" (buffer-string)))))
            (should (equal (gptel-agent-process-send-test--bytes file) input))
            (should (= (length results) 1))
            (should-not (gptel-agent--job-send job))))
      (delete-directory directory t))))

(ert-deftest gptel-agent-process-send-large-reading-consumer-gets-exact-bytes ()
  (gptel-agent-process-send-test--large-reader 0))

(ert-deftest gptel-agent-process-send-slow-reader-succeeds-before-overall-deadline ()
  ;; Transient backpressure can last much longer than a scheduling delay;
  ;; this consumer still starts before the call's overall 4000ms deadline.
  (gptel-agent-process-send-test--large-reader 0.4))

(ert-deftest gptel-agent-process-send-nonreader-has-real-write-bound ()
  ;; The safety kill makes an accidentally synchronous/unbounded regression
  ;; fail in seconds instead of hanging ERT.  The child never consumes stdin.
  (gptel-agent-process-send-test--with-job "sleep 60"
    (let* ((start (float-time)) results safety-fired
           (safety (run-at-time 2 nil
                                (lambda ()
                                  (setq safety-fired t)
                                  (when (process-live-p process) (delete-process process))))))
      (unwind-protect
          (progn
            (should-not
             (gptel-agent--process-tool
              (lambda (result) (push result results)) "send" id (make-string 1048576 ?x)
              nil nil nil nil 200))
            (should (< (- (float-time) start) 0.2))
            (should-not results)
            (gptel-agent-process-send-test--wait (lambda () results))
            (should (< (- (float-time) start) 1.0))
            (should-not safety-fired)
            (should (process-live-p process))
            (should (string-match-p "^send_reason: timeout$" (car results)))
            (should (string-match-p "^input_available: no$" (car results)))
            (let ((lower (gptel-agent-process-send-test--field (car results) "written_bytes_min"))
                  (upper (gptel-agent-process-send-test--field (car results) "written_bytes_max")))
              (should (<= 0 lower upper 1048576))
              (should (< upper 1048576))
              (should (<= (- upper lower) 4096)))
            (should (gptel-agent--job-input-disabled job))
            (should-not (gptel-agent--job-send job))
            (should (string-match-p "input_status: disabled"
                                    (gptel-agent--manage-process "list")))
            (let (retry)
              (gptel-agent--process-tool (lambda (result) (setq retry result))
                                         "send" id "must not flush retained bytes")
              (should (string-prefix-p "Error: " retry)))
            (accept-process-output nil 0.15)
            (should (= (length results) 1)))
        (cancel-timer safety)))))

(ert-deftest gptel-agent-process-send-dispatch-does-not-hold-later-bash-tool ()
  (gptel-agent-process-send-test--with-job "sleep 60"
    (let* ((gptel-confirm-tool-calls nil)
           (gptel-agent-use-project-instructions nil)
           (backend (gptel-make-openai-responses
                     "process-send-progress" :key "unused" :host "example.invalid"
                     :models '(test)))
           (send-call (list :name "Process" :args
                            (list :action "send" :process_id id
                                  :input (make-string 1048576 ?x) :timeout_ms 200)))
           (bash-call (list :name "Bash" :args
                            (list :command "printf parallel" :query "Read result")))
           (fsm (gptel-make-fsm
                 :table gptel-send--transitions
                 :handlers '((TOOL gptel--handle-tool-use))
                 :info (list :buffer (current-buffer) :backend backend
                             :callback #'ignore :tool-use (list send-call bash-call))))
           (safety (run-at-time 2 nil (lambda ()
                                       (when (process-live-p process) (delete-process process))))))
      (unwind-protect
          (progn
            (setq-local gptel-tools (list (gptel-get-tool "Process") (gptel-get-tool "Bash")))
            (gptel-agent--localize-agent-tool fsm)
            (setf (plist-get (gptel-fsm-info fsm) :tools) gptel-tools)
            (gptel--fsm-transition fsm 'TOOL)
            (gptel-agent-process-send-test--wait (lambda () (plist-get bash-call :result)))
            (should (equal (plist-get bash-call :result) "parallel"))
            (should-not (plist-get send-call :result))
            (gptel-agent-process-send-test--wait (lambda () (plist-get send-call :result)))
            (should (string-match-p "send_reason: timeout" (plist-get send-call :result)))
            (should (eq (gptel-fsm-state fsm) 'TRET))
            (should (= (length (plist-get (gptel-fsm-info fsm) :tool-result)) 2)))
        (cancel-timer safety)))))

(ert-deftest gptel-agent-process-send-limit-and-busy-reject-before-writing ()
  (gptel-agent-process-send-test--with-job "sleep 60"
    (let ((gptel-agent-process-input-limit 4) result)
      ;; Check encoded bytes, rather than accepting three multibyte characters.
      (gptel-agent--process-tool (lambda (value) (setq result value)) "send" id "ééé")
      (should (string-prefix-p "Error: " result))
      (should-not (gptel-agent--job-send job)))
    (let ((fsm (gptel-make-fsm :state 'TOOL :info (list :buffer (current-buffer)))))
      (gptel-agent--process-tool #'ignore "send" id (make-string 1048576 ?x)
                                 nil nil nil nil nil fsm)
      (should (gptel-agent--job-send job))
      (let ((pending (gptel-agent--job-send job)) result)
        (gptel-agent--process-tool (lambda (value) (setq result value)) "send" id "second")
        (should (string-prefix-p "Error: " result))
        (should (eq (gptel-agent--job-send job) pending)))
      (gptel-agent-process-send-test--cancel fsm)
      (should-not (gptel-agent--job-send job))
      (should-not (gptel-agent--job-input-disabled job)))))

(ert-deftest gptel-agent-process-send-cancel-queued-cleans-without-poisoning-input ()
  (gptel-agent-process-send-test--with-job "sleep 60"
    (let* ((fsm (gptel-make-fsm :state 'TOOL :info (list :buffer (current-buffer)))) results)
      (gptel-agent--process-tool (lambda (result) (push result results)) "send" id "cancelled"
                                 nil nil nil nil nil fsm)
      (should (plist-get (gptel-fsm-info fsm) :post))
      (gptel-agent-process-send-test--cancel fsm)
      (should-not (plist-get (gptel-fsm-info fsm) :post))
      (should-not (gptel-agent--job-send job))
      (should-not (gptel-agent--job-input-disabled job))
      (accept-process-output nil 0.05)
      (should-not results)
      (gptel-agent--process-tool (lambda (result) (push result results)) "send" id "replacement")
      (gptel-agent-process-send-test--wait (lambda () results))
      (should (= (length results) 1))
      (should (string-match-p "send_reason: sent" (car results)))
      (should (process-live-p process)))))

(ert-deftest gptel-agent-process-send-cancel-blocked-write-stops-and-silences-callback ()
  (gptel-agent-process-send-test--with-job "sleep 60"
    (let* ((fsm (gptel-make-fsm :state 'TOOL :info (list :buffer (current-buffer))))
           (send (symbol-function 'process-send-string))
           results cancelled
           (safety (run-at-time 2 nil (lambda ()
                                       (when (process-live-p process) (delete-process process))))))
      (unwind-protect
          ;; An actual blocked OS write triggers cancellation.  Writes that
          ;; return promptly cancel this probe, so queued-only cancellation
          ;; cannot accidentally make this test pass.
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (target input)
                       (let ((probe (run-at-time
                                     0.02 nil
                                     (lambda ()
                                       (setq cancelled t)
                                       (gptel-agent-process-send-test--cancel fsm)))))
                         (unwind-protect (funcall send target input)
                           (cancel-timer probe))))))
            (gptel-agent--process-tool (lambda (result) (push result results))
                                       "send" id (make-string 1048576 ?x)
                                       nil nil nil nil nil fsm)
            (gptel-agent-process-send-test--wait
             (lambda () (and cancelled (null (gptel-agent--job-send job)))))
            (should-not results)
            (should (process-live-p process))
            (should (gptel-agent--job-input-disabled job))
            (should-not (plist-get (gptel-fsm-info fsm) :post))
            (accept-process-output nil 0.15)
            (should-not results))
        (cancel-timer safety)))))

(ert-deftest gptel-agent-process-send-zero-deadline-writes-nothing ()
  (gptel-agent-process-send-test--with-job "sleep 60"
    (let (results)
      (gptel-agent--process-tool (lambda (result) (push result results))
                                 "send" id "late" nil nil nil nil 0)
      (gptel-agent-process-send-test--wait (lambda () results))
      (should (= (length results) 1))
      (should (string-match-p "^send_reason: timeout$" (car results)))
      (should (= (gptel-agent-process-send-test--field (car results) "written_bytes_max") 0))
      (should-not (gptel-agent--job-input-disabled job))
      (should-not (gptel-agent--job-send job)))))

(ert-deftest gptel-agent-process-send-abandoned-suffix-never-flushes-later ()
  (skip-unless (executable-find "python3"))
  (dolist (cancel '(nil t))
    (let* ((directory (make-temp-file "gptel-send-drain-" t))
           (gate (expand-file-name "read-now" directory))
           (file (expand-file-name "received" directory))
           (script
            (concat
             "import array,fcntl,os,select,signal,sys,termios,time\n"
             "signal.signal(signal.SIGINT, signal.SIG_IGN)\n"
             "print('ready',flush=True)\n"
             "while not os.path.exists(sys.argv[1]): time.sleep(.01)\n"
             "queued=array.array('i',[0])\n"
             "fcntl.ioctl(0,termios.FIONREAD,queued,True)\n"
             "print('queued: '+str(queued[0]),flush=True)\n"
             "os.set_blocking(0,False)\n"
             "def drain():\n"
             " data=bytearray(); end=time.monotonic()+.25\n"
             " while time.monotonic()<end:\n"
             "  if select.select([0],[],[],max(0,end-time.monotonic()))[0]:\n"
             "   part=os.read(0,2097152)\n"
             "   if not part: break\n"
             "   data.extend(part)\n"
             " return data\n"
             "first=drain(); print('first:'+str(len(first)),flush=True)\n"
             "second=drain()\n"
             "with open(sys.argv[2],'wb') as f: f.write(first+second)\n"
             "print('second:'+str(len(second)),flush=True)\n"
             "time.sleep(60)\n")))
      (unwind-protect
          (gptel-agent-process-send-test--with-job
              (mapconcat #'shell-quote-argument
                         (list (executable-find "python3") "-c" script gate file) " ")
            (let* ((fsm (gptel-make-fsm :state 'TOOL :info (list :buffer (current-buffer))))
                   (send (symbol-function 'process-send-string))
                   results
                   (safety (run-at-time 5 nil (lambda ()
                                               (when (process-live-p process)
                                                 (delete-process process))))))
              (unwind-protect
                  (progn
                    (gptel-agent-process-send-test--wait
                     (lambda () (with-current-buffer (process-buffer process)
                                  (string-match-p "ready" (buffer-string)))))
                    (cl-letf (((symbol-function 'process-send-string)
                               (lambda (target input)
                                 (let ((probe (when cancel
                                                (run-at-time
                                                 0.02 nil
                                                 (lambda ()
                                                   (gptel-agent-process-send-test--cancel fsm))))))
                                   (unwind-protect (funcall send target input)
                                     (when probe (cancel-timer probe)))))))
                      (gptel-agent--process-tool (lambda (result) (push result results))
                                                 "send" id (make-string 1048576 ?x)
                                                 nil nil nil nil (if cancel 1000 200) fsm)
                      (gptel-agent-process-send-test--wait
                       (lambda () (null (gptel-agent--job-send job)))))
                    (if cancel (should-not results) (should (= (length results) 1)))
                    (should (gptel-agent--job-input-disabled job))
                    (let* ((summary (gptel-agent--manage-process "list"))
                           (lower (gptel-agent-process-send-test--field summary "written_bytes_min"))
                           (upper (gptel-agent-process-send-test--field summary "written_bytes_max"))
                           retry)
                      (should (< upper 1048576))
                      (gptel-agent--process-tool (lambda (result) (setq retry result))
                                                 "send" id "must-not-appear")
                      (should (string-prefix-p "Error: " retry))
                      ;; Signal-only controls must not flush Emacs's retained
                      ;; C write queue.  The consumer deliberately ignores INT.
                      (gptel-agent--manage-process "interrupt" id)
                      (with-temp-file gate)
                      (gptel-agent-process-send-test--wait
                       (lambda () (with-current-buffer (process-buffer process)
                                    (string-match-p "second:" (buffer-string)))))
                      (let ((received (gptel-agent-process-send-test--bytes file))
                            (queued (with-current-buffer (process-buffer process)
                                      (gptel-agent-process-send-test--field
                                       (buffer-string) "queued"))))
                        (should (<= lower (length received) upper))
                        ;; Count the kernel pipe's bytes before its first read:
                        ;; even the uncertain chunk must not flush afterward.
                        (should (= (length received) queued))
                        (should (equal received (make-string (length received) ?x))))
                      (should (with-current-buffer (process-buffer process)
                                (string-match-p "^second:0$" (buffer-string))))
                      (gptel-agent--manage-process "kill" id)
                      (should-not (gptel-agent--job-send job))
                      (if cancel (should-not results) (should (= (length results) 1)))))
                (cancel-timer safety))))
        (delete-directory directory t)))))

(ert-deftest gptel-agent-process-send-exit-resolves-once-and-cleans ()
  (gptel-agent-process-send-test--with-job "sleep 60"
    (let (results)
      (gptel-agent--process-tool (lambda (result) (push result results)) "send" id "queued")
      (delete-process process)
      (gptel-agent-process-send-test--wait (lambda () results))
      (should (= (length results) 1))
      (should (string-match-p "^send_reason: exit$" (car results)))
      (should (= (gptel-agent-process-send-test--field (car results) "written_bytes_max") 0))
      (should-not (gptel-agent--job-send job))
      (gptel-agent--process-sentinel job process "killed\n")
      (accept-process-output nil 0.05)
      (should (= (length results) 1)))))

(ert-deftest gptel-agent-process-send-exit-during-blocked-write-resolves-once ()
  (gptel-agent-process-send-test--with-job "sleep 60"
    (let* ((send (symbol-function 'process-send-string)) results exited
           (safety (run-at-time 2 nil (lambda ()
                                       (when (process-live-p process) (delete-process process))))))
      (unwind-protect
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (target input)
                       (let ((probe (run-at-time 0.02 nil
                                                 (lambda ()
                                                   (setq exited t)
                                                   (delete-process target)))))
                         (unwind-protect (funcall send target input)
                           (cancel-timer probe))))))
            (gptel-agent--process-tool (lambda (result) (push result results))
                                       "send" id (make-string 1048576 ?x))
            (gptel-agent-process-send-test--wait (lambda () results))
            (should exited)
            (should-not (process-live-p process))
            (should (string-match-p "^send_reason: exit$" (car results)))
            (should-not (gptel-agent--job-send job))
            (gptel-agent--process-sentinel job process "killed\n")
            (accept-process-output nil 0.05)
            (should (= (length results) 1)))
        (cancel-timer safety)))))

(ert-deftest gptel-agent-process-send-two-blocked-jobs-both-finish-once ()
  (gptel-agent-process-send-test--with-job "sleep 60"
    (let* ((other (gptel-agent--start-background-process "sleep 60"))
           (other-job (process-get other 'gptel-agent-job))
           results
           (safety (run-at-time 2 nil
                                (lambda ()
                                  (dolist (target (list process other))
                                    (when (process-live-p target) (delete-process target)))))))
      (unwind-protect
          (progn
            (dolist (target (list (cons job 200) (cons other-job 400)))
              (gptel-agent--process-tool
               (lambda (result) (push result results))
               "send" (gptel-agent--job-id (car target)) (make-string 1048576 ?x)
               nil nil nil nil (cdr target)))
            (gptel-agent-process-send-test--wait (lambda () (= (length results) 2)))
            (dolist (result results)
              (should (string-match-p "^send_reason: timeout$" result)))
            (dolist (target-job (list job other-job))
              (should-not (gptel-agent--job-send target-job))
              (should (gptel-agent--job-input-disabled target-job))
              (should (process-live-p (gptel-agent--job-process target-job))))
            (accept-process-output nil 0.05)
            (should (= (length results) 2)))
        (cancel-timer safety)))))

(ert-deftest gptel-agent-process-send-source-kill-cancels-child-input-only ()
  (gptel-agent-process-send-test--with-job "sleep 60"
    (let ((parent (current-buffer))
          (child (generate-new-buffer " *process-send-child*")) results)
      (unwind-protect
          (progn
            (with-current-buffer child
              (setq-local gptel-agent--supervised-run
                          (gptel-agent--make-run :parent-buffer parent))
              (gptel-agent--process-tool (lambda (result) (push result results))
                                         "send" id "queued"))
            (kill-buffer child)
            (should-not (gptel-agent--job-send job))
            (should-not (gptel-agent--job-input-disabled job))
            (should (process-live-p process))
            (accept-process-output nil 0.05)
            (should-not results))
        (when (buffer-live-p child) (kill-buffer child))))))

(ert-deftest gptel-agent-process-send-owner-kill-cleans-pending-input ()
  (let (process job results)
    (gptel-agent-process-send-test--with-session
      (setq process (gptel-agent--start-background-process "sleep 60")
            job (process-get process 'gptel-agent-job))
      (gptel-agent--process-tool (lambda (result) (push result results))
                                 "send" (gptel-agent--job-id job) "queued"))
    (should-not (process-live-p process))
    (should-not (gptel-agent--job-send job))
    (accept-process-output nil 0.05)
    (should-not results)))

(ert-deftest gptel-agent-process-send-keeps-confirmation-and-session-ownership ()
  (let ((confirm (gptel-tool-confirm (gptel-get-tool "Process"))))
    (should (funcall confirm "send"))
    (should-not (funcall confirm "read")))
  (gptel-agent-process-send-test--with-job "sleep 60"
    (gptel-agent-process-send-test--with-session
      (let (results)
        (gptel-agent--process-tool (lambda (result) (push result results)) "send" id "foreign")
        (should (= (length results) 1))
        (should (string-prefix-p "Error: Unknown process_id" (car results)))))
    (should-not (gptel-agent--job-send job))))

(provide 'gptel-agent-process-send-test)
;;; gptel-agent-process-send-test.el ends here
