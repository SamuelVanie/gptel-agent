;;; gptel-agent-process-startup-test.el --- Reentrant job launch -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)

(defun gptel-agent-process-startup-test--wait (predicate)
  "Service real process events until PREDICATE holds, for at most five seconds."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.005))
    (should (funcall predicate))))

(defmacro gptel-agent-process-startup-test--session (&rest body)
  "Run BODY with isolated SOURCE, registries and process cleanup."
  (declare (indent 0))
  `(let ((source (generate-new-buffer " *process-startup-test*"))
         (gptel-agent--process-registry nil)
         (gptel-agent--process-retention-timer nil)
         (original-make-process (symbol-function 'make-process))
         process job results)
     (unwind-protect
         (with-current-buffer source ,@body)
       (when (buffer-live-p source) (kill-buffer source))
       (dolist (remaining (copy-sequence gptel-agent--process-registry))
         (gptel-agent--process-dispose remaining))
       (when (process-live-p process) (delete-process process))
       (when gptel-agent--process-retention-timer
         (cancel-timer gptel-agent--process-retention-timer)))))

(defun gptel-agent-process-startup-test--empty (job source process)
  "Assert JOB and PROCESS no longer retain startup resources in SOURCE."
  (should-not (process-live-p process))
  (should-not (process-get process 'gptel-agent-job))
  (should-not (buffer-live-p (gptel-agent--job-buffer job)))
  (should-not gptel-agent--process-registry)
  (should-not gptel-agent--process-retention-timer)
  (should-not (gptel-agent--job-launching job))
  (should-not (gptel-agent--job-cleanup-pending job))
  (when (buffer-live-p source)
    (should-not (buffer-local-value 'gptel-agent--processes source))))

(ert-deftest gptel-agent-process-startup-completion-keeps-both-registries ()
  (gptel-agent-process-startup-test--session
    (let ((gptel-agent-process-job-limit 1)
          (gptel-agent-process-output-limit 5)
          (gptel-agent-process-total-output-limit 5))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq job (car gptel-agent--process-registry)
                         process (apply original-make-process args))
                   (gptel-agent-process-startup-test--wait
                    (lambda () (gptel-agent--job-completed-at job)))
                   ;; An early completed job must remain reserved even when
                   ;; another launch or a retention sweep tries to evict it.
                   (setf (gptel-agent--job-completed-at job) 1)
                   (gptel-agent--process-prune 1)
                   (should (eq process (gptel-agent--job-process job)))
                   (should (equal gptel-agent--process-registry (list job)))
                   (should (equal (buffer-local-value 'gptel-agent--processes source)
                                  (list job)))
                   (should-error (gptel-agent--start-background-process "true"))
                   process)))
        (gptel-agent--execute-bash
         (lambda (value) (push value results)) "printf 123456789" "Read" t))
      (should (= (length results) 1))
      (should (string-match-p "status: exit" (car results)))
      (should (eq (process-get process 'gptel-agent-job) job))
      (should-not (gptel-agent--job-launching job))
      (should (string-suffix-p "56789" (gptel-agent--process-read job nil 20)))
      (should (equal gptel-agent--process-registry (list job)))
      ;; Once launch has returned, ordinary expiry removes both registrations.
      (gptel-agent--process-prune)
      (gptel-agent-process-startup-test--empty job source process))))

(ert-deftest gptel-agent-process-startup-preserves-partial-output-decoder ()
  (gptel-agent-process-startup-test--session
    (let ((coding-system-for-read 'utf-8-unix))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq job (car gptel-agent--process-registry)
                         process (apply original-make-process args))
                   ;; Decode the first byte before returning the handle.  The
                   ;; filter sees nothing until the second byte follows later.
                   (should (accept-process-output process 1))
                   (should (= (gptel-agent--process-output-size job) 0))
                   process)))
        (gptel-agent--start-background-process
         "printf '\\303'; read -r answer; printf '\\251'"))
      (process-send-string process "\n")
      (gptel-agent-process-startup-test--wait
       (lambda () (gptel-agent--job-completed-at job)))
      (should (equal (with-current-buffer (gptel-agent--job-buffer job)
                       (buffer-string))
                     "é")))))

(ert-deftest gptel-agent-process-startup-owner-cancellation-stops-returned-process ()
  (dolist (phase '(before-create before-output after-output))
    (gptel-agent-process-startup-test--session
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq job (car gptel-agent--process-registry))
                   (when (eq phase 'before-create)
                     (kill-buffer source)
                     (should (memq job gptel-agent--process-registry))
                     (should (gptel-agent--job-cleanup-pending job)))
                   (setq process (apply original-make-process args))
                   (when (eq phase 'after-output)
                     (gptel-agent-process-startup-test--wait
                      (lambda () (> (gptel-agent--process-output-size job) 0)))
                     (should (eq (gptel-agent--job-process job) process)))
                   (unless (eq phase 'before-create) (kill-buffer source))
                   process)))
        (gptel-agent--execute-bash
         (lambda (value) (push value results)) "printf ready; read -r answer" "Read" t))
      (should (= (length results) 1))
      (should (string-match-p "launch was cancelled" (car results)))
      (gptel-agent-process-startup-test--empty job source process))))

(ert-deftest gptel-agent-process-startup-filter-error-stops-live-process-once ()
  (gptel-agent-process-startup-test--session
    (cl-letf (((symbol-function 'gptel-agent--process-output-room)
               (lambda (&rest _) (error "Simulated capture failure")))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq job (car gptel-agent--process-registry)
                       process (apply original-make-process args))
                 (gptel-agent-process-startup-test--wait
                  (lambda () (gptel-agent--job-launch-error job)))
                 (should-not (process-live-p process))
                 process)))
      (gptel-agent--execute-bash
       (lambda (value) (push value results)) "printf ready; read -r answer" "Read" t))
    (should (= (length results) 1))
    (should (string-match-p "Could not capture background Bash output.*Simulated capture failure"
                            (car results)))
    (gptel-agent--process-sentinel job process "late event")
    (gptel-agent--process-filter job process "late output")
    (gptel-agent-process-startup-test--empty job source process)
    (should (= (length results) 1))))

(ert-deftest gptel-agent-process-startup-error-after-creation-recovers-handle ()
  (dolist (deliver-output '(nil t))
    (gptel-agent-process-startup-test--session
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq job (car gptel-agent--process-registry)
                         process (apply original-make-process args))
                   (when deliver-output
                     (gptel-agent-process-startup-test--wait
                      (lambda () (> (gptel-agent--process-output-size job) 0))))
                   (error "Handler failed after creation"))))
        (gptel-agent--execute-bash
         (lambda (value) (push value results)) "printf ready; read -r answer" "Read" t))
      (should (equal results '("Error: Handler failed after creation")))
      (gptel-agent-process-startup-test--empty job source process))))

(ert-deftest gptel-agent-process-startup-quit-releases-reservation ()
  (gptel-agent-process-startup-test--session
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq job (car gptel-agent--process-registry)
                       process (apply original-make-process args))
                 (signal 'quit nil))))
      (should (eq (condition-case nil
                      (gptel-agent--start-background-process "read -r answer")
                    (quit 'cancelled))
                  'cancelled)))
    (gptel-agent-process-startup-test--empty job source process)))

(ert-deftest gptel-agent-process-startup-failed-cleanup-keeps-quota-until-retry ()
  (gptel-agent-process-startup-test--session
    (cl-letf (((symbol-function 'kill-process)
               (lambda (&rest _) (error "Termination unavailable")))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq job (car gptel-agent--process-registry)
                       process (apply original-make-process args))
                 (kill-buffer source)
                 process)))
      (gptel-agent--execute-bash
       (lambda (value) (push value results)) "read -r answer" "Read" t)
      (should (= (length results) 1))
      (should (process-live-p process))
      (should (gptel-agent--job-cleanup-pending job))
      (should (memq job gptel-agent--process-registry))
      (should (timerp gptel-agent--process-retention-timer))
      (should-not (gptel-agent--job-launching job)))
    (gptel-agent--process-prune)
    (gptel-agent-process-startup-test--empty job source process)))

(provide 'gptel-agent-process-startup-test)
;;; gptel-agent-process-startup-test.el ends here
