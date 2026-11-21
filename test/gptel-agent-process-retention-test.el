;;; gptel-agent-process-retention-test.el --- Job resource limits -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)

(defun gptel-agent-process-retention-test--wait (predicate)
  "Service subprocesses for at most five seconds until PREDICATE holds."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.005))
    (should (funcall predicate))))

(defmacro gptel-agent-process-retention-test--sessions (&rest body)
  "Run BODY with two isolated conversation buffers, A and B."
  (declare (indent 0))
  `(let ((a (generate-new-buffer " *job-retention-a*"))
         (b (generate-new-buffer " *job-retention-b*"))
         (gptel-agent--process-registry nil)
         (gptel-agent--process-retention-timer nil))
     (unwind-protect (progn ,@body)
       (when (buffer-live-p a) (kill-buffer a))
       (when (buffer-live-p b) (kill-buffer b))
       (when gptel-agent--process-retention-timer
         (cancel-timer gptel-agent--process-retention-timer)))))

(defun gptel-agent-process-retention-test--start (owner command)
  "Start COMMAND in OWNER and return its job."
  (with-current-buffer owner
    (process-get (gptel-agent--start-background-process command) 'gptel-agent-job)))

(defun gptel-agent-process-retention-test--size ()
  "Return aggregate retained characters in the isolated registry."
  (cl-loop for job in gptel-agent--process-registry
           sum (gptel-agent--process-output-size job)))

(ert-deftest gptel-agent-process-retention-global-job-quota-rejects-live-jobs ()
  (let ((gptel-agent-process-job-limit 2))
    (gptel-agent-process-retention-test--sessions
      (let ((one (gptel-agent-process-retention-test--start a "read -r answer"))
            (two (gptel-agent-process-retention-test--start b "read -r answer")))
        (dolist (owner (list a b))
          (let ((error (should-error
                        (gptel-agent-process-retention-test--start owner "true"))))
            (should (string-match-p "Background job quota reached"
                                    (error-message-string error)))))
        (should (= (length gptel-agent--process-registry) 2))
        (should (process-live-p (gptel-agent--job-process one)))
        (should (process-live-p (gptel-agent--job-process two)))
        (kill-buffer a)
        (should (= (length gptel-agent--process-registry) 1))
        (should-not (buffer-live-p (gptel-agent--job-buffer one)))
        (gptel-agent-process-retention-test--start b "read -r answer")
        (should (= (length gptel-agent--process-registry) 2))))))

(ert-deftest gptel-agent-process-retention-trims-before-inserting-across-owners ()
  (let ((gptel-agent-process-output-limit 10)
        (gptel-agent-process-total-output-limit 12))
    (gptel-agent-process-retention-test--sessions
      (let ((one (gptel-agent-process-retention-test--start a "read -r answer"))
            (two (gptel-agent-process-retention-test--start b "read -r answer"))
            (peak 0))
        (dolist (job (list one two))
          (with-current-buffer (gptel-agent--job-buffer job)
            (add-hook 'after-change-functions
                      (lambda (&rest _)
                        (setq peak (max peak (gptel-agent-process-retention-test--size))))
                      nil t)))
        (gptel-agent--process-filter one nil "ABCDEFGHIJ")
        (gptel-agent--process-filter two nil "klmnopqrst")
        (should (= (gptel-agent--job-base one) 8))
        (should (= (gptel-agent-process-retention-test--size) 12))
        (let ((read (gptel-agent--process-read one 0 20)))
          (should (string-match-p "characters 0..8 were discarded" read))
          (should (string-match-p "output_end: 10" read))
          (should (string-suffix-p "IJ" read)))
        (gptel-agent--process-filter one nil (make-string 10000 ?é))
        (should (= (gptel-agent-process-retention-test--size) 12))
        (should (<= peak 12))
        (should (= (gptel-agent--job-base one) 10000))
        (should (= (gptel-agent--job-base two) 8))
        (should (string-suffix-p (make-string 10 ?é)
                                 (gptel-agent--process-read one nil 20 "tail")))))))

(ert-deftest gptel-agent-process-retention-evicts-oldest-completed-at-admission ()
  (let ((gptel-agent-process-job-limit 2))
    (gptel-agent-process-retention-test--sessions
      (let ((old (gptel-agent-process-retention-test--start a "printf first")))
        (gptel-agent-process-retention-test--wait
         (lambda () (gptel-agent--job-completed-at old)))
        (let ((live (gptel-agent-process-retention-test--start b "read -r answer")))
          (gptel-agent-process-retention-test--start a "read -r answer")
          (should (= (length gptel-agent--process-registry) 2))
          (should-not (buffer-live-p (gptel-agent--job-buffer old)))
          (should (process-live-p (gptel-agent--job-process live)))
          (with-current-buffer a
            (let ((error (should-error
                          (gptel-agent--manage-process "read" (gptel-agent--job-id old)))))
              (should (string-match-p "expire or be evicted" (error-message-string error))))))))))

(ert-deftest gptel-agent-process-retention-completed-count-is-global ()
  (let ((gptel-agent-process-completed-limit 1))
    (gptel-agent-process-retention-test--sessions
      (let ((old (gptel-agent-process-retention-test--start a "printf first")))
        (gptel-agent-process-retention-test--wait
         (lambda () (gptel-agent--job-completed-at old)))
        (let ((new (gptel-agent-process-retention-test--start b "printf second")))
          (gptel-agent-process-retention-test--wait
           (lambda () (gptel-agent--job-completed-at new)))
          (should (equal gptel-agent--process-registry (list new)))
          (should-not (buffer-live-p (gptel-agent--job-buffer old)))
          (should (string-suffix-p "second" (gptel-agent--process-read new nil 20))))))))

(ert-deftest gptel-agent-process-retention-output-prefers-completed-before-running ()
  (let ((gptel-agent-process-output-limit 10)
        (gptel-agent-process-total-output-limit 20))
    (gptel-agent-process-retention-test--sessions
      (let ((live (gptel-agent-process-retention-test--start a "read -r answer")))
        (gptel-agent--process-filter live nil "1234567890")
        (let ((done (gptel-agent-process-retention-test--start b "printf ABCDEFGHIJ")))
          (gptel-agent-process-retention-test--wait
           (lambda () (gptel-agent--job-completed-at done)))
          (let ((new (gptel-agent-process-retention-test--start a "read -r answer")))
            (gptel-agent--process-filter new nil "klmnop")
            (should (= (gptel-agent-process-retention-test--size) 20))
            (should (= (gptel-agent--job-base done) 6))
            (should (= (gptel-agent--job-base live) 0))
            (should (string-suffix-p "GHIJ" (gptel-agent--process-read done nil 20)))))))))

(ert-deftest gptel-agent-process-retention-timer-expires-completed-keeps-running ()
  (let ((gptel-agent-process-retention-seconds 0.04))
    (gptel-agent-process-retention-test--sessions
      (let ((done (gptel-agent-process-retention-test--start a "printf done"))
            (live (gptel-agent-process-retention-test--start b "read -r answer")))
        (gptel-agent-process-retention-test--wait
         (lambda () (not (memq done gptel-agent--process-registry))))
        (should-not (buffer-live-p (gptel-agent--job-buffer done)))
        (should (process-live-p (gptel-agent--job-process live)))
        (kill-buffer b)
        (should-not gptel-agent--process-registry)
        (should-not gptel-agent--process-retention-timer)))))

(ert-deftest gptel-agent-process-retention-pending-actions-protect-completed-jobs ()
  (gptel-agent-process-retention-test--sessions
    (let ((job (gptel-agent-process-retention-test--start a "printf done")))
      (gptel-agent-process-retention-test--wait
       (lambda () (gptel-agent--job-completed-at job)))
      (setf (gptel-agent--job-completed-at job) 1)
      ;; The flags stand for queued continuations; disposal must not touch them.
      (dolist (slot '(waiters send eof))
        (unwind-protect
            (progn
              (pcase slot
                ('waiters (setf (gptel-agent--job-waiters job) '(pending)))
                ('send (setf (gptel-agent--job-send job) 'pending))
                ('eof (setf (gptel-agent--job-eof job) 'pending)))
              (gptel-agent--process-prune)
              (should (memq job gptel-agent--process-registry))
              (should (buffer-live-p (gptel-agent--job-buffer job))))
          (setf (gptel-agent--job-waiters job) nil
                (gptel-agent--job-send job) nil
                (gptel-agent--job-eof job) nil)))
      (gptel-agent--process-prune)
      (should-not (memq job gptel-agent--process-registry))
      (should-not gptel-agent--process-retention-timer))))

(ert-deftest gptel-agent-process-retention-launch-failure-releases-reservation ()
  (gptel-agent-process-retention-test--sessions
    (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (error "launch failed"))))
      (should-error (gptel-agent-process-retention-test--start a "true")))
    (should-not gptel-agent--process-registry)
    (should-not gptel-agent--process-retention-timer)
    (with-current-buffer a (should-not gptel-agent--processes))))

(ert-deftest gptel-agent-process-retention-failed-kill-retains-bounded-resources ()
  (let ((gptel-agent-process-job-limit 1)
        (gptel-agent-process-output-limit 5)
        (gptel-agent-process-total-output-limit 5))
    (gptel-agent-process-retention-test--sessions
      (let* ((job (gptel-agent-process-retention-test--start a "read -r answer"))
             (process (gptel-agent--job-process job))
             (filter (process-filter process)))
        (cl-letf (((symbol-function 'kill-process)
                   (lambda (&rest _) (error "Simulated termination failure"))))
          (should-error (gptel-agent--process-dispose job))
          (should (memq job gptel-agent--process-registry))
          (should (memq job (buffer-local-value 'gptel-agent--processes a)))
          (should (process-live-p process))
          (should (eq (process-filter process) filter))
          (funcall filter process "123456789")
          (should (= (gptel-agent-process-retention-test--size) 5))
          (should (= (gptel-agent--job-base job) 4))
          (should-error (gptel-agent-process-retention-test--start b "true"))
          ;; The owner can die while termination is unavailable; the global
          ;; registry must retain and retry its bounded resources afterward.
          (kill-buffer a)
          (should (memq job gptel-agent--process-registry))
          (should (buffer-live-p (gptel-agent--job-buffer job))))
        (gptel-agent--process-prune)
        (should-not (process-live-p process))
        (should-not (buffer-live-p (gptel-agent--job-buffer job)))
        (should-not gptel-agent--process-registry)
        (should-not gptel-agent--process-retention-timer)))))

(ert-deftest gptel-agent-process-retention-failed-delete-keeps-reservation-until-retry ()
  (gptel-agent-process-retention-test--sessions
    (let* ((job (gptel-agent-process-retention-test--start a "printf done"))
           (process (gptel-agent--job-process job)))
      (gptel-agent-process-retention-test--wait
       (lambda () (gptel-agent--job-completed-at job)))
      (cl-letf (((symbol-function 'delete-process)
                 (lambda (&rest _) (error "Simulated process deletion failure"))))
        (should-error (gptel-agent--process-dispose job))
        (should (memq job gptel-agent--process-registry))
        (should (gptel-agent--job-cleanup-pending job))
        (should (string-suffix-p "done" (gptel-agent--process-read job nil 20)))
        ;; Repeated cleanup failures must not interrupt pruning other jobs.
        (gptel-agent--process-prune)
        (should (memq job gptel-agent--process-registry)))
      (gptel-agent--process-prune)
      (should-not (process-live-p process))
      (should-not (buffer-live-p (gptel-agent--job-buffer job)))
      (should-not gptel-agent--process-registry))))

(provide 'gptel-agent-process-retention-test)
;;; gptel-agent-process-retention-test.el ends here
