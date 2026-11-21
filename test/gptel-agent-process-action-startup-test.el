;;; gptel-agent-process-action-startup-test.el --- Action setup reentrancy -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)

(defun gptel-agent-process-action-startup-test--wait (predicate)
  "Service real timers and processes until PREDICATE, for up to five seconds."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.005))
    (should (funcall predicate))))

(defun gptel-agent-process-action-startup-test--event-during-validation
    (action event &optional handler-error)
  "Trigger EVENT while a filename handler validates ACTION's job.
EVENT is `cancel', `kill-source', or `remove-job'.
When HANDLER-ERROR is non-nil, signal an error after servicing EVENT."
  (let ((owner (generate-new-buffer " *action-startup-owner*"))
        (source (generate-new-buffer " *action-startup-source*"))
        (gptel-agent--process-registry nil)
        (gptel-agent--process-retention-timer nil)
        process job fsm event-timer event-ran results queued
        (writes 0) (closes 0))
    (unwind-protect
        (progn
          (with-current-buffer owner
            (setq process (gptel-agent--start-background-process "cat")
                  job (process-get process 'gptel-agent-job)))
          (with-current-buffer source
            (setq-local gptel-agent--supervised-run
                        (gptel-agent--make-run :parent-buffer owner))
            (setq fsm (gptel-make-fsm :state 'TOOL :info (list :buffer source)))
            (let ((remote-p (symbol-function 'file-remote-p))
                  (send (symbol-function 'process-send-string))
                  (eof (symbol-function 'process-send-eof))
                  (armed t))
              (cl-letf (((symbol-function 'file-remote-p)
                         (lambda (file &rest args)
                           (when (and armed (equal file (gptel-agent--job-directory job)))
                             (setq armed nil
                                   event-timer
                                   (run-at-time
                                    0.001 nil
                                    (lambda ()
                                      (pcase event
                                        ('kill-source (kill-buffer source))
                                        ('remove-job (gptel-agent--process-dispose job))
                                        ('cancel
                                         (setf (gptel-fsm-state fsm) 'ABRT)
                                         (dolist (cleanup (copy-sequence
                                                           (plist-get (gptel-fsm-info fsm) :post)))
                                           (funcall cleanup (gptel-fsm-info fsm)))))
                                      (setq event-ran t))))
                             ;; A file handler may wait for its transport and
                             ;; service request cancellation during validation.
                             (gptel-agent-process-action-startup-test--wait
                              (lambda () event-ran))
                             (when handler-error
                               (error "Filename handler failed after cancellation")))
                           (apply remote-p file args)))
                        ((symbol-function 'process-send-string)
                         (lambda (&rest args)
                           (cl-incf writes)
                           (apply send args)))
                        ((symbol-function 'process-send-eof)
                         (lambda (&rest args)
                           (cl-incf closes)
                           (apply eof args))))
                (gptel-agent--process-tool
                 (lambda (result) (push result results)) action (gptel-agent--job-id job)
                 (and (equal action "send") "must-not-be-sent\n")
                 nil nil nil nil nil fsm)
                (setq queued (or (gptel-agent--job-send job) (gptel-agent--job-eof job)))
                ;; Give an incorrectly registered pump the opportunity to run.
                (accept-process-output nil 0.05))))
          (should event-ran)
          (should-not queued)
          (should (= writes 0))
          (should (= closes 0))
          (should-not (plist-get (gptel-fsm-info fsm) :post))
          (should-not (gptel-agent--job-send job))
          (should-not (gptel-agent--job-eof job))
          (should-not (gptel-agent--job-input-closed job))
          (if (eq event 'remove-job)
              (progn
                (should (= (length results) 1))
                (should (string-match-p "^Error: Process is no longer running" (car results)))
                (should-not (process-live-p process))
                (should-not gptel-agent--process-registry)
                (should-not gptel-agent--process-retention-timer)
                (with-current-buffer owner (should-not gptel-agent--processes)))
            (should-not results)
            (should (process-live-p process))
            (should (equal gptel-agent--process-registry (list job)))
            (with-current-buffer owner
              (should (equal gptel-agent--processes (list job))))))
      (when event-timer (cancel-timer event-timer))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p owner) (kill-buffer owner))
      (when gptel-agent--process-retention-timer
        (cancel-timer gptel-agent--process-retention-timer)))))

(ert-deftest gptel-agent-process-send-cancelled-during-handler-validation ()
  (gptel-agent-process-action-startup-test--event-during-validation "send" 'cancel))

(ert-deftest gptel-agent-process-eof-cancelled-during-handler-validation ()
  (gptel-agent-process-action-startup-test--event-during-validation "eof" 'cancel))

(ert-deftest gptel-agent-process-send-source-killed-during-handler-validation ()
  (gptel-agent-process-action-startup-test--event-during-validation "send" 'kill-source))

(ert-deftest gptel-agent-process-eof-source-killed-during-handler-validation ()
  (gptel-agent-process-action-startup-test--event-during-validation "eof" 'kill-source))

(ert-deftest gptel-agent-process-send-job-removed-during-handler-validation ()
  (gptel-agent-process-action-startup-test--event-during-validation "send" 'remove-job))

(ert-deftest gptel-agent-process-eof-job-removed-during-handler-validation ()
  (gptel-agent-process-action-startup-test--event-during-validation "eof" 'remove-job))

(ert-deftest gptel-agent-process-send-cancelled-before-handler-error ()
  (gptel-agent-process-action-startup-test--event-during-validation "send" 'cancel t))

(ert-deftest gptel-agent-process-eof-cancelled-before-handler-error ()
  (gptel-agent-process-action-startup-test--event-during-validation "eof" 'cancel t))

(ert-deftest gptel-agent-process-send-source-killed-before-handler-error ()
  (gptel-agent-process-action-startup-test--event-during-validation "send" 'kill-source t))

(ert-deftest gptel-agent-process-eof-source-killed-before-handler-error ()
  (gptel-agent-process-action-startup-test--event-during-validation "eof" 'kill-source t))

(provide 'gptel-agent-process-action-startup-test)
;;; gptel-agent-process-action-startup-test.el ends here
