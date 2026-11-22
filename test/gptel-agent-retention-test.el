;;; gptel-agent-retention-test.el --- Finished worker ownership -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)
(require 'gptel-openai)

(cl-defmacro gptel-agent-retention-test--with-run (&body body)
  "Run BODY with an owned worker and retained conversation."
  (declare (indent 0))
  `(let* ((parent (generate-new-buffer " *retention-parent*"))
          (child (generate-new-buffer " *retention-child*"))
          (parent-fsm (gptel-make-fsm :state 'WAIT :info (list :buffer parent)))
          (session (gptel-agent--make-session :id "retained-worker" :resumable t))
          (run (gptel-agent--make-run
                :id "retained-worker" :state 'requesting :agent "researcher"
                :description "Retained conversation" :parent-buffer parent
                :parent-fsm parent-fsm :child-buffer child :session session))
          (gptel-agent--runs (make-hash-table :test #'equal))
          (gptel-agent--run-history nil))
     (setf (gptel-agent--session-run session) run)
     (unwind-protect
         (progn ,@body)
       (when-let* ((gate (gptel-agent--workers-for parent-fsm)))
         (gptel-agent--workers-close gate))
       (when (buffer-live-p child) (kill-buffer child))
       (when (buffer-live-p parent) (kill-buffer parent)))))

(ert-deftest gptel-agent-retention-blocking-delivers-before-releasing-parent ()
  (gptel-agent-retention-test--with-run
    (let (delivered)
      (setf (gptel-agent--run-callback run)
            (lambda (result)
              (should (eq (current-buffer) parent))
              (should (eq (gptel-agent--run-parent-fsm run) parent-fsm))
              (should (functionp (gptel-agent--run-callback run)))
              (setq delivered result)))
      (should (gptel-agent--run-finish run 'completed "Retained answer"))
      (should (string-match-p "Retained answer" delivered))
      (should (gptel-agent--run-delivered run))
      (should-not (gptel-agent--run-parent-fsm run))
      (should-not (gptel-agent--run-callback run))
      (should (eq (gptel-agent--run-parent-buffer run) parent))
      (should (buffer-live-p child))
      (should (eq (gptel-agent--session-last-result session) run))
      (should-not (gptel-agent--run-finish run 'failed "Late result")))))

(ert-deftest gptel-agent-retention-background-enqueues-before-releasing-parent ()
  (gptel-agent-retention-test--with-run
    (let ((gate (gptel-agent--workers-install parent-fsm)))
      (setf (gptel-agent--run-background run) t
            (gptel-agent--run-callback run)
            (lambda (_) (ert-fail "Background launch callback called twice")))
      (should (gptel-agent--run-finish run 'completed "Queued answer"))
      (should-not (gptel-agent--run-parent-fsm run))
      (should-not (gptel-agent--run-callback run))
      (should-not (gptel-agent--run-delivered run))
      ;; The inbox owns the outcome after enqueueing; it does not need a
      ;; back-reference to the request in order to deliver the report.
      (should (equal (gptel-agent--workers-events gate) (list run)))
      (should (eq (gptel-agent--workers-take gate) run))
      (should (string-match-p "Queued answer" (gptel-agent--worker-report run)))
      (should (gptel-agent--run-delivered run))
      (should-not (gptel-agent--workers-take gate)))))

(ert-deftest gptel-agent-retention-releases-parent-when-delivery-is-skipped-or-exits ()
  (dolist (delivery '(suppressed dead-parent error throw))
    (ert-info ((format "Delivery: %s" delivery))
      (gptel-agent-retention-test--with-run
        (let (called)
          (setf (gptel-agent--run-callback run)
                (lambda (_)
                  (setq called t)
                  (should (eq (gptel-agent--run-parent-fsm run) parent-fsm))
                  (pcase delivery
                    ('error (error "Expected callback failure"))
                    ('throw (throw 'callback-exit t))
                    (_ (ert-fail "Unexpected result delivery")))))
          (when (eq delivery 'dead-parent) (kill-buffer parent))
          (catch 'callback-exit
            (gptel-agent--run-finish
             run 'completed "Retained answer" nil (eq delivery 'suppressed)))
          (should (eq called (and (memq delivery '(error throw)) t)))
          (should-not (gptel-agent--run-parent-fsm run))
          (should-not (gptel-agent--run-callback run))
          (should (buffer-live-p child)))))))

(defun gptel-agent-retention-test--complete-worker (parent weak-parents)
  "Finish a real worker for PARENT and weakly track its owning request.
Only WEAK-PARENTS and the retained worker session escape this function."
  (let ((parent-fsm
         (gptel-make-fsm
          :info (list :buffer parent :position 1
                      :data (list :messages
                                  (vector (list :role "user" :content
                                                (make-string 1000000 ?p))))))))
    (puthash parent-fsm t weak-parents)
    (gptel-agent--task
     (lambda (_result) (setf (gptel-fsm-state parent-fsm) 'DONE))
     "researcher" "Retained task" "Worker context"
     parent-fsm '(("researcher" :system "Research" :task-timeout nil
                   :max-request-rounds nil)))
    (let* ((session (cdar (buffer-local-value 'gptel-agent--sessions parent)))
           (run (gptel-agent--session-run session))
           (child-fsm (gptel-agent--run-fsm run))
           (info (gptel-fsm-info child-fsm)))
      (funcall (plist-get info :callback) "Worker answer" info)
      (gptel--fsm-transition child-fsm 'DONE)
      (should (eq (gptel-agent--run-state run) 'completed)))
    ;; The parent has finished its own turn.  Keeping that request's active
    ;; gate would legitimately keep it alive regardless of worker cleanup.
    (gptel-agent--workers-close (gptel-agent--workers-for parent-fsm))))

(ert-deftest gptel-agent-retention-completed-conversations-do-not-retain-parent-requests ()
  (let ((parent (generate-new-buffer " *retention-many-parents*"))
        (weak-parents (make-hash-table :test #'eq :weakness 'key))
        (gptel-agent--runs (make-hash-table :test #'equal))
        (gptel-agent--run-history nil)
        (gptel-post-response-functions nil)
        (gptel-post-request-hook nil)
        (gptel-pre-response-hook nil)
        (gptel-log-level nil))
    (unwind-protect
        (cl-letf (((symbol-function 'gptel--update-status) #'ignore)
                  ((symbol-function 'gptel-curl-get-response) #'ignore))
          (with-current-buffer parent
            (insert "Parent chat\n")
            (setq-local gptel-backend
                        (gptel-make-openai "retention-test" :key "unused"
                                           :host "example.invalid" :models '(test))
                        gptel-model 'test gptel-use-curl t gptel-stream nil
                        gptel-tools nil gptel-agent-preset nil
                        gptel-prompt-transform-functions nil)
            (dotimes (_ 20)
              (gptel-agent-retention-test--complete-worker parent weak-parents))
            (garbage-collect)
            (should (= (hash-table-count weak-parents) 0))
            (should (= (length gptel-agent--sessions) 20))
            (dolist (entry gptel-agent--sessions)
              (let* ((session (cdr entry))
                     (run (gptel-agent--session-run session))
                     (conversation (prin1-to-string
                                    (gptel-agent--session-request-info session))))
                (should (gptel-agent--session-resumable session))
                (should (buffer-live-p (gptel-agent--run-child-buffer run)))
                (should (string-match-p "Worker context" conversation))
                (should (string-match-p "Worker answer" conversation))))))
      (when (buffer-live-p parent) (kill-buffer parent)))))

(provide 'gptel-agent-retention-test)
;;; gptel-agent-retention-test.el ends here
