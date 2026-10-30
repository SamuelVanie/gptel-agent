;;; gptel-agent-workers-test.el --- Background worker lifecycle -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)
(require 'gptel-openai)
(require 'gptel-openai-responses)
(require 'gptel-anthropic)
(require 'gptel-gemini)

(defvar gptel-agent-workers-test--backend nil)
(defvar gptel-agent-workers-test--stream nil)
(defvar gptel-agent-workers-test--mode #'fundamental-mode)

(cl-defmacro gptel-agent-workers-test--with-chat (&body body)
  "Run BODY through real request/tool handlers with only transport mocked."
  (declare (indent 0))
  `(let* ((parent (generate-new-buffer " *worker-parent*"))
          (fsm (gptel-make-fsm :table gptel-send--transitions
                               :handlers gptel-send--handlers))
          (gptel-agent--runs (make-hash-table :test #'equal))
          (gptel-agent--run-history nil)
          (gptel-post-response-functions nil)
          (gptel-post-request-hook nil)
          (gptel-pre-response-hook nil)
          (gptel-log-level nil)
          (gptel-prompt-prefix-alist '((fundamental-mode . "USER> ")))
          (snapshot '(("researcher" :system "Research" :task-timeout nil
                       :max-request-rounds nil)
                      ("executor" :system "Execute" :task-timeout nil
                       :max-request-rounds nil)))
          requests statuses acknowledgements tool-callback)
     (unwind-protect
         (cl-letf (((symbol-function 'gptel--update-status)
                    (lambda (status &rest _) (push (cons (current-buffer) status) statuses)))
                   ((symbol-function 'gptel-curl-get-response)
                    (lambda (request)
                      (push (cons request (copy-tree (plist-get (gptel-fsm-info request) :data) t))
                            requests)))
                   ((symbol-function 'gptel--url-get-response)
                    (lambda (&rest _) (ert-fail "Unexpected URL transport"))))
           (with-current-buffer parent
             (funcall gptel-agent-workers-test--mode)
             (insert "User request\n")
             (setq-local gptel-backend
                         (or gptel-agent-workers-test--backend
                             (gptel-make-openai "workers-test" :key "unused"
                                                :host "example.invalid" :stream t :models '(test)))
                         gptel-model 'test gptel-stream gptel-agent-workers-test--stream
                         gptel-use-curl t
                         gptel-mode t gptel-use-tools t gptel-agent-preset nil
                         gptel-confirm-tool-calls nil
                         gptel-pre-tool-call-functions nil gptel-post-tool-call-functions nil
                         gptel-agent--agent-snapshot snapshot
                         gptel-tools
                         (list (gptel-get-tool "Agent")
                               (gptel-make-tool
                                :name "WorkerTestTool" :async t :include t
                                :description "Deferred test operation" :args nil
                                :function (lambda (callback) (setq tool-callback callback)))))
             (gptel-request "User request" :fsm fsm :buffer parent :stream gptel-stream
               :transforms '(gptel-agent--localize-agent-tool)))
           ,@body)
       (when (buffer-live-p parent) (kill-buffer parent)))))

(defun gptel-agent-workers-test--launch (fsm &optional type id background)
  "Launch from FSM, returning the worker's run.  BACKGROUND defaults to t."
  (let* ((info (gptel-fsm-info fsm))
         (tool (cl-find "Agent" (plist-get info :tools)
                        :key #'gptel-tool-name :test #'equal))
         ack)
    (funcall (gptel-tool-function tool) (lambda (result) (setq ack result))
             (or type "researcher") "Bounded task" "Find evidence" nil nil id
             (if (eq background 'blocking) nil t))
    (let ((run (car (sort (hash-table-values gptel-agent--runs)
                          (lambda (a b) (> (gptel-agent--run-started-at a)
                                           (gptel-agent--run-started-at b)))))))
      (if (eq background 'blocking)
          (should-not ack)
        (should (string-match-p "run_id:" (or ack ""))))
      run)))

(defun gptel-agent-workers-test--answer (fsm text &optional stream)
  "Complete a model response through FSM's actual TYPE and finalization paths."
  (let* ((info (gptel-fsm-info fsm)) (backend (plist-get info :backend)))
    (gptel--fsm-transition fsm 'TYPE)
    (funcall
     (plist-get info :callback)
     (gptel--parse-response
      backend
      (cond ((gptel-openai-responses-p backend)
             `(:status "completed" :output [(:type "message" :role "assistant"
                                                  :content [(:type "output_text" :text ,text)])]))
            ((gptel-anthropic-p backend) `(:content [(:type "text" :text ,text)] :stop_reason "end_turn"))
            ((gptel-gemini-p backend) `(:candidates [(:content (:role "model" :parts [(:text ,text)])
                                                            :finishReason "STOP")]))
            (t `(:choices [(:message (:role "assistant" :content ,text))])))
      info)
     info)
    (when stream (funcall (plist-get info :callback) t info))
    (gptel--fsm-transition fsm)))

(defun gptel-agent-workers-test--tool (fsm name args)
  "Simulate a backend tool response in FSM, preserving actual tool dispatch."
  (let* ((info (gptel-fsm-info fsm)) (backend (plist-get info :backend)))
    (gptel--parse-response
     backend
     (cond
      ((gptel-openai-responses-p backend)
       `(:output [(:type "function_call" :call_id "call-test" :name ,name
                        :arguments ,(gptel--json-encode args))]))
      ((gptel-anthropic-p backend)
       `(:content [(:type "tool_use" :id "call-test" :name ,name :input ,args)]))
      ((gptel-gemini-p backend)
       `(:candidates [(:content (:role "model" :parts [(:functionCall (:name ,name :args ,args))]))]))
      (t `(:choices [(:message (:role "assistant" :content :null
                                 :tool_calls [(:id "call-test" :type "function"
                                                   :function (:name ,name :arguments ,(gptel--json-encode args)))]))])))
     info)
    (gptel--fsm-transition fsm 'TYPE)
    (gptel--fsm-transition fsm)))

(defun gptel-agent-workers-test--wake (fsm)
  "Run an event-loop wake deterministically, cancelling its scheduled timer."
  (gptel-agent--workers-wake (gptel-agent--workers-for fsm)))

(defun gptel-agent-workers-test--data (fsm)
  "Return FSM's current payload as text for order and occurrence assertions."
  (prin1-to-string (plist-get (gptel-fsm-info fsm) :data)))

(defun gptel-agent-workers-test--count (needle text)
  "Count literal NEEDLE occurrences in TEXT."
  (let ((start 0) (count 0))
    (while (string-match (regexp-quote needle) text start)
      (setq start (match-end 0) count (1+ count)))
    count))

(ert-deftest gptel-agent-workers-first-agent-overlay-follows-response-prefix ()
  (dolist (stream '(nil t))
    (dolist (prefix '("ASSISTANT> " "ASSISTANT>\n"))
      (dolist (background '(nil t))
        (let ((gptel-agent-workers-test--stream stream)
              (gptel-response-prefix-alist `((fundamental-mode . ,prefix))))
          (gptel-agent-workers-test--with-chat
            (gptel-agent-workers-test--tool
             fsm "Agent" `(:subagent_type "researcher" :description "Research"
                                         :prompt "Investigate" :background ,background))
            (let* ((run (car (hash-table-values gptel-agent--runs)))
                   (ov (gptel-agent--run-overlay run))
                   (anchor (overlay-end ov)))
              (with-current-buffer parent
                ;; The prefix must already be visible while the first agent runs.
                (goto-char (point-min))
                (should (search-forward prefix nil t))
                (should (<= (point) anchor)))
              (gptel-agent-workers-test--answer
               (gptel-agent--run-fsm run) "RESEARCH-RESULT")
              (gptel-agent-workers-test--tool fsm "WorkerTestTool" nil)
              (funcall tool-callback "NEXT-TOOL-RESULT")
              (with-current-buffer parent
                (should (= 1 (gptel-agent-workers-test--count prefix (buffer-string))))
                (should (= anchor (overlay-end ov)))
                (goto-char anchor)
                (should (search-forward "NEXT-TOOL-RESULT" nil t))))))))))

(ert-deftest gptel-agent-workers-overlay-keeps-styles-through-status-updates ()
  (let ((gptel-agent-workers-test--mode #'org-mode))
    (gptel-agent-workers-test--with-chat
      (gptel-agent-workers-test--tool fsm "WorkerTestTool" nil)
      (funcall tool-callback "PRECEDING-TOOL-RESULT")
      (let* ((run (gptel-agent-workers-test--launch fsm))
             (child (gptel-agent--run-fsm run))
             (ov (gptel-agent--run-overlay run))
             (displays (list (overlay-get ov 'after-string))))
        (gptel-agent-workers-test--tool fsm "WorkerTestTool" nil)
        (funcall tool-callback "FOLLOWING-TOOL-RESULT")
        (with-current-buffer parent (font-lock-ensure))
        ;; Run the delayed indicator synchronously, without leaving a timer behind.
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_time _repeat function &rest args)
                     (apply function args))))
          (gptel-agent--indicate-wait child))
        (push (overlay-get ov 'after-string) displays)
        (setf (plist-get (gptel-fsm-info child) :tool-use)
              '((:name "Read" :args (:file_path "example.el"))))
        (gptel-agent--indicate-tool-call child)
        (let* ((display (overlay-get ov 'after-string))
               (name (1+ (string-match "(Read " display))))
          (should (memq 'font-lock-keyword-face
                        (get-text-property name 'face display))))
        (push (overlay-get ov 'after-string) displays)
        (gptel-agent--workers-status (gptel-agent--workers-for child) "Waiting for agents")
        (push (overlay-get ov 'after-string) displays)
        (setf (gptel-agent--run-state run) 'completed)
        (gptel-agent--run-update-terminal-overlay run "Done")
        (push (overlay-get ov 'after-string) displays)
        (dolist (display displays)
          ;; Reset the surrounding tool block's face and prefixes after
          ;; every asynchronous status update, through the final rule.
          (should (eq (get-text-property 0 'face display) 'default))
          (let ((last-face (get-text-property (1- (length display)) 'face display)))
            (should (memq 'default last-face))
            (should (equal (car last-face)
                           (get-text-property 0 'face gptel-agent--hrule))))
          (should-not (text-property-not-all 0 (length display) 'line-prefix "" display))
          (should-not (text-property-not-all 0 (length display) 'wrap-prefix "" display)))))))

(ert-deftest gptel-agent-workers-parent-uses-tools-before-worker-finishes ()
  (gptel-agent-workers-test--with-chat
    ;; Agent's immediate acknowledgement goes through the normal tool pipeline.
    (gptel-agent-workers-test--tool
     fsm "Agent" '(:subagent_type "researcher" :description "Research"
                                  :prompt "Investigate" :background t))
    (let ((run (car (hash-table-values gptel-agent--runs))))
      (should (eq (gptel-fsm-state fsm) 'WAIT))
      (should (gptel-agent--run-active-p run))
      (gptel-agent-workers-test--tool fsm "WorkerTestTool" nil)
      (should tool-callback)
      (funcall tool-callback "INDEPENDENT-TOOL-RESULT")
      (should (eq (gptel-fsm-state fsm) 'WAIT))
      (should (gptel-agent--run-active-p run))
      (should (string-match-p "INDEPENDENT-TOOL-RESULT" (gptel-agent-workers-test--data fsm))))))

(ert-deftest gptel-agent-workers-early-final-never-runs-ready-or-clears-history ()
  (gptel-agent-workers-test--with-chat
    (let* ((run (gptel-agent-workers-test--launch fsm))
           (child-fsm (gptel-agent--run-fsm run)))
      (gptel-agent-workers-test--answer fsm "Interim answer")
      (should (eq (gptel-fsm-state fsm) 'AGWT))
      (should (member (cons parent " Waiting for agents") statuses))
      (should-not (member (cons parent " Ready") statuses))
      (should (string-match-p "Interim answer" (gptel-agent-workers-test--data fsm)))
      (with-current-buffer parent
        (should-not (string-suffix-p (gptel-prompt-prefix-string) (buffer-string))))
      (let ((before (length requests)))
        (gptel-agent-workers-test--wake fsm)
        (should (= before (length requests))))
      (gptel-agent-workers-test--answer child-fsm "FINAL-WORKER-EVIDENCE")
      (gptel-agent-workers-test--wake fsm)
      (should (eq (gptel-fsm-state fsm) 'WAIT))
      (should-not (member (cons parent " Ready") statuses))
      (should (= 1 (gptel-agent-workers-test--count "FINAL-WORKER-EVIDENCE"
                                                   (gptel-agent-workers-test--data fsm))))
      (gptel-agent-workers-test--answer fsm "Actual final answer")
      (should (eq (gptel-fsm-state fsm) 'DONE))
      (should (member (cons parent " Ready") statuses))
      (should-not (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))))

(ert-deftest gptel-agent-workers-last-result-before-finalization-is-not-lost ()
  (gptel-agent-workers-test--with-chat
    (let ((run (gptel-agent-workers-test--launch fsm)))
      (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "RACING-RESULT")
      ;; The run is already gone from the live registry, but its inbox is not.
      (should (zerop (hash-table-count gptel-agent--runs)))
      (gptel-agent-workers-test--answer fsm "Premature answer")
      (should (eq (gptel-fsm-state fsm) 'AGWT))
      (gptel-agent-workers-test--wake fsm)
      (should (string-match-p "RACING-RESULT" (gptel-agent-workers-test--data fsm)))
      (should-not (member (cons parent " Ready") statuses)))))

(ert-deftest gptel-agent-workers-results-during-tools-follow-all-tool-results-once ()
  (gptel-agent-workers-test--with-chat
    (let ((run (gptel-agent-workers-test--launch fsm)))
      (gptel-agent-workers-test--tool fsm "WorkerTestTool" nil)
      (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "TOOL-RACE-REPORT")
      (gptel-agent-workers-test--wake fsm)
      (should-not (string-match-p "TOOL-RACE-REPORT" (gptel-agent-workers-test--data fsm)))
      (funcall tool-callback "TOOL-RESULT-FIRST")
      (let ((payload (gptel-agent-workers-test--data fsm)))
        (should (< (string-match "TOOL-RESULT-FIRST" payload)
                   (string-match "TOOL-RACE-REPORT" payload)))
        (should (= 1 (gptel-agent-workers-test--count "TOOL-RACE-REPORT" payload))))
      (gptel-agent-workers-test--wake fsm)
      (gptel-agent--run-finish run 'completed "DUPLICATE")
      (should-not (string-match-p "DUPLICATE" (gptel-agent-workers-test--data fsm))))))

(ert-deftest gptel-agent-workers-explicit-wait-shares-delivery-and-does-not-poll ()
  (gptel-agent-workers-test--with-chat
    (let* ((run (gptel-agent-workers-test--launch fsm))
           (before (length requests)))
      (gptel-agent-workers-test--tool fsm "AgentControl" '(:action "wait"))
      (should (eq (gptel-fsm-state fsm) 'TOOL))
      (should (= before (length requests)))
      (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "WAIT-RESULT")
      (gptel-agent-workers-test--wake fsm)
      (should (eq (gptel-fsm-state fsm) 'WAIT))
      (should (= 1 (gptel-agent-workers-test--count "WAIT-RESULT"
                                                   (gptel-agent-workers-test--data fsm))))
      (should-not (gptel-agent--workers-events (gptel-agent--workers-for fsm)))
      (gptel-agent--control (lambda (value) (push value acknowledgements)) fsm "wait")
      (should (string-match-p "no_eligible_work" (car acknowledgements))))))

(ert-deftest gptel-agent-workers-selective-waits-and-queued-outcomes ()
  (gptel-agent-workers-test--with-chat
    (let* ((first (gptel-agent-workers-test--launch fsm))
           (second (gptel-agent-workers-test--launch fsm))
           (first-id (gptel-agent--session-id (gptel-agent--run-session first)))
           waited)
      (gptel-agent--control (lambda (value) (push value waited)) fsm "wait" nil (vector first-id))
      (gptel-agent-workers-test--answer (gptel-agent--run-fsm second) "SECOND-REPORT")
      (gptel-agent-workers-test--wake fsm)
      (should-not waited)
      (gptel-agent-workers-test--answer (gptel-agent--run-fsm first) "FIRST-REPORT")
      (gptel-agent-workers-test--wake fsm)
      (should (= (length waited) 1))
      (should (string-match-p "FIRST-REPORT" (car waited)))
      ;; Already queued second outcome returns synchronously.
      (gptel-agent--control (lambda (value) (push value waited)) fsm "wait")
      (should (= (length waited) 2))
      (should (string-match-p "SECOND-REPORT" (car waited)))
      (should-not (gptel-agent--workers-events (gptel-agent--workers-for fsm))))))

(ert-deftest gptel-agent-workers-followup-preserves-undelivered-invocations ()
  (gptel-agent-workers-test--with-chat
    (let* ((first (gptel-agent-workers-test--launch fsm))
           (id (gptel-agent--session-id (gptel-agent--run-session first))))
      (gptel-agent-workers-test--answer (gptel-agent--run-fsm first) "ORIGINAL-RESULT")
      (let ((next (gptel-agent-workers-test--launch fsm "researcher" id)))
        (should (equal id (gptel-agent--session-id (gptel-agent--run-session next))))
        (should-not (equal (gptel-agent--run-id first) (gptel-agent--run-id next)))
        (gptel-agent--control (lambda (value) (push value acknowledgements)) fsm "read" id)
        (should (string-match-p "ORIGINAL-RESULT" (car acknowledgements)))
        (gptel-agent-workers-test--answer (gptel-agent--run-fsm next) "FOLLOWUP-RESULT")
        (gptel-agent-workers-test--answer fsm "Interim")
        (gptel-agent-workers-test--wake fsm)
        (let ((payload (gptel-agent-workers-test--data fsm)))
          (should (= 1 (gptel-agent-workers-test--count "ORIGINAL-RESULT" payload)))
          (should (= 1 (gptel-agent-workers-test--count "FOLLOWUP-RESULT" payload))))))))

(ert-deftest gptel-agent-workers-steering-near-completion-reuses-run-and-budgets ()
  (gptel-agent-workers-test--with-chat
    (let* ((run (gptel-agent-workers-test--launch fsm))
           (child (gptel-agent--run-fsm run))
           (id (gptel-agent--session-id (gptel-agent--run-session run))))
      (setf (gptel-agent--run-tool-calls run) 3 (gptel-agent--run-retries run) 1)
      (gptel-agent--control (lambda (value) (push value acknowledgements))
                            fsm "send" id nil "STEERING-CORRECTION")
      (should (string-match-p "accepted" (car acknowledgements)))
      (should-not (string-match-p "STEERING-CORRECTION" (gptel-agent-workers-test--data child)))
      (gptel-agent-workers-test--answer child "Obsolete conclusion")
      (should (eq (gptel-fsm-state child) 'AGWT))
      (should (gptel-agent--run-active-p run))
      (gptel-agent-workers-test--wake child)
      (should (= 2 (gptel-agent--run-rounds run)))
      (should (= 3 (gptel-agent--run-tool-calls run)))
      (should (= 1 (gptel-agent--run-retries run)))
      (should (string-match-p "STEERING-CORRECTION" (gptel-agent-workers-test--data child)))
      (gptel-agent-workers-test--answer child "Corrected conclusion")
      (should (string-match-p "Corrected conclusion" (gptel-agent--run-result run)))
      (should-not (string-match-p "Obsolete conclusion" (gptel-agent--run-result run)))
      (gptel-agent--control (lambda (value) (push value acknowledgements))
                            fsm "send" id nil "Too late")
      (should (string-match-p "Use Agent with agent_id" (car acknowledgements))))))

(ert-deftest gptel-agent-workers-nested-completion-gates-every-level ()
  (gptel-agent-workers-test--with-chat
    (let* ((run (gptel-agent-workers-test--launch fsm "executor"))
           (child (gptel-agent--run-fsm run))
           (grandchild (gptel-agent-workers-test--launch child "researcher")))
      (gptel-agent-workers-test--answer child "Worker interim")
      (gptel-agent-workers-test--answer fsm "Parent interim")
      (should (eq (gptel-fsm-state child) 'AGWT))
      (should (eq (gptel-fsm-state fsm) 'AGWT))
      (gptel-agent-workers-test--answer (gptel-agent--run-fsm grandchild) "NESTED-EVIDENCE")
      (gptel-agent-workers-test--wake child)
      (should (eq (gptel-fsm-state fsm) 'AGWT))
      (should (string-match-p "NESTED-EVIDENCE" (gptel-agent-workers-test--data child)))
      (gptel-agent-workers-test--answer child "Nested synthesis")
      (gptel-agent-workers-test--wake fsm)
      (should (string-match-p "Nested synthesis" (gptel-agent-workers-test--data fsm)))
      (should-not (member (cons parent " Ready") statuses)))))

(ert-deftest gptel-agent-workers-cancellation-closes-subtree-and-late-wakeups ()
  (dolist (method '(abort kill error cancel timeout))
    (gptel-agent-workers-test--with-chat
      (let* ((run (gptel-agent-workers-test--launch fsm "executor"))
             (child (gptel-agent--run-fsm run))
             (nested (gptel-agent-workers-test--launch child "researcher"))
             (gate (gptel-agent--workers-for fsm))
             (child-gate (gptel-agent--workers-for child)))
        (gptel-agent-workers-test--answer fsm "Interim")
        (pcase method
          ('abort (gptel-abort parent))
          ('kill (kill-buffer parent))
          ('error (setf (plist-get (gptel-fsm-info fsm) :error) "fatal")
                  (gptel--fsm-transition fsm 'ERRS))
          ('cancel (gptel-agent--control #'ignore fsm "cancel"
                                         (gptel-agent--session-id (gptel-agent--run-session run))))
          ('timeout (gptel-agent--run-finish run 'timed-out "TIMEOUT" 'timeout)))
        (should-not (gptel-agent--run-active-p run))
        (should-not (gptel-agent--run-active-p nested))
        (should (gptel-agent--workers-closed child-gate))
        (let ((before (length requests)))
          (gptel-agent--workers-wake child-gate)
          (funcall (plist-get (gptel-fsm-info child) :callback) "LATE" (gptel-fsm-info child))
          (should (= before (length requests))))
        (if (memq method '(cancel timeout))
            (progn
              (should-not (gptel-agent--workers-closed gate))
              (gptel-agent-workers-test--wake fsm)
              (should (string-match-p (if (eq method 'cancel) "cancelled" "timed-out")
                                      (gptel-agent-workers-test--data fsm))))
          (should (gptel-agent--workers-closed gate))
          (should-not (gptel-agent--workers-wakeup gate)))))))

(ert-deftest gptel-agent-workers-blocking-default-and-false-remain-blocking ()
  (dolist (background '(nil :json-false))
    (gptel-agent-workers-test--with-chat
      (gptel-agent-workers-test--tool
       fsm "Agent" (append '(:subagent_type "researcher" :description "Blocking" :prompt "Task")
                             (when background (list :background background))))
      (let ((run (car (hash-table-values gptel-agent--runs))))
        (should (eq (gptel-fsm-state fsm) 'TOOL))
        (should-not (gptel-agent--run-background run))
        (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "BLOCKING-RESULT")
        (should (eq (gptel-fsm-state fsm) 'WAIT))
        (should (string-match-p "BLOCKING-RESULT" (gptel-agent-workers-test--data fsm)))
        (should-not (gptel-agent--workers-events (gptel-agent--workers-for fsm)))))))

(ert-deftest gptel-agent-workers-leaves-unrelated-request-tables-unchanged ()
  (let ((table (copy-tree gptel-send--transitions))
        (handlers (copy-tree gptel-send--handlers))
        (plain (gptel-make-fsm :table gptel-send--transitions
                              :handlers gptel-send--handlers)))
    (gptel-agent-workers-test--with-chat
      (should-not (eq (gptel-fsm-table fsm) (gptel-fsm-table plain)))
      (should (equal table gptel-send--transitions))
      (should (equal handlers gptel-send--handlers))
      (should-not (gptel-agent--workers-for plain)))))

(ert-deftest gptel-agent-workers-provider-ordering-includes-results-before-reports ()
  (dolist (backend (list (gptel-make-openai "order-openai" :key "unused" :models '(test))
                         (gptel-make-openai-responses "order-responses" :key "unused" :models '(test))
                         (gptel-make-anthropic "order-anthropic" :key "unused" :models '(test))
                         (gptel-make-gemini "order-gemini" :key "unused" :models '(test))))
    (let ((gptel-agent-workers-test--backend backend))
      (gptel-agent-workers-test--with-chat
        (let ((run (gptel-agent-workers-test--launch fsm)))
          (gptel-agent-workers-test--tool fsm "WorkerTestTool" nil)
          (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "PROVIDER-REPORT")
          (gptel-agent-workers-test--wake fsm)
          (should-not (string-match-p "PROVIDER-REPORT" (gptel-agent-workers-test--data fsm)))
          (funcall tool-callback "PROVIDER-TOOL-RESULT")
          (let ((payload (gptel-agent-workers-test--data fsm)))
            (should (< (string-match "PROVIDER-TOOL-RESULT" payload)
                       (string-match "PROVIDER-REPORT" payload)))
            (should (= 1 (gptel-agent-workers-test--count "PROVIDER-REPORT" payload))))
          ;; A second completion before a final answer must force synthesis
          ;; using the same provider-specific retained history.
          (setq run (gptel-agent-workers-test--launch fsm))
          (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "SECOND-PROVIDER-REPORT")
          (gptel-agent-workers-test--answer fsm "PROVIDER-INTERIM-ANSWER")
          (gptel-agent-workers-test--wake fsm)
          (let ((payload (gptel-agent-workers-test--data fsm)))
            (should (= 1 (gptel-agent-workers-test--count "PROVIDER-INTERIM-ANSWER" payload)))
            (should (< (string-match "PROVIDER-INTERIM-ANSWER" payload)
                       (string-match "SECOND-PROVIDER-REPORT" payload)))))))))

(ert-deftest gptel-agent-workers-completion-during-streaming-waits-for-boundary ()
  (let ((gptel-agent-workers-test--stream t))
    (gptel-agent-workers-test--with-chat
      (let* ((run (gptel-agent-workers-test--launch fsm))
             (info (gptel-fsm-info fsm)))
        (should (plist-get info :stream))
        (gptel--fsm-transition fsm 'TYPE)
        (funcall (plist-get info :callback) "STREAM-PART-ONE " info)
        (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "STREAMING-REPORT" t)
        (let ((before (length requests)))
          (gptel-agent-workers-test--wake fsm)
          (should (= before (length requests))))
        (should-not (string-match-p "STREAMING-REPORT" (gptel-agent-workers-test--data fsm)))
        (funcall (plist-get info :callback) "STREAM-PART-TWO" info)
        (funcall (plist-get info :callback) t info)
        (gptel--fsm-transition fsm)
        (should (eq (gptel-fsm-state fsm) 'AGWT))
        (gptel-agent-workers-test--wake fsm)
        (let ((payload (gptel-agent-workers-test--data fsm)))
          (should (string-match-p "STREAM-PART-ONE STREAM-PART-TWO" payload))
          (should (= 1 (gptel-agent-workers-test--count "STREAMING-REPORT" payload))))
        (should-not (member (cons parent " Ready") statuses))))))

(ert-deftest gptel-agent-workers-gemini-streaming-interim-is-not-duplicated ()
  (let ((gptel-agent-workers-test--backend
         (gptel-make-gemini "gemini-stream" :key "unused" :models '(test)))
        (gptel-agent-workers-test--stream t))
    (gptel-agent-workers-test--with-chat
      (let* ((run (gptel-agent-workers-test--launch fsm))
             (info (gptel-fsm-info fsm)))
        (gptel--fsm-transition fsm 'TYPE)
        (with-temp-buffer
          (insert "{\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"GEMINI-STREAM-INTERIM\"}]},\"finishReason\":\"STOP\"}]}")
          (goto-char (point-min))
          (funcall (plist-get info :callback)
                   (gptel-curl--parse-stream (plist-get info :backend) info) info))
        (funcall (plist-get info :callback) t info)
        (gptel--fsm-transition fsm)
        (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "GEMINI-CHILD-RESULT" t)
        (gptel-agent-workers-test--wake fsm)
        (should (= 1 (gptel-agent-workers-test--count "GEMINI-STREAM-INTERIM"
                                                     (gptel-agent-workers-test--data fsm))))))))

(ert-deftest gptel-agent-workers-steering-during-tool-wait-is-ordered ()
  (gptel-agent-workers-test--with-chat
    (let* ((run (gptel-agent-workers-test--launch fsm))
           (child (gptel-agent--run-fsm run))
           (id (gptel-agent--session-id (gptel-agent--run-session run))))
      (gptel-agent-workers-test--tool child "WorkerTestTool" nil)
      (gptel-agent--control #'ignore fsm "send" id nil "STEER-AFTER-TOOL")
      (gptel-agent-workers-test--wake child)
      (should (eq (gptel-fsm-state child) 'TOOL))
      (should-not (string-match-p "STEER-AFTER-TOOL" (gptel-agent-workers-test--data child)))
      (funcall tool-callback "CHILD-TOOL-RESULT")
      (let ((payload (gptel-agent-workers-test--data child)))
        (should (< (string-match "CHILD-TOOL-RESULT" payload)
                   (string-match "STEER-AFTER-TOOL" payload)))))))

(ert-deftest gptel-agent-workers-failure-retry-and-permission-wait-stay-active ()
  (gptel-agent-workers-test--with-chat
    (let* ((run (gptel-agent-workers-test--launch fsm "executor"))
           (child (gptel-agent--run-fsm run))
           (info (gptel-fsm-info child))
           (gptel-agent-retry-delay 3600))
      (setf (plist-get (gptel-fsm-info child) :error) "connection reset"
            (plist-get (gptel-fsm-info child) :status) "Transport failure")
      (gptel--fsm-transition child 'ERRS)
      (should (eq (gptel-agent--run-state run) 'retry-wait))
      (gptel-agent-workers-test--answer fsm "Interim during retry")
      (should (eq (gptel-fsm-state fsm) 'AGWT))
      (cancel-timer (gptel-agent--run-retry-timer run))
      (setf (gptel-agent--run-retry-timer run) nil)
      (gptel--fsm-transition child 'WAIT)
      (should (= 1 (gptel-agent--run-rounds run)))
      ;; Confirmation leaves the worker active, without running the tool.
      (with-current-buffer (gptel-agent--run-child-buffer run)
        (setq-local gptel-confirm-tool-calls t))
      (cl-letf (((symbol-function 'gptel--display-tool-calls) #'ignore))
        (gptel-agent-workers-test--tool child "WorkerTestTool" nil))
      (should-not tool-callback)
      (should (plist-get (gptel-fsm-info child) :tool-pending))
      (should (gptel-agent--run-active-p run))
      (gptel-agent-workers-test--wake fsm)
      (should (eq (gptel-fsm-state fsm) 'AGWT))
      ;; Unrecoverable error cancels a nested child before reporting failure.
      (let ((nested (gptel-agent-workers-test--launch child "researcher")))
        (setq info (gptel-fsm-info child))
        (setf (plist-get info :error) "Authentication failed"
              (plist-get info :http-status) "401")
        (gptel--fsm-transition child 'ERRS)
        (should (eq (gptel-agent--run-state run) 'failed))
        (should-not (gptel-agent--run-active-p nested))
        (gptel-agent-workers-test--wake fsm)
        (should (string-match-p "status: failed" (gptel-agent-workers-test--data fsm)))))))

(ert-deftest gptel-agent-workers-controls-reject-foreign-ids ()
  (gptel-agent-workers-test--with-chat
    (let* ((run (gptel-agent-workers-test--launch fsm))
           (id (gptel-agent--session-id (gptel-agent--run-session run)))
           (child (gptel-agent--run-fsm run)))
      (dolist (action '("read" "send" "cancel"))
        (gptel-agent--control (lambda (value) (push value acknowledgements))
                              child action id nil "Bad steering")
        (should (string-match-p "Unknown agent_id" (car acknowledgements))))
      (gptel-agent--control (lambda (value) (push value acknowledgements))
                            child "wait" nil (vector id))
      (should (string-match-p "Unknown agent_id" (car acknowledgements)))
      (gptel-agent--control (lambda (value) (push value acknowledgements)) fsm "list")
      (should (string-match-p id (car acknowledgements)))
      (should (gptel-agent--run-active-p run)))))

(ert-deftest gptel-agent-workers-async-setup-accepts-steering-and-cannot-revive ()
  (dolist (cancel '(nil t))
    (gptel-agent-workers-test--with-chat
      (let (continue)
        (with-current-buffer parent
          (setq-local gptel-prompt-transform-functions
                      (list (lambda (callback _fsm) (setq continue callback)))))
        (let* ((run (gptel-agent-workers-test--launch fsm))
               (child (gptel-agent--run-fsm run))
               (id (gptel-agent--session-id (gptel-agent--run-session run))))
          (should continue)
          (should (= (length requests) 1))
          (gptel-agent--control (lambda (value) (push value acknowledgements))
                                fsm "send" id nil "SETUP-STEERING")
          (should (string-match-p "accepted" (car acknowledgements)))
          (when cancel (gptel-agent--control #'ignore fsm "cancel" id))
          (funcall continue)
          (if cancel
              (progn
                (should (= (length requests) 1))
                (should (eq (gptel-agent--run-state run) 'cancelled))
                (should (gptel-agent--workers-closed (gptel-agent--workers-for child))))
            (should (= (length requests) 2))
            (should (string-match-p "SETUP-STEERING" (gptel-agent-workers-test--data child)))
            (should (= (gptel-agent--run-rounds run) 1))))))))

(ert-deftest gptel-agent-workers-terminal-during-launch-acknowledges-once ()
  (gptel-agent-workers-test--with-chat
    (cl-letf (((symbol-function 'gptel-curl-get-response)
               (lambda (child) (gptel-agent-workers-test--answer child "SYNCHRONOUS-RESULT"))))
      (gptel-agent--task (lambda (result) (push result acknowledgements))
                         "researcher" "Immediate task" "Task" fsm snapshot nil nil nil t))
    (should (= 1 (length acknowledgements)))
    (should (string-match-p "run_id:" (car acknowledgements)))
    (should-not (string-match-p "SYNCHRONOUS-RESULT" (car acknowledgements)))
    (gptel-agent-workers-test--answer fsm "Interim")
    (gptel-agent-workers-test--wake fsm)
    (should (= 1 (gptel-agent-workers-test--count "SYNCHRONOUS-RESULT"
                                                 (gptel-agent-workers-test--data fsm))))))

(ert-deftest gptel-agent-workers-real-timer-wakes-parked-parent-once ()
  (gptel-agent-workers-test--with-chat
    (let* ((run (gptel-agent-workers-test--launch fsm))
           (before (length requests)))
      (gptel-agent-workers-test--answer fsm "Interim")
      (gptel-agent-workers-test--answer (gptel-agent--run-fsm run) "TIMER-REPORT")
      (let ((deadline (+ (float-time) 1)))
        (while (and (eq (gptel-fsm-state fsm) 'AGWT) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (eq (gptel-fsm-state fsm) 'WAIT))
      (should (= (1+ before) (length requests)))
      (should-not (gptel-agent--workers-wakeup (gptel-agent--workers-for fsm)))
      (should (= 1 (gptel-agent-workers-test--count "TIMER-REPORT"
                                                   (gptel-agent-workers-test--data fsm)))))))

(ert-deftest gptel-agent-workers-failed-buffer-disposal-retains-readable-outcome ()
  (gptel-agent-workers-test--with-chat
    (let* ((run (gptel-agent-workers-test--launch fsm))
           (id (gptel-agent--session-id (gptel-agent--run-session run)))
           (gptel-agent-keep-failed-run-buffers nil))
      (gptel-agent--run-finish run 'failed "DISPOSED-FAILURE" 'test)
      (should-not (buffer-live-p (gptel-agent--run-child-buffer run)))
      (gptel-agent--control (lambda (value) (push value acknowledgements)) fsm "read" id)
      (should (string-match-p "DISPOSED-FAILURE" (car acknowledgements)))
      (gptel-agent--control (lambda (value) (push value acknowledgements)) fsm "wait" nil (vector id))
      (should (string-match-p "DISPOSED-FAILURE" (car acknowledgements)))
      (should (gptel-agent--run-delivered run)))))

(ert-deftest gptel-agent-workers-completion-reentrant-during-injection-is-retained ()
  (gptel-agent-workers-test--with-chat
    (let* ((first (gptel-agent-workers-test--launch fsm))
           (second (gptel-agent-workers-test--launch fsm))
           (original (symbol-function 'gptel--inject-prompt))
           fired)
      (gptel-agent-workers-test--answer (gptel-agent--run-fsm first) "FIRST-BOUNDARY-REPORT")
      (gptel-agent-workers-test--answer fsm "Interim")
      (cl-letf (((symbol-function 'gptel--inject-prompt)
                 (lambda (&rest args)
                   (unless fired
                     (setq fired t)
                     (gptel-agent--run-finish second 'failed "REENTRANT-REPORT" 'test))
                   (apply original args))))
        (gptel-agent-workers-test--wake fsm))
      (should (gptel-agent--run-delivered first))
      (should-not (gptel-agent--run-delivered second))
      (gptel-agent-workers-test--answer fsm "Still interim")
      (should (eq (gptel-fsm-state fsm) 'AGWT))
      (gptel-agent-workers-test--wake fsm)
      (should (gptel-agent--run-delivered second))
      (should (= 1 (gptel-agent-workers-test--count "REENTRANT-REPORT"
                                                   (gptel-agent-workers-test--data fsm)))))))

(provide 'gptel-agent-workers-test)
;;; gptel-agent-workers-test.el ends here
