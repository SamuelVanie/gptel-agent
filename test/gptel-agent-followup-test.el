;;; gptel-agent-followup-test.el --- Retained Agent conversations -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)
(require 'gptel-openai)
(require 'gptel-anthropic)
(require 'gptel-gemini)
(require 'gptel-openai-responses)

(cl-defmacro gptel-agent-followup-test--with-chat (&body body)
  "Run BODY with a real request pipeline and a captured network transport."
  (declare (indent 0))
  `(let* ((parent (generate-new-buffer " *agent-followup-parent*"))
          (parent-fsm (gptel-make-fsm))
          (snapshot '(("researcher" :description "Research" :system "Original system"
                       :task-timeout 60 :max-request-rounds 4)
                      ("executor" :description "Executes work" :system "Execute")))
          (gptel-agent--runs (make-hash-table :test #'equal))
          (gptel-agent--run-history nil)
          requests received
          (deliver (lambda (value) (push value received))))
     (unwind-protect
         (cl-letf (((symbol-function 'gptel--update-status) #'ignore)
                   ((symbol-function 'gptel-curl-get-response)
                    (lambda (fsm) (push fsm requests)))
                   ((symbol-function 'gptel--url-get-response)
                    (lambda (_fsm) (ert-fail "Unexpected URL transport"))))
           (with-current-buffer parent
             (insert "Parent prompt\n")
             (setq-local gptel-backend
                         (gptel-make-openai "followup-test" :key "unused"
                                            :host "example.invalid" :stream t :models '(test-model))
                         gptel-model 'test-model
                         gptel-use-curl t
                         gptel-stream nil
                         gptel-tools (list (gptel-get-tool "Read")
                                           (gptel-get-tool "Agent"))
                         gptel-prompt-transform-functions nil
                         gptel-agent-preset nil)
             (setf (gptel-fsm-info parent-fsm)
                   (list :buffer parent :position (point-marker))))
           ,@body)
       (when (buffer-live-p parent) (kill-buffer parent)))))

(defun gptel-agent-followup-test--finish (fsm text)
  "Deliver final TEXT through FSM's actual callback and DONE handler."
  (let ((info (gptel-fsm-info fsm)))
    (funcall (plist-get info :callback) text info)
    (gptel--fsm-transition fsm 'DONE)))

(ert-deftest gptel-agent-followup-empty-id-starts-a-new-conversation ()
  (gptel-agent-followup-test--with-chat
    (with-current-buffer parent
      (setq-local gptel-agent--agent-snapshot snapshot)
      (gptel-agent--localize-agent-tool parent-fsm)
      (let ((tool (cl-find "Agent" gptel-tools
                           :key #'gptel-tool-name :test #'equal))
            ids)
        (should (plist-get (cl-find "agent_id" (gptel-tool-args tool)
                                    :key (lambda (arg) (plist-get arg :name))
                                    :test #'equal)
                           :optional))
        (dolist (id '(nil :null "" " \t\n"))
          (apply (gptel-tool-function tool) deliver
                 (gptel--map-tool-args
                  tool (append (list :subagent_type "researcher"
                                     :description "New task" :prompt "Find evidence")
                               (when id (list :agent_id id)))))
          (should (= (length requests) (1+ (length ids))))
          (let* ((fsm (car requests))
                 (run (plist-get (gptel-fsm-info fsm) :context))
                 (new-id (gptel-agent--session-id (gptel-agent--run-session run))))
            (should-not (member new-id ids))
            (push new-id ids)
            (gptel-agent-followup-test--finish fsm "Done")
            (should (string-prefix-p (concat "agent_id: " new-id) (car received)))))))))

(ert-deftest gptel-agent-followup-keeps-evidence-and-renews-supervision ()
  (gptel-agent-followup-test--with-chat
    (gptel-agent--task deliver "researcher" "First task" "Find evidence"
                       parent-fsm snapshot)
    (should (= (length requests) 1))
    (let* ((first (car requests))
           (info (gptel-fsm-info first))
           (run (plist-get info :context))
           (session (gptel-agent--run-session run))
           (id (gptel-agent--session-id session))
           (child (gptel-agent--run-child-buffer run))
           (evidence (concat (make-string 6000 ?x) "UNTRUNCATED-EVIDENCE")))
      ;; Exercise the real backend's tool-call parser and the tool-result path.
      (gptel--parse-response
       (plist-get info :backend)
       '(:choices [(:message (:role "assistant" :content "Checking"
                                    :tool_calls [(:id "call-1" :type "function"
                                                      :function (:name "Read" :arguments "{}"))]))])
       info)
      (plist-put info :tool-result
                 (list (list (gptel-get-tool "Read") nil evidence)))
      (setf (plist-get (car (plist-get info :tool-use)) :result) evidence
            (gptel-agent--run-finalization-reason run) "enough evidence"
            (gptel-agent--run-tool-calls run) 3
            (gptel-agent--run-web-tool-calls run) 2
            (gptel-agent--run-retries run) 1
            (gptel-fsm-state first) 'TRET)
      (gptel-agent--handle-task-tool-result first)
      (should-not (plist-get info :tools))
      (gptel-agent-followup-test--finish first "Original answer")
      (should (string-match-p (concat "agent_id: " id) (car received)))
      (should (buffer-live-p child))
      (should-not (gptel-agent--run-timeout-timer run))
      ;; Change parent config and request identity between invocations.
      (with-current-buffer parent
        (setq-local gptel-model 'changed-model gptel-agent-preset 'missing-preset))
      (setq parent-fsm (gptel-make-fsm :info (list :buffer parent :position 1)))
      (with-temp-buffer
        (gptel-agent--task deliver "researcher" "Correct answer" "Check the edge case"
                           parent-fsm snapshot nil nil id))
      (let* ((next (car requests))
             (next-info (gptel-fsm-info next))
             (next-run (plist-get next-info :context))
             (data (plist-get next-info :data))
             (messages (append (plist-get data :messages) nil)))
        (should-not (eq next first))
        (should (eq (gptel-agent--run-child-buffer next-run) child))
        (should (eq (gptel-agent--run-parent-fsm next-run) parent-fsm))
        (should (equal (gptel-agent--session-id
                        (gptel-agent--run-session next-run)) id))
        (should (eq (plist-get next-info :model) 'test-model))
        (should (equal (plist-get (car messages) :content) "Original system"))
        (should (seq-find (lambda (msg) (equal (plist-get msg :content) evidence))
                          messages))
        (should (equal (plist-get (nth (- (length messages) 2) messages) :content)
                       "Original answer"))
        (should (string-prefix-p "Check the edge case"
                                 (plist-get (car (last messages)) :content)))
        (should (plist-get data :tools))
        (should (plist-get next-info :tools))
        (should (= (gptel-agent--run-rounds next-run) 1))
        (should (= (gptel-agent--run-retries next-run) 0))
        (should (= (gptel-agent--run-tool-calls next-run) 0))
        (should (= (gptel-agent--run-web-tool-calls next-run) 0))
        (should-not (gptel-agent--run-finalization-requested next-run))
        (should (timerp (gptel-agent--run-timeout-timer next-run)))
        ;; Nested Agent closures must capture the new FSM.
        (let ((tool (cl-find "Agent" (plist-get next-info :tools)
                             :key #'gptel-tool-name :test #'equal)) captured)
          (cl-letf (((symbol-function 'gptel-agent--task)
                     (lambda (&rest args) (setq captured args))))
            (funcall (gptel-tool-function tool) #'ignore "executor" "Nested" "Task"))
          (should (eq (nth 4 captured) next)))
        ;; Old callbacks cannot write into or finish the new invocation.
        (funcall (plist-get info :callback) "LATE OLD RESPONSE" info)
        (should (equal (gptel-agent--run-response next-run) ""))
        (should (= (length received) 1))
        (gptel-agent-followup-test--finish next "Corrected answer")
        (gptel-agent--task deliver "researcher" "Another task" "One more check"
                           parent-fsm snapshot nil nil id)
        (should (eq (plist-get (gptel-fsm-info (car requests)) :buffer) child))
        (should (string-match-p "Corrected answer"
                                (prin1-to-string
                                 (plist-get (gptel-fsm-info (car requests)) :data))))
        (kill-buffer parent)
        (should-not (buffer-live-p child))
        (should (= (length received) 2))
        (should (= (hash-table-count gptel-agent--runs) 0))))))

(ert-deftest gptel-agent-followup-validates-ownership-state-and-type ()
  (gptel-agent-followup-test--with-chat
    (gptel-agent--task deliver "researcher" "First" "Investigate"
                       parent-fsm snapshot)
    (let* ((fsm (car requests))
           (run (plist-get (gptel-fsm-info fsm) :context))
           (id (gptel-agent--session-id (gptel-agent--run-session run))))
      (gptel-agent--task deliver "researcher" "Busy" "Follow up"
                         parent-fsm snapshot nil nil id)
      (should (string-prefix-p "Error:" (car received)))
      (should (= (length requests) 1))
      (gptel-agent-followup-test--finish fsm "Done")
      (dolist (args (list (list "unknown-id" "researcher" parent-fsm)
                          (list id "executor" parent-fsm)
                          (list id "researcher"
                                (gptel-make-fsm :info (list :buffer (current-buffer))))))
        (gptel-agent--task deliver (nth 1 args) "Invalid" "Follow up"
                           (nth 2 args) snapshot nil nil (car args))
        (should (string-prefix-p "Error:" (car received))))
      (kill-buffer (gptel-agent--run-child-buffer run))
      (should-not (buffer-local-value 'gptel-agent--sessions parent))
      (gptel-agent--task deliver "researcher" "Deleted" "Follow up"
                         parent-fsm snapshot nil nil id)
      (should (string-prefix-p "Error:" (car received)))
      (should (= (length requests) 1)))))

(ert-deftest gptel-agent-followup-can-start-during-result-delivery ()
  (gptel-agent-followup-test--with-chat
    (gptel-agent--task
     (lambda (result)
       (string-match "agent_id: \\(agent-[0-9]+\\)" result)
       (gptel-agent--task deliver "researcher" "Follow up" "Correct it"
                          parent-fsm snapshot nil nil (match-string 1 result)))
     "researcher" "First" "Investigate" parent-fsm snapshot nil nil :null)
    (let ((first (car requests)))
      (gptel-agent-followup-test--finish first "Original answer")
      (should (= (length requests) 2))
      (let ((run (plist-get (gptel-fsm-info (car requests)) :context)))
        (should (gptel-agent--run-active-p run))
        (should (buffer-live-p (gptel-agent--run-child-buffer run)))
        (should (timerp (gptel-agent--run-timeout-timer run)))))))

(ert-deftest gptel-agent-followup-keeps-result-explorer-scope ()
  (gptel-agent-followup-test--with-chat
    (let ((first-file (make-temp-file "agent-scope-" nil nil "First evidence"))
          (second-file (make-temp-file "agent-scope-" nil nil "Other evidence"))
          (gptel-agent--saved-tool-results (make-hash-table :test #'equal))
          (query "Find evidence"))
      (unwind-protect
          (progn
            (setq snapshot '(("result-explorer" :system "Extract evidence"
                              :tools ("ResultRead" "ResultGrep"))))
            (dolist (file (list first-file second-file))
              (puthash file (list :query query) gptel-agent--saved-tool-results))
            (gptel-agent--task deliver "result-explorer" "Extract" "Ignored"
                               parent-fsm snapshot first-file query)
            (let* ((fsm (car requests))
                   (run (plist-get (gptel-fsm-info fsm) :context))
                   (id (gptel-agent--session-id (gptel-agent--run-session run))))
              (gptel-agent-followup-test--finish fsm "Found evidence")
              (gptel-agent--task deliver "result-explorer" "Other file" "Extract"
                                 parent-fsm snapshot second-file query id)
              (should (string-prefix-p "Error:" (car received)))
              (should (= (length requests) 1))
              (gptel-agent--task deliver "result-explorer" "Clarify" "Explain line 1"
                                 parent-fsm snapshot first-file query id)
              (should (= (length requests) 2))
              (should (equal (buffer-local-value 'gptel-agent--result-file-scope
                                                 (gptel-agent--run-child-buffer run))
                             first-file))))
        (delete-file first-file)
        (delete-file second-file)))))

(ert-deftest gptel-agent-parent-close-removes-completed-failed-and-nested-chats ()
  (gptel-agent-followup-test--with-chat
    (let (children)
      (dotimes (i 3)
        (gptel-agent--task deliver "researcher" "Task" "Investigate"
                           parent-fsm snapshot)
        (let* ((fsm (car requests))
               (run (plist-get (gptel-fsm-info fsm) :context)))
          (push (gptel-agent--run-child-buffer run) children)
          (pcase i
            (0
             (gptel-agent-followup-test--finish fsm "Done")
             (gptel-agent--task deliver "researcher" "Nested" "Investigate"
                                (gptel-make-fsm
                                 :info (list :buffer (car children) :position 1))
                                snapshot)
             (push (plist-get (gptel-fsm-info (car requests)) :buffer) children))
            (1 (gptel-agent--run-finish run 'failed "failure" 'test)))))
      (should (seq-every-p #'buffer-live-p children))
      (setq received nil)
      ;; No extra per-child confirmation, even with active descendants.
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (ert-fail "Unexpected child kill prompt"))))
        (kill-buffer parent))
      (should-not received)
      (should-not (seq-some #'buffer-live-p children))
      (should (= (hash-table-count gptel-agent--runs) 0)))))

(ert-deftest gptel-agent-followup-preserves-backend-message-formats ()
  (dolist (backend (list (gptel-make-openai "followup-openai" :key "unused"
                                            :host "example.invalid")
                         (gptel-make-anthropic "followup-anthropic" :key "unused")
                         (gptel-make-gemini "followup-gemini" :key "unused")
                         (gptel-make-openai-responses "followup-responses" :key "unused")))
    (dolist (already-included '(nil t))
      (let* ((key (cond ((gptel-gemini-p backend) :contents)
                        ((gptel-openai-responses-p backend) :input)
                        (t :messages)))
             (data (list key (vconcat (gptel--parse-list backend '("Original prompt")))))
             (session (gptel-agent--make-session
                       :request-info (list :data (copy-tree data t))))
             (run (gptel-agent--make-run :session session
                                         :response "Earlier commentaryFinal answer"
                                         :response-checkpoint 18)))
        (when already-included
          (gptel--inject-prompt backend data
                                (gptel--parse-list backend '((response . "Final answer")))))
        (gptel-agent--save-session-response run (list :data data :backend backend))
        (let ((messages (plist-get (plist-get (gptel-agent--session-request-info session)
                                              :data) key)))
          (should (= (length messages) 2))
          (should (string-match-p "Final answer" (prin1-to-string (elt messages 1))))
          (should-not (string-match-p "Earlier commentary" (prin1-to-string messages))))))))

(provide 'gptel-agent-followup-test)
;;; gptel-agent-followup-test.el ends here
