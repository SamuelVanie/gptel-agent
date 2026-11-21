;;; gptel-agent-tool-continuation-test.el --- Tool narration history -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent-workers-test)
(require 'gptel-agent-questions-test)

(defun gptel-agent-tool-continuation-test--event (name data)
  "Encode an SSE event with NAME and DATA."
  (concat (when name (concat "event: " name "\n"))
          "data: " (gptel--json-encode data) "\n\n"))

(defun gptel-agent-tool-continuation-test--stream (backend text id)
  "Return separate text and tool stream chunks for BACKEND, TEXT and ID."
  (let ((event #'gptel-agent-tool-continuation-test--event))
    (cond
     ((gptel-openai-responses-p backend)
      (list
       (funcall event "response.output_text.delta" `(:delta ,text))
       (concat
        (funcall event "response.output_item.done"
                 `(:item (:type "function_call" :call_id ,id
                               :name "WorkerTestTool" :arguments "{}")))
        (funcall event "response.completed" '(:response (:status "completed"))))))
     ((gptel-anthropic-p backend)
      (list
       (concat
        (funcall event "content_block_start"
                 '(:index 0 :content_block (:type "text" :text "")))
        (funcall event "content_block_delta"
                 `(:index 0 :delta (:type "text_delta" :text ,text)))
        (funcall event "content_block_stop" '(:index 0)))
       (concat
        (funcall event "content_block_start"
                 `(:index 1 :content_block (:type "tool_use" :id ,id
                                                :name "WorkerTestTool" :input ())))
        (funcall event "content_block_delta"
                 '(:index 1 :delta (:type "input_json_delta" :partial_json "{}")))
        (funcall event "content_block_stop" '(:index 1))
        (funcall event "message_delta" '(:delta (:stop_reason "tool_use"))))))
     ((gptel-gemini-p backend)
      (list
       (gptel--json-encode `(:candidates [(:content (:role "model" :parts [(:text ,text)]))]))
       (gptel--json-encode
        '(:candidates [(:content (:role "model" :parts [(:functionCall (:name "WorkerTestTool" :args ()))])
                                 :finishReason "STOP")]))))
     (t
      (list
       (funcall event nil `(:choices [(:delta (:role "assistant" :content ,text))]))
       (concat
        (funcall event nil
                 `(:choices [(:delta (:tool_calls [(:index 0 :id ,id :type "function"
                                                           :function (:name "WorkerTestTool" :arguments "{}"))]))]))
        "data: [DONE]\n\n"))))))

(defun gptel-agent-tool-continuation-test--response (backend text id)
  "Return a complete tool response for BACKEND, TEXT and ID."
  (cond
   ((gptel-openai-responses-p backend)
    `(:status "completed"
              :output [(:type "message" :role "assistant"
                              :content [(:type "output_text" :text ,text)])
                       (:type "function_call" :call_id ,id :name "WorkerTestTool" :arguments "{}")]))
   ((gptel-anthropic-p backend)
    `(:content [(:type "text" :text ,text)
                (:type "tool_use" :id ,id :name "WorkerTestTool" :input ())]
               :stop_reason "tool_use"))
   ((gptel-gemini-p backend)
    `(:candidates [(:content (:role "model" :parts [(:text ,text)
                                                  (:functionCall (:name "WorkerTestTool" :args ()))])
                             :finishReason "STOP")]))
   (t
    `(:choices [(:message (:role "assistant" :content ,text
                                :tool_calls [(:id ,id :type "function"
                                                  :function (:name "WorkerTestTool" :arguments "{}"))])
                          :finish_reason "tool_calls")]))))

(defun gptel-agent-tool-continuation-test--deliver (fsm text id stream split)
  "Deliver TEXT and a tool call through FSM's real backend parser.
Use the streaming parser when STREAM is non-nil, with separate network
chunks when SPLIT is non-nil.  Network transport alone is mocked."
  (let* ((info (gptel-fsm-info fsm))
         (backend (plist-get info :backend)))
    (gptel--fsm-transition fsm 'TYPE)
    (if stream
        (let* ((chunks (gptel-agent-tool-continuation-test--stream backend text id))
               (chunks (if split chunks (list (apply #'concat chunks)))))
          (dolist (chunk chunks)
            (let ((response (with-temp-buffer
                              (insert chunk)
                              (goto-char (point-min))
                              (gptel-curl--parse-stream backend info))))
              (unless (equal response "")
                (funcall (plist-get info :callback) response info))))
          (funcall (plist-get info :callback) t info))
      (funcall (plist-get info :callback)
               (gptel--parse-response
                backend (gptel-agent-tool-continuation-test--response backend text id) info)
               info))
    (gptel--fsm-transition fsm)))

(defun gptel-agent-tool-continuation-test--check (backend stream)
  "Check BACKEND keeps narration exactly once with STREAM enabled or disabled."
  (dolist (immediate '(nil t))
    (dolist (split (if stream '(nil t) '(nil)))
      (ert-info ((format "backend=%s stream=%s split=%s immediate=%s"
                         (gptel-backend-name backend) stream split immediate))
        (let ((gptel-agent-workers-test--backend backend)
              (gptel-agent-workers-test--stream stream))
          (gptel-agent-workers-test--with-chat
            ;; Nonblocking tools acknowledge synchronously; deferred tools invoke
            ;; the same result callback later from the event loop.
            (when immediate
              (let ((tool (cl-find "WorkerTestTool" (plist-get (gptel-fsm-info fsm) :tools)
                                   :key #'gptel-tool-name :test #'equal))
                    (round 0))
                (setf (gptel-tool-function tool)
                      (lambda (callback)
                        (funcall callback (format "TOOL-RESULT-%d" (cl-incf round)))))))
            (cl-loop
             for text in (list "FIRST-NARRATION" "SECOND-NARRATION")
             for round from 1
             do
             (gptel-agent-tool-continuation-test--deliver
              fsm text (format "continuation-call-%d" round) stream split)
             (should (= 1 (with-current-buffer parent
                            (gptel-agent-workers-test--count text (buffer-string)))))
             (unless immediate
               (should (eq (gptel-fsm-state fsm) 'TOOL))
               (should (= round (length requests)))
               (should tool-callback)
               (funcall tool-callback (format "TOOL-RESULT-%d" round)))
             (should (eq (gptel-fsm-state fsm) 'WAIT))
             (should (= (1+ round) (length requests)))
             (let ((payload (prin1-to-string (cdar requests))))
               (should (= 1 (gptel-agent-workers-test--count text payload)))
               (should (< (string-match text payload)
                          (string-match (format "TOOL-RESULT-%d" round) payload)))
               (should (= 1 (gptel-agent-workers-test--count "FIRST-NARRATION" payload)))))
            (gptel-agent-workers-test--answer fsm "FINAL-ANSWER" stream)
            (should (eq (gptel-fsm-state fsm) 'DONE))))))))

(ert-deftest gptel-agent-tool-continuation-openai-stream ()
  (gptel-agent-tool-continuation-test--check
   (gptel-make-openai "continuation-openai" :key "unused" :host "example.invalid"
                      :models '(test) :stream t) t))

(ert-deftest gptel-agent-tool-continuation-openai-nonstream ()
  (gptel-agent-tool-continuation-test--check
   (gptel-make-openai "continuation-openai" :key "unused" :host "example.invalid"
                      :models '(test) :stream t) nil))

(ert-deftest gptel-agent-tool-continuation-openai-after-system-message-removal ()
  (let ((gptel-system-prompt "MANAGED-SYSTEM-PROMPT")
        (gptel-agent-workers-test--backend
         (gptel-make-openai "continuation-openai" :key "unused" :host "example.invalid"
                            :models '(test) :stream t))
        (gptel-agent-workers-test--stream t))
    (gptel-agent-workers-test--with-chat
      (let* ((info (gptel-fsm-info fsm))
             (data (plist-get info :data))
             (messages (plist-get data :messages)))
        (should (equal (plist-get (elt messages 0) :role) "system"))
        (should (= (plist-get info :agent-tool-prompt-count) (length messages)))
        ;; Managed instructions can remove a system message after the gate's
        ;; WAIT handler records its boundary, before the transport sends it.
        (plist-put data :messages (seq-subseq messages 1)))
      (gptel-agent-tool-continuation-test--deliver
       fsm "NARRATION-AFTER-SHRINK" "shrink-call" t nil)
      (should (eq (gptel-fsm-state fsm) 'TOOL))
      (funcall tool-callback "SHRINK-TOOL-RESULT")
      (should (eq (gptel-fsm-state fsm) 'WAIT))
      (let ((payload (prin1-to-string (cdar requests))))
        (should (= 1 (gptel-agent-workers-test--count "NARRATION-AFTER-SHRINK" payload)))
        (should (< (string-match "NARRATION-AFTER-SHRINK" payload)
                   (string-match "SHRINK-TOOL-RESULT" payload))))
      (gptel-agent-workers-test--answer fsm "FINAL-ANSWER" t))))

(ert-deftest gptel-agent-tool-continuation-responses-stream ()
  (gptel-agent-tool-continuation-test--check
   (gptel-make-openai-responses "continuation-responses" :key "unused" :models '(test) :stream t) t))

(ert-deftest gptel-agent-tool-continuation-responses-nonstream ()
  (gptel-agent-tool-continuation-test--check
   (gptel-make-openai-responses "continuation-responses" :key "unused" :models '(test) :stream t) nil))

(ert-deftest gptel-agent-tool-continuation-openai-oauth ()
  (skip-unless (require 'gptel-openai-oauth nil t))
  ;; Construct directly: the test needs the real subtype and parser dispatch,
  ;; but must never load account tokens or initiate authentication.
  (let ((backend (gptel--make-openai-oauth
                  :name "continuation-oauth" :key "unused" :models '(test) :stream t)))
    (dolist (stream '(nil t))
      (gptel-agent-tool-continuation-test--check backend stream))))

(ert-deftest gptel-agent-tool-continuation-anthropic-stream ()
  (gptel-agent-tool-continuation-test--check
   (gptel-make-anthropic "continuation-anthropic" :key "unused" :models '(test) :stream t) t))

(ert-deftest gptel-agent-tool-continuation-anthropic-nonstream ()
  (gptel-agent-tool-continuation-test--check
   (gptel-make-anthropic "continuation-anthropic" :key "unused" :models '(test) :stream t) nil))

(ert-deftest gptel-agent-tool-continuation-gemini-stream ()
  (gptel-agent-tool-continuation-test--check
   (gptel-make-gemini "continuation-gemini" :key "unused" :models '(test) :stream t) t))

(ert-deftest gptel-agent-tool-continuation-gemini-nonstream ()
  (gptel-agent-tool-continuation-test--check
   (gptel-make-gemini "continuation-gemini" :key "unused" :models '(test) :stream t) nil))

(defun gptel-agent-tool-continuation-test--named-tool (fsm text name args)
  "Deliver TEXT and a named tool call with ARGS through a Responses FSM."
  (let* ((info (gptel-fsm-info fsm))
         (response
          (gptel--parse-response
           (plist-get info :backend)
           `(:status "completed"
                     :output [(:type "message" :role "assistant"
                                     :content [(:type "output_text" :text ,text)])
                              (:type "function_call" :call_id "named-call" :name ,name
                                     :arguments ,(gptel--json-encode args))])
           info)))
    (gptel--fsm-transition fsm 'TYPE)
    (funcall (plist-get info :callback) response info)
    (gptel--fsm-transition fsm)))

(ert-deftest gptel-agent-tool-continuation-ask-add-keeps-question-introduction ()
  (let ((gptel-agent-workers-test--backend
         (gptel-make-openai-responses "continuation-ask" :key "unused" :models '(test))))
    (gptel-agent-questions-test--with-chat
      (gptel-agent-tool-continuation-test--named-tool
       fsm "QUESTION-INTRODUCTION" "AskUserQuestion"
       '(:questions [(:question "Which color?" :choices [(:value "Blue") (:value "Red")])]))
      (should (eq (gptel-fsm-state fsm) 'WAIT))
      (should (= 2 (length requests)))
      (should (plist-get (gptel-fsm-info fsm) :agent-questions))
      (should (= 1 (gptel-agent-workers-test--count
                    "QUESTION-INTRODUCTION" (gptel-agent-workers-test--data fsm))))
      (should (= 1 (with-current-buffer source
                     (gptel-agent-workers-test--count "QUESTION-INTRODUCTION" (buffer-string))))))))

(ert-deftest gptel-agent-tool-continuation-background-agent-keeps-delegation-introduction ()
  (let ((gptel-agent-workers-test--backend
         (gptel-make-openai-responses "continuation-agent" :key "unused" :models '(test))))
    (gptel-agent-workers-test--with-chat
      (gptel-agent-tool-continuation-test--named-tool
       fsm "DELEGATION-INTRODUCTION" "Agent"
       '(:subagent_type "researcher" :description "Find evidence"
                         :prompt "Investigate the issue" :background t))
      (should (eq (gptel-fsm-state fsm) 'WAIT))
      ;; Initial parent request, worker request and the parent's continuation.
      (should (= 3 (length requests)))
      (should (= 1 (length (gptel-agent--workers-active (gptel-agent--workers-for fsm)))))
      (should (= 1 (gptel-agent-workers-test--count
                    "DELEGATION-INTRODUCTION" (prin1-to-string (cdar requests)))))
      (should (= 1 (with-current-buffer parent
                     (gptel-agent-workers-test--count "DELEGATION-INTRODUCTION" (buffer-string))))))))

(ert-deftest gptel-agent-tool-continuation-bash-only-plain-request ()
  (let ((source (generate-new-buffer " *continuation-plain*"))
        (fsm (gptel-make-fsm))
        requests responses)
    (unwind-protect
        (cl-letf (((symbol-function 'gptel-curl-get-response)
                   (lambda (request) (push request requests)))
                  ((symbol-function 'gptel-agent--execute-bash)
                   (lambda (callback &rest _) (funcall callback "BASH-RESULT"))))
          (with-current-buffer source
            (setq-local gptel-backend
                        (gptel-make-openai-responses "continuation-bash" :key "unused" :models '(test))
                        gptel-model 'test gptel-use-curl t gptel-use-tools t
                        gptel-confirm-tool-calls nil
                        gptel-pre-tool-call-functions nil gptel-post-tool-call-functions nil
                        gptel-tools (list (gptel-get-tool "Bash")))
            (gptel-request "Run a command" :fsm fsm :buffer source
              :transforms '(gptel-agent--localize-agent-tool)
              :callback (lambda (response _info) (push response responses))))
          (should (gptel-agent--workers-for fsm))
          (gptel-agent-tool-continuation-test--named-tool
           fsm "COMMAND-INTRODUCTION" "Bash" '(:command "true" :query "Check success"))
          (should (eq (gptel-fsm-state fsm) 'WAIT))
          (should (= 2 (length requests)))
          (should (= 1 (cl-count "COMMAND-INTRODUCTION" responses :test #'equal)))
          (should (= 1 (gptel-agent-workers-test--count
                        "COMMAND-INTRODUCTION" (gptel-agent-workers-test--data fsm)))))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest gptel-agent-tool-continuation-process-wait-timeout-and-exit ()
  (gptel-agent-workers-test--with-chat
    (cl-labels ((wait-for (predicate)
                 (let ((deadline (+ (float-time) 5)))
                   (while (and (not (funcall predicate)) (< (float-time) deadline))
                     (accept-process-output nil 0.02))
                   (should (funcall predicate)))))
      (let* ((process
              (with-current-buffer parent
                (setq-local gptel-tools (list (gptel-get-tool "Process"))
                            gptel-pre-tool-call-functions
                            (list #'gptel-agent--detect-repetition))
                (gptel-agent--localize-agent-tool fsm)
                (setf (plist-get (gptel-fsm-info fsm) :tools) gptel-tools)
                (gptel-agent--execute-bash
                 #'ignore "printf ready; read -r answer; printf finished; exit 7"
                 "Check completion" t)))
             (job (process-get process 'gptel-agent-job))
             ;; Keep the reported argument spelling: unknown max_char is
             ;; ignored by schema dispatch and must not disrupt continuation.
             (args (list :until "exit" :action "wait"
                         :process_id (gptel-agent--job-id job) :input ""
                         :offset 0 :max_char 3000 :view "unread" :timeout_ms 1000)))
        (should (= (length requests) 1))
        (gptel-agent-workers-test--tool fsm "Process" args)
        (let* ((call (car (plist-get (gptel-fsm-info fsm) :tool-use)))
               (waiter (car (gptel-agent--job-waiters job)))
               (timer (gptel-agent--process-waiter-timer waiter)))
          (should (eq (gptel-fsm-state fsm) 'TOOL))
          (should (timerp timer))
          (wait-for (lambda () (= (length requests) 2)))
          (should (eq (gptel-fsm-state fsm) 'WAIT))
          (should (string-match-p "wait_reason: timeout" (plist-get call :result)))
          (should (string-match-p "status: run" (plist-get call :result)))
          (should (string-suffix-p "ready" (plist-get call :result)))
          (should (string-match-p "wait_reason: timeout" (prin1-to-string (cdar requests))))
          (should (process-live-p process))
          (should-not (gptel-agent--job-waiters job))
          (should-not (gptel-agent--process-waiter-timer waiter))
          (should-not (memq timer timer-list)))
        ;; A subsequent identical wait must use the same live job and resume
        ;; the model again when the process exits, without a stale timer firing.
        (gptel-agent-workers-test--tool fsm "Process" args)
        (let* ((call (car (plist-get (gptel-fsm-info fsm) :tool-use)))
               (waiter (car (gptel-agent--job-waiters job)))
               (timer (gptel-agent--process-waiter-timer waiter)))
          (should (eq (gptel-fsm-state fsm) 'TOOL))
          (process-send-string process "continue\n")
          (wait-for (lambda () (= (length requests) 3)))
          (should (eq (gptel-fsm-state fsm) 'WAIT))
          (should (string-match-p "wait_reason: exit" (plist-get call :result)))
          (should (string-match-p "status: exit\nexit_code: 7" (plist-get call :result)))
          (should (string-suffix-p "readyfinished" (plist-get call :result)))
          (let ((payload (prin1-to-string (cdar requests))))
            (should (string-match-p "wait_reason: timeout" payload))
            (should (string-match-p "wait_reason: exit" payload)))
          (should-not (gptel-agent--job-waiters job))
          (should-not (gptel-agent--process-waiter-timer waiter))
          (should-not (memq timer timer-list)))
        (gptel-agent-workers-test--answer fsm "The command exited with code 7.")
        (should (eq (gptel-fsm-state fsm) 'DONE))))))

(ert-deftest gptel-agent-tool-continuation-keeps-complete-signed-blocks ()
  (let* ((content [(:type "thinking" :thinking "Reasoning" :signature "OPAQUE-SIGNATURE")
                  (:type "text" :text "Before" :citations [(:type "page_location" :page 1)])
                  (:type "tool_use" :id "call-1" :name "Read" :input (:path "one"))
                  (:type "text" :text "After")
                  (:type "tool_use" :id "call-2" :name "Read" :input (:path "two"))])
         (message (list :role "assistant" :content content))
         (fsm (gptel-make-fsm
               :info (list :data (list :messages (vector '(:role "user" :content "Prompt") message))
                           :agent-tool-prompt-count 1
                           :tool-use '((:id "call-1") (:id "call-2")))))
         (gate (gptel-agent--make-workers :fsm fsm :answer "BeforeAfter"))
         (original (copy-tree content t)))
    (gptel-agent--workers-save-tool-answer gate)
    (should (eq content (plist-get message :content)))
    (should (equal content original))))

(provide 'gptel-agent-tool-continuation-test)
;;; gptel-agent-tool-continuation-test.el ends here
