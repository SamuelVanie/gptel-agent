;;; gptel-agent-delegation-test.el --- Per-call child configuration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent-followup-test)
(require 'gptel-agent-workers-test)

(defconst gptel-agent-delegation-test--backends
  '(gptel-make-openai gptel-make-openai-responses
    gptel-make-anthropic gptel-make-gemini))

(defun gptel-agent-delegation-test--backend (constructor name models)
  "Create NAME using CONSTRUCTOR and MODELS, without OpenAI API auto-routing."
  (let ((backend (funcall constructor name :key "unused" :host "example.invalid"
                          :models models)))
    (should (eq (type-of backend)
                (pcase constructor
                  ('gptel-make-openai 'gptel-openai)
                  ('gptel-make-openai-responses 'gptel-openai-responses)
                  ('gptel-make-anthropic 'gptel-anthropic)
                  ('gptel-make-gemini 'gptel-gemini))))
    backend))

(cl-defmacro gptel-agent-delegation-test--with-chat (&body body)
  "Run BODY with isolated registries and a real captured Agent transport."
  (declare (indent 0))
  `(let ((gptel--known-backends nil)
         (gptel--known-presets (copy-tree gptel--known-presets))
         (gptel--known-tools (copy-tree gptel--known-tools)))
     (gptel-agent-followup-test--with-chat
       (setq snapshot (copy-tree snapshot))
       ,@body)))

(defun gptel-agent-delegation-test--start
    (callback parent-fsm snapshot &optional model extra-tools agent-id)
  "Start or resume an Agent with the public MODEL and EXTRA-TOOLS options."
  (gptel-agent--task callback "researcher" "Configured task" "Find evidence"
                     parent-fsm snapshot nil nil agent-id nil nil model extra-tools))

(defun gptel-agent-delegation-test--names (tools)
  "Return the exposed names of TOOLS."
  (mapcar #'gptel-tool-name tools))

(defun gptel-agent-delegation-test--tool (name category &optional confirm)
  "Register a harmless test tool NAME in CATEGORY with CONFIRM."
  (gptel-make-tool :name name :category category :description "Delegation test tool"
                  :args nil :function (lambda () "test tool result")
                  :confirm confirm))

(ert-deftest gptel-agent-delegation-model-advertisement-is-request-local ()
  (gptel-agent-delegation-test--with-chat
    (let ((base-args (copy-tree (gptel-tool-args (gptel-get-tool "Agent")))))
      (gptel-make-openai "First" :key "unused" :host "example.invalid" :models '(shared model:version))
      (gptel-make-anthropic "Second" :key "unused" :models '(shared))
      (with-current-buffer parent
        (setq-local gptel-agent--agent-snapshot snapshot)
        (gptel-agent--localize-agent-tool parent-fsm)
        (let* ((tool (cl-find "Agent" gptel-tools :key #'gptel-tool-name :test #'equal))
               (argument (cl-find "model" (gptel-tool-args tool)
                                  :key (lambda (arg) (plist-get arg :name)) :test #'equal))
               (extra (cl-find "extra_tools" (gptel-tool-args tool)
                               :key (lambda (arg) (plist-get arg :name)) :test #'equal)))
          (should (plist-get argument :optional))
          (should (plist-get extra :optional))
          (dolist (choice '("First:shared" "First:model:version" "Second:shared"))
            (should (string-match-p (regexp-quote choice)
                                    (plist-get argument :description))))
          (gptel-make-openai "Later" :key "unused" :host "example.invalid" :models '(later-model))
          (should-not (string-match-p "Later:later-model" (plist-get argument :description)))
          ;; Exercise named JSON arguments, not only the internal function.
          (apply (gptel-tool-function tool) deliver
                 (gptel--map-tool-args
                  tool '(:subagent_type "researcher" :description "Select backend"
                         :prompt "Investigate" :model "Second:shared"
                         :extra_tools ["Grep"])))
          (should (= (length requests) 1))
          (let ((info (gptel-fsm-info (car requests))))
            (should (eq (plist-get info :backend) (gptel-get-backend "Second")))
            (should (eq (plist-get info :model) 'shared))
            (should (member "Grep" (gptel-agent-delegation-test--names (plist-get info :tools)))))
          (gptel-agent-followup-test--finish (car requests) "Done")))
      (should (equal base-args (gptel-tool-args (gptel-get-tool "Agent")))))))

(ert-deftest gptel-agent-delegation-model-overrides-all-layers-and-survives-followup ()
  (dolist (make-backend gptel-agent-delegation-test--backends)
    (gptel-agent-delegation-test--with-chat
      (let* ((selected-model (make-symbol "shared:version"))
             (selected (gptel-agent-delegation-test--backend
                        make-backend "Selected" (list selected-model)))
             hooks)
        (put selected-model :capabilities '(tool))
        (gptel-make-openai "Preset" :key "unused" :host "example.invalid"
                          :models '(preset-model hook-model))
        (gptel-make-preset 'delegation-base :backend "Preset" :model 'preset-model
                          :temperature 0.25 :max-tokens 321)
        (gptel-make-preset 'delegation-shared :parents 'delegation-base
                          :post (lambda () (push 'shared hooks)))
        (with-current-buffer parent (setq-local gptel-agent-preset 'delegation-shared))
        (setcdr (assoc "researcher" snapshot)
                (list :description "Research" :system "Agent system"
                      :backend "Preset" :model 'preset-model
                      :post (lambda () (push 'agent hooks)
                              (setq-local gptel-model 'hook-model))))
        (gptel-agent-delegation-test--start deliver parent-fsm snapshot
                                           "Selected:shared:version")
        (should (equal (reverse hooks) '(shared agent)))
        (should (= (length requests) 1))
        (let* ((fsm (car requests)) (info (gptel-fsm-info fsm))
               (run (plist-get info :context)) (child (gptel-agent--run-child-buffer run))
               (id (gptel-agent--session-id (gptel-agent--run-session run))))
          (should (eq (plist-get info :backend) selected))
          (should (eq (plist-get info :model) selected-model))
          (should (eq (buffer-local-value 'gptel-model child) selected-model))
          (should (equal (buffer-local-value 'gptel-system-prompt child) "Agent system"))
          (should (= (buffer-local-value 'gptel-temperature child) 0.25))
          (should (= (buffer-local-value 'gptel-max-tokens child) 321))
          (should (stringp (gptel--json-encode (plist-get info :data))))
          (should (eq (buffer-local-value 'gptel-model parent) 'test-model))
          (gptel-agent-followup-test--finish fsm "First answer")
          (with-current-buffer parent
            (setq-local gptel-agent-preset 'missing-preset gptel-model 'changed-parent))
          (gptel-agent-delegation-test--start deliver parent-fsm snapshot nil nil id)
          (let ((next-info (gptel-fsm-info (car requests))))
            (should (eq (plist-get next-info :backend) selected))
            (should (eq (plist-get next-info :model) selected-model))
            (should (string-match-p "First answer"
                                    (gptel--json-encode (plist-get next-info :data)))))
          (should (equal (reverse hooks) '(shared agent)))
          (gptel-agent-followup-test--finish (car requests) "Done"))))))

(ert-deftest gptel-agent-delegation-empty-overrides-preserve-defaults ()
  (gptel-agent-delegation-test--with-chat
    (with-current-buffer parent
      (setf (gptel-backend-models gptel-backend) '(test-model preset-model))
      (setq-local gptel-agent-preset '(:model preset-model :tools ("Read"))))
    (dolist (pair '((nil nil) ("" []) (:null :null)))
      (gptel-agent-delegation-test--start deliver parent-fsm snapshot (car pair) (cadr pair))
      (let ((info (gptel-fsm-info (car requests))))
        (should (eq (plist-get info :model) 'preset-model))
        (should (equal (gptel-agent-delegation-test--names (plist-get info :tools)) '("Read"))))
      (gptel-agent-followup-test--finish (car requests) "Done"))))

(ert-deftest gptel-agent-delegation-invalid-overrides-never-launch ()
  (gptel-agent-delegation-test--with-chat
    (dolist (model '("Missing:test" "followup-test:missing" "test-model" " " 42 t))
      (gptel-agent-delegation-test--start deliver parent-fsm snapshot model)
      (should (string-prefix-p "Error:" (car received)))
      (should-not requests))
    (dolist (extra '("Read" 42 [42] [""] ["NoSuchDelegationTool"]
                    ["Read" "NoSuchDelegationTool"]))
      (gptel-agent-delegation-test--start deliver parent-fsm snapshot nil extra)
      (should (string-prefix-p "Error:" (car received)))
      (should-not requests)
      (should-not (cl-some #'gptel-agent--run-active-p (hash-table-values gptel-agent--runs))))))

(ert-deftest gptel-agent-delegation-appends-groups-without-replacing-default-tools ()
  (dolist (make-backend gptel-agent-delegation-test--backends)
    (gptel-agent-delegation-test--with-chat
      (gptel-agent-delegation-test--backend make-backend "Selected" '(test-model))
      (let ((one (gptel-agent-delegation-test--tool "DelegationOne" "delegation-group" t))
            (two (gptel-agent-delegation-test--tool "DelegationTwo" "delegation-group"))
            (registered-read (gptel-get-tool "Read"))
            (parent-tools (buffer-local-value 'gptel-tools parent))
            baseline)
        (setcdr (assoc "researcher" snapshot)
                (list :description "Research" :system "Research" :tools '("Read")
                      :post (lambda ()
                              (setq baseline (gptel--copy-tool (gptel-get-tool "Read")))
                              (setf (gptel-tool-confirm baseline) nil
                                    (gptel-tool-description baseline) "Preserve this configured copy")
                              (setq-local gptel-tools (list baseline)))))
        (gptel-agent-delegation-test--start
         deliver parent-fsm snapshot "Selected:test-model"
         ["Read" "DelegationOne" "delegation-group" "DelegationOne"])
        (should (= (length requests) 1))
        (let* ((fsm (car requests)) (info (gptel-fsm-info fsm))
               (run (plist-get info :context))
               (configured (plist-get (gptel-agent--run-configuration run) :tools))
               (names (gptel-agent-delegation-test--names (plist-get info :tools))))
          (should (eq (car configured) baseline))
          (should (equal (car names) "Read"))
          (should (= (length names) 3))
          (should (member "DelegationOne" names))
          (should (member "DelegationTwo" names))
          (should-not (gptel-tool-confirm (car configured)))
          (should (gptel-tool-confirm
                   (cl-find "DelegationOne" configured :key #'gptel-tool-name :test #'equal)))
          (should (string-match-p "DelegationOne" (gptel--json-encode (plist-get info :data))))
          (should (eq registered-read (gptel-get-tool "Read")))
          (should (eq one (gptel-get-tool "DelegationOne")))
          (should (eq two (gptel-get-tool "DelegationTwo")))
          (should (eq parent-tools (buffer-local-value 'gptel-tools parent)))
          (gptel-agent-followup-test--finish fsm "Done"))
        ;; The next sibling uses only its own configured defaults.
        (gptel-agent-delegation-test--start deliver parent-fsm snapshot)
        (should (equal (gptel-agent-delegation-test--names
                        (plist-get (gptel-fsm-info (car requests)) :tools)) '("Read")))
        (gptel-agent-followup-test--finish (car requests) "Done")))))

(ert-deftest gptel-agent-delegation-resolves-added-group-after-preset-hooks ()
  (gptel-agent-delegation-test--with-chat
    (with-current-buffer parent
      (setq-local gptel-agent-preset
                  (list :tools '("Read")
                        :post (lambda ()
                                (gptel-agent-delegation-test--tool
                                 "HookRegistered" "hook-delegation-group" t)))))
    (gptel-agent-delegation-test--start deliver parent-fsm snapshot nil ["hook-delegation-group"])
    (ert-info ((format "Spawn result: %S" received))
      (should (= (length requests) 1)))
    (should (equal (gptel-agent-delegation-test--names
                    (plist-get (gptel-fsm-info (car requests)) :tools)) '("Read" "HookRegistered")))
    (gptel-agent-followup-test--finish (car requests) "Done")))

(ert-deftest gptel-agent-delegation-keeps-tool-mode-and-additions-after-synthesis ()
  (dolist (make-backend gptel-agent-delegation-test--backends)
    (dolist (mode '(nil t force))
      (gptel-agent-delegation-test--with-chat
        (gptel-agent-delegation-test--backend make-backend "Selected" '(test-model))
        (gptel-agent-delegation-test--tool "DelegationExtra" "delegation-extra")
        (with-current-buffer parent
          (setq-local gptel-agent-preset (list :use-tools mode :tools '("Read"))
                      gptel-prompt-transform-functions '(gptel-agent--localize-skill-tool)))
        (gptel-agent-delegation-test--start deliver parent-fsm snapshot
                                           "Selected:test-model" ["DelegationExtra"])
        (let* ((first (car requests)) (info (gptel-fsm-info first))
               (run (plist-get info :context)) (child (gptel-agent--run-child-buffer run))
               (id (gptel-agent--session-id (gptel-agent--run-session run)))
               (tool-fields (mapcar (lambda (key) (cons key (plist-get (plist-get info :data) key)))
                                    '(:tool_choice :toolChoice :tool_config :toolConfig))))
          (should (eq (buffer-local-value 'gptel-use-tools child) mode))
          (should (equal (gptel-agent-delegation-test--names
                          (buffer-local-value 'gptel-tools child)) '("Read" "DelegationExtra")))
          (should (eq (and (plist-get info :tools) t) (and mode t)))
          (should (eq (and (plist-get (plist-get info :data) :tools) t) (and mode t)))
          (setf (gptel-agent--run-finalization-reason run) "enough evidence")
          (gptel-agent--begin-finalization run info)
          (should-not (buffer-local-value 'gptel-use-tools child))
          (gptel-agent-followup-test--finish first "Synthesis complete")
          (gptel-agent-delegation-test--start deliver parent-fsm snapshot nil nil id)
          (let* ((next (car requests)) (next-info (gptel-fsm-info next))
                 (data (plist-get next-info :data)))
            (should (eq (buffer-local-value 'gptel-use-tools child) mode))
            (should (equal (gptel-agent-delegation-test--names
                            (buffer-local-value 'gptel-tools child)) '("Read" "DelegationExtra")))
            (should (eq (and (plist-get next-info :tools) t) (and mode t)))
            (should (eq (and (plist-get data :tools) t) (and mode t)))
            (dolist (field tool-fields)
              (should (equal (plist-get data (car field)) (cdr field))))
            (should (stringp (gptel--json-encode data)))
            (gptel-agent-followup-test--finish next "Done")))))))

(ert-deftest gptel-agent-delegation-followup-rejects-overrides-without-changing-session ()
  (gptel-agent-delegation-test--with-chat
    (gptel-agent-delegation-test--start deliver parent-fsm snapshot)
    (let* ((first (car requests)) (run (plist-get (gptel-fsm-info first) :context))
           (session (gptel-agent--run-session run)) (id (gptel-agent--session-id session)))
      (gptel-agent-followup-test--finish first "Original answer")
      (let ((saved (copy-tree (gptel-agent--session-request-info session) t)))
        (dolist (pair '(("followup-test:test-model" nil) (nil ["Grep"])))
          (gptel-agent-delegation-test--start deliver parent-fsm snapshot (car pair) (cadr pair) id)
          (should (string-prefix-p "Error:" (car received)))
          (should (= (length requests) 1))
          (should (gptel-agent--session-resumable session))
          (should (eq (gptel-agent--session-run session) run))
          (should (equal saved (gptel-agent--session-request-info session)))))
      (gptel-agent-delegation-test--start deliver parent-fsm snapshot "" [] id)
      (should (= (length requests) 2))
      (gptel-agent-followup-test--finish (car requests) "Done"))))

(ert-deftest gptel-agent-delegation-preserves-mode-changed-by-prompt-transform ()
  (dolist (mode '(nil force))
    (gptel-agent-delegation-test--with-chat
      (with-current-buffer parent
        (setq-local gptel-agent-preset '(:tools ("Read") :use-tools t)
                    gptel-prompt-transform-functions
                    (list (lambda (_fsm) (setq-local gptel-use-tools mode)))))
      (gptel-agent-delegation-test--start deliver parent-fsm snapshot nil ["Grep"])
      (let* ((first (car requests)) (info (gptel-fsm-info first))
             (run (plist-get info :context))
             (id (gptel-agent--session-id (gptel-agent--run-session run))))
        (should (eq (plist-get (gptel-agent--run-configuration run) :use-tools) mode))
        (should (eq (and (plist-get info :tools) t) (and mode t)))
        (should (eq (and (plist-get (plist-get info :data) :tools) t) (and mode t)))
        (gptel-agent-followup-test--finish first "First answer")
        (gptel-agent-delegation-test--start deliver parent-fsm snapshot nil nil id)
        (let* ((next (car requests)) (next-info (gptel-fsm-info next)))
          (should (eq (buffer-local-value 'gptel-use-tools (gptel-agent--run-child-buffer run)) mode))
          (should (eq (and (plist-get next-info :tools) t) (and mode t)))
          (should (eq (and (plist-get (plist-get next-info :data) :tools) t) (and mode t)))
          (when mode
            (should (equal (plist-get (plist-get next-info :data) :tool_choice) "required")))
          (gptel-agent-followup-test--finish next "Done"))))))

(ert-deftest gptel-agent-delegation-added-tool-executes-through-every-provider ()
  (dolist (make-backend gptel-agent-delegation-test--backends)
    (gptel-agent-delegation-test--with-chat
      (gptel-agent-delegation-test--backend make-backend "Selected" '(test-model))
      (let ((executions 0))
        (gptel-make-tool :name "DelegationRun" :category "delegation-run"
                        :description "Return a test result" :args nil
                        :function (lambda () (cl-incf executions) "ADDED-TOOL-EVIDENCE"))
        (with-current-buffer parent
          (setq-local gptel-agent-preset '(:tools ("Read")) gptel-confirm-tool-calls nil))
        (gptel-agent-delegation-test--start deliver parent-fsm snapshot
                                           "Selected:test-model" ["delegation-run"])
        (let* ((fsm (car requests)) (info (gptel-fsm-info fsm))
               (run (plist-get info :context)))
          (gptel-agent-workers-test--tool fsm "DelegationRun" nil)
          (should (= executions 1))
          (should (= (length requests) 2))
          (should (string-match-p "ADDED-TOOL-EVIDENCE"
                                  (gptel--json-encode (plist-get info :data))))
          (should (gptel-agent--run-active-p run))
          (gptel-agent-workers-test--answer fsm "Verified added tool")
          (should (eq (gptel-agent--run-state run) 'completed))
          (should (string-match-p "Verified added tool" (car received))))))))

(ert-deftest gptel-agent-delegation-background-keeps-overrides-and-delivers-once ()
  (let ((gptel--known-backends nil)
        (gptel--known-tools (copy-tree gptel--known-tools)))
    (let ((selected (gptel-make-anthropic "BackgroundSelected" :key "unused"
                                         :models '(background-model))))
      (gptel-agent-delegation-test--tool "BackgroundExtra" "background-extra")
      (gptel-agent-workers-test--with-chat
        (gptel-agent-workers-test--tool
         fsm "Agent" '(:subagent_type "researcher" :description "Background task"
                       :prompt "Gather evidence" :background t
                       :model "BackgroundSelected:background-model"
                       :extra_tools ["BackgroundExtra"]))
        (let* ((run (car (hash-table-values gptel-agent--runs)))
               (child (gptel-agent--run-fsm run))
               (info (gptel-fsm-info child)))
          (should (gptel-agent--run-background run))
          (should (eq (plist-get info :backend) selected))
          (should (eq (plist-get info :model) 'background-model))
          (should (member "BackgroundExtra"
                          (gptel-agent-delegation-test--names (plist-get info :tools))))
          (should (string-match-p "agent_id:" (gptel-agent-workers-test--data fsm)))
          (let ((before (length requests)))
            (gptel-agent-workers-test--tool fsm "AgentControl" '(:action "wait"))
            (should (= before (length requests))))
          (gptel-agent-workers-test--answer child "CONFIGURED-BACKGROUND-RESULT")
          (gptel-agent-workers-test--wake fsm)
          (should (eq (gptel-agent--run-state run) 'completed))
          (should (= 1 (gptel-agent-workers-test--count
                        "CONFIGURED-BACKGROUND-RESULT" (gptel-agent-workers-test--data fsm))))
          (should-not (gptel-agent--workers-events (gptel-agent--workers-for fsm))))))))

(ert-deftest gptel-agent-delegation-added-special-tools-belong-to-child ()
  (gptel-agent-delegation-test--with-chat
    (let ((gptel-default-mode #'text-mode)
          (gptel-post-response-functions nil)
          (gptel-post-request-hook nil)
          (gptel-pre-response-hook nil)
          discussion)
      (with-current-buffer parent
        (setq-local gptel-prompt-transform-functions nil
                    gptel-agent-preset '(:tools ("Read") :prompt-transform-functions nil)
                    gptel-confirm-tool-calls nil))
      (setf (plist-get (cdr (assoc "researcher" snapshot)) :agents) '("test-worker"))
      (gptel-agent-delegation-test--start
       deliver parent-fsm snapshot nil ["Skill" "Agent" "Discussion" "Bash" "Process" "MainChat"])
      (let* ((fsm (car requests)) (info (gptel-fsm-info fsm))
             (run (plist-get info :context)) (child (gptel-agent--run-child-buffer run))
             (tools (plist-get info :tools))
             (agent (cl-find "Agent" tools :key #'gptel-tool-name :test #'equal))
             observed result)
        (unwind-protect
            (progn
              (should-not (member "MainChat" (gptel-agent-delegation-test--names tools)))
              (should (equal (plist-get (car (gptel-tool-args agent)) :enum) ["test-worker"]))
              ;; Call from an unrelated approval buffer: dispatch must enter
              ;; the child and retain its FSM, never the parent's request.
              (with-temp-buffer
                (cl-letf (((symbol-function 'gptel-agent--execute-bash)
                           (lambda (_callback _command _query _background owner)
                             (setq observed (list (current-buffer) owner)))))
                  (funcall (gptel-tool-function
                            (cl-find "Bash" tools :key #'gptel-tool-name :test #'equal))
                           #'ignore "printf ignored" "Check owner")
                  (should (equal observed (list child fsm))))
                (cl-letf (((symbol-function 'gptel-agent--process-tool)
                           (lambda (&rest args)
                             (setq observed (list (current-buffer) (car (last args)))))))
                  (funcall (gptel-tool-function
                            (cl-find "Process" tools :key #'gptel-tool-name :test #'equal))
                           #'ignore "list")
                  (should (equal observed (list child fsm))))
                (cl-letf (((symbol-function 'gptel-agent--get-skill)
                           (lambda (_skill _args owner _registry) (setq observed owner))))
                  (funcall (gptel-tool-function
                            (cl-find "Skill" tools :key #'gptel-tool-name :test #'equal))
                           "delegation-test-skill")
                  (should (eq observed fsm)))
                (funcall (gptel-tool-function agent) (lambda (value) (setq result value))
                         "researcher" "Disallowed self" "Investigate")
                (should (string-prefix-p "Error:" result))
                (funcall (gptel-tool-function
                          (cl-find "Discussion" tools :key #'gptel-tool-name :test #'equal))
                         (lambda (value) (setq result value)) "Nested discussion" "Investigate"))
              (setq discussion (car (buffer-local-value 'gptel-agent-discussion--children child)))
              (ert-info ((format "Discussion result: %S" result)) (should discussion))
              (should (eq (gptel-agent-discussion--chat-owner discussion) child))
              (should-not (buffer-local-value 'gptel-agent-discussion--children parent))
              (should (buffer-live-p (gptel-agent-discussion--chat-buffer discussion)))
              (gptel-agent-workers-test--answer
               (gptel-agent-discussion--chat-fsm discussion) "Nested answer")
              (gptel-agent-followup-test--finish fsm "Done"))
          (when discussion
            (setf (gptel-agent-discussion--chat-owner discussion) nil)
            (when (buffer-live-p (gptel-agent-discussion--chat-buffer discussion))
              (kill-buffer (gptel-agent-discussion--chat-buffer discussion)))))))))

(ert-deftest gptel-agent-delegation-preserves-post-localizer-wrappers ()
  (dolist (extra-tools '(nil ["DelegationWrapperExtra"]))
    (gptel-agent-delegation-test--with-chat
      (let* ((executions 0)
             (wrapper (lambda (callback _command _query &optional _background)
                        (cl-incf executions)
                        (funcall callback "CUSTOM-WRAPPER-RESULT")))
             result)
        (gptel-agent-delegation-test--tool "DelegationWrapperExtra" "delegation-wrapper")
        (with-current-buffer parent
          (setq-local
           gptel-agent-preset '(:tools ("Bash") :use-tools t)
           gptel-prompt-transform-functions
           (list #'gptel-agent--localize-agent-tool
                 (lambda (_fsm)
                   (setf (gptel-tool-function
                          (cl-find "Bash" gptel-tools :key #'gptel-tool-name :test #'equal))
                         wrapper)
                   (setq-local gptel-use-tools 'force)))))
        (gptel-agent-delegation-test--start deliver parent-fsm snapshot nil extra-tools)
        (let* ((fsm (car requests)) (info (gptel-fsm-info fsm))
               (run (plist-get info :context))
               (tools (plist-get info :tools))
               (bash (cl-find "Bash" tools :key #'gptel-tool-name :test #'equal)))
          (should (eq (gptel-tool-function bash) wrapper))
          (should (eq (plist-get (gptel-agent--run-configuration run) :use-tools) 'force))
          (should (equal (plist-get (plist-get info :data) :tool_choice) "required"))
          (should (= (cl-count "DelegationWrapperExtra" tools
                              :key #'gptel-tool-name :test #'equal)
                     (if extra-tools 1 0)))
          (cl-letf (((symbol-function 'gptel-agent--execute-bash)
                     (lambda (&rest _) (ert-fail "The custom wrapper was overwritten"))))
            (funcall (gptel-tool-function bash) (lambda (value) (setq result value))
                     "unused" "Verify wrapper"))
          (should (= executions 1))
          (should (equal result "CUSTOM-WRAPPER-RESULT"))
          (gptel-agent-followup-test--finish fsm "Done"))))))

(provide 'gptel-agent-delegation-test)
;;; gptel-agent-delegation-test.el ends here
