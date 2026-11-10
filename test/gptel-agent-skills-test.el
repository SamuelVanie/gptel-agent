;;; gptel-agent-skills-test.el --- Skill tool activation -*- lexical-binding: t; -*-

(require 'ert)
(require 'yaml)
(require 'gptel-agent-discussion-test)
(require 'gptel-agent-followup-test)

(defvar gptel-agent-skills-test--extra-tools nil)
(defvar gptel-agent-skills-test--effects nil)

(cl-defmacro gptel-agent-skills-test--with-chat (header &body body)
  "Run BODY with a skill containing HEADER and real request/tool dispatch."
  (declare (indent 1))
  `(let* ((root (make-temp-file "gptel-skills-" t))
          (file (expand-file-name "SKILL.md" root))
          (parent (generate-new-buffer " *skill-parent*"))
          (gptel-agent--skills `(("test-skill" ,root :description "Test capability")))
          (gptel-agent--enabled-skills nil)
          (gptel-agent--skill-file-cache nil)
          (gptel--known-tools (copy-tree gptel--known-tools))
          (gptel-agent--runs (make-hash-table :test #'equal))
          (gptel-default-mode #'text-mode)
          (gptel-post-response-functions nil) (gptel-post-request-hook nil)
          (gptel-pre-response-hook nil) (gptel-log-level nil)
          (gptel-prompt-prefix-alist '((fundamental-mode . "USER> ")))
          (gptel-response-prefix-alist '((fundamental-mode . "ASSISTANT> ")))
          (executions 0) requests fsm)
     (unwind-protect
         (cl-letf (((symbol-function 'gptel--update-status) #'ignore)
                   ((symbol-function 'gptel-curl-get-response)
                    (lambda (request)
                      (let ((info (gptel-fsm-info request)))
                        (unless (plist-get info :callback)
                          (plist-put info :callback #'gptel--insert-response)))
                      (push (cons request (gptel--json-encode
                                           (plist-get (gptel-fsm-info request) :data)))
                            requests))))
           (with-temp-file file
             (insert "---\nname: test-skill\ndescription: Test capability\n"
                     ,header "---\nSKILL-WORKFLOW-INSTRUCTIONS\n"))
           (gptel-make-tool :name "SkillMutation" :category "skill-test"
                            :description "Test mutation" :confirm t :args nil
                            :function (lambda () (cl-incf executions) "Changed"))
           (with-current-buffer parent
             (setq-local gptel-backend
                         (or gptel-agent-workers-test--backend
                             (gptel-make-openai "skills-test" :key "unused" :models '(test)))
                         gptel-agent--agent-snapshot
                         '(("researcher" :description "Research" :system "Research carefully"
                            :task-timeout nil :max-request-rounds nil))
                         gptel-model 'test gptel-mode t gptel-use-tools t
                         gptel-use-curl t gptel-stream nil gptel-agent-preset nil
                         gptel-confirm-tool-calls 'auto
                         gptel-pre-tool-call-functions nil gptel-post-tool-call-functions nil
                         gptel-prompt-transform-functions
                         '(gptel-agent--localize-skill-tool gptel-agent--localize-agent-tool
                           gptel-agent-discussion--localize)
                         gptel-tools (cons (gptel-get-tool "Skill")
                                           (mapcar #'gptel-get-tool
                                                   gptel-agent-skills-test--extra-tools)))
             (insert "USER> Initial request")
             (gptel-send)
             (setq fsm (caar requests)))
           ,@body)
       (when (buffer-live-p parent)
         (dolist (chat (buffer-local-value 'gptel-agent-discussion--children parent))
           (setf (gptel-agent-discussion--chat-owner chat) nil)
           (when (buffer-live-p (gptel-agent-discussion--chat-buffer chat))
             (kill-buffer (gptel-agent-discussion--chat-buffer chat))))
         (kill-buffer parent))
       (delete-directory root t))))

(defun gptel-agent-skills-test--tool (fsm name)
  (cl-find name (plist-get (gptel-fsm-info fsm) :tools)
           :key #'gptel-tool-name :test #'equal))

(defun gptel-agent-skills-test--load (fsm)
  (gptel-agent-workers-test--tool fsm "Skill" '(:skill "test-skill")))

(defun gptel-agent-skills-test--wire-names (fsm)
  "Inspect the JSON wire schema, including each provider's nesting."
  (let* ((backend (plist-get (gptel-fsm-info fsm) :backend))
         (data (json-parse-string (gptel--json-encode (plist-get (gptel-fsm-info fsm) :data))
                                  :object-type 'plist))
         (tools (plist-get data :tools)))
    (when (gptel-gemini-p backend)
      (setq tools (plist-get (aref tools 0) :functionDeclarations)))
    (mapcar (lambda (tool)
              (plist-get (or (plist-get tool :function) tool) :name))
            (append tools nil))))

(ert-deftest gptel-agent-skills-activation-preserves-configured-tools ()
  (dolist (local '(nil t))
    (let ((gptel-agent-skills-test--extra-tools '("Read" "Agent")))
      (gptel-agent-skills-test--with-chat "allowed-tools: Discussion DiscussionControl\n"
        ;; Users can configure tools globally as well as buffer-locally.
        (let ((gptel-tools (buffer-local-value 'gptel-tools parent)))
          (unless local
            (with-current-buffer parent (kill-local-variable 'gptel-tools)))
          (gptel-agent-skills-test--load fsm)
          (dolist (name '("Skill" "Read" "Agent"))
            (should (member name (mapcar #'gptel-tool-name
                                        (buffer-local-value 'gptel-tools parent)))))
          (dolist (name '("Skill" "Read" "Agent" "AgentControl"
                          "Discussion" "DiscussionControl"))
            (should (member name (gptel-agent-skills-test--wire-names fsm))))
          (gptel-agent-workers-test--answer fsm "Ready")
          (with-current-buffer parent
            (goto-char (point-max)) (insert "Continue") (gptel-send)
            (setq fsm (caar requests)))
          (dolist (name '("Skill" "Read" "Agent" "AgentControl"
                          "Discussion" "DiscussionControl"))
            (should (member name (gptel-agent-skills-test--wire-names fsm)))))))))

(ert-deftest gptel-agent-skills-discussion-inherits-current-tools ()
  (let ((gptel-agent-skills-test--extra-tools '("Read")))
    (gptel-agent-skills-test--with-chat "allowed-tools: Discussion DiscussionControl\n"
      (gptel-agent-skills-test--load fsm)
      ;; Activate another tool after Discussion's dispatcher has been bound.
      (with-temp-file file
        (insert "---\nname: test-skill\ndescription: Test capability\n"
                "allowed-tools: SkillMutation\n---\nMore tools\n"))
      (gptel-agent-skills-test--load fsm)
      (dolist (preset '(nil ""))
        (gptel-agent-workers-test--tool
         fsm "Discussion" (append '(:title "Inherited tools" :prompt "Independent")
                                  (and preset (list :preset preset))))
        (let* ((chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
               (child (gptel-agent-discussion--chat-buffer chat))
               (child-fsm (gptel-agent-discussion--chat-fsm chat)))
          (dolist (name '("Skill" "Read" "Discussion" "DiscussionControl"
                          "SkillMutation" "DiscussionReport"))
            (should (member name (gptel-agent-skills-test--wire-names child-fsm))))
          (should (gptel-tool-confirm (gptel-agent-skills-test--tool child-fsm "SkillMutation")))
          (should-not (buffer-local-value 'gptel-agent--skill-tools child))
          (gptel-agent-workers-test--answer child-fsm "Ready")
          (with-current-buffer child
            (goto-char (point-max)) (insert "Continue") (gptel-send))
          (setq child-fsm (gptel-agent-discussion--chat-fsm chat))
          (dolist (name '("Skill" "Read" "Discussion" "DiscussionControl"
                          "SkillMutation" "DiscussionReport"))
            (should (member name (gptel-agent-skills-test--wire-names child-fsm)))))))))

(ert-deftest gptel-agent-skills-discussion-preset-tools-and-reporting ()
  (let ((gptel-agent-skills-test--extra-tools '("Read"))
        (gptel--known-presets nil))
    (gptel-make-preset 'skill-child-tools :tools '("Write" "AskUserQuestion"))
    (gptel-agent-skills-test--with-chat "allowed-tools: Discussion DiscussionControl\n"
      (gptel-agent-skills-test--load fsm)
      (gptel-agent-workers-test--tool
       fsm "Discussion" '(:title "Preset tools" :prompt "Independent" :preset "skill-child-tools"))
      (let* ((chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
             (child-fsm (gptel-agent-discussion--chat-fsm chat)))
        (should (equal (sort (gptel-agent-skills-test--wire-names child-fsm) #'string<)
                       '("AskUserQuestion" "DiscussionReport" "Write")))
        (gptel-agent-workers-test--tool child-fsm "DiscussionReport" '(:message "PRESET-REPORT"))
        (should (equal "PRESET-REPORT"
                       (plist-get (car (buffer-local-value 'gptel-agent-discussion--inbox parent)) :text))))
      (should (equal (sort (gptel-agent-skills-test--wire-names fsm) #'string<)
                     '("Discussion" "DiscussionControl" "Read" "Skill"))))))

(ert-deftest gptel-agent-skills-discussion-retains-request-settings ()
  (let ((gptel-system-prompt "REQUEST-SYSTEM"))
    (gptel-agent-skills-test--with-chat "allowed-tools: Discussion DiscussionControl\n"
      (with-current-buffer parent
        (setq-local gptel-system-prompt "LATER-BUFFER-SYSTEM"))
      (gptel-agent-skills-test--load fsm)
      (gptel-agent-workers-test--tool
       fsm "Discussion" '(:title "Request settings" :prompt "Independent"))
      (let* ((chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
             (child (gptel-agent-discussion--chat-buffer chat)))
        (should (equal (buffer-local-value 'gptel-system-prompt child) "REQUEST-SYSTEM"))))))

(ert-deftest gptel-agent-skills-activation-includes-companion-tools ()
  (gptel-agent-skills-test--with-chat "allowed-tools: Agent\n"
    (gptel-agent-skills-test--load fsm)
    (dolist (name '("Skill" "Agent" "AgentControl"))
      (should (member name (gptel-agent-skills-test--wire-names fsm))))
    (gptel-agent-workers-test--tool fsm "AgentControl" '(:action "list"))
    (should-not (string-match-p "Unknown tool\\|not available" (gptel-agent-workers-test--data fsm)))))

(ert-deftest gptel-agent-skills-same-turn-all-providers ()
  (dolist (backend (list (gptel-make-openai "s-oai" :key "unused" :models '(test))
                         (gptel-make-openai-responses "s-responses" :key "unused" :models '(test))
                         (gptel-make-anthropic "s-anthropic" :key "unused" :models '(test))
                         (gptel-make-gemini "s-gemini" :key "unused" :models '(test))))
    (let ((gptel-agent-workers-test--backend backend))
      (gptel-agent-skills-test--with-chat "allowed-tools: Discussion DiscussionControl\n"
        (should (equal (gptel-agent-skills-test--wire-names fsm) '("Skill")))
        (should-not (string-match-p "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests)))
        (gptel-agent-skills-test--load fsm)
        (should (string-match-p "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests)))
        (dolist (name '("Discussion" "DiscussionControl"))
          (should (gptel-agent-skills-test--tool fsm name))
          (should (member name (gptel-agent-skills-test--wire-names fsm))))
        (should (= (length requests) 2))
        (gptel-agent-workers-test--tool fsm "Discussion"
                                      '(:title "From skill" :prompt "Independent prompt"))
        (let* ((chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
               (child (gptel-agent-discussion--chat-buffer chat))
               (child-fsm (gptel-agent-discussion--chat-fsm chat)))
          (should chat)
          (ert-info ((format "Chat: %S" chat)) (should (eq (gptel-agent-discussion--chat-state chat) 'running)))
          (should-not (buffer-local-value 'gptel-agent--skill-tools child))
          (dolist (name '("Skill" "Discussion" "DiscussionControl"))
            (should (member name (gptel-agent-skills-test--wire-names child-fsm))))
          (should (gptel-agent-skills-test--tool child-fsm "DiscussionReport"))
          (should (member "DiscussionReport" (gptel-agent-skills-test--wire-names child-fsm)))
          (should-not (string-match-p "SKILL-WORKFLOW-INSTRUCTIONS"
                                      (gptel-agent-workers-test--data child-fsm)))
          (gptel-agent-workers-test--tool child-fsm "DiscussionReport" '(:message "SKILL-CHILD-REPORT"))
          (should (equal "SKILL-CHILD-REPORT"
                         (plist-get (car (buffer-local-value 'gptel-agent-discussion--inbox parent)) :text)))
          (gptel-agent-workers-test--tool fsm "DiscussionControl" '(:action "list"))
          (should (string-match-p "From skill" (cdar requests)))
          ;; A subsequent parent turn binds a new FSM without changing the old one.
          (let ((old-tool (gptel-agent-skills-test--tool fsm "DiscussionControl"))
                (old-fsm fsm))
            (gptel-agent-workers-test--answer fsm "Parent finished")
            (with-current-buffer parent
              (goto-char (point-max)) (insert "Next turn") (gptel-send)
              (setq fsm (caar requests)))
            (should-not (eq fsm old-fsm))
            (should-not (eq (gptel-tool-function old-tool)
                            (gptel-tool-function
                             (gptel-agent-skills-test--tool fsm "DiscussionControl")))))
          (gptel-agent-workers-test--tool fsm "DiscussionControl"
                                        (list :action "close" :discussion_id
                                              (gptel-agent-discussion--chat-id chat)))
          (should (eq (gptel-agent-discussion--chat-state chat) 'closed)))))))

(ert-deftest gptel-agent-skills-absent-field-is-instruction-only ()
  (gptel-agent-skills-test--with-chat ""
    (gptel-agent-skills-test--load fsm)
    (should (string-match-p "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests)))
    (should-not (string-match-p "activation failed" (cdar requests)))
    (should-not (buffer-local-value 'gptel-agent--skill-tools parent))
    (should (equal (gptel-agent-skills-test--wire-names fsm) '("Skill")))))

(ert-deftest gptel-agent-skills-atomic-declaration-failures ()
  (dolist (declaration '("SkillMutation Missing" "SkillMutation Bash(git:*)"
                         "SkillMutation *" "SkillMutation skill-test"
                         "[SkillMutation]" "false" "null"))
    (gptel-agent-skills-test--with-chat (concat "allowed-tools: " declaration "\n")
      (gptel-agent-skills-test--load fsm)
      (should (string-match-p "activation failed" (cdar requests)))
      (should (string-match-p "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests)))
      (should-not (buffer-local-value 'gptel-agent--skill-tools parent))
      (should (equal (gptel-agent-skills-test--wire-names fsm) '("Skill")))
      (should (gptel-tool-confirm (gptel-get-tool "SkillMutation"))))))

(ert-deftest gptel-agent-skills-ambiguous-name-rejected ()
  (let* ((one (gptel--make-tool :name "Same" :category "one" :function #'ignore))
         (two (gptel--make-tool :name "Same" :category "two" :function #'ignore)))
    (should-error (gptel-agent--skill-resolve-tools "Same" (list one two)))))

(ert-deftest gptel-agent-skills-duplicate-activation-is-idempotent ()
  (gptel-agent-skills-test--with-chat "allowed-tools: Discussion DiscussionControl Discussion\n"
    (gptel-agent-skills-test--load fsm)
    (let ((handlers (copy-tree (gptel-agent--workers-handlers (gptel-agent--workers-for fsm))))
          (fn (gptel-tool-function (gptel-agent-skills-test--tool fsm "Discussion"))))
      (gptel-agent-skills-test--load fsm)
      (should (eq fn (gptel-tool-function (gptel-agent-skills-test--tool fsm "Discussion"))))
      (should (equal handlers (gptel-agent--workers-handlers (gptel-agent--workers-for fsm)))))
    (should (= 3 (length (plist-get (gptel-fsm-info fsm) :tools))))
    (should (= 2 (length (buffer-local-value 'gptel-agent--skill-tools parent))))))

(ert-deftest gptel-agent-skills-auto-preapproval-is-local ()
  (gptel-agent-skills-test--with-chat "allowed-tools: SkillMutation\n"
    (gptel-agent-skills-test--load fsm)
    (gptel-agent-workers-test--tool fsm "SkillMutation" nil)
    (should (= executions 1))
    (should (gptel-tool-confirm (gptel-get-tool "SkillMutation")))
    (should (eq (buffer-local-value 'gptel-confirm-tool-calls parent) 'auto))
    (with-temp-buffer
      (should-not gptel-agent--skill-tools)
      (setq-local gptel-tools (list (gptel-get-tool "Skill")))
      (gptel-agent--localize-skill-tool (gptel-make-fsm :info (list :buffer (current-buffer))))
      (should (equal (mapcar #'gptel-tool-name gptel-tools) '("Skill"))))))

(ert-deftest gptel-agent-skills-always-confirm-and-hook-overrides ()
  (dolist (mode '(always confirm block allow))
    (gptel-agent-skills-test--with-chat "allowed-tools: SkillMutation\n"
      (gptel-agent-skills-test--load fsm)
      (with-current-buffer parent
        (setq-local gptel-confirm-tool-calls (if (memq mode '(always allow)) t 'auto))
        (unless (eq mode 'always)
          (setq-local gptel-pre-tool-call-functions
                      (list (lambda (_call)
                              (if (eq mode 'block)
                                  (list :block "Blocked by test hook")
                                (list :confirm (eq mode 'confirm))))))))
      (gptel-agent-workers-test--tool fsm "SkillMutation" nil)
      (should (= executions (if (eq mode 'allow) 1 0)))
      (if (memq mode '(always confirm))
          (progn
            (should (eq (gptel-fsm-state fsm) 'TOOL))
            (with-current-buffer parent
              (should (cl-some (lambda (ov) (overlay-get ov 'gptel-tool))
                               (overlays-in (point-min) (point-max))))))
        (should (eq (gptel-fsm-state fsm) 'WAIT))))))

(ert-deftest gptel-agent-skills-registry-snapshot-is-stable ()
  (gptel-agent-skills-test--with-chat "allowed-tools: SkillMutation\n"
    (let ((global (gptel-get-tool "Skill"))
          (description (gptel-tool-description (gptel-get-tool "Skill"))))
      (setq gptel-agent--skills nil)
      (gptel-agent--update-skill-tool)
      (gptel-make-tool :name "SkillMutation" :category "skill-test" :args nil
                       :description "Changed registry" :function (lambda () (error "Wrong tool")))
      (unwind-protect
          (progn
            (gptel-agent-skills-test--load fsm)
            (gptel-agent-workers-test--tool fsm "SkillMutation" nil)
            (should (= executions 1))
            (should (equal (gptel-tool-description
                            (gptel-agent-skills-test--tool fsm "SkillMutation"))
                           "Test mutation")))
        (setf (gptel-tool-description global) description)))))

(ert-deftest gptel-agent-skills-data-reader-does-not-evaluate-agent-hooks ()
  (let ((gptel-agent-skills-test--effects nil))
    (gptel-agent-skills-test--with-chat
        (concat "pre: '(push :pre gptel-agent-skills-test--effects)'\n"
                "post: '(push :post gptel-agent-skills-test--effects)'\n"
                "allowed-tools: SkillMutation\n")
      (let ((metadata (gptel-agent--cached-read-skill-file file)))
        (should (equal metadata '("test-skill" :description "Test capability"))))
      (gptel-agent-skills-test--load fsm)
      (should-not gptel-agent-skills-test--effects)
      (should (gptel-agent-skills-test--tool fsm "SkillMutation")))))

(ert-deftest gptel-agent-skills-agent-markdown-and-org-pre-post-unchanged ()
  (let ((gptel-agent-skills-test--effects nil))
    (dolist (data (list
                  (gptel-agent--parse-yaml-frontmatter
                   (concat "pre: '(progn (push :parse-pre gptel-agent-skills-test--effects) "
                           "(lambda () (push :pre gptel-agent-skills-test--effects)))'\n"
                           "post: '(progn (push :parse-post gptel-agent-skills-test--effects) "
                           "(lambda () (push :post gptel-agent-skills-test--effects)))'\n") #'always)
                  (gptel-agent--org-properties-alist-to-plist
                   '(("PRE" . "(progn (push :parse-pre gptel-agent-skills-test--effects) (lambda () (push :pre gptel-agent-skills-test--effects)))")
                     ("POST" . "(progn (push :parse-post gptel-agent-skills-test--effects) (lambda () (push :post gptel-agent-skills-test--effects)))")) #'always)))
      (with-temp-buffer (gptel--apply-preset data)))
    (should (equal (nreverse gptel-agent-skills-test--effects)
                   '(:parse-pre :parse-post :parse-pre :parse-post :pre :post :pre :post)))))

(ert-deftest gptel-agent-skills-default-agent-is-lazy ()
  (let* ((agent (cdr (gptel-agent-read-file
                      (expand-file-name "agents/gptel-agent.md" default-directory))))
         (gptel-agent-dirs nil))
    (dolist (name '("Discussion" "DiscussionControl"))
      (should-not (member name (plist-get agent :tools))))
    (should-not (string-match-p "managed_discussions" (plist-get agent :system)))))


(ert-deftest gptel-agent-skills-existing-tool-and-child-approval-are-independent ()
  (let ((gptel-agent-skills-test--extra-tools '("SkillMutation")))
    (gptel-agent-skills-test--with-chat
        "allowed-tools: Discussion DiscussionControl SkillMutation\n"
      (should (gptel-tool-confirm (gptel-agent-skills-test--tool fsm "SkillMutation")))
      (gptel-agent-skills-test--load fsm)
      (should-not (gptel-tool-confirm (gptel-agent-skills-test--tool fsm "SkillMutation")))
      (gptel-agent-workers-test--tool fsm "Discussion" '(:title "Permissions" :prompt "Independent"))
      (let* ((chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
             (child-fsm (gptel-agent-discussion--chat-fsm chat)))
        (should (gptel-tool-confirm (gptel-agent-skills-test--tool child-fsm "SkillMutation")))
        (gptel-agent-workers-test--tool child-fsm "SkillMutation" nil)
        (should (= executions 0))
        (should (eq (gptel-fsm-state child-fsm) 'TOOL))
        (should (car (gptel-agent-discussion--chat-pending chat)))
        ;; Pre-approving the manager must not let it move a child's approval.
        (let ((pending (car (gptel-agent-discussion--chat-pending chat))))
          (gptel-agent-workers-test--tool
           fsm "DiscussionControl"
           (list :action "request_approval"
                 :discussion_id (gptel-agent-discussion--chat-id chat)
                 :request_id (gptel-agent-discussion--interaction-id pending))))
        (should (= executions 0))
        (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
               (overlays (gptel-agent-discussion--interaction-overlays pending)))
          (should (= 1 (length overlays)))
          (should (eq (overlay-buffer (car overlays))
                      (gptel-agent-discussion--chat-buffer chat)))
          (should (string-match-p "Approve or reject tool calls in discussion buffer"
                                  (gptel-agent-workers-test--data fsm)))
          (with-current-buffer (gptel-agent-discussion--chat-buffer chat)
            (gptel--accept-tool-calls
             (gptel-agent-discussion--interaction-calls pending) (car overlays))))
        (should (= executions 1))))))

(ert-deftest gptel-agent-skills-worker-followup-binds-new-request ()
  (let ((gptel-agent-skills-test--extra-tools '("Agent")))
    (gptel-agent-skills-test--with-chat "allowed-tools: SkillMutation\n"
      (with-current-buffer parent (setq-local gptel-confirm-tool-calls nil))
      (gptel-agent-skills-test--load fsm)
      (gptel-agent-workers-test--tool
       fsm "Agent" '(:subagent_type "researcher" :description "First" :prompt "Research"))
      (let* ((first (caar requests))
             (run (plist-get (gptel-fsm-info first) :context))
             (session (gptel-agent--run-session run))
             (id (gptel-agent--session-id session)))
        (should-not (gptel-agent-skills-test--tool first "SkillMutation"))
        (gptel-agent-skills-test--load first)
        (gptel-agent-followup-test--finish first "First result")
        (gptel-agent-workers-test--tool
         fsm "Agent" (list :subagent_type "researcher" :description "Follow up"
                           :prompt "More research" :agent_id id))
        (let ((next (caar requests)))
          (should-not (eq first next))
          (should (gptel-agent-skills-test--tool next "SkillMutation"))
          (should (member "SkillMutation" (gptel-agent-skills-test--wire-names next)))
          ;; Reloading on follow-up must target next, never the completed FSM.
          (gptel-agent-skills-test--load next)
          (should-not (string-match-p "activation failed" (cdar requests)))
          (gptel-agent-workers-test--tool next "SkillMutation" nil)
          (should (= executions 1))
          (gptel-agent-followup-test--finish next "Follow-up result"))))))

(ert-deftest gptel-agent-skills-ambiguous-activation-adds-nothing ()
  (gptel-agent-skills-test--with-chat "allowed-tools: SkillMutation Read\n"
    ;; Take a new request snapshot containing a name registered twice.
    (gptel-make-tool :name "Read" :category "duplicate" :description "Duplicate"
                     :args nil :function #'ignore)
    (with-current-buffer parent
      (gptel-agent--localize-skill-tool fsm)
      (setf (plist-get (gptel-fsm-info fsm) :tools) gptel-tools))
    (gptel-agent-skills-test--load fsm)
    (should (string-match-p "Ambiguous" (cdar requests)))
    (should-not (gptel-agent-skills-test--tool fsm "SkillMutation"))
    (should-not (buffer-local-value 'gptel-agent--skill-tools parent))))

(ert-deftest gptel-agent-skills-cache-and-discovery-preserve-precedence ()
  (gptel-agent-skills-test--with-chat "allowed-tools: SkillMutation\n"
    (let* ((default-directory (file-name-as-directory root))
           (first (expand-file-name "first" root))
           (second (expand-file-name "second" root))
           (project (expand-file-name ".skills" root))
           (gptel-agent-skill-dirs (list first second ".skills")))
      (dolist (dir (list first second project))
        (make-directory dir)
        (with-temp-file (expand-file-name "SKILL.md" dir)
          (insert "---\nname: priority\ndescription: " (file-name-nondirectory dir)
                  "\nallowed-tools: SkillMutation\n---\nPrivate workflow")))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (gptel-agent--update-skills)
        (should (equal (plist-get (cddr (assoc "priority" gptel-agent--skills)) :description)
                       ".skills"))
        (let ((message (gptel-agent--skills-system-message gptel-agent--skills)))
          (should-not (string-match-p "SkillMutation\\|Private workflow" message)))
        (setq gptel-agent-skill-dirs (list first second))
        (gptel-agent--update-skills)
        (should (equal (plist-get (cddr (assoc "priority" gptel-agent--skills)) :description)
                       "first")))
      ;; A cached metadata read must not prevent reading declarations and body.
      (should (equal (plist-get (cdr (gptel-agent--cached-read-skill-file
                                     (expand-file-name "SKILL.md" first) t)) :allowed-tools)
                     "SkillMutation")))))


(ert-deftest gptel-agent-skills-discovery-follows-symlinked-files-and-directories ()
  (let* ((root (make-temp-file "gptel-skills-links-" t))
         (default-directory (file-name-as-directory root))
         (source (expand-file-name "source/managed-discussions" root))
         (build (expand-file-name "build/managed-discussions" root))
         (gptel-agent--bundled-skill-dir (expand-file-name "build/" root))
         (gptel-agent-skill-dirs nil)
         (gptel-agent--skills nil)
         (gptel-agent--skill-file-cache nil))
    (unwind-protect
        (progn
          (make-directory source t)
          (make-directory build t)
          (with-temp-file (expand-file-name "SKILL.md" source)
            (insert "---\nname: linked-skill\ndescription: Linked skill\n---\nInstructions"))
          ;; Straight builds contain real directories with symlinked files.
          (make-symbolic-link (expand-file-name "SKILL.md" source)
                              (expand-file-name "SKILL.md" build))
          (gptel-agent--update-skills)
          (should (equal gptel-agent--skills
                         `(("linked-skill" ,(file-name-as-directory build)
                            :description "Linked skill"))))
          ;; Some Emacs builds report file symlinks as directories during
          ;; completion.  Discovery must use the actual file type instead.
          (let ((complete (symbol-function 'file-name-all-completions)))
            (cl-letf (((symbol-function 'file-name-all-completions)
                       (lambda (prefix dir)
                         (mapcar (lambda (name)
                                   (if (and (not (directory-name-p name))
                                            (file-symlink-p (expand-file-name name dir)))
                                       (concat name "/")
                                     name))
                                 (funcall complete prefix dir)))))
              (gptel-agent--update-skills)
              (should (assoc "linked-skill" gptel-agent--skills))))
          ;; User skill directories can themselves be symlinks.
          (let* ((user-dir (expand-file-name "user" root))
                 (linked-dir (expand-file-name "linked" user-dir)))
            (make-directory user-dir)
            (make-symbolic-link source linked-dir)
            (setq gptel-agent-skill-dirs (list user-dir))
            (gptel-agent--update-skills)
            (should (equal (cadr (assoc "linked-skill" gptel-agent--skills))
                           (file-name-as-directory linked-dir)))))
      (delete-directory root t))))

(ert-deftest gptel-agent-skills-discovery-skips-symlink-cycles-and-broken-links ()
  (let* ((root (make-temp-file "gptel-skills-cycle-" t))
         (default-directory (file-name-as-directory root))
         (skill-dir (expand-file-name "skill" root))
         (gptel-agent--bundled-skill-dir root)
         (gptel-agent-skill-dirs nil)
         (gptel-agent--skills nil)
         (gptel-agent--skill-file-cache nil))
    (unwind-protect
        (progn
          (make-directory skill-dir)
          (with-temp-file (expand-file-name "SKILL.md" skill-dir)
            (insert "---\nname: valid-skill\ndescription: Valid skill\n---\nInstructions"))
          (make-symbolic-link root (expand-file-name "loop" skill-dir))
          (make-symbolic-link (expand-file-name "missing" root)
                              (expand-file-name "SKILL.md" root))
          (gptel-agent--update-skills)
          (should (equal gptel-agent--skills
                         `(("valid-skill" ,(file-name-as-directory skill-dir)
                            :description "Valid skill")))))
      (delete-directory root t))))

(ert-deftest gptel-agent-skills-bundled-discovery-and-user-override ()
  (gptel-agent-skills-test--with-chat ""
    (let ((gptel-agent-skill-dirs nil))
      (should (member gptel-agent--bundled-skill-dir
                      (eval (car (get 'gptel-agent-skill-dirs 'standard-value)) t)))
      (gptel-agent--update-skills)
      (let* ((skill (assoc "managed-discussions" gptel-agent--skills))
             (path (expand-file-name "SKILL.md" (cadr skill)))
             (data (cdr (gptel-agent--cached-read-skill-file path t))))
        (should (equal (cadr skill)
                       (expand-file-name "managed-discussions/" gptel-agent--bundled-skill-dir)))
        (should (equal (plist-get data :allowed-tools) "Discussion DiscussionControl"))
        (should (string-match-p "Tool approvals stay in the child discussion buffer"
                                (plist-get data :system)))
        (should-not (string-match-p "request_approval" (plist-get data :system)))
        (should-not (plist-member (cddr skill) :allowed-tools))
        (should-not (plist-member (cddr skill) :system)))
      (setq gptel-agent-skill-dirs (list root))
      (with-temp-file file
        (insert "---\nname: managed-discussions\ndescription: Personal override\n---\nPersonal instructions"))
      (gptel-agent--update-skills)
      (should (equal (cadr (assoc "managed-discussions" gptel-agent--skills))
                     (file-name-as-directory root))))))

(ert-deftest gptel-agent-skills-bundled-skill-loads-through-tool ()
  (gptel-agent-skills-test--with-chat ""
    (let ((gptel-agent-skill-dirs nil)) (gptel-agent--update-skills))
    (gptel-agent-workers-test--answer fsm "Ready")
    (with-current-buffer parent
      (goto-char (point-max)) (insert "Create a separate conversation") (gptel-send)
      (setq fsm (caar requests)))
    (should-not (member "Discussion" (gptel-agent-skills-test--wire-names fsm)))
    (should-not (string-match-p "self-contained opening prompt" (cdar requests)))
    (gptel-agent-workers-test--tool fsm "Skill" '(:skill "managed-discussions"))
    (should (gptel-agent-skills-test--tool fsm "Discussion"))
    (should (gptel-agent-skills-test--tool fsm "DiscussionControl"))
    (should (string-match-p "self-contained opening prompt" (cdar requests)))))

(provide 'gptel-agent-skills-test)
