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
      (should (gptel-tool-confirm (gptel-get-tool "SkillMutation")))
      ;; Failed activation must remain visible and retryable on later calls.
      (gptel-agent-skills-test--load fsm)
      (should-not (string-match-p "already loaded in the current context" (cdar requests)))
      (should (= 2 (gptel-agent-workers-test--count
                    "allowed-tools activation failed" (cdar requests)))))))

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

(ert-deftest gptel-agent-skills-reuse-current-context-all-providers ()
  (dolist (backend (list (gptel-make-openai "reuse-oai" :key "unused" :models '(test))
                        (gptel-make-openai-responses "reuse-responses" :key "unused" :models '(test))
                        (gptel-make-anthropic "reuse-anthropic" :key "unused" :models '(test))
                        (gptel-make-gemini "reuse-gemini" :key "unused" :models '(test))))
    (let ((gptel-agent-workers-test--backend backend))
      (gptel-agent-skills-test--with-chat ""
        (gptel-agent-skills-test--load fsm)
        (gptel-agent-skills-test--load fsm)
        (should (= 1 (gptel-agent-workers-test--count
                      "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests))))
        (should (string-match-p "already loaded in the current context" (cdar requests)))
        ;; A new turn reparses the transcript, including whitespace trimming.
        (gptel-agent-workers-test--answer fsm "Ready")
        (with-current-buffer parent
          (goto-char (point-max)) (insert "Similar task") (gptel-send))
        (setq fsm (caar requests))
        (gptel-agent-skills-test--load fsm)
        (should (= 1 (gptel-agent-workers-test--count
                      "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests))))
        (should (= 2 (gptel-agent-workers-test--count
                      "already loaded in the current context" (cdar requests))))
        ;; The source buffer still has the instructions, but the request does not.
        (gptel-agent-workers-test--answer fsm "Ready again")
        (with-current-buffer parent
          (setq-local gptel--num-messages-to-send 0)
          (goto-char (point-max)) (insert "Task without history") (gptel-send))
        (setq fsm (caar requests))
        (should-not (string-match-p "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests)))
        (gptel-agent-skills-test--load fsm)
        (should (= 1 (gptel-agent-workers-test--count
                      "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests))))
        (should-not (string-match-p "already loaded in the current context" (cdar requests)))))))

(ert-deftest gptel-agent-skills-changed-instructions-load-again ()
  (gptel-agent-skills-test--with-chat ""
    (gptel-agent-skills-test--load fsm)
    (let ((mtime (file-attribute-modification-time (file-attributes file))))
      (write-region "UPDATED-WORKFLOW\n" nil file t 'silent)
      (set-file-times file (time-add mtime 1)))
    (gptel-agent-skills-test--load fsm)
    (should (= 2 (gptel-agent-workers-test--count
                  "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests))))
    (should (string-match-p "UPDATED-WORKFLOW" (cdar requests)))
    (should-not (string-match-p "already loaded in the current context" (cdar requests)))))

(ert-deftest gptel-agent-skills-reuse-still-activates-tools ()
  (gptel-agent-skills-test--with-chat "allowed-tools: SkillMutation\n"
    (gptel-agent-skills-test--load fsm)
    (gptel-agent-workers-test--answer fsm "Ready")
    ;; Restored transcripts can contain instructions without live tool grants.
    (with-current-buffer parent
      (setq gptel-agent--skill-tools nil)
      (goto-char (point-max)) (insert "Use the skill again") (gptel-send))
    (setq fsm (caar requests))
    (should-not (gptel-agent-skills-test--tool fsm "SkillMutation"))
    (gptel-agent-skills-test--load fsm)
    (should (string-match-p "already loaded in the current context" (cdar requests)))
    (should (= 1 (gptel-agent-workers-test--count
                  "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests))))
    (should (member "SkillMutation" (gptel-agent-skills-test--wire-names fsm)))
    (gptel-agent-workers-test--tool fsm "SkillMutation" nil)
    (should (= executions 1))))

(ert-deftest gptel-agent-skills-reuse-preserves-discovery-and-filtering ()
  (gptel-agent-skills-test--with-chat ""
    (let* ((global (gptel-agent--skill-copy-tool (gptel-get-tool "Skill")))
           (gptel--known-tools `(("test" ("Skill" . ,global))))
           (original (gptel-agent-skills-test--tool fsm "Skill"))
           (description (gptel-tool-description original)))
      (gptel-agent-skills-test--load fsm)
      ;; Discovery and filtering still update the registry immediately; an
      ;; in-flight request keeps its snapshot until the next user turn.
      (push (list "another-skill" root :description "Another capability") gptel-agent--skills)
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (&rest _) '("another-skill"))))
        (gptel-agent-toggle-skill))
      (should (string-match-p "Another capability" (gptel-tool-description global)))
      (should-not (string-match-p "Test capability" (gptel-tool-description global)))
      (should (equal description (gptel-tool-description original)))
      (gptel-agent-workers-test--answer fsm "Ready")
      (with-current-buffer parent
        (goto-char (point-max)) (insert "Next request") (gptel-send))
      (setq fsm (caar requests))
      (should (equal (gptel-tool-description global)
                     (gptel-tool-description (gptel-agent-skills-test--tool fsm "Skill"))))
      (gptel-agent-skills-test--load fsm)
      (should (string-match-p "not enabled for this request" (cdar requests)))
      ;; Re-enabling a loaded skill advertises it and still reuses its body.
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (&rest _) '("test-skill" "another-skill"))))
        (gptel-agent-toggle-skill))
      (gptel-agent-workers-test--answer fsm "Ready")
      (with-current-buffer parent
        (goto-char (point-max)) (insert "Use the first skill") (gptel-send))
      (setq fsm (caar requests))
      (let ((current (gptel-tool-description (gptel-agent-skills-test--tool fsm "Skill"))))
        (should (string-match-p "Test capability" current))
        (should (string-match-p "Another capability" current)))
      (gptel-agent-skills-test--load fsm)
      (should (string-match-p "already loaded in the current context" (cdar requests)))
      (should (= 1 (gptel-agent-workers-test--count
                    "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests)))))))

(ert-deftest gptel-agent-skills-reuse-ignores-ordinary-messages ()
  (gptel-agent-skills-test--with-chat ""
    (let* ((result (gptel-agent--get-skill "test-skill"))
           (data (plist-get (gptel-fsm-info fsm) :data)))
      (gptel--inject-prompt
       (plist-get (gptel-fsm-info fsm) :backend) data
       (list (list :role "user" :content result)
             (list :role "assistant" :content result)))
      (gptel-agent-skills-test--load fsm)
      (should (= 3 (gptel-agent-workers-test--count
                    "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests))))
      (should-not (string-match-p "already loaded in the current context" (cdar requests))))))

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

(ert-deftest gptel-agent-skills-discussion-custom-transforms-bind-own-request ()
  (dolist (parent-finished '(nil t))
    (let ((gptel-agent-skills-test--extra-tools '("Discussion"))
          (gptel--known-presets (copy-tree gptel--known-presets)))
      (gptel-agent-skills-test--with-chat "allowed-tools: SkillMutation\n"
        (with-current-buffer parent (setq-local gptel-confirm-tool-calls nil))
        (gptel-make-preset 'discussion-skill-custom-transforms
          :prompt-transform-functions '(ignore))
        (gptel-agent-workers-test--tool
         fsm "Discussion" '(:title "Independent skill" :prompt "Load the skill"
                                   :preset "discussion-skill-custom-transforms"))
        (let* ((chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
               (child (gptel-agent-discussion--chat-buffer chat))
               (child-fsm (gptel-agent-discussion--chat-fsm chat)))
          (when parent-finished
            (gptel-agent-workers-test--answer fsm "Parent finished"))
          ;; An inherited Skill dispatcher must activate grants on the child's
          ;; request, even when its preset replaces the usual transforms.
          (gptel-agent-skills-test--load child-fsm)
          (should (gptel-agent-skills-test--tool child-fsm "SkillMutation"))
          (should (member "SkillMutation" (gptel-agent-skills-test--wire-names child-fsm)))
          (should (buffer-local-value 'gptel-agent--skill-tools child))
          (should-not (gptel-agent-skills-test--tool fsm "SkillMutation"))
          (should-not (buffer-local-value 'gptel-agent--skill-tools parent))
          (gptel-agent-workers-test--tool child-fsm "SkillMutation" nil)
          (should (= executions 1)))))))

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
        (should (= 1 (gptel-agent-workers-test--count
                      "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests))))
        (should-not (string-match-p "already loaded in the current context" (cdar requests)))
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
          (should (string-match-p "already loaded in the current context" (cdar requests)))
          (should (= 1 (gptel-agent-workers-test--count
                        "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests))))
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

(ert-deftest gptel-agent-skills-manual-frontmatter-is-data ()
  (let* ((root (make-temp-file "gptel-manual-skill-" t))
         (gptel-agent--skill-file-cache nil))
    (unwind-protect
        (dolist (value '("true" "false" "null" "\"false\"" "\"arbitrary\""))
          (let ((file (make-temp-file (expand-file-name "skill-" root) nil ".md")))
            (with-temp-file file
              (insert "---\nname: manual\ndescription: Manual workflow\n"
                      "disable-model-invocation: " value "\n"
                      "allowed-tools: Read Bash\n---\nPrivate instructions\n"))
            (dolist (full '(nil t))
              (let ((data (cdr (gptel-agent--cached-read-skill-file file full))))
                (should (eq (plist-get data :disable-model-invocation)
                            (equal value "true")))
                (unless full
                  (should-not (plist-member data :system))
                  (should-not (plist-member data :allowed-tools)))))))
      (delete-directory root t))))

(ert-deftest gptel-agent-skills-manual-excluded-from-discovery-and-toggle ()
  (let* ((gptel-agent--skills
          '(("manual" "/unused/manual/" :description "Private description"
             :disable-model-invocation t)
            ("automatic" "/unused/automatic/" :description "Public description")))
         ;; A stale saved selection must never re-expose a manual skill.
         (gptel-agent--enabled-skills '("manual" "automatic" "missing"))
         (tool (gptel-agent--skill-copy-tool (gptel-get-tool "Skill")))
         (gptel--known-tools `(("test" ("Skill" . ,tool)))))
    (gptel-agent--update-skill-tool)
    (should-not (plist-member (car (gptel-tool-args tool)) :enum))
    (should (string-match-p "Public description" (gptel-tool-description tool)))
    (should-not (string-match-p "manual\\|Private description\\|missing"
                                (gptel-tool-description tool)))
    (cl-letf (((symbol-function 'completing-read-multiple)
               (lambda (_prompt candidates _predicate require-match initial &rest _)
                 (should (equal candidates '("automatic")))
                 (should require-match)
                 (should (equal initial "automatic"))
                 '("automatic"))))
      (gptel-agent-toggle-skill))
    (setq gptel-agent--skills (list (car gptel-agent--skills))
          gptel-agent--enabled-skills nil)
    (gptel-agent--update-skill-tool)
    (should-not (plist-member (car (gptel-tool-args tool)) :enum))
    (should-not (string-match-p "Private description" (gptel-tool-description tool)))))

(defun gptel-agent-skills-test--complete-reference ()
  "Insert and accept a slash reference through Emacs completion."
  (gptel-agent--setup-skill-completion)
  (insert "/test-ski")
  (let ((completion-at-point-functions '(gptel-agent-skill-capf)))
    (completion-at-point)))

(ert-deftest gptel-agent-skills-manual-completion-inserts-only-a-request ()
  (gptel-agent-skills-test--with-chat "disable-model-invocation: true\n"
    (let ((gptel-agent-skill-dirs '("."))
          (gptel-agent--bundled-skill-dir (expand-file-name "no-bundled" root))
          (reader (symbol-function 'gptel-agent--cached-read-skill-file)))
      (with-current-buffer parent
        (setq default-directory (file-name-as-directory root))
        (goto-char (point-max))
        (insert "\n")
        (gptel-agent--setup-skill-completion)
        (buffer-enable-undo)
        (undo-boundary)
        (let ((before (buffer-string)) (request-count (length requests)))
          (insert "/")
          (cl-letf (((symbol-function 'gptel-agent--cached-read-skill-file)
                     (lambda (path &optional full)
                       (should-not full)
                       (funcall reader path full))))
            (pcase-let ((`(,start ,end ,candidates . ,props) (gptel-agent-skill-capf)))
              (should (equal (buffer-substring start end) "/"))
              (should (equal (mapcar #'car candidates) '("/test-skill")))
              (should (equal (funcall (plist-get props :annotation-function) "/test-skill")
                             "  Test capability")))
            (let ((completion-at-point-functions '(gptel-agent-skill-capf)))
              (completion-at-point)))
          (should (equal (buffer-substring-no-properties (1+ (length before)) (point-max))
                         "Use the Skill tool to load \"test-skill\"."))
          (should (equal (get-text-property (1+ (length before)) 'display) "/test-skill"))
          (should-not (get-text-property (1- (length before)) 'display))
          (insert " More details")
          (font-lock-ensure)
          (should-not (get-text-property (1- (point)) 'display))
          (should (= (length requests) request-count))
          (should-not gptel-agent--skill-tools)
          (should-not (string-match-p "SKILL-WORKFLOW-INSTRUCTIONS" (buffer-string)))
          (undo-boundary) (undo)
          (should (equal (concat before "/") (buffer-string))))))))

(ert-deftest gptel-agent-skills-manual-loads-through-skill-all-providers ()
  (dolist (backend (list (gptel-make-openai "m-oai" :key "unused" :models '(test))
                         (gptel-make-openai-responses "m-responses" :key "unused" :models '(test))
                         (gptel-make-anthropic "m-anthropic" :key "unused" :models '(test))
                         (gptel-make-gemini "m-gemini" :key "unused" :models '(test))))
    (let ((gptel-agent-workers-test--backend backend))
      (gptel-agent-skills-test--with-chat
          "disable-model-invocation: true\nallowed-tools: SkillMutation\n"
        (let ((gptel-agent-skill-dirs (list root))
              (gptel-agent--bundled-skill-dir root)
              (gptel-agent--enabled-skills 'none)
              (reference (expand-file-name "checklist.md" root)))
          (with-temp-file reference (insert "Read on demand"))
          (write-region "Read checklist.md.\n" nil file t 'silent)
          (gptel-agent--update-skills)
          (gptel-agent-workers-test--answer fsm "Ready")
          (with-current-buffer parent
            (goto-char (point-max)) (insert "An ordinary request") (gptel-send))
          (setq fsm (caar requests))
          ;; Neither descriptions nor schemas advertise hidden names.
          (should-not (string-match-p "test-skill\\|Test capability" (cdar requests)))
          (gptel-agent-workers-test--answer fsm "Ready")
          (with-current-buffer parent
            (goto-char (point-max))
            (gptel-agent-skills-test--complete-reference)
            (gptel-send))
          (setq fsm (caar requests))
          (should (string-match-p "Use the Skill tool to load" (cdar requests)))
          (should-not (string-match-p "/test-skill" (cdar requests)))
          (should-not (string-match-p "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests)))
          (should-not (gptel-agent-skills-test--tool fsm "SkillMutation"))
          (should-not (buffer-local-value 'gptel-agent--skill-tools parent))
          ;; Loading uses the same instructions, paths and grants as any skill.
          (gptel-agent-skills-test--load fsm)
          (should (string-match-p "SKILL-WORKFLOW-INSTRUCTIONS" (cdar requests)))
          (should (string-match-p (regexp-quote reference) (cdar requests)))
          (should (member "SkillMutation" (gptel-agent-skills-test--wire-names fsm)))
          (should-not (gptel-tool-confirm (gptel-agent-skills-test--tool fsm "SkillMutation")))
          (gptel-agent-workers-test--tool fsm "SkillMutation" nil)
          (should (= executions 1)))))))

(ert-deftest gptel-agent-skills-disabled-ordinary-skill-remains-disabled ()
  (gptel-agent-skills-test--with-chat "allowed-tools: SkillMutation\n"
    (let ((gptel-agent--enabled-skills '("another-skill")))
      (gptel-agent-workers-test--answer fsm "Ready")
      (with-current-buffer parent
        (goto-char (point-max)) (insert "Next request") (gptel-send))
      (setq fsm (caar requests))
      (should-not (string-match-p "test-skill\\|Test capability" (cdar requests)))
      (gptel-agent-skills-test--load fsm)
      (should (string-match-p "not enabled for this request" (cdar requests)))
      (should-not (gptel-agent-skills-test--tool fsm "SkillMutation")))))

(ert-deftest gptel-agent-skills-slash-completion-context-and-selection ()
  (cl-letf (((symbol-function 'gptel-agent--update-skills)
             (lambda () '(("manual" "/unused/" :description "Manual" :disable-model-invocation t)
                          ("automatic" "/unused/" :description "Automatic")))))
    (dolist (prefix '("/" "Please /man" "\n/man"))
      (with-temp-buffer
        (setq-local gptel-mode t)
        (insert prefix)
        (pcase-let ((`(,_ ,_ ,candidates . ,props) (gptel-agent-skill-capf)))
          (should (equal (mapcar #'car candidates) '("/manual")))
          (funcall (plist-get props :exit-function) "/manual" 'exact)
          (should (equal (buffer-string) prefix)))))
    (dolist (prefix '("https://example/man" "./man" "~/man" "dir/man" "/tmp/man" "ordinary"))
      (with-temp-buffer
        (setq-local gptel-mode t)
        (insert prefix)
        (should-not (gptel-agent-skill-capf))))
    (with-temp-buffer
      (insert "/man")
      (should-not (gptel-agent-skill-capf))
      (setq-local gptel-mode t)
      (put-text-property (point-min) (point-max) 'gptel 'response)
      (should-not (gptel-agent-skill-capf))
      (remove-text-properties (point-min) (point-max) '(gptel nil))
      (setq buffer-read-only t)
      ;; Company queries CAPFs with this binding to forbid premature edits.
      (should (gptel-agent-skill-capf))
      (should (equal (buffer-string) "/man")))))

(ert-deftest gptel-agent-skills-decoration-restores-and-cleans-up ()
  (dolist (mode '(text-mode org-mode))
    (with-temp-buffer
      (funcall mode)
      (insert "Use the Skill tool to load \"manual\".")
      (let ((raw (buffer-string)))
        (gptel-mode 1)
        (font-lock-ensure)
        (should (equal (get-text-property (point-min) 'display) "/manual"))
        (should (equal (buffer-substring-no-properties (point-min) (point-max)) raw))
        (should (memq #'gptel-agent-skill-capf completion-at-point-functions))
        (gptel-mode -1)
        (font-lock-ensure)
        (should-not (get-text-property (point-min) 'display))
        (should-not (memq #'gptel-agent-skill-capf completion-at-point-functions))
        (gptel-mode 1)
        (font-lock-ensure)
        (delete-char -1)
        (font-lock-ensure)
        (should-not (get-text-property (point-min) 'display))))))

(ert-deftest gptel-agent-skills-decoration-leaves-responses-and-tools-visible ()
  (with-temp-buffer
    (text-mode)
    (insert (propertize "Use the Skill tool to load \"manual\".\n" 'gptel 'response)
            (propertize "Use the Skill tool to load \"manual\".\n" 'gptel '(tool . "id")))
    (gptel-mode 1)
    (font-lock-ensure)
    (should-not (text-property-not-all (point-min) (point-max) 'display nil))))


(defun gptel-agent-skills-test--scoped-tool (source)
  "Return SOURCE's Skill dispatcher as localized in a temporary prompt buffer."
  (with-temp-buffer
    (setq-local default-directory temporary-file-directory
                gptel-tools (list (gptel-get-tool "Skill")))
    (gptel-agent--localize-skill-tool
     (gptel-make-fsm :state 'TOOL :info (list :buffer source)))
    (car gptel-tools)))

(ert-deftest gptel-agent-skills-unselected-tool-does-not-discover ()
  (with-temp-buffer
    (setq-local gptel-tools nil gptel-agent--skill-tools nil)
    (cl-letf (((symbol-function 'gptel-agent--ensure-skills)
               (lambda () (ert-fail "A request without Skill scanned skill directories"))))
      (gptel-agent--localize-skill-tool
       (gptel-make-fsm :info (list :buffer (current-buffer)))))))

(ert-deftest gptel-agent-skills-none-survives-send-and-restore ()
  (require 'gptel-agent-session)
  ;; Existing records store nil for the untouched default.
  (dolist (selection '(nil none))
    (gptel-agent-skills-test--with-chat ""
      (gptel-agent-workers-test--answer fsm "Ready")
      (let ((session (expand-file-name "chat.eld" root)))
        (with-current-buffer parent
          (when selection
            (cl-letf (((symbol-function 'completing-read-multiple)
                       (lambda (_prompt choices _pred _match initial &rest _)
                         (should (equal choices '("test-skill")))
                         (should (equal initial "test-skill"))
                         nil)))
              (gptel-agent-toggle-skill)))
          (gptel-agent-session-export-buffer parent session))
        (let ((restored (gptel-agent-session-import-buffer session)))
          (unwind-protect
              (dolist (buffer (list parent restored))
                (with-current-buffer buffer
                  (should (eq gptel-agent--enabled-skills selection))
                  (goto-char (point-max))
                  (insert "Use a skill")
                  (gptel-send))
                (setq fsm (caar requests))
                (should (eq (not (string-match-p "Test capability" (cdar requests)))
                            (eq selection 'none)))
                (gptel-agent-skills-test--load fsm)
                (should (string-match-p
                         (if selection "not enabled for this request" "SKILL-WORKFLOW-INSTRUCTIONS")
                         (cdar requests)))
                (gptel-agent-workers-test--answer fsm "Ready"))
            (kill-buffer restored)))
        (when selection
          (with-current-buffer parent
            (cl-letf (((symbol-function 'completing-read-multiple)
                       (lambda (_prompt choices _pred _match initial &rest _)
                         (should (equal initial ""))
                         choices)))
              (gptel-agent-toggle-skill))
            (should (equal gptel-agent--enabled-skills '("test-skill")))))))))

(ert-deftest gptel-agent-skills-project-discovery-and-cd-are-isolated ()
  (let* ((root (make-temp-file "gptel-skill-scopes-" t))
         (first (generate-new-buffer " *skill-project-one*"))
         (second (generate-new-buffer " *skill-project-two*"))
         (gptel-agent-skill-dirs '(".skills"))
         (gptel-agent--bundled-skill-dir (expand-file-name "no-bundled" root))
         (gptel-agent--skills nil)
         (gptel-agent--enabled-skills nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
          (dolist (entry (list (cons first "FIRST") (cons second "SECOND")))
            (let* ((directory (expand-file-name (cdr entry) root))
                   (skill-directory (expand-file-name ".skills/deploy" directory)))
              (make-directory skill-directory t)
              (with-temp-file (expand-file-name "SKILL.md" skill-directory)
                (insert "---\nname: deploy\ndescription: " (cdr entry)
                        " deployment\n---\nDeploy " (cdr entry)))
              (with-current-buffer (car entry)
                (setq default-directory (file-name-as-directory directory))
                (gptel-agent--update-skills))))
          ;; Discovery in another chat cannot alter either a fresh request or
          ;; a dispatcher whose request is already in flight.
          (let ((old (gptel-agent-skills-test--scoped-tool first))
                (other (gptel-agent-skills-test--scoped-tool second)))
            (should (string-match-p "Deploy FIRST" (funcall (gptel-tool-function old) "deploy")))
            (should (string-match-p "Deploy SECOND" (funcall (gptel-tool-function other) "deploy")))
            (with-current-buffer second (gptel-agent--update-skills))
            (should (string-match-p "FIRST deployment"
                                    (gptel-tool-description (gptel-agent-skills-test--scoped-tool first))))
            (with-current-buffer first
              (cd (buffer-local-value 'default-directory second)))
            (let ((next (gptel-agent-skills-test--scoped-tool first)))
              (should (string-match-p "Deploy SECOND" (funcall (gptel-tool-function next) "deploy"))))
            (should (string-match-p "Deploy FIRST" (funcall (gptel-tool-function old) "deploy")))
            ;; Changing into a scope with no skills must clear the old registry.
            (with-current-buffer first (cd root))
            (let ((empty (gptel-agent-skills-test--scoped-tool first)))
              (should (equal (funcall (gptel-tool-function empty) "deploy")
                             "Error: skill deploy not found.")))
            (should (string-match-p "Deploy SECOND" (funcall (gptel-tool-function other) "deploy")))
            ;; Directory configuration changes also invalidate discovery even
            ;; when the conversation itself has not moved.
            (with-current-buffer first
              (setq-local gptel-agent-skill-dirs
                          (list (expand-file-name "SECOND/.skills" root))))
            (let ((configured (gptel-agent-skills-test--scoped-tool first)))
              (should (string-match-p "Deploy SECOND"
                                      (funcall (gptel-tool-function configured) "deploy")))))
          (should-not (default-value 'gptel-agent--skills)))
      (kill-buffer first) (kill-buffer second) (delete-directory root t))))

(ert-deftest gptel-agent-skills-selection-is-conversation-local ()
  (let* ((first (generate-new-buffer " *skill-selection-one*"))
         (second (generate-new-buffer " *skill-selection-two*"))
         (gptel-agent--skills '(("deploy" "/unused" :description "Deploy")
                               ("check" "/unused" :description "Check")))
         (gptel-agent--enabled-skills nil)
         (gptel--known-tools (copy-tree gptel--known-tools)))
    (unwind-protect
        (let ((old (gptel-agent-skills-test--scoped-tool first)))
          (with-current-buffer first
            (cl-letf (((symbol-function 'completing-read-multiple)
                       (lambda (&rest _) '("check"))))
              (gptel-agent-toggle-skill)))
          (let ((next (gptel-agent-skills-test--scoped-tool first))
                (other (gptel-agent-skills-test--scoped-tool second)))
            (should (string-match-p "Deploy" (gptel-tool-description old)))
            (should (string-match-p "Deploy" (gptel-tool-description other)))
            (should-not (string-match-p "Deploy" (gptel-tool-description next)))
            (should (string-match-p "not enabled for this request"
                                    (funcall (gptel-tool-function next) "deploy"))))
          (should-not (buffer-local-value 'gptel-agent--enabled-skills second))
          (should-not (default-value 'gptel-agent--enabled-skills)))
      (kill-buffer first) (kill-buffer second))))

(ert-deftest gptel-agent-skills-children-inherit-custom-discovery-configuration ()
  (let ((gptel-agent-skills-test--extra-tools '("Agent" "Discussion")))
    (gptel-agent-skills-test--with-chat ""
      (let ((directory (expand-file-name "capabilities/deploy" root)))
        (make-directory directory t)
        (with-temp-file (expand-file-name "SKILL.md" directory)
          (insert "---\nname: deploy\ndescription: Project deployment\n---\nPROJECT-DEPLOYMENT")))
      (gptel-agent-workers-test--answer fsm "Ready")
      (with-current-buffer parent
        (setq-local default-directory (file-name-as-directory root)
                    gptel-agent-skill-dirs '("capabilities")
                    gptel-agent--skills nil
                    gptel-agent--enabled-skills '("deploy")
                    gptel-confirm-tool-calls nil
                    gptel-agent-preset '(:tools ("Skill")))
        (goto-char (point-max)) (insert "Delegate") (gptel-send))
      (setq fsm (caar requests))
      (let ((gptel-agent--skills nil)
            (gptel-agent-skill-dirs '("nonexistent-global-directory")))
        (gptel-agent-workers-test--tool
         fsm "Agent" '(:subagent_type "researcher" :description "Use project skill"
                        :prompt "Load deploy" :background t))
        (let* ((run (car (hash-table-values gptel-agent--runs)))
               (child (gptel-agent--run-child-buffer run))
               (request (gptel-agent--run-fsm run)))
          (should (equal (buffer-local-value 'gptel-agent-skill-dirs child) '("capabilities")))
          (should (equal (buffer-local-value 'gptel-agent--enabled-skills child) '("deploy")))
          (gptel-agent-workers-test--tool request "Skill" '(:skill "deploy"))
          (should (string-match-p "PROJECT-DEPLOYMENT" (cdar requests)))
          (gptel-agent-workers-test--answer request "Worker done"))
        (gptel-agent-workers-test--tool fsm "Discussion" '(:title "Project skill" :prompt "Load deploy"))
        (let* ((chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
               (child (gptel-agent-discussion--chat-buffer chat))
               (request (gptel-agent-discussion--chat-fsm chat)))
          (should (equal (buffer-local-value 'gptel-agent-skill-dirs child) '("capabilities")))
          (should (equal (buffer-local-value 'gptel-agent--enabled-skills child) '("deploy")))
          (gptel-agent-workers-test--tool request "Skill" '(:skill "deploy"))
          (should (string-match-p "PROJECT-DEPLOYMENT" (cdar requests)))
          (gptel-agent-workers-test--answer request "Discussion done"))))))

(ert-deftest gptel-agent-skills-failed-discovery-retries-the-same-scope ()
  (let* ((root (make-temp-file "gptel-skill-retry-" t))
         (gptel-agent-skill-dirs (list root))
         (gptel-agent--bundled-skill-dir root))
    (unwind-protect
        (with-temp-buffer
          (with-temp-file (expand-file-name "SKILL.md" root)
            (insert "---\nname: retry\n"))
          (should-error (gptel-agent--ensure-skills))
          (with-temp-file (expand-file-name "SKILL.md" root)
            (insert "---\nname: retry\ndescription: Repaired skill\n---\nReady"))
          (should (assoc "retry" (gptel-agent--ensure-skills))))
      (delete-directory root t))))


(ert-deftest gptel-agent-skills-children-retain-request-config-and-preset-overrides ()
  (dolist (captured-selection '(nil none ("deploy")))
    (dolist (override '(nil t))
      (let ((gptel-agent-skills-test--extra-tools '("Agent" "Discussion"))
            (gptel--known-presets (copy-tree gptel--known-presets)))
        (gptel-make-preset 'skill-child-selection
          :agent--enabled-skills '("deploy" "child-extra"))
        (gptel-agent-skills-test--with-chat ""
          (dolist (name '("captured" "later"))
            (let ((directory (expand-file-name (concat name "/deploy") root)))
              (make-directory directory t)
              (with-temp-file (expand-file-name "SKILL.md" directory)
                (insert "---\nname: deploy\ndescription: Project deployment\n---\n"
                        (upcase name) "-DEPLOYMENT"))))
          (gptel-agent-workers-test--answer fsm "Ready")
          (with-current-buffer parent
            (setq-local default-directory (file-name-as-directory root)
                        gptel-agent-skill-dirs '("later")
                        gptel-agent--skills nil
                        gptel-agent--enabled-skills '("owner-before")
                        gptel-confirm-tool-calls nil
                        gptel-agent-preset
                        (append '(:tools ("Skill"))
                                (when override '(:agent--enabled-skills ("deploy" "child-extra"))))
                        gptel-prompt-transform-functions
                        (cons (lambda (request)
                                ;; These are request-only settings, as supplied
                                ;; by an inline preset in the prompt buffer.
                                (when (eq (plist-get (gptel-fsm-info request) :buffer) parent)
                                  (setq-local gptel-agent-skill-dirs '("captured")
                                              gptel-agent--enabled-skills captured-selection)))
                              gptel-prompt-transform-functions))
            (goto-char (point-max)) (insert "Delegate") (gptel-send))
          (setq fsm (caar requests))
          (with-current-buffer parent
            (setq-local gptel-agent--enabled-skills '("owner-after")))
          (let ((gptel-agent--skills nil)
                (expected-selection (if override '("deploy" "child-extra") captured-selection)))
            (gptel-agent-workers-test--tool
             fsm "Agent" '(:subagent_type "researcher" :description "Use captured skills"
                            :prompt "Load deploy" :background t))
            (let* ((run (car (hash-table-values gptel-agent--runs)))
                   (child (gptel-agent--run-child-buffer run))
                   (request (gptel-agent--run-fsm run)))
              (should (equal (buffer-local-value 'gptel-agent-skill-dirs child) '("captured")))
              (should (equal (buffer-local-value 'gptel-agent--enabled-skills child) expected-selection))
              (gptel-agent-workers-test--tool request "Skill" '(:skill "deploy"))
              (should (string-match-p (if (eq expected-selection 'none)
                                          "not enabled for this request"
                                        "CAPTURED-DEPLOYMENT")
                                      (cdar requests)))
              (gptel-agent-workers-test--answer request "Worker done"))
            (gptel-agent-workers-test--tool
             fsm "Discussion" (append '(:title "Captured skill selection" :prompt "Load deploy")
                                      (when override '(:preset "skill-child-selection"))))
            (let* ((chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
                   (child (gptel-agent-discussion--chat-buffer chat))
                   (request (gptel-agent-discussion--chat-fsm chat)))
              (should (equal (buffer-local-value 'gptel-agent-skill-dirs child) '("captured")))
              (should (equal (buffer-local-value 'gptel-agent--enabled-skills child) expected-selection))
              (gptel-agent-workers-test--tool request "Skill" '(:skill "deploy"))
              (should (string-match-p (if (eq expected-selection 'none)
                                          "not enabled for this request"
                                        "CAPTURED-DEPLOYMENT")
                                      (cdar requests)))
              (gptel-agent-workers-test--answer request "Discussion done"))))))))

(provide 'gptel-agent-skills-test)
