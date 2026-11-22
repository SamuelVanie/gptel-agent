;;; gptel-agent-project-preset-test.el --- Project-specific presets -*- lexical-binding: t; -*-

(require 'gptel-agent-startup-header-test)
(require 'gptel-agent-discussion-test)
(require 'gptel-agent-followup-test)
(require 'gptel-transient)
(require 'gptel-openai)

(defun gptel-agent-project-preset-test--project (root name tokens)
  "Create project NAME under ROOT with a distinct prompt and token limit."
  (let* ((project (file-name-as-directory (expand-file-name name root)))
         (dir (expand-file-name "agents" project)))
    (make-directory dir t)
    (dolist (preset '("gptel-agent" "gptel-plan"))
      (with-temp-file (expand-file-name (concat preset ".md") dir)
        (insert (format "---\nmax-tokens: %d\n---\n%s %s" tokens name preset))))
    (with-temp-file (expand-file-name (concat name "-worker.md") dir)
      (insert "---\ndescription: Project worker\n---\nWork on this project."))
    project))

(defun gptel-agent-project-preset-test--refresh (directory)
  (with-temp-buffer
    (setq default-directory directory)
    (gptel-agent-update)))

(defmacro gptel-agent-project-preset-test--with-projects (&rest body)
  "Run BODY with Alpha and Bravo projects and collect outgoing request settings."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "gptel-agent-presets-" t))
          (alpha (gptel-agent-project-preset-test--project root "Alpha" 111))
          (bravo (gptel-agent-project-preset-test--project root "Bravo" 222))
          (gptel-agent-dirs '("agents")) (gptel-agent-skill-dirs nil)
          (gptel-agent--agents nil) (gptel-agent--agent-files nil)
          (gptel-agent--registered-agents nil) (gptel-agent--file-cache nil)
          (gptel--known-presets nil) (gptel--known-backends nil)
          (gptel-backend (gptel-make-openai "presets-test" :host "presets.invalid"
                                            :key "unused" :models '(test)))
          (gptel-model 'test) (gptel-default-mode 'text-mode)
          (gptel-use-curl t) (gptel-stream nil) (gptel--set-buffer-locally t)
          (gptel-agent-session-mode nil) requests)
     (unwind-protect
         (cl-letf (((symbol-function 'gptel-curl-get-response)
                    (lambda (fsm)
                      (let* ((data (plist-get (gptel-fsm-info fsm) :data))
                             (system (aref (plist-get data :messages) 0)))
                        (push (list (plist-get system :content)
                                    (plist-get data :max_tokens)) requests)))))
           (gptel-agent-project-preset-test--refresh alpha)
           (gptel-agent-project-preset-test--refresh bravo)
           ,@body)
       (delete-directory root t))))

(ert-deftest gptel-agent-project-preset-header-selects-current-project ()
  (gptel-agent-project-preset-test--with-projects
    (let ((chat-name (generate-new-buffer-name "*gptel-agent:Alpha*"))
          (gptel-use-header-line t))
      (save-window-excursion
        (unwind-protect
            (progn
              (gptel-agent alpha)
              (gptel-agent-project-preset-test--refresh bravo)
              (with-current-buffer chat-name
                (gptel-agent-startup-header-test--click-selector header-line-format)
                (gptel-request "Plan this change")
                (should (equal (pop requests) '("Alpha gptel-plan" 111)))))
          (when (get-buffer chat-name) (kill-buffer chat-name)))))))

(ert-deftest gptel-agent-project-preset-menu-and-inline-select-current-project ()
  (gptel-agent-project-preset-test--with-projects
    (with-temp-buffer
      (setq default-directory alpha)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "gptel-plan")))
        (call-interactively #'gptel-preset))
      (gptel-request "Plan this change")
      (should (equal (pop requests) '("Alpha gptel-plan" 111)))
      ;; A programmatic send can originate outside the chat.  Both its
      ;; directory and its local discovery configuration belong to the chat.
      (rename-file (expand-file-name "agents" alpha) (expand-file-name "local-agents" alpha))
      (setq-local gptel-agent-dirs '("local-agents"))
      (let ((chat (current-buffer)))
        (with-temp-buffer
          (setq default-directory bravo)
          (gptel-agent-update)
          (gptel-request "@gptel-agent Write the change" :buffer chat
            :transforms '(gptel--transform-apply-preset))))
      (should (equal (pop requests) '("Alpha gptel-agent" 111)))
      (should (equal gptel-system-prompt "Alpha gptel-plan")))
    ;; An ordinary chat has no prior agent snapshot.  Its first inline Plan
    ;; request must retain the workers selected in the temporary prompt.
    (with-temp-buffer
      (setq-local default-directory alpha gptel-agent-dirs '("local-agents")
                  gptel-use-tools t gptel-tools (list (gptel-get-tool "Agent")))
      (let* ((fsm (gptel-request "@gptel-plan Delegate the investigation"
                    :transforms '(gptel--transform-apply-preset gptel-agent--localize-agent-tool)))
             (data (plist-get (gptel-fsm-info fsm) :data))
             (tools (json-parse-string (gptel--json-encode (plist-get data :tools))
                                       :object-type 'plist))
             (agent (seq-find (lambda (tool) (equal (map-nested-elt tool '(:function :name)) "Agent"))
                              tools)))
        (should (equal (map-nested-elt agent '(:function :parameters :properties :subagent_type :enum))
                       ["Alpha-worker"]))))))

(ert-deftest gptel-agent-project-preset-honors-explicit-overrides ()
  (gptel-agent-project-preset-test--with-projects
    (with-temp-buffer
      (setq default-directory alpha)
      ;; Refresh must also upgrade the package's entries after a live reload.
      (gptel-make-preset 'gptel-plan
        :system "Previous project" :agent--current-agent "gptel-plan"
        :agent--registry-snapshot '(:eval (gptel-agent--refresh-registry)))
      (gptel-agent-project-preset-test--refresh bravo)
      (gptel-make-preset 'custom-plan :parents 'gptel-plan :max-tokens 333)
      (gptel-preset 'custom-plan)
      (gptel-request "Use the derived preset")
      (should (equal (pop requests) '("Alpha gptel-plan" 333)))
      (setq-local gptel-max-tokens 555)
      (gptel-request "Use my request settings" :system "Turn override")
      (should (equal (pop requests) '("Turn override" 555)))
      (dolist (name '(unrelated gptel-plan))
        (gptel-make-preset name :system "User preset" :max-tokens 444)
        (gptel-agent-project-preset-test--refresh bravo)
        (gptel-preset name)
        (gptel-request "Use my preset")
        (should (equal (pop requests) '("User preset" 444)))))))

(ert-deftest gptel-agent-project-preset-rejects-missing-definition ()
  (gptel-agent-project-preset-test--with-projects
    (with-temp-buffer
      (setq default-directory (file-name-as-directory root))
      (should-error (gptel-preset 'gptel-plan) :type 'user-error))))

(defvar gptel-runner--agents)
(defvar gptel-runner--runs)

(ert-deftest gptel-agent-project-preset-runner-selects-node-workspace ()
  (skip-unless (require 'gptel-runner nil t))
  (gptel-agent-project-preset-test--with-projects
    (let* ((gptel-runner--agents (make-hash-table :test #'eq))
           (gptel-runner--runs (make-hash-table :test #'equal))
           (default-directory bravo)
           run chat)
      (unwind-protect
          (progn
            (gptel-runner-register-agent 'planner :preset 'gptel-plan)
            (setq run (gptel-runner-start
                       (gptel-runner-agent-step :id 'plan :agent 'planner :prompt "Plan this change")
                       :workspace alpha :completion 'response :persist nil)
                  chat (gptel-runner-call-buffer (car (gptel-runner-run-calls run))))
            (should (equal (pop requests) '("Alpha gptel-plan" 111))))
        (when run (gptel-runner-abort-run run))
        (when (buffer-live-p chat) (kill-buffer chat))))))

(ert-deftest gptel-agent-project-preset-children-resolve-in-selected-workspace ()
  (gptel-agent-project-preset-test--with-projects
    (gptel-agent-project-preset-test--refresh alpha)
    (gptel-agent-discussion-test--with-chat
      (with-current-buffer parent
        (setq default-directory alpha)
        (gptel-agent-workers-test--tool
         fsm "Discussion" (list :title "Review" :prompt "Review"
                                :preset "gptel-plan" :directory bravo))
        (setq chat (car gptel-agent-discussion--children))
        (with-current-buffer (gptel-agent-discussion--chat-buffer chat)
          (should (equal gptel-system-prompt "Bravo gptel-plan"))
          (should (= gptel-max-tokens 222))
          (should (assoc "Bravo-worker" gptel-agent--registry-snapshot))
          (should-not (assoc "Alpha-worker" gptel-agent--registry-snapshot)))))
    (gptel-agent-followup-test--with-chat
      (with-current-buffer parent
        (setq default-directory alpha)
        (setq-local gptel-agent-preset 'gptel-plan)
        (gptel-agent--task deliver "researcher" "Review" "Review"
                           parent-fsm snapshot nil nil nil nil bravo)
        (should (gptel-fsm-p (car requests)))
        (with-current-buffer (plist-get (gptel-fsm-info (car requests)) :buffer)
          (should (= gptel-max-tokens 222))
          (should (equal gptel-agent--current-agent "researcher"))
          (should (equal gptel-agent--agent-snapshot snapshot)))
        (gptel-agent-followup-test--finish (car requests) "Done")))))

(provide 'gptel-agent-project-preset-test)
;;; gptel-agent-project-preset-test.el ends here
