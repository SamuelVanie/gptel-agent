;;; gptel-agent-registration-test.el --- Elisp agent registration -*- lexical-binding: t; -*-

(require 'gptel-agent-tools-test)
(require 'gptel-agent-workers-test)

(ert-deftest gptel-agent-registration-survives-refresh-and-replaces-definitions ()
  (gptel-agent-test--with-main-presets
    (let* ((properties (list :description "Reviews code" :system "Review carefully."
                             :tools (list "Read") :agents nil :task-timeout 60))
           (name (apply #'gptel-agent-register 'reviewer properties)))
      (should (equal name "reviewer"))
      (setcar (plist-get properties :tools) "Write")
      (dotimes (_ 2)
        (gptel-agent-update)
        (let ((definition (gptel-agent--agent-plist name)))
          (should (equal (plist-get definition :system) "Review carefully."))
          (should (equal (plist-get definition :tools) '("Read")))
          (should (plist-member definition :agents))
          (should-not (plist-get definition :agents))
          (should (= (plist-get definition :task-timeout) 60))))
      (gptel-agent-register "reviewer" :description "New role" :system "New prompt")
      (gptel-agent-update)
      (let ((definition (cdr (assoc "reviewer" gptel-agent--registry-snapshot))))
        (should (equal definition '(:description "New role" :system "New prompt")))))))

(ert-deftest gptel-agent-registration-overrides-file-body-and-metadata ()
  (gptel-agent-test--with-main-presets
    (should (assoc "researcher" gptel-agent--agent-files))
    (gptel-agent-register 'researcher
      :description "Custom researcher" :system "Use my research instructions.")
    (dotimes (_ 2)
      (gptel-agent-update)
      (should-not (assoc "researcher" gptel-agent--agent-files))
      (should (equal (gptel-agent--agent-plist "researcher")
                     '(:description "Custom researcher"
                       :system "Use my research instructions.")))
      (should (equal (plist-get (cdr (assoc "researcher" gptel-agent--agents))
                                :description)
                     "Custom researcher")))))

(ert-deftest gptel-agent-registration-allowlist-is-additive-and-idempotent ()
  (gptel-agent-test--with-main-presets
    (gptel-agent-register 'reviewer :description "Reviews code")
    (should (equal (gptel-agent-add-subagent 'gptel-agent 'reviewer) "reviewer"))
    (gptel-agent-add-subagent "gptel-agent" "reviewer")
    ;; File-defined workers can also be added without registering them again.
    (gptel-agent-add-subagent 'gptel-agent 'introspector)
    (dotimes (_ 2)
      (gptel-agent-update)
      (should (equal (plist-get (gptel-agent--agent-plist "gptel-agent") :agents)
                     '("researcher" "result-explorer" "reviewer" "introspector")))
      (should (equal (plist-get (gptel-agent--agent-plist "ask") :agents)
                     '("researcher" "result-explorer"))))))

(ert-deftest gptel-agent-registration-preserves-unrestricted-and-empty-allowlists ()
  (gptel-agent-test--with-main-presets
    (gptel-agent-register 'custom-parent :description "Parent")
    (gptel-agent-add-subagent 'custom-parent 'researcher)
    (gptel-agent-update)
    (should-not (plist-member (gptel-agent--agent-plist "custom-parent") :agents))
    ;; The explicit addition survives replacing the parent's definition.
    (gptel-agent-register 'custom-parent :description "Parent" :agents nil)
    (gptel-agent-update)
    (should (equal (plist-get (gptel-agent--agent-plist "custom-parent") :agents)
                   '("researcher")))
    (gptel-agent-register 'custom-parent
      :description "Parent" :agents '(result-explorer "researcher"))
    (gptel-agent-update)
    (should (equal (plist-get (gptel-agent--agent-plist "custom-parent") :agents)
                   '("result-explorer" "researcher")))))

(ert-deftest gptel-agent-registration-additions-follow-file-changes ()
  (gptel-agent-test--with-main-presets
    (let* ((directory (make-temp-file "gptel-agent-registration-" t))
           (file (expand-file-name "custom-parent.md" directory))
           (gptel-agent-dirs (cons directory gptel-agent-dirs)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "---\nname: custom-parent\nagents: []\n---\nFirst prompt."))
            (gptel-agent-add-subagent 'custom-parent 'researcher)
            (should (equal (plist-get (gptel-agent--agent-plist "custom-parent") :agents)
                           '("researcher")))
            (with-temp-file file
              (insert "---\nname: custom-parent\nagents: [introspector]\n---\nNew prompt."))
            (set-file-times file (time-add (current-time) 2))
            (gptel-agent-update)
            (let ((definition (gptel-agent--agent-plist "custom-parent")))
              (should (equal (plist-get definition :agents) '("introspector" "researcher")))
              (should (equal (plist-get definition :system) "New prompt.")))
            (should (equal (plist-get (cdr (gptel-agent-read-file file)) :agents)
                           '("introspector"))))
        (delete-directory directory t)))))

(ert-deftest gptel-agent-registration-validates-before-changing-definitions ()
  (gptel-agent-test--with-main-presets
    (dolist (args '((nil :description "Invalid name")
                    (" " :description "Invalid name")
                    (42 :description "Invalid name")
                    (:reviewer :description "Invalid name")
                    (reviewer :system "Missing description")
                    (reviewer :description " ")
                    (reviewer description "Not a keyword")
                    (reviewer :description "Odd plist" :tools)
                    (reviewer :description "Extra name" :name other)
                    (reviewer :description "Invalid agents" :agents "researcher")
                    (reviewer :description "Invalid agent name" :agents (nil))))
      (should-error (apply #'gptel-agent-register args) :type 'user-error)
      (should-not gptel-agent--registered-agents))
    (dolist (args '((missing-parent researcher)
                    (gptel-agent missing-worker)
                    (gptel-agent ask)
                    (gptel-agent gptel-plan)
                    (ask gptel-agent)
                    (researcher researcher)
                    (researcher RESEARCHER)))
      (should-error (apply #'gptel-agent-add-subagent args) :type 'user-error)
      (should-not gptel-agent--additional-subagents))))

(ert-deftest gptel-agent-registration-refresh-keeps-existing-request-stable ()
  (gptel-agent-test--with-main-presets
    (setq-local gptel-agent--current-agent "gptel-agent"
                gptel-tools (list (gptel-get-tool "Agent")))
    (let* ((fsm (gptel-make-fsm :info (list :buffer (current-buffer))))
           (old-registry gptel-agent--registry-snapshot))
      (gptel-agent--localize-agent-tool fsm)
      (let* ((old-tool (cl-find "Agent" gptel-tools :key #'gptel-tool-name :test #'equal))
             (old-enum (copy-sequence (plist-get (car (gptel-tool-args old-tool)) :enum))))
        (gptel-agent-register 'reviewer :description "Reviews code" :system "Review.")
        (gptel-agent-add-subagent 'gptel-agent 'reviewer)
        (should-not (assoc "reviewer" (gptel-agent--snapshot-agent-definitions)))
        (gptel-agent-update)
        (should (assoc "reviewer" (gptel-agent--snapshot-agent-definitions)))
        (should-not (assoc "reviewer" old-registry))
        (gptel-agent--localize-agent-tool (gptel-make-fsm))
        (let ((new-tool (cl-find "Agent" gptel-tools :key #'gptel-tool-name :test #'equal)))
          (should (member "reviewer" (append (plist-get (car (gptel-tool-args new-tool)) :enum) nil))))
        (should (equal (plist-get (car (gptel-tool-args old-tool)) :enum) old-enum))
        (let (result)
          (funcall (gptel-tool-function old-tool) (lambda (value) (setq result value))
                   "reviewer" "Review" "Check code")
          (should (string-match-p "not available for this request" result)))
        (setq-local gptel-agent--enabled-agents '("researcher"))
        (should-not (assoc "reviewer" (gptel-agent--snapshot-agent-definitions)))
        (setq-local gptel-agent--enabled-agents nil
                    gptel-agent--agent-snapshot old-registry)
        (should-not (assoc "reviewer" (gptel-agent--snapshot-agent-definitions)))))))

(ert-deftest gptel-agent-registration-starts-worker-with-elisp-configuration ()
  (gptel-agent-test--with-main-presets
    (let (hook-ran)
      (gptel-agent-register 'reviewer
        :description "Reviews code" :system "Review carefully."
        :tools '("Read") :temperature 0.3 :agents nil
        :pre (lambda () (setq hook-ran t)))
      (gptel-agent-add-subagent 'gptel-agent 'reviewer)
      (should-not hook-ran)
      (gptel-agent-workers-test--with-chat
        (with-current-buffer parent
          (setq-local gptel-agent--current-agent "gptel-agent"
                      gptel-agent--agent-snapshot nil)
          (gptel-agent-update)
          (let (result)
            (gptel-agent--task (lambda (value) (setq result value))
                               "reviewer" "Review code" "Check the patch" fsm
                               (gptel-agent--snapshot-agent-definitions)
                               nil nil nil t)
            (should (string-match-p "run_id:" result))))
        (should hook-ran)
        (let ((run (car (hash-table-values gptel-agent--runs))))
          (should (equal (gptel-agent--run-agent run) "reviewer"))
          (with-current-buffer (gptel-agent--run-child-buffer run)
            (should (string-match-p "Review carefully" gptel--system-message))
            (should (= gptel-temperature 0.3))
            (should (equal (mapcar #'gptel-tool-name gptel-tools) '("Read")))))))))

(provide 'gptel-agent-registration-test)
;;; gptel-agent-registration-test.el ends here
