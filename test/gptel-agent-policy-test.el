;;; gptel-agent-policy-test.el --- Argument policies and inheritance -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent-skills-test)
(require 'gptel-agent-session-test)
(require 'gptel-agent-instructions-test)

(ert-deftest gptel-agent-policy-names-in-markdown-and-org-presets ()
  (dolist (format '(markdown org))
    (let ((file (make-temp-file "tool-policy-preset" nil (if (eq format 'org) ".org" ".md"))))
      (unwind-protect
          (dolist (setting '("mr-review" "nil"))
            (with-temp-file file
              (insert (if (eq format 'org)
                          (format ":PROPERTIES:\n:NAME: example\n:AGENT-TOOL-POLICY: %s\n:END:\n" setting)
                        (format "---\nname: example\nagent-tool-policy: %s\n---\n" setting))
                      "Review the change."))
            (let ((preset (cdr (gptel-agent-read-file file))))
              (should (plist-member preset :agent-tool-policy))
              (should (eq (plist-get preset :agent-tool-policy) (intern setting)))))
        (delete-file file)))))

(ert-deftest gptel-agent-policy-bash-command-and-typed-optional-flag ()
  (let ((gptel-agent-policy--definitions (make-hash-table))
        (ceiling (list (cons 'review default-directory)))
        (tool (gptel-get-tool "Bash")))
    (gptel-agent-define-tool-policy 'review
      :allow '((:tool "Bash" :args
                (:background (optional false)
                 :command (rx "glab mr " (or "list" (seq (or "view" "diff") " "
                                                          (in "1-9") (* (in "0-9"))))
                              " -R https://gitlab.example.com/team/project")))))
    (cl-flet ((admitted (command &rest flags)
                (gptel-agent-policy--allows-p
                 ceiling tool (append (list :command command :query "Review") flags)
                 default-directory)))
      (let ((repo " -R https://gitlab.example.com/team/project"))
        (dolist (command '("glab mr list" "glab mr view 1" "glab mr diff 123"))
          (should (admitted (concat command repo)))
          (should (admitted (concat command repo) :background :json-false)))
        (dolist (value '(nil "false" :null 0 t))
          (should-not (admitted (concat "glab mr list" repo) :background value)))
        (dolist (command '("glab mr " "glab mr merge 3" "glab mr view 1 --web"
                           "glab mr view 0" "GLAB MR LIST" "x glab mr list"))
          (should-not (admitted (concat command repo))))
        (dolist (suffix '("/other" " && echo x" "\n" "; echo x"))
          (should-not (admitted (concat "glab mr list" repo suffix))))))))

(ert-deftest gptel-agent-policy-request-schema-and-deny-precedence ()
  (let ((gptel-agent-policy--definitions (make-hash-table))
        (ceiling (list (cons 'catalog default-directory)))
        (tool (gptel--make-tool :name "UnregisteredCatalog"
                                :args '((:name "tool" :type string) (:name "args" :type object)))))
    (gptel-agent-define-tool-policy 'catalog
      :allow '((:tool "UnregisteredCatalog" :args (:tool (one-of "read" "find")
                                                   :args (object :limit 3 :flag (optional true))))))
    (cl-flet ((admitted (args)
                (gptel-agent-policy--allows-p ceiling tool args default-directory)))
      (should (admitted '(:tool "find" :args (:other [6] :limit 3))))
      (should (admitted '(:tool "read" :args (:limit 3 :flag t))))
      (dolist (args '((:tool "read" :args (:limit "3")) (:tool "find" :args [3])
                      (:tool "find" :args (:limit 3 :flag :json-false))
                      (:tool "else" :args (:limit 3)) (:tool "read" :args nil)))
        (should-not (admitted args)))
      (gptel-agent-define-tool-policy 'catalog
        :allow '((:tool "UnregisteredCatalog"))
        :deny '((:tool "UnregisteredCatalog" :args (:tool "write"))))
      (should (admitted '(:tool "read")))
      (should-not (admitted '(:tool "write")))
      (gptel-agent-define-tool-policy 'catalog
        :allow '((:tool "UnregisteredCatalog" :args (:Tool (optional "read")))))
      (should-not (admitted '(:tool "read")))
      (should-error (gptel-agent-define-tool-policy 'catalog
                      :allow '((:tool "UnregisteredCatalog" :origin "oops"))))
      (should-not (admitted nil)))))

(ert-deftest gptel-agent-policy-origin-scheme-authority-and-effective-port ()
  (let ((matches (gptel-agent-policy--compile-value
                  '(url-origin "https://catalog.example.org:443"))))
    (dolist (url '("https://CATALOG.example.org" "https://catalog.example.org:443/"
                   "https://a@catalog.example.org/x?next=https://other.example/#result"))
      (should (funcall matches url)))
    (dolist (url '("https://catalog.example.org:444" "http://catalog.example.org/"
                   "https://catalog.example.org@other.example/" "https://catalog.example.org.evil/"
                   "https://catalog.example.org%2f@evil/" "https://evil\\@catalog.example.org/"
                   "https://catalog.example.org:invalid/" "https:catalog.example.org"
                   "//catalog.example.org/" nil 42))
      (should-not (funcall matches url)))
    (should-error (gptel-agent-policy--compile-value '(url-origin "https://example.org/a")))))

(ert-deftest gptel-agent-policy-literal-objects-and-arrays ()
  (let ((matches (gptel-agent-policy--compile-value
                  '(:labels ["one" "two"] :flags [true false] :rank 3))))
    (should (funcall matches '(:flags [t :json-false] :rank 3 :labels ["one" "two"])))
    (dolist (args '((:flags ["true" :json-false] :rank 3 :labels ["one" "two"])
                    (:flags [t :json-false] :rank 3 :labels ["two" "one"])
                    (:flags [t :json-false] :rank 3 :labels ["one" "two"] :extra t)))
      (should-not (funcall matches args)))))

(ert-deftest gptel-agent-policy-optional-nested-key-honors-closed-schema ()
  (let* ((gptel-agent-policy--definitions (make-hash-table))
         (ceiling (list (cons 'schema-test default-directory)))
         (item '(:type object :properties (:reviewed (:type boolean))
                       :additionalProperties :json-false))
         (tool (gptel--make-tool :name "Batch"
                                 :args (list (list :name "rows" :type 'array :items item))))
         (args '(:rows [(:reviewed t)])))
    (gptel-agent-define-tool-policy 'schema-test
      :allow '((:tool "Batch" :args (:rows (every (object :reviewd (optional false)))))))
    (should-not (gptel-agent-policy--allows-p ceiling tool args default-directory))
    (setf (gptel-tool-args tool) '((:name "rows" :type array :items (:type object))))
    (should (gptel-agent-policy--allows-p ceiling tool args default-directory))))

(ert-deftest gptel-agent-policy-ambiguous-null-fails-closed-in-deny ()
  (let ((gptel-agent-policy--definitions (make-hash-table))
        (ceiling (list (cons 'nullable default-directory)))
        (tool (gptel--make-tool :name "Review" :args '((:name "field")))))
    (dolist (pattern '(null (object :key (optional true))))
      (gptel-agent-define-tool-policy 'nullable
        :allow '((:tool "Review"))
        :deny `((:tool "Review" :args (:field ,pattern))))
      (should (gptel-agent-policy--allows-p ceiling tool nil default-directory))
      (should-not (gptel-agent-policy--allows-p ceiling tool '(:field nil) default-directory)))))

(ert-deftest gptel-agent-policy-child-paths-use-parent-workspace ()
  (let* ((gptel-agent-policy--definitions (make-hash-table))
         (root (make-temp-file "agent-policy-root-" t))
         (parent (expand-file-name "parent" root))
         (child (expand-file-name "child" root))
         (ceiling (list (cons 'reads parent)))
         (tool (gptel-get-tool "Read")))
    (unwind-protect
        (progn
          (make-directory parent)
          (make-directory child)
          (gptel-agent-define-tool-policy 'reads
            :allow '((:tool "Read" :args (:file_path (under workspace)))))
          (should (gptel-agent-policy--allows-p ceiling tool '(:file_path "../parent/read") child))
          (should-not (gptel-agent-policy--allows-p ceiling tool '(:file_path "read") child))
          (make-symbolic-link child (expand-file-name "outside" parent))
          (should-not (gptel-agent-policy--allows-p ceiling tool '(:file_path "outside/read") parent))
          (should-not (gptel-agent-policy--allows-p
                       ceiling tool '(:file_path "outside/../read") parent))
          (let ((hidden (expand-file-name ".git" parent)))
            (make-directory hidden)
            (gptel-agent-define-tool-policy 'reads
              :allow '((:tool "Read"))
              :deny '((:tool "Read" :args (:file_path (under ".git")))))
            (should-not (gptel-agent-policy--allows-p
                         ceiling tool '(:file_path ".git/new/subfile") parent))
            (should (eq (not (gptel-agent-policy--allows-p
                              ceiling tool '(:file_path ".GIT/new/subfile") parent))
                        (and (file-equal-p hidden (expand-file-name ".GIT" parent)) t)))))
      (delete-directory root t))))

(ert-deftest gptel-agent-policy-each-live-version-keeps-its-workspace ()
  (let* ((gptel-agent-policy--definitions (make-hash-table))
         (parent (cons 'review default-directory))
         (child (cons 'review temporary-file-directory))
         (ceiling (list parent child))
         (tool (gptel-get-tool "Read"))
         (args (list :file_path (expand-file-name "README.org") :start_line 1)))
    (gptel-agent-define-tool-policy 'review
      :allow '((:tool "Read" :args (:file_path (under workspace)))))
    (let ((original (gptel-agent-policy--freeze (list parent))))
      (gptel-agent-define-tool-policy 'review
        :allow '((:tool "Read" :args (:start_line 1))))
      (let ((gptel-agent-policy--snapshot (gptel-agent-policy--freeze ceiling original child)))
        (should (gptel-agent-policy--allows-p ceiling tool args temporary-file-directory))
        (should-not (gptel-agent-policy--allows-p
                     ceiling tool (plist-put (copy-sequence args) :start_line 2) default-directory)))
      (let ((gptel-agent-policy--snapshot
             (gptel-agent-policy--freeze (list parent) original parent)))
        (should-not (gptel-agent-policy--allows-p
                     (list parent) tool (list :file_path "README.org") default-directory))))))

(ert-deftest gptel-agent-policy-live-eval-runs-only-admitted-expressions ()
  (let ((gptel-agent-policy--definitions (make-hash-table)))
    (gptel-agent-instructions-test--with-project
      (gptel-agent-define-tool-policy 'inspect
        :allow '((:tool "Eval" :args (:expression (rx (or "(buffer-name)" "(point)"))))))
      (with-current-buffer parent
        (setq-local gptel-agent-tool-policy 'inspect
                    gptel-tools (list (gptel-get-tool "Eval"))))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (gptel-agent-instructions-test--tool fsm "Eval" '(:expression "(buffer-name)"))
      (should (string-search (buffer-name parent) (gptel-agent-workers-test--data fsm)))
      (dolist (expression '("(point) " "(+ 100 23)"))
        (gptel-agent-instructions-test--tool fsm "Eval" (list :expression expression)))
      (should (string-search "Tool policy denied Eval" (gptel-agent-workers-test--data fsm)))
      (should-not (string-search "Result:\n123" (gptel-agent-workers-test--data fsm)))
      (with-current-buffer parent (setq-local gptel-agent-tool-policy nil))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (gptel-agent-instructions-test--tool fsm "Eval" '(:expression "(+ 100 23)"))
      (should (string-search "Result:\n123" (gptel-agent-workers-test--data fsm))))))

(ert-deftest gptel-agent-policy-bash-dispatch-checks-before-launch ()
  (let ((gptel-agent-policy--definitions (make-hash-table)) commands)
    (gptel-agent-instructions-test--with-project
      (gptel-agent-define-tool-policy 'list-only
        :allow '((:tool "Bash" :args (:command "glab mr list" :background (optional false)))))
      (with-current-buffer parent
        (setq-local gptel-agent-tool-policy 'list-only gptel-tools (list (gptel-get-tool "Bash"))))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (cl-letf (((symbol-function 'gptel-agent--start-foreground-bash)
                 (lambda (callback command _query _fsm)
                   (push command commands) (funcall callback "Local fixture")))
                ((symbol-function 'gptel-agent--start-background-process)
                 (lambda (&rest _) (ert-fail "Disallowed background command launched"))))
        (dolist (args '((:command "glab mr list" :query "List reviews")
                        (:command "glab mr list" :background :json-false :query "List reviews")
                        (:command "glab mr list" :background t :query "List reviews")
                        (:command "glab mr list; echo x" :query "List reviews")))
          (gptel-agent-instructions-test--tool fsm "Bash" args))
        (should (equal commands '("glab mr list" "glab mr list")))
        (should (string-search "Tool policy denied Bash" (gptel-agent-workers-test--data fsm)))))))

(ert-deftest gptel-agent-policy-agent-follows-parent-when-child-preset-clears-policy ()
  (let ((gptel-agent-policy--definitions (make-hash-table)))
    (gptel-agent-followup-test--with-chat
      (gptel-agent-define-tool-policy 'parent-read
        :allow '((:tool "Read" :args (:file_path (under workspace)))))
      (with-current-buffer parent
        (setq-local gptel-agent-tool-policy 'parent-read
                    gptel-agent-preset '(:agent-tool-policy nil)))
      (setf (plist-get (gptel-fsm-info parent-fsm) :agent-tool-policy-ceiling)
            (gptel-agent-policy-buffer-ceiling parent))
      (gptel-agent--task deliver "researcher" "Read" "Look for sources" parent-fsm snapshot)
      (let* ((child-fsm (car requests))
             (ceiling (gptel-agent-policy-request-ceiling child-fsm)))
        (should ceiling)
        (should (equal ceiling (gptel-agent-policy-request-ceiling parent-fsm)))
        (should-not (gptel-agent-policy-request-allows-p
                     child-fsm "Read" '(:file_path "/outside-the-project")))
        (gptel-agent-followup-test--finish child-fsm "Done")))))

(ert-deftest gptel-agent-policy-skill-grant-and-child-preset-cannot-widen ()
  (let ((gptel-agent-policy--definitions (make-hash-table))
        (gptel--known-presets nil))
    (gptel-agent-define-tool-policy 'manager :allow '((:tool "Skill") (:tool "Discussion")))
    (gptel-make-preset 'ordinary-child :agent-tool-policy nil :tools '("SkillMutation"))
    (gptel-agent-skills-test--with-chat "allowed-tools: Discussion SkillMutation\n"
      (gptel-agent-workers-test--answer fsm "Ready")
      (with-current-buffer parent
        (setq-local gptel-agent-tool-policy 'manager)
        (goto-char (point-max)) (insert "Start guarded turn") (gptel-send))
      (setq fsm (caar requests))
      (gptel-agent-skills-test--load fsm)
      (should (gptel-agent-skills-test--tool fsm "SkillMutation"))
      (gptel-agent-workers-test--tool fsm "SkillMutation" nil)
      (should (zerop executions))
      (should (string-match-p "Tool policy denied" (gptel-agent-workers-test--data fsm)))
      (gptel-agent-workers-test--tool
       fsm "Discussion" '(:title "Child" :prompt "Run tools" :preset "ordinary-child"))
      (let* ((child (car (buffer-local-value 'gptel-agent-discussion--children parent)))
             (child-fsm (gptel-agent-discussion--chat-fsm child)))
        (should (equal (gptel-agent-policy-request-ceiling child-fsm)
                       (gptel-agent-policy-request-ceiling fsm)))
        (gptel-agent-workers-test--tool child-fsm "SkillMutation" nil)
        (should (string-match-p "Tool policy denied" (gptel-agent-workers-test--data child-fsm)))
        (should (zerop executions))))))

(ert-deftest gptel-agent-policy-restore-request-and-missing-definition ()
  (let ((gptel-agent-policy--definitions (make-hash-table)))
    (gptel-agent-session-test--with-store
      (gptel-agent-define-tool-policy 'reads :allow '((:tool "Read")))
      (let* ((chat (gptel-agent-session-test--buffer " *policy-session*" "Review"))
             (ceiling (list (cons 'reads default-directory)))
             (fsm (gptel-make-fsm :info (list :buffer chat :agent-tool-policy-ceiling ceiling)))
             (restored (gptel-agent-session-buffer-from-record
                        (gptel-agent-session-buffer-record chat fsm) t)))
        (let ((settings (plist-get (gptel-agent-session-buffer-record chat) :settings)))
          (should-not (assq 'gptel-agent-tool-policy settings))
          (should-not (assq 'gptel-agent-policy--inherited settings)))
        (with-current-buffer restored
          (should (equal ceiling (gptel-agent-policy-buffer-ceiling)))
          (should (gptel-agent-policy--allows-p ceiling (gptel-get-tool "Read") nil default-directory))
          (should-not (gptel-agent-policy--allows-p ceiling (gptel-get-tool "Bash") nil default-directory))
          (remhash 'reads gptel-agent-policy--definitions)
          (should-not (gptel-agent-policy--allows-p
                       (gptel-agent-policy-buffer-ceiling) (gptel-get-tool "Read") nil default-directory)))))))

(provide 'gptel-agent-policy-test)
;;; gptel-agent-policy-test.el ends here
