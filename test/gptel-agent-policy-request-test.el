;;; gptel-agent-policy-request-test.el --- Request policy integration -*- lexical-binding: t; -*-

(require 'gptel-agent-access-test)
(require 'gptel-agent-policy-request)

(cl-defmacro gptel-agent-policy-test--catalog (&body body)
  (declare (indent 0))
  `(gptel-agent-instructions-test--with-project
     (let ((gptel--known-tools (copy-tree gptel--known-tools))
           (gptel--known-presets (copy-tree gptel--known-presets))
           (gptel-agent-policy--definitions (copy-hash-table gptel-agent-policy--definitions))
           observed)
       ;; A trusted policy may refer to a tool before its package is loaded.
       (gptel-agent-define-tool-policy
        'catalog-policy
        :allow '((:tool "CatalogSearch"
                  :args (:request (object :endpoint (url-origin "https://catalog.example.org")
                                          :query (rx "MR-" (+ (in "0-9"))))
                         :collections (every (one-of "design" "engineering"))
                         :draft (optional false)))))
       (let ((tool (gptel-make-tool
                    :name "CatalogSearch" :description "Search the catalog"
                    :args '((:name "request" :type object
                             :properties (:endpoint (:type string) :query (:type string)))
                            (:name "collections" :type array :items (:type string))
                            (:name "draft" :type boolean :optional t))
                    :function (lambda (request collections draft)
                                (push (list (current-buffer) request collections draft) observed)
                                "catalog-result")))
             (valid '(:request (:endpoint "https://catalog.example.org/items" :query "MR-27")
                      :collections ["design" "engineering"])))
         (gptel-make-preset 'ordinary-catalog :tools (list (gptel-tool-name tool))
                            :agent-tool-policy 'catalog-policy)
         (with-current-buffer parent
           (setq-local gptel-agent-use-project-instructions nil)
           (gptel--apply-preset 'ordinary-catalog (lambda (sym val) (set (make-local-variable sym) val))))
         ,@body))))

(ert-deftest gptel-agent-policy-foreign-tools-with-built-in-names ()
  (gptel-agent-instructions-test--with-project
    (let* ((gptel--known-tools (copy-tree gptel--known-tools))
           (gptel--known-presets (copy-tree gptel--known-presets))
           (gptel-agent-policy--definitions (make-hash-table))
           (names '("Read" "Grep" "Write" "Bash" "WebFetch" "Skill" "Agent"
                    "AgentControl" "Discussion" "DiscussionReport" "MainChat"))
           observed)
      (dolist (name names)
        (let ((label name))
          (gptel-make-tool
           :name name :category "catalog" :description "Look up a catalog entry"
           :args '((:name "entry" :type string :description "Entry identifier"))
           :function (lambda (entry) (push (list label entry) observed) entry))))
      (gptel-agent-define-tool-policy
       'catalog
       :allow (mapcar (lambda (name) (list :tool name :args '(:entry "A"))) names))
      (gptel-make-preset 'catalog-tools
                         :tools (mapcar (lambda (name) (list "catalog" name)) names)
                         :agent-tool-policy 'catalog)
      (with-current-buffer parent
        (setq-local gptel-agent-use-project-instructions t
                    gptel-confirm-tool-calls 'auto
                    gptel-pre-tool-call-functions '(smv/gptel-ediff-tool-call))
        (gptel--apply-preset 'catalog-tools (lambda (sym val) (set (make-local-variable sym) val)))
        (setq fsm (gptel-request "Catalog lookups" :buffer parent :stream nil :callback #'ignore
                    :transforms '(gptel-agent-instructions--transform gptel-agent--localize-skill-tool
                                  gptel-agent--localize-agent-tool gptel-agent-discussion--localize
                                  gptel-agent-chat--localize)
                    :fsm (gptel-make-fsm :table gptel-send--transitions :handlers gptel-send--handlers))))
      (dolist (name names)
        (gptel-agent-instructions-test--tool fsm name '(:entry "B"))
        (gptel-agent-instructions-test--tool fsm name '(:entry "A")))
      (should (equal (nreverse observed) (mapcar (lambda (name) (list name "A")) names))))))

(ert-deftest gptel-agent-policy-third-party-normal-request ()
  (gptel-agent-policy-test--catalog
    (setq fsm (gptel-agent-instructions-test--request parent))
    (should (equal (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm) :tools))
                   '("CatalogSearch")))
    (dolist (args (list valid (append valid '(:draft :json-false))))
      (gptel-agent-instructions-test--tool fsm "CatalogSearch" args))
    (should (equal (mapcar #'car observed) (list parent parent)))
    (should (eq (nth 3 (car observed)) :json-false))
    (dolist (override '((:draft t) (:draft "false")
                        (:collections ["design" "other"]) (:collections "design")
                        (:request (:endpoint "https://catalog.example.org" :query "MR-8 extra"))
                        (:request (:endpoint "https://catalog.example.org.attacker.test/" :query "MR-2"))
                        (:request (:endpoint "https://catalog.example.org/"))))
      (let ((args (gptel--merge-plists (copy-tree valid t) override)))
        (gptel-agent-instructions-test--tool fsm "CatalogSearch" args)))
    (should (= (length observed) 2))
    (should (string-match-p "Tool policy denied CatalogSearch" (gptel-agent-workers-test--data fsm)))))

(ert-deftest gptel-agent-policy-rewrite-before-side-effecting-hook ()
  (gptel-agent-policy-test--catalog
    (let (reviewed)
      (with-current-buffer parent
        (setq-local gptel-pre-tool-call-functions
                    (list (lambda (_input) '(:confirm nil :args (:collections ["no"])))
                          (lambda (_input) (setq reviewed t) nil))))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (gptel-agent-instructions-test--tool fsm "CatalogSearch" valid)
      (should-not reviewed)
      (should-not observed)
      (should (string-match-p "Tool policy denied" (gptel-agent-workers-test--data fsm))))))

(ert-deftest gptel-agent-policy-programmatic-skip-hooks-and-prompt-preset ()
  (gptel-agent-policy-test--catalog
    (gptel-make-preset 'clear-catalog :agent-tool-policy nil)
    (with-current-buffer parent
      (setq fsm (gptel-request "Find @clear-catalog now" :callback #'ignore
                  :transforms '(gptel--transform-apply-preset))))
    (should (gptel-agent-policy-request-ceiling fsm))
    (gptel-agent-instructions-test--tool fsm "CatalogSearch" (append valid '(:draft t)))
    (should-not observed)
    (gptel-agent-instructions-test--tool fsm "CatalogSearch" valid)
    (should (eq (caar observed) parent))))

(ert-deftest gptel-agent-policy-host-constructed-tool-state ()
  (gptel-agent-policy-test--catalog
    (let* ((call (list :name "CatalogSearch" :args (append valid '(:draft t)) :result nil))
           (request (gptel-make-fsm
                     :state 'TOOL :info (list :buffer parent :tools (list tool)
                                              :backend (buffer-local-value 'gptel-backend parent)
                                              :tool-use (list call) :tool-result nil))))
      (cl-letf (((symbol-function 'gptel--fsm-transition) #'ignore))
        (gptel--handle-tool-use request))
      (should-not observed)
      (should (string-match-p "Tool policy denied" (plist-get call :result))))))

(ert-deftest gptel-agent-policy-edited-confirmation-uses-effective-named-args ()
  (gptel-agent-policy-test--catalog
    (with-current-buffer parent (setq-local gptel-confirm-tool-calls t))
    (setq fsm (gptel-agent-instructions-test--request parent))
    (let ((pending (gptel-agent-access-test--ask fsm "CatalogSearch" valid)))
      (should pending)
      ;; This is the tuple replacement performed by the native inspector.
      (setf (caar pending) tool
            (cadar pending) (append valid '(:draft "false")))
      (gptel--accept-tool-calls pending))
    (should-not observed)
    (should (string-match-p "Tool policy denied" (gptel-agent-workers-test--data fsm)))
    (let ((pending (gptel-agent-access-test--ask fsm "CatalogSearch" valid)))
      (setf (cadar pending) (append valid '(:draft :json-false)))
      (with-temp-buffer (gptel--accept-tool-calls pending)))
    (should (eq (caar observed) parent))
    (let ((pending (gptel-agent-access-test--ask fsm "CatalogSearch" valid)))
      ;; A global fallback with the same schema/name still has no membership.
      (setf (caar pending) (gptel--copy-tool tool))
      (gptel--accept-tool-calls pending))
    (should (= (length observed) 1))))

(ert-deftest gptel-agent-policy-unknown-configured-request ()
  (gptel-agent-policy-test--catalog
    (with-current-buffer parent (setq-local gptel-agent-tool-policy 'misspelled))
    (setq fsm (gptel-agent-instructions-test--request parent))
    ;; Defining a previously missing name cannot open an already denied request.
    (gptel-agent-define-tool-policy 'misspelled :allow '((:tool "CatalogSearch")))
    (gptel-agent-instructions-test--tool fsm "CatalogSearch" valid)
    (should-not observed)
    (should (string-match-p "Tool policy denied" (gptel-agent-workers-test--data fsm)))))

(ert-deftest gptel-agent-policy-final-check-after-confirmation-predicate ()
  (gptel-agent-policy-test--catalog
    (setf (gptel-tool-confirm tool)
          (lambda (request _collections _draft)
            (plist-put request :query "rewritten by confirmation predicate") nil))
    (with-current-buffer parent (setq-local gptel-confirm-tool-calls 'auto))
    (setq fsm (gptel-agent-instructions-test--request parent))
    (gptel-agent-instructions-test--tool fsm "CatalogSearch" valid)
    (should-not observed)
    (should (string-match-p "Tool policy denied" (gptel-agent-workers-test--data fsm)))))

(ert-deftest gptel-agent-policy-preserves-effective-arguments-after-empty-hook-rewrite ()
  (gptel-agent-policy-test--catalog
    (with-current-buffer parent
      (setq-local gptel-pre-tool-call-functions (list (lambda (_) '(:args nil)))))
    (setq fsm (gptel-agent-instructions-test--request parent))
    (gptel-agent-instructions-test--tool fsm "CatalogSearch" valid)
    (should (eq (caar observed) parent))))

(ert-deftest gptel-agent-policy-edited-discussion-approval-retains-ceiling ()
  (let ((gptel-agent-policy--definitions (make-hash-table))
        (gptel-agent-tool-policy 'managed-confirmation))
    (gptel-agent-define-tool-policy 'managed-confirmation
      :allow '((:tool "Discussion") (:tool "DiscussionMutation")))
    (gptel-agent-discussion-test--with-chat
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (let ((child (gptel-agent-discussion--chat-buffer chat))
            (child-fsm (gptel-agent-discussion--chat-fsm chat)))
        (with-current-buffer child (setq-local gptel-confirm-tool-calls t))
        (gptel-agent-workers-test--tool child-fsm "DiscussionMutation" nil)
        (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
               (call (car (gptel-agent-discussion--interaction-calls pending)))
               (foreign (gptel--make-tool :name "DiscussionMutation" :args nil
                                          :function (lambda () (cl-incf executions)))))
          (gptel--accept-tool-calls (list (list foreign nil (nth 2 call))))
          (should (= executions 0))
          (should (string-match-p "Tool policy denied" (gptel-agent-workers-test--data child-fsm))))))))

(ert-deftest gptel-agent-policy-worker-steering-and-resume-across-parent-versions ()
  (let ((gptel-agent-policy--definitions (make-hash-table))
        (gptel-agent-tool-policy 'steering-policy))
    (gptel-agent-define-tool-policy 'steering-policy
      :allow '((:tool "Agent") (:tool "AgentControl") (:tool "WorkerTestTool")))
    (gptel-agent-workers-test--with-chat
      (let* ((run (gptel-agent-workers-test--launch fsm))
             (worker (gptel-agent--run-fsm run))
             (id (gptel-agent--session-id (gptel-agent--run-session run)))
             next)
        (gptel-agent--control (lambda (s) (push s acknowledgements)) fsm "send" id nil "first")
        (should (string-match-p "accepted" (car acknowledgements)))
        ;; New request in the same owner, after a stricter definition was loaded.
        (gptel-agent-define-tool-policy 'steering-policy :allow '((:tool "Agent") (:tool "AgentControl")))
        (with-current-buffer parent
          (setq next (gptel-request "Next request" :transforms '(gptel-agent--localize-agent-tool))))
        (gptel-agent--control (lambda (s) (push s acknowledgements)) next "send" id nil "second")
        (should (string-match-p "different tool policy" (car acknowledgements)))
        (should (equal (gptel-agent--workers-steering (gptel-agent--workers-for worker)) '("first")))
        (gptel-agent-workers-test--answer worker "Obsolete")
        (gptel-agent-workers-test--wake worker)
        (gptel-agent-workers-test--answer worker "Finished")
        (let* ((resumed (gptel-agent-workers-test--launch next "researcher" id))
               (child (gptel-agent--run-fsm resumed)))
          (should (gptel-agent-policy-request-covers-p child fsm))
          (should (gptel-agent-policy-request-covers-p child next))
          (gptel-agent-workers-test--tool child "WorkerTestTool" nil)
          (should-not tool-callback)
          (should (string-match-p "Tool policy denied" (gptel-agent-workers-test--data child))))))))

(ert-deftest gptel-agent-policy-managed-discussion-steering-requires-live-policy-version ()
  (let ((gptel-agent-policy--definitions (make-hash-table))
        (gptel-agent-tool-policy 'discussion-steering))
    (gptel-agent-define-tool-policy 'discussion-steering
      :allow '((:tool "Discussion") (:tool "DiscussionControl") (:tool "AskUserQuestion")))
    (gptel-agent-discussion-test--with-chat
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (let ((child (gptel-agent-discussion--chat-fsm chat)))
        (gptel-agent-workers-test--tool
         child "AskUserQuestion" '(:questions [(:question "Color?" :choices [(:value "Red")])]))
        (gptel-agent-workers-test--tool child "AskUserQuestion" '(:action "wait"))
        (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
               (id (gptel-agent-discussion--interaction-id pending)))
          (gptel-agent-workers-test--answer fsm "First parent finished")
          (gptel-agent-define-tool-policy 'discussion-steering :allow '((:tool "DiscussionControl")))
          (with-current-buffer parent
            (goto-char (point-max))
            (insert "Next parent request")
            (gptel-send)
            (setq fsm gptel-agent-discussion--fsm))
          (dolist (action '("send" "answer"))
            (should (string-match-p "different tool policy"
                                    (gptel-agent-discussion-test--control fsm action chat "Red" id))))
          (should (gptel-agent-discussion--pending pending)))))))

(provide 'gptel-agent-policy-request-test)
;;; gptel-agent-policy-request-test.el ends here
