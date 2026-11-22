;;; gptel-agent-result-policy-test.el --- Mandatory result scope -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent-followup-test)
(require 'gptel-agent-workers-test)

(cl-defmacro gptel-agent-result-policy-test--with-chat (&body body)
  "Run BODY with a saved result and the real worker request pipeline."
  (declare (indent 0))
  `(gptel-agent-followup-test--with-chat
     (let* ((gptel-agent-policy--definitions (make-hash-table :test #'eq))
            (gptel--known-tools (copy-tree gptel--known-tools))
            (file (with-temp-buffer
                    (insert "Scoped evidence alpha\nScoped evidence beta\n")
                    (gptel-agent--save-tool-result "policy" "Find evidence" parent))))
       (unwind-protect
           (progn
             (setq snapshot
                   (copy-tree
                    '(("result-explorer" :description "Extract evidence"
                       :system "Extract evidence" :tools ("ResultRead" "ResultGrep"))
                      ("test-worker" :description "Other worker" :system "Execute"))))
             (with-current-buffer parent
               (setq-local gptel-confirm-tool-calls nil
                           gptel-pre-tool-call-functions nil
                           gptel-post-tool-call-functions nil))
             ,@body)
         (gptel-agent--result-forget file)))))

(defun gptel-agent-result-policy-test--spawn (deliver parent-fsm snapshot file &optional id extras)
  "Start or resume a scoped worker, optionally adding EXTRAS."
  (gptel-agent--task deliver "result-explorer" "Read evidence" "Extract"
                     parent-fsm snapshot file "Find evidence" id nil nil nil extras))

(defun gptel-agent-result-policy-test--backends ()
  "Return four distinct provider implementations without real API endpoints."
  (list (gptel-make-openai "result-policy-chat" :key "unused"
                           :host "example.invalid" :models '(policy-test))
        (gptel-make-openai-responses "result-policy-responses" :key "unused"
                                     :host "example.invalid" :models '(policy-test))
        (gptel-make-anthropic "result-policy-anthropic" :key "unused"
                              :host "example.invalid" :models '(policy-test))
        (gptel-make-gemini "result-policy-gemini" :key "unused"
                           :host "example.invalid" :models '(policy-test))))

(ert-deftest gptel-agent-result-policy-appended-tools-cannot-execute-or-delegate ()
  (dolist (backend (gptel-agent-result-policy-test--backends))
    (ert-info ((gptel-backend-name backend))
      (gptel-agent-result-policy-test--with-chat
        (with-current-buffer parent
          (setq-local gptel-backend backend gptel-model 'policy-test))
        (gptel-make-tool :name "ResultPolicyForeign" :category "result-policy-test"
                        :description "Unscoped operation" :args nil :function #'ignore)
        (gptel-agent-result-policy-test--spawn
         deliver parent-fsm snapshot file nil
         ["Read" "Bash" "Eval" "Skill" "Agent" "Discussion" "result-policy-test"])
        (let* ((child (car requests))
               (tools (plist-get (gptel-fsm-info child) :tools))
               (initial-runs (hash-table-count gptel-agent--runs)))
          (should (eq (plist-get (gptel-fsm-info child) :backend) backend))
          (dolist (entry '(("Read" :file_path "/outside")
                          ("Bash" :command "true" :query "Check")
                          ("Eval" :expression "(+ 1 1)")
                          ("Skill" :skill "managed-discussions")
                          ("Agent" :subagent_type "test-worker" :description "Escape" :prompt "Escape")
                          ("Discussion" :title "Escape" :prompt "Escape")
                          ("ResultPolicyForeign")))
            (should (cl-find (car entry) tools :key #'gptel-tool-name :test #'equal))
            (cl-letf (((symbol-function 'gptel-agent-policy--invoke)
                       (lambda (&rest _) (ert-fail "A forbidden tool reached execution")))
                      ((symbol-function 'gptel-agent-policy--confirm-p)
                       (lambda (&rest _) (ert-fail "A forbidden tool reached confirmation"))))
              (gptel-agent-workers-test--tool child (car entry) (cdr entry)))
            (should (string-match-p (concat "Tool policy denied " (car entry))
                                    (gptel-agent-workers-test--data child))))
          (should (= initial-runs (hash-table-count gptel-agent--runs)))
          (should-not (buffer-local-value 'gptel-agent-discussion--children
                                         (plist-get (gptel-fsm-info child) :buffer)))
          (gptel-agent-workers-test--tool child "ResultRead" '(:start_line 1 :end_line 1))
          (should (string-match-p "1:Scoped evidence alpha" (gptel-agent-workers-test--data child)))
          (should (gptel-agent-policy-request-allows-p child "ResultGrep" '(:regex "beta"))))))))

(ert-deftest gptel-agent-result-policy-scoped-search-executes-through-request-dispatch ()
  (dolist (backend (gptel-agent-result-policy-test--backends))
    (ert-info ((gptel-backend-name backend))
      (gptel-agent-result-policy-test--with-chat
        (with-current-buffer parent
          (setq-local gptel-backend backend gptel-model 'policy-test))
        (cl-letf (((symbol-function 'gptel-agent--result-search-backend)
                   (lambda () '(:name emacs))))
          (gptel-agent-result-policy-test--spawn deliver parent-fsm snapshot file)
          (let ((child (car requests))
                (deadline (+ (float-time) 3)))
            (gptel-agent-workers-test--tool child "ResultGrep" '(:regex "beta"))
            (while (and (eq (gptel-fsm-state child) 'TOOL) (< (float-time) deadline))
              (accept-process-output nil 0.01))
            (should (eq (gptel-fsm-state child) 'WAIT))
            (should (string-match-p "Scoped evidence beta" (gptel-agent-workers-test--data child)))
            (should-not (string-match-p "Tool policy denied ResultGrep"
                                        (gptel-agent-workers-test--data child)))))))))

(ert-deftest gptel-agent-result-policy-survives-preset-hooks-and-user-policy-clearing ()
  (dolist (selected '(nil result-policy-broad))
    (gptel-agent-result-policy-test--with-chat
      (gptel-agent-define-tool-policy 'result-policy-broad
        :allow '((:tool "ResultRead") (:tool "Read") (:tool "Agent")))
      (with-current-buffer parent
        (setq-local gptel-agent-preset
                    (list :agent-tool-policy selected
                          :post (lambda ()
                                  (setq-local gptel-agent-policy--inherited nil
                                              gptel-agent-policy--inherited-snapshot nil)))))
      (gptel-agent-result-policy-test--spawn deliver parent-fsm snapshot file nil ["Read"])
      (let ((child (car requests)))
        (should (assq gptel-agent-policy--result-explorer-name
                       (gptel-agent-policy-request-ceiling child)))
        (should-not (gptel-agent-policy-request-allows-p child "Read" '(:file_path "/outside")))
        (should (gptel-agent-policy-request-allows-p child "ResultRead"
                                                    '(:start_line 1 :end_line 1)))))))

(ert-deftest gptel-agent-result-policy-trust-cannot-bypass-ceiling-or-user-narrowing ()
  (gptel-agent-result-policy-test--with-chat
    (gptel-agent-define-tool-policy 'result-policy-first-line
      :allow '((:tool "ResultRead" :args (:start_line 1 :end_line 1))
               (:tool "Read")))
    (with-current-buffer parent
      (setq-local gptel-agent-preset '(:agent-tool-policy result-policy-first-line)))
    (gptel-agent-result-policy-test--spawn deliver parent-fsm snapshot file nil ["Read"])
    (let* ((child (car requests))
           (tools (plist-get (gptel-fsm-info child) :tools)))
      (dolist (name '("Read" "ResultRead"))
        (gptel-agent-policy-request-trust-tool
         child (cl-find name tools :key #'gptel-tool-name :test #'equal)))
      (should-not (gptel-agent-policy-request-allows-p child "Read" '(:file_path "/outside")))
      (should-not (gptel-agent-policy-request-allows-p child "ResultRead"
                                                     '(:start_line 2 :end_line 2)))
      (should (gptel-agent-policy-request-allows-p child "ResultRead"
                                                  '(:start_line 1 :end_line 1)))
      (gptel-agent-workers-test--tool child "ResultRead" '(:start_line 2 :end_line 2))
      (should (string-match-p "Tool policy denied ResultRead"
                              (gptel-agent-workers-test--data child))))))

(ert-deftest gptel-agent-result-policy-rejects-foreign-tools-with-scoped-names ()
  (gptel-agent-result-policy-test--with-chat
    (let (executed)
      (dolist (name '("ResultRead" "ResultGrep"))
        (gptel-make-tool :name name :category "foreign-result-policy"
                        :description "Foreign implementation" :args nil
                        :function (lambda () (setq executed t) "unscoped")))
      (setf (plist-get (cdar snapshot) :tools)
            '(("foreign-result-policy" "ResultRead")
              ("foreign-result-policy" "ResultGrep")))
      (gptel-agent-result-policy-test--spawn deliver parent-fsm snapshot file)
      (let ((child (car requests)))
        (dolist (name '("ResultRead" "ResultGrep"))
          (gptel-agent-policy-request-trust-tool
           child (gptel-agent-policy--tool child name))
          (gptel-agent-workers-test--tool child name nil)
          (should (string-match-p (concat "Tool policy denied " name)
                                  (gptel-agent-workers-test--data child)))))
      (should-not executed))))

(ert-deftest gptel-agent-result-policy-canonical-definition-and-restored-ceiling ()
  (let ((gptel-agent-policy--definitions (make-hash-table :test #'eq)))
    (should-error
     (gptel-agent-define-tool-policy 'gptel-agent--result-explorer
       :allow '((:tool "Read"))))
    ;; A restored ceiling resolves without a user definition and ignores a
    ;; same-named registry entry left by older configuration.
    (puthash 'gptel-agent--result-explorer
             (cons (gptel-agent-policy--compile-rules '((:tool "Read"))) nil)
             gptel-agent-policy--definitions)
    (with-temp-buffer
      (gptel-agent-policy-inherit
       (list (cons 'gptel-agent--result-explorer default-directory)))
      (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)
                                            :tools (list (gptel-get-tool "Read")
                                                         (gptel-get-tool "ResultRead"))))))
        (gptel-agent-policy-request-capture fsm)
        (should-not (gptel-agent-policy-request-allows-p fsm "Read" '(:file_path "/outside")))
        (should (gptel-agent-policy-request-allows-p fsm "ResultRead"
                                                    '(:start_line 1 :end_line 1)))))))

(ert-deftest gptel-agent-result-policy-preserves-unrelated-trusted-tool-behavior ()
  (let ((gptel-agent-policy--definitions (make-hash-table :test #'eq)))
    (gptel-agent-define-tool-policy 'result-policy-deny-all)
    (with-temp-buffer
      (setq-local gptel-agent-tool-policy 'result-policy-deny-all)
      (let* ((tool (gptel-get-tool "Read"))
             (fsm (gptel-make-fsm :info (list :buffer (current-buffer) :tools (list tool)))))
        (gptel-agent-policy-request-capture fsm)
        (should-not (gptel-agent-policy-request-allows-p fsm "Read" '(:file_path "/outside")))
        (gptel-agent-policy-request-trust-tool fsm tool)
        (should (gptel-agent-policy-request-allows-p fsm "Read" '(:file_path "/outside")))))))

(ert-deftest gptel-agent-result-policy-followup-keeps-ceiling-and-new-parent-limits ()
  (gptel-agent-result-policy-test--with-chat
    (gptel-agent-result-policy-test--spawn deliver parent-fsm snapshot file nil ["Read"])
    (let* ((first (car requests))
           (run (plist-get (gptel-fsm-info first) :context))
           (id (gptel-agent--session-id (gptel-agent--run-session run))))
      (gptel-agent-followup-test--finish first "Initial evidence")
      (gptel-agent-define-tool-policy 'result-policy-parent
        :allow '((:tool "ResultRead" :args (:start_line 1 :end_line 1)) (:tool "Read")))
      (with-current-buffer parent
        (setq-local gptel-agent-tool-policy 'result-policy-parent)
        (gptel-agent-policy-request-capture parent-fsm))
      (gptel-agent-result-policy-test--spawn deliver parent-fsm snapshot file id)
      (let ((child (car requests)))
        (should-not (eq child first))
        (should (gptel-agent-policy-request-covers-p child parent-fsm))
        (should-not (gptel-agent-policy-request-allows-p child "Read" '(:file_path "/outside")))
        (should-not (gptel-agent-policy-request-allows-p child "ResultRead"
                                                       '(:start_line 2 :end_line 2)))
        (gptel-agent-workers-test--tool child "ResultRead" '(:start_line 1 :end_line 1))
        (should (string-match-p "1:Scoped evidence alpha"
                                (gptel-agent-workers-test--data child)))))))

(ert-deftest gptel-agent-result-policy-edited-approval-cannot-substitute-appended-tool ()
  (gptel-agent-result-policy-test--with-chat
    (gptel-agent-result-policy-test--spawn deliver parent-fsm snapshot file nil ["Read"])
    (let* ((child (car requests))
           (read (gptel-agent-policy--tool child "Read"))
           pending executed)
      (with-current-buffer (plist-get (gptel-fsm-info child) :buffer)
        (setq-local gptel-confirm-tool-calls t))
      (cl-letf (((symbol-function 'gptel--display-tool-calls)
                 (lambda (calls _info) (setq pending calls))))
        (gptel-agent-workers-test--tool child "ResultRead" '(:start_line 1 :end_line 1)))
      (should pending)
      ;; The native confirmation inspector can substitute both tool and args.
      (setf (caar pending) read
            (cadar pending) (list :file_path file))
      (let ((original (gptel-tool-function read)))
        (unwind-protect
            (progn
              (setf (gptel-tool-function read) (lambda (&rest _) (setq executed t) "unscoped"))
              (gptel--accept-tool-calls pending))
          (setf (gptel-tool-function read) original)))
      (should-not executed)
      (should (string-match-p "Tool policy denied Read" (gptel-agent-workers-test--data child))))))

(ert-deftest gptel-agent-result-policy-trusted-host-descendants-inherit-mandatory-ceiling ()
  (gptel-agent-result-policy-test--with-chat
    (let ((gptel-default-mode #'text-mode)
          (gptel--known-presets (copy-tree gptel--known-presets)))
      (gptel-agent-result-policy-test--spawn deliver parent-fsm snapshot file nil ["Read"])
      (let* ((explorer (car requests))
             (owner (plist-get (gptel-fsm-info explorer) :buffer))
             (reset (lambda ()
                      (setq-local gptel-agent-policy--inherited nil
                                  gptel-agent-policy--inherited-snapshot nil)))
             (config (with-current-buffer owner
                       (mapcar (lambda (symbol) (cons symbol (symbol-value symbol)))
                               gptel-agent--request-config-variables)))
             chat)
        ;; Host APIs may create descendants directly; unlike a model tool call
        ;; they skip the creation denial but must still carry the ceiling.
        (setf (cdr (assoc "test-worker" snapshot))
              (list :system "Ordinary child" :tools '("Read")
                    :agent-tool-policy nil :post reset))
        (gptel-agent--task deliver "test-worker" "Host child" "Read outside"
                           explorer snapshot)
        (let ((worker (car requests)))
          (should-not (eq worker explorer))
          (should (gptel-agent-policy-request-covers-p worker explorer))
          (gptel-agent-workers-test--tool worker "Read" (list :file_path file))
          (should (string-match-p "Tool policy denied Read" (gptel-agent-workers-test--data worker))))
        (gptel-make-preset 'result-policy-host-child :tools '("Read")
                           :agent-tool-policy nil :post reset)
        (unwind-protect
            (progn
              (setq chat (gptel-agent-discussion--create
                          owner config "Host discussion" "Read outside"
                          "result-policy-host-child" nil nil nil explorer))
              (let ((discussion (gptel-agent-discussion--chat-fsm chat)))
                (should discussion)
                (should (gptel-agent-policy-request-covers-p discussion explorer))
                (gptel-agent-workers-test--tool discussion "Read" (list :file_path file))
                (should (string-match-p "Tool policy denied Read"
                                        (gptel-agent-workers-test--data discussion)))))
          (when chat
            (setf (gptel-agent-discussion--chat-owner chat) nil)
            (when (buffer-live-p (gptel-agent-discussion--chat-buffer chat))
              (kill-buffer (gptel-agent-discussion--chat-buffer chat)))))))))

(provide 'gptel-agent-result-policy-test)
;;; gptel-agent-result-policy-test.el ends here
