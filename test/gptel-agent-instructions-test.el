;;; gptel-agent-instructions-test.el --- Scoped instructions -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent-workers-test)
(require 'gptel-agent-discussion-test)
(require 'gptel-agent-followup-test)
(require 'gptel-ollama)

(defvar gptel-agent-instructions-test--backend nil)

(defun gptel-agent-instructions-test--write (path text)
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert text)))

(cl-defmacro gptel-agent-instructions-test--with-shared-file (&body body)
  (declare (indent 0))
  `(let* ((shared-directory (file-name-as-directory
                            (file-truename (make-temp-file "agent-shared-instructions-" t))))
          (shared-file (expand-file-name "rules.md" shared-directory)))
     (unwind-protect
         (progn
           (gptel-agent-instructions-test--write shared-file "SHARED-INSTRUCTION")
           ,@body)
       (delete-directory shared-directory t))))

(cl-defmacro gptel-agent-instructions-test--with-project (&body body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (file-truename (make-temp-file "agent-instructions-" t))))
          (default-directory root)
          (front (expand-file-name "frontend/" root))
          (back (expand-file-name "backend/" root))
          (parent (generate-new-buffer " *instruction-owner*"))
          requests fsm
          (gptel-log-level nil)
          (gptel-post-request-hook nil))
     (unwind-protect
         (progn
           (gptel-agent-instructions-test--write (concat root "AGENTS.md") "ROOT-INSTRUCTION")
           (gptel-agent-instructions-test--write (concat front "AGENTS.md") "FRONT-INSTRUCTION")
           (gptel-agent-instructions-test--write (concat back "AGENTS.md") "BACK-INSTRUCTION")
           (gptel-agent-instructions-test--write (concat root "old.txt") "old")
           (with-current-buffer parent
             (setq-local default-directory root
                         gptel-agent-use-project-instructions t
                         gptel-backend (or gptel-agent-instructions-test--backend
                                           (gptel-make-openai "instructions" :key "unused" :models '(test)))
                         gptel-model 'test gptel-use-curl t gptel-stream nil
                         gptel-system-prompt "BASE-PROMPT"
                         gptel-context nil gptel-use-context nil
                         gptel-use-tools t gptel-confirm-tool-calls nil
                         gptel-pre-tool-call-functions nil gptel-post-tool-call-functions nil
                         gptel-tools (mapcar #'gptel-get-tool '("Read" "Glob" "Grep" "Write" "Insert" "Edit" "Mkdir" "Rmdir" "PathInfo"))))
           (cl-letf (((symbol-function 'project-current) (lambda (&rest _) (cons 'transient root)))
                     ((symbol-function 'gptel--update-status) #'ignore)
                     ((symbol-function 'gptel-curl-get-response)
                      (lambda (request)
                        (push (copy-tree (plist-get (gptel-fsm-info request) :data) t) requests)))
                     ((symbol-function 'gptel--url-get-response)
                      (lambda (_request) (ert-fail "Unexpected URL transport"))))
             ,@body))
       (when (buffer-live-p parent) (kill-buffer parent))
       (delete-directory root t))))

(defun gptel-agent-instructions-test--request (owner)
  (with-current-buffer owner
    (gptel-request "User task" :buffer owner :stream nil :callback #'ignore
                   :transforms '(gptel-agent-instructions--transform
                                 gptel-agent--localize-repository-search
                                 gptel-agent-discussion--localize)
                   :fsm (gptel-make-fsm :table gptel-send--transitions
                                        :handlers gptel-send--handlers))))

(defun gptel-agent-instructions-test--payload (fsm)
  (gptel-agent-workers-test--data fsm))

(defun gptel-agent-instructions-test--tool (fsm name args)
  (let* ((info (gptel-fsm-info fsm)) (backend (plist-get info :backend)))
    (if (not (gptel-ollama-p backend))
        (gptel-agent-workers-test--tool fsm name args)
      (gptel--parse-response
       backend `(:message (:role "assistant" :content ""
                                 :tool_calls [(:function (:name ,name :arguments ,args))])) info)
      (gptel--fsm-transition fsm 'TYPE)
      (gptel--fsm-transition fsm))))

(ert-deftest gptel-agent-instructions-startup-owner-chain-and-boundary ()
  (gptel-agent-instructions-test--with-project
    (with-current-buffer parent (setq default-directory front))
    (with-temp-buffer
      (setq default-directory temporary-file-directory)
      (setq fsm (gptel-agent-instructions-test--request parent)))
    (let* ((info (gptel-fsm-info fsm)) (sent (plist-get info :agent-instructions-sent)))
      (should (equal (mapcar (lambda (r) (plist-get r :text)) sent)
                     '("ROOT-INSTRUCTION" "FRONT-INSTRUCTION")))
      (should-not (string-match-p "BACK-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
      (should (equal (buffer-local-value 'gptel-system-prompt parent) "BASE-PROMPT"))
      (should (equal (buffer-local-value 'gptel-agent-instructions--boundary parent) root)))))

(ert-deftest gptel-agent-instructions-all-presets-without-tools ()
  (let ((gptel--known-presets (copy-tree gptel--known-presets))
        (gptel-agent-use-project-instructions t)
        (gptel-agent-instructions-test--backend
         (gptel-make-openai-responses "preset-instructions" :key "unused" :models '(test))))
    (gptel-make-preset 'ordinary-instruction-test :system "PRESET-BASE")
    (gptel-make-preset 'agent-instruction-test :system "PRESET-BASE" :agent--current-agent "ask")
    (dolist (preset '(nil ordinary-instruction-test agent-instruction-test))
      (let ((gptel--preset preset))
        (dolist (local-preset '(nil t))
          (gptel-agent-instructions-test--with-project
            (with-current-buffer parent
              (kill-local-variable 'gptel-agent-use-project-instructions)
              (when local-preset (setq-local gptel--preset preset))
              (setq-local gptel-agent--current-agent nil)
              (setq gptel-tools nil gptel-use-tools nil gptel-system-prompt "PRESET-BASE"))
            (setq fsm (gptel-agent-instructions-test--request parent))
            (should (= 1 (length requests)))
            (should-not (plist-get (car requests) :tools))
            (should (string-match-p "ROOT-INSTRUCTION" (plist-get (car requests) :instructions)))
            (should (cdr (assq 'gptel-agent-use-project-instructions
                               (plist-get (gptel-fsm-info fsm) :discussion-config))))
            (should (equal (buffer-local-value 'gptel-system-prompt parent) "PRESET-BASE"))
            (should-not (buffer-local-value 'gptel-agent--current-agent parent))))))))

(ert-deftest gptel-agent-instructions-global-and-local-preferences ()
  (let ((gptel--preset nil))
    (dolist (global '(nil t))
      (dolist (local '(inherit enable disable))
        (let ((gptel-agent-use-project-instructions global))
          (gptel-agent-instructions-test--with-project
            (with-current-buffer parent
              (pcase local
                ('inherit (kill-local-variable 'gptel-agent-use-project-instructions))
                ('enable (setq-local gptel-agent-use-project-instructions t))
                ('disable (setq-local gptel-agent-use-project-instructions nil))))
            (setq fsm (gptel-agent-instructions-test--request parent))
            (should (eq (and (plist-get (gptel-fsm-info fsm) :agent-instructions-sent) t)
                        (if (eq local 'inherit) global (eq local 'enable))))))))))

(ert-deftest gptel-agent-instructions-global-changes-and-local-toggle ()
  (let ((gptel--preset nil)
        (gptel-agent-use-project-instructions t))
    (gptel-agent-instructions-test--with-project
      (with-current-buffer parent
        (kill-local-variable 'gptel-agent-use-project-instructions))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (should (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
      ;; Existing conversations without local overrides follow global changes.
      (setq gptel-agent-use-project-instructions nil)
      (gptel--handle-wait fsm)
      (should-not (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
      (should-not (string-match-p "ROOT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
      ;; Toggle without a prefix uses the effective preference of this buffer.
      (with-current-buffer parent (gptel-agent-toggle-project-instructions))
      (gptel--handle-wait fsm)
      (should (string-match-p "ROOT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
      (with-current-buffer parent (gptel-agent-toggle-project-instructions))
      (setq gptel-agent-use-project-instructions t)
      (gptel--handle-wait fsm)
      (should-not (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
      (should-not (string-match-p "ROOT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
      (with-temp-buffer (should (gptel-agent-instructions--configuration))))))

(ert-deftest gptel-agent-instructions-read-scope-replacement-all-providers ()
  (dolist (backend (list (gptel-make-openai "i-openai" :key "unused" :models '(test))
                         (gptel-make-openai-responses "i-responses" :key "unused" :models '(test))
                         (gptel-make-anthropic "i-anthropic" :key "unused" :models '(test))
                         (gptel-make-gemini "i-gemini" :key "unused" :models '(test))
                         (gptel-make-ollama "i-ollama" :models '(test))))
    (let ((gptel-agent-instructions-test--backend backend))
      (gptel-agent-instructions-test--with-project
        (setq fsm (gptel-agent-instructions-test--request parent))
        (dotimes (_ 2)
          (gptel-agent-instructions-test--tool fsm "Read" (list :file_path (concat front "AGENTS.md"))))
        (let ((payload (gptel-agent-instructions-test--payload fsm)))
          (should (= 1 (gptel-agent-workers-test--count "ROOT-INSTRUCTION" payload)))
          (should (= 1 (gptel-agent-workers-test--count "FRONT-INSTRUCTION" payload)))
          (should (string-match-p "Loaded project instruction source" payload)))
        (gptel-agent-instructions-test--tool fsm "Read" (list :file_path (concat back "AGENTS.md")))
        (let ((payload (gptel-agent-instructions-test--payload fsm)))
          (should-not (string-match-p "FRONT-INSTRUCTION" payload))
          (should (= 1 (gptel-agent-workers-test--count "BACK-INSTRUCTION" payload)))
          (should (string-match-p "BASE-PROMPT" payload)))
        (should (= 3 (length (buffer-local-value 'gptel-agent-instructions--registry parent))))
        (with-current-buffer parent (gptel-agent-toggle-project-instructions -1))
        (gptel--handle-wait fsm)
        (should-not (string-match-p (regexp-opt '("ROOT-INSTRUCTION" "BACK-INSTRUCTION"))
                                    (gptel-agent-instructions-test--payload fsm)))
        (should (string-match-p "Loaded project instruction source"
                                (gptel-agent-instructions-test--payload fsm)))))))

(ert-deftest gptel-agent-instructions-cross-scope-edit-requires-new-proposal ()
  (dolist (review '(nil t))
    (gptel-agent-instructions-test--with-project
      (with-current-buffer parent
        (setq gptel-confirm-tool-calls review)
        (setq gptel-pre-tool-call-functions '(smv/gptel-ediff-tool-call)))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (let ((args (list :file_path (expand-file-name "new.txt" front)  :content "first proposal")))
        (gptel-agent-workers-test--tool fsm "Write" args)
        (should-not (file-exists-p (concat front "new.txt")))
        (should-not (buffer-local-value 'gptel-agent-review--queue parent))
        (should (eq (gptel-fsm-state fsm) 'WAIT))
        (should (string-match-p "Deferred before staging" (gptel-agent-instructions-test--payload fsm)))
        (should (string-match-p "FRONT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
        ;; Retry is a fresh model decision, with different arguments.
        (with-current-buffer parent
          (setq gptel-confirm-tool-calls nil gptel-pre-tool-call-functions nil))
        (setf (plist-get args :content) "reconsidered")
        (gptel-agent-workers-test--tool fsm "Write" args)
        (should (equal (with-temp-buffer (insert-file-contents (concat front "new.txt")) (buffer-string))
                       "reconsidered"))))))

(ert-deftest gptel-agent-instructions-batch-discovers-all-before-any-staging ()
  (gptel-agent-instructions-test--with-project
    (with-current-buffer parent
      (setq gptel-confirm-tool-calls t gptel-pre-tool-call-functions '(smv/gptel-ediff-tool-call)))
    (setq fsm (gptel-agent-instructions-test--request parent))
    (gptel-agent-workers-test--tool
     fsm "Edit" (list :mode "batch" :root_directory root :operations
                      [(:operation "write" :file_path "old.txt" :content "new")
                       (:operation "write" :file_path "frontend/new.txt" :content "front")
                       (:operation "write" :file_path "backend/new.txt" :content "back")]))
    (should (equal (with-temp-buffer (insert-file-contents (concat root "old.txt")) (buffer-string)) "old"))
    (should-not (file-exists-p (concat front "new.txt")))
    (should-not (buffer-local-value 'gptel-agent-review--queue parent))
    (let ((payload (gptel-agent-instructions-test--payload fsm)))
      (dolist (text '("ROOT-INSTRUCTION" "FRONT-INSTRUCTION" "BACK-INSTRUCTION"))
        (should (= 1 (gptel-agent-workers-test--count text payload)))))))

(ert-deftest gptel-agent-instructions-refresh-change-addition-removal-and-retry ()
  (gptel-agent-instructions-test--with-project
    (setq fsm (gptel-agent-instructions-test--request parent))
    (let ((old-snapshot (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
          (args (list :file_path (expand-file-name "new.txt" root)  :content "new")))
      (gptel-agent-instructions-test--write (concat root "AGENTS.md") "ROOT-CHANGED")
      (gptel-agent-workers-test--tool fsm "Write" args)
      (should-not (file-exists-p (concat root "new.txt")))
      (should (equal (plist-get (car old-snapshot) :text) "ROOT-INSTRUCTION"))
      (should-not (string-match-p "ROOT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
      (should (string-match-p "ROOT-CHANGED" (gptel-agent-instructions-test--payload fsm)))
      (delete-file (concat root "AGENTS.md"))
      (gptel-agent-workers-test--tool fsm "Write" args)
      (should-not (file-exists-p (concat root "new.txt")))
      (should-not (string-match-p "ROOT-CHANGED" (gptel-agent-instructions-test--payload fsm)))
      (gptel--handle-wait fsm)
      (should-not (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
      (gptel-agent-instructions-test--write (concat root "AGENTS.md") "ROOT-ADDED")
      (gptel-agent-workers-test--tool fsm "Write" args)
      (should-not (file-exists-p (concat root "new.txt")))
      (gptel-agent-workers-test--tool fsm "Write" args)
      (should (file-exists-p (concat root "new.txt"))))))

(ert-deftest gptel-agent-instructions-delayed-approval-rechecks-and-dead-owner-stops ()
  (gptel-agent-instructions-test--with-project
    (setq fsm (gptel-agent-instructions-test--request parent))
    (let* ((tool (cl-find "Write" (plist-get (gptel-fsm-info fsm) :tools)
                          :key #'gptel-tool-name :test #'equal))
           (function (gptel-tool-function tool)))
      (gptel-agent-instructions-test--write (concat root "AGENTS.md") "APPROVAL-CHANGE")
      (should (string-match-p "Deferred before staging" (funcall function (concat root "new.txt") "new")))
      (should-not (file-exists-p (concat root "new.txt")))
      (gptel--handle-wait fsm)
      (gptel--fsm-transition fsm 'ABRT)
      (should (string-match-p "Cancelled" (funcall function (concat root "new.txt") "new")))
      (should-not (file-exists-p (concat root "new.txt"))))))

(ert-deftest gptel-agent-instructions-path-policy-and-unavailable-sources ()
  (gptel-agent-instructions-test--with-project
    (with-current-buffer parent
      (gptel-agent-instructions--initialize)
      (make-symbolic-link front (concat root "alias"))
      (should (equal (gptel-agent-instructions--directory (concat root "alias/new.txt")) front))
      (should-error (gptel-agent-instructions--directory "/ssh:example:/tmp/new"))
      (should (equal (gptel-agent-instructions--directory (expand-file-name "../outside/file" root))
                     (expand-file-name "../outside/" root)))
      (make-symbolic-link temporary-file-directory (concat root "escape"))
      (should (equal (gptel-agent-instructions--directory (concat root "escape/new"))
                     (file-truename temporary-file-directory)))
      (let ((gptel-agent-project-instructions-max-bytes 4))
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          (should (plist-get (gptel-agent-instructions--load (concat root "AGENTS.md")) :error)))))
    (setq fsm (gptel-agent-instructions-test--request parent))
    (cl-letf (((symbol-function 'file-readable-p) (lambda (_path) nil))
              ((symbol-function 'display-warning) #'ignore))
      (gptel-agent-workers-test--tool fsm "Write" (list :file_path (expand-file-name "new.txt" root)  :content "new")))
    (should-not (file-exists-p (concat root "new.txt")))
    (should (string-match-p "UNAVAILABLE" (gptel-agent-instructions-test--payload fsm)))
    (should-not (string-match-p "ROOT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))))

(ert-deftest gptel-agent-instructions-controls-context-and-base-prompts ()
  (gptel-agent-instructions-test--with-project
    (with-current-buffer parent
      (setq-local gptel-agent-use-project-instructions nil))
    (setq fsm (gptel-agent-instructions-test--request parent))
    (should-not (buffer-local-value 'gptel-agent-instructions--registry parent))
    (with-current-buffer parent (gptel-agent-toggle-project-instructions 1))
    (gptel--handle-wait fsm)
    (should (string-match-p "ROOT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
    (dolist (prompt (list (lambda () "FUNCTION-BASE") '("LIST-BASE" "EXAMPLE-USER" "EXAMPLE-ANSWER")))
      (with-current-buffer parent
        (setq gptel-system-prompt prompt gptel-use-context t
              gptel-context (list (list (concat front "AGENTS.md")) (list (concat back "AGENTS.md")))))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (should (eq prompt (buffer-local-value 'gptel-system-prompt parent)))
      (dolist (text '("ROOT-INSTRUCTION" "FRONT-INSTRUCTION" "BACK-INSTRUCTION"))
        (should (= 1 (gptel-agent-workers-test--count text (gptel-agent-instructions-test--payload fsm))))))))

(ert-deftest gptel-agent-instructions-rejects-provider-retained-state ()
  (let ((gptel-agent-instructions-test--backend
         (gptel-make-openai-responses "stateful" :key "unused" :models '(test))))
    (gptel-agent-instructions-test--with-project
      (setq fsm (gptel-agent-instructions-test--request parent))
      (setf (plist-get (plist-get (gptel-fsm-info fsm) :data) :previous_response_id) "old")
      (should-error (gptel--handle-wait fsm))
      (should (= 1 (length requests))))))

(ert-deftest gptel-agent-instructions-worker-registry-is-private-and-followup-rebuilds ()
  (gptel-agent-instructions-test--with-project
    (gptel-agent-followup-test--with-chat
      (with-current-buffer parent
        (setq-local default-directory root gptel-agent-use-project-instructions t)
        (gptel-agent-instructions--initialize)
        (gptel-agent-instructions--records (list back)))
      (gptel-agent--task deliver "researcher" "First task" "Find evidence" parent-fsm snapshot)
      (let* ((first (car requests)) (info (gptel-fsm-info first))
             (run (plist-get info :context)) (session (gptel-agent--run-session run))
             (id (gptel-agent--session-id session)) (child (gptel-agent--run-child-buffer run)))
        (should (string-match-p "ROOT-INSTRUCTION" (gptel-agent-instructions-test--payload first)))
        (should-not (string-match-p "BACK-INSTRUCTION" (gptel-agent-instructions-test--payload first)))
        (should (= 1 (length (buffer-local-value 'gptel-agent-instructions--registry child))))
        (gptel-agent-workers-test--tool first "Read" (list :file_path (concat front "AGENTS.md")))
        (gptel-agent-followup-test--finish first "Finished")
        (should-not (string-match-p (regexp-opt '("ROOT-INSTRUCTION" "FRONT-INSTRUCTION"))
                                    (gptel--json-encode
                                     (plist-get (gptel-agent--session-request-info session) :data))))
        (gptel-agent-instructions-test--write (concat front "AGENTS.md") "FRONT-FOLLOWUP")
        (gptel-agent--task deliver "researcher" "Follow-up" "Continue" parent-fsm snapshot nil nil id)
        (let ((next (car requests)))
          (should (eq child (plist-get (gptel-fsm-info next) :buffer)))
          (should (= 2 (length (buffer-local-value 'gptel-agent-instructions--registry child))))
          (let ((payload (gptel-agent-instructions-test--payload next)))
            (should (= 1 (gptel-agent-workers-test--count "FRONT-FOLLOWUP" payload)))
            (should-not (string-match-p (regexp-opt '("FRONT-INSTRUCTION" "BACK-INSTRUCTION")) payload)))
          (gptel-agent-followup-test--finish next "Follow-up done"))
        (should (= 2 (length (buffer-local-value 'gptel-agent-instructions--registry parent))))))))

(ert-deftest gptel-agent-instructions-sibling-discussions-do-not-inherit-content ()
  (gptel-agent-instructions-test--with-project
    (setq fsm (gptel-agent-instructions-test--request parent))
    (gptel-agent-workers-test--tool fsm "Read" (list :file_path (concat front "AGENTS.md")))
    (let ((config (plist-get (gptel-fsm-info fsm) :discussion-config)) children)
      (unwind-protect
          (progn
            (dolist (dir (list front back))
              (push (gptel-agent-discussion--create parent config "Scoped chat" "Work here" nil dir) children))
            (let* ((back-chat (car children)) (front-chat (cadr children))
                   (back-buffer (gptel-agent-discussion--chat-buffer back-chat))
                   (front-buffer (gptel-agent-discussion--chat-buffer front-chat))
                   (back-fsm (gptel-agent-discussion--chat-fsm back-chat))
                   (front-fsm (gptel-agent-discussion--chat-fsm front-chat)))
              (should-not (string-match-p "FRONT-INSTRUCTION" (gptel-agent-instructions-test--payload back-fsm)))
              (should-not (string-match-p "BACK-INSTRUCTION" (gptel-agent-instructions-test--payload front-fsm)))
              (should-not (eq (buffer-local-value 'gptel-agent-instructions--registry front-buffer)
                              (buffer-local-value 'gptel-agent-instructions--registry parent)))
              (should (equal (buffer-local-value 'gptel-system-prompt back-buffer) "BASE-PROMPT"))
              (with-current-buffer front-buffer (gptel-agent-toggle-project-instructions -1))
              (gptel--handle-wait front-fsm)
              (should-not (string-match-p "ROOT-INSTRUCTION" (gptel-agent-instructions-test--payload front-fsm)))
              (should (string-match-p "BACK-INSTRUCTION" (gptel-agent-instructions-test--payload back-fsm)))
              (should (string-match-p "FRONT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))))
        (dolist (chat children)
          (setf (gptel-agent-discussion--chat-owner chat) nil)
          (let ((buffer (gptel-agent-discussion--chat-buffer chat)))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest gptel-agent-instructions-result-explorer-does-not-discover ()
  (gptel-agent-instructions-test--with-project
    (with-current-buffer parent (setq-local gptel-agent--result-file-scope (concat root "old.txt")))
    (cl-letf (((symbol-function 'gptel-agent-instructions--load)
               (lambda (&rest _) (ert-fail "Restricted worker discovered instructions"))))
      (setq fsm (gptel-agent-instructions-test--request parent)))
    (should-not (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
    (should-not (buffer-local-value 'gptel-agent-instructions--registry parent))))

(ert-deftest gptel-agent-instructions-agent-name-does-not-disable-discovery ()
  (let ((gptel--known-presets (copy-tree gptel--known-presets))
        (gptel--preset 'instruction-test))
    (gptel-make-preset 'instruction-test :agent--current-agent "result-explorer")
    (gptel-agent-instructions-test--with-project
      (with-current-buffer parent
        (setq-local gptel-agent--current-agent "result-explorer"))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (should (string-match-p "ROOT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm))))))

(ert-deftest gptel-agent-instructions-compaction-keeps-private-state ()
  (gptel-agent-instructions-test--with-project
    (setq fsm (gptel-agent-instructions-test--request parent))
    (gptel-agent-workers-test--tool fsm "Read" (list :file_path (concat front "AGENTS.md")))
    (with-current-buffer parent
      (insert "User task\nWork in progress\n")
      (let ((registry gptel-agent-instructions--registry)
            (compaction (gptel-agent-compact)))
        (should-not (string-match-p (regexp-opt '("ROOT-INSTRUCTION" "FRONT-INSTRUCTION"))
                                    (gptel-agent-instructions-test--payload compaction)))
        (funcall (plist-get (gptel-fsm-info compaction) :callback) "Condensed history" (gptel-fsm-info compaction))
        (should (eq registry gptel-agent-instructions--registry))
        (gptel-agent-instructions-test--write (concat front "AGENTS.md") "AFTER-COMPACTION")
        (setq fsm (gptel-agent-instructions-test--request parent))
        (should (= 1 (gptel-agent-workers-test--count "AFTER-COMPACTION"
                                                     (gptel-agent-instructions-test--payload fsm))))))))

(ert-deftest gptel-agent-instructions-inline-preset-uses-owned-state-without-prompt-leak ()
  (gptel-agent-instructions-test--with-project
    (let ((gptel--known-presets (copy-tree gptel--known-presets)))
      (gptel-make-preset 'instruction-test :system "PRESET-BASE" :agent--current-agent "gptel-agent")
      (with-current-buffer parent
        (kill-local-variable 'gptel-agent-use-project-instructions)
        (setq fsm (gptel-request "@instruction-test Work here" :buffer parent :callback #'ignore
                                :transforms '(gptel--transform-apply-preset)
                                :fsm (gptel-make-fsm :table gptel-send--transitions
                                                     :handlers gptel-send--handlers))))
      (let ((payload (gptel-agent-instructions-test--payload fsm)))
        (should (string-match-p "PRESET-BASE" payload))
        (should (= 1 (gptel-agent-workers-test--count "ROOT-INSTRUCTION" payload))))
      (should (equal (buffer-local-value 'gptel-system-prompt parent) "BASE-PROMPT"))
      (should-not (buffer-local-value 'gptel-agent--current-agent parent)))))

(ert-deftest gptel-agent-instructions-patches-and-directory-creation-are-guarded ()
  (gptel-agent-instructions-test--with-project
    (setq fsm (gptel-agent-instructions-test--request parent))
    (let* ((patch "--- /dev/null\n+++ b/frontend/new.txt\n@@ -0,0 +1 @@\n+new\n")
           (args (list :mode "batch" :root_directory root :operations (vector (list :operation "patch" :root_directory "." :new_str patch)))))
      (gptel-agent-workers-test--tool fsm "Edit" args)
      (should-not (file-exists-p (concat front "new.txt")))
      (should (string-match-p "FRONT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm))))
    (gptel-agent-workers-test--tool fsm "Mkdir" (list :directory_path (concat back "new/deeper")))
    (should-not (file-exists-p (concat back "new")))
    (should (string-match-p "BACK-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
    (gptel-agent-workers-test--tool fsm "Mkdir" (list :directory_path (concat back "new/deeper")))
    (should (file-directory-p (concat back "new/deeper")))))

(ert-deftest gptel-agent-instructions-rechecks-after-edit-preparation ()
  (gptel-agent-instructions-test--with-project
    (setq fsm (gptel-agent-instructions-test--request parent))
    (let ((prepare (symbol-function 'gptel-agent-edit-prepare)))
      (cl-letf (((symbol-function 'gptel-agent-edit-prepare)
                 (lambda (&rest args)
                   (prog1 (apply prepare args)
                     (gptel-agent-instructions-test--write (concat root "AGENTS.md") "CHANGED-DURING-PREPARE")))))
        (gptel-agent-workers-test--tool fsm "Write" (list :file_path (expand-file-name "new.txt" root)  :content "new"))))
    (should-not (file-exists-p (concat root "new.txt")))
    (should (string-match-p "CHANGED-DURING-PREPARE" (gptel-agent-instructions-test--payload fsm)))))

(ert-deftest gptel-agent-instructions-no-project-stops-at-working-directory ()
  (gptel-agent-instructions-test--with-project
    (with-current-buffer parent (setq default-directory front))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (setq fsm (gptel-agent-instructions-test--request parent)))
    (should (equal (buffer-local-value 'gptel-agent-instructions--boundary parent) front))
    (should (string-match-p "FRONT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
    (should-not (string-match-p "ROOT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))))

(ert-deftest gptel-agent-instructions-reenable-refreshes-and-snapshots-stay-immutable ()
  (gptel-agent-instructions-test--with-project
    (setq fsm (gptel-agent-instructions-test--request parent))
    (let ((submitted (car requests))
          (snapshot (plist-get (gptel-fsm-info fsm) :agent-instructions-sent)))
      (with-current-buffer parent (gptel-agent-toggle-project-instructions -1))
      (should (eq snapshot (plist-get (gptel-fsm-info fsm) :agent-instructions-sent)))
      (gptel--handle-wait fsm)
      (should-not (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
      (gptel-agent-instructions-test--write (concat root "AGENTS.md") "REENABLED-INSTRUCTION")
      (with-current-buffer parent (gptel-agent-toggle-project-instructions 1))
      (gptel--handle-wait fsm)
      (should (string-match-p "REENABLED-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
      (should (string-match-p "ROOT-INSTRUCTION" (gptel--json-encode submitted)))
      (should-not (string-match-p "REENABLED-INSTRUCTION" (gptel--json-encode submitted))))))

(ert-deftest gptel-agent-instructions-remote-policy-no-implicit-reads ()
  (gptel-agent-instructions-test--with-project
    (with-current-buffer parent
      (gptel-agent-instructions--initialize)
      (let ((truename (symbol-function 'file-truename)) remote-access)
        (cl-letf (((symbol-function 'file-truename)
                   (lambda (file &rest args)
                     (if (file-remote-p file)
                         (progn (setq remote-access t) (error "Remote access"))
                       (apply truename file args)))))
          (should-error (gptel-agent-instructions--tool-targets
                         "Edit" '(:mode "batch" :root_directory "/ssh:example:/tmp/" :operations [(:operation "delete" :file_path "a")]))))
        (should-not remote-access)))))

(ert-deftest gptel-agent-instructions-internal-symlink-keeps-source-scope ()
  (gptel-agent-instructions-test--with-project
    (with-current-buffer parent
      (gptel-agent-instructions--initialize)
      (delete-file (concat root "AGENTS.md"))
      (make-symbolic-link (concat front "AGENTS.md") (concat root "AGENTS.md"))
      (let ((record (gptel-agent-instructions--load (concat root "AGENTS.md"))))
        (should-not (plist-get record :error))
        (should (equal (plist-get record :text) "FRONT-INSTRUCTION"))
        (should (equal (plist-get record :path) (concat root "AGENTS.md")))
        (should (equal (plist-get record :scope) root))))))

(ert-deftest gptel-agent-instructions-shared-symlinks-startup-read-and-context ()
  (gptel-agent-instructions-test--with-shared-file
    ;; Two independent projects share one file, using absolute and relative links.
    (dolist (relative '(nil t))
      (gptel-agent-instructions-test--with-project
        (make-symbolic-link (if relative (file-relative-name shared-file root) shared-file)
                            (concat root "AGENTS.md") t)
        ;; A nested link chain still has its own, more specific scope.
        (make-symbolic-link "../AGENTS.md" (concat front "AGENTS.md") t)
        ;; Measure the target contents, not the longer symlink target name.
        (let ((gptel-agent-project-instructions-max-bytes (length "SHARED-INSTRUCTION")))
          (setq fsm (gptel-agent-instructions-test--request parent)))
        (let ((record (car (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))))
          (should (equal (plist-get record :text) "SHARED-INSTRUCTION"))
          (should (equal (plist-get record :path) (concat root "AGENTS.md")))
          (should (equal (plist-get record :scope) root)))
        (gptel-agent-workers-test--tool
         fsm "Read" (list :file_path (concat front "AGENTS.md") :offset 1 :limit 1))
        (let ((records (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
              (payload (gptel-agent-instructions-test--payload fsm)))
          (should (equal (mapcar (lambda (r) (plist-get r :scope)) records) (list root front)))
          (should (equal (mapcar (lambda (r) (plist-get r :path)) records)
                         (list (concat root "AGENTS.md") (concat front "AGENTS.md"))))
          (should (= 2 (gptel-agent-workers-test--count "SHARED-INSTRUCTION" payload)))
          (should (string-match-p "Loaded project instruction source" payload)))
        (with-current-buffer parent
          (setq gptel-use-context t gptel-context (list (list (concat front "AGENTS.md")))))
        (setq fsm (gptel-agent-instructions-test--request parent))
        (should (equal (mapcar (lambda (r) (plist-get r :scope))
                              (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
                       (list root front)))
        ;; Reading the shared instructions does not expand ordinary tool access.
        (make-symbolic-link shared-file (concat root "ordinary.md"))
        (with-current-buffer parent
          (should (gptel-agent-access--outside-p
                         "Read" (list :file_path shared-file)))
          (should (gptel-agent-access--outside-p
                         "Read" (list :file_path (concat root "ordinary.md"))))
          (should (gptel-agent-access--outside-p
                         "Write" (list :file_path (concat root "AGENTS.md") :content "overwrite"))))
        (gptel-agent-workers-test--tool fsm "Read" (list :file_path (concat back "AGENTS.md")))
        (should (equal (mapcar (lambda (r) (plist-get r :scope))
                              (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
                       (list root back)))))))

(ert-deftest gptel-agent-instructions-shared-symlink-refresh-and-retarget ()
  (gptel-agent-instructions-test--with-shared-file
    (gptel-agent-instructions-test--with-project
      (make-symbolic-link shared-file (concat root "AGENTS.md") t)
      (setq fsm (gptel-agent-instructions-test--request parent))
      (let ((snapshot (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))
            (replacement (expand-file-name "replacement.md" shared-directory))
            (args (list :file_path (concat root "new.txt") :content "new")))
        ;; Replacing the shared file on save must refresh the next outgoing call.
        (gptel-agent-instructions-test--write replacement "SHARED-REPLACED")
        (rename-file replacement shared-file t)
        (gptel--handle-wait fsm)
        (should (string-match-p "SHARED-REPLACED" (gptel-agent-instructions-test--payload fsm)))
        (should (equal (plist-get (car snapshot) :text) "SHARED-INSTRUCTION"))
        ;; A change after the request blocks an edit until the model sees it.
        (gptel-agent-instructions-test--write shared-file "SHARED-CHANGED")
        (gptel-agent-workers-test--tool fsm "Write" args)
        (should-not (file-exists-p (concat root "new.txt")))
        (should (string-match-p "SHARED-CHANGED" (gptel-agent-instructions-test--payload fsm)))
        (gptel-agent-instructions-test--write replacement "SHARED-RETARGETED")
        (make-symbolic-link replacement (concat root "AGENTS.md") t)
        (gptel-agent-workers-test--tool fsm "Write" args)
        (should-not (file-exists-p (concat root "new.txt")))
        (should (string-match-p "SHARED-RETARGETED" (gptel-agent-instructions-test--payload fsm)))
        (should-not (string-match-p "SHARED-CHANGED" (gptel-agent-instructions-test--payload fsm)))
        (gptel-agent-workers-test--tool fsm "Write" args)
        (should (file-exists-p (concat root "new.txt")))))))

(ert-deftest gptel-agent-instructions-unavailable-symlink-targets-block-writes ()
  (gptel-agent-instructions-test--with-shared-file
    (dolist (failure '(missing directory cycle oversized unreadable))
      (gptel-agent-instructions-test--with-project
        (make-symbolic-link shared-file (concat root "AGENTS.md") t)
        (setq fsm (gptel-agent-instructions-test--request parent))
        (pcase failure
          ('missing (make-symbolic-link (concat shared-directory "missing.md") (concat root "AGENTS.md") t))
          ('directory (make-symbolic-link shared-directory (concat root "AGENTS.md") t))
          ('cycle (make-symbolic-link "AGENTS.md" (concat root "AGENTS.md") t)))
        (let ((gptel-agent-project-instructions-max-bytes
               (if (eq failure 'oversized) 4 gptel-agent-project-instructions-max-bytes))
              (readable (symbol-function 'file-readable-p)))
          (cl-letf (((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'file-readable-p)
                     (lambda (path) (and (not (eq failure 'unreadable)) (funcall readable path)))))
            (gptel-agent-workers-test--tool
             fsm "Write" (list :file_path (concat root "new.txt") :content "new"))))
        (should-not (file-exists-p (concat root "new.txt")))
        (should (string-match-p "UNAVAILABLE" (gptel-agent-instructions-test--payload fsm)))
        (should-not (string-match-p "SHARED-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
        ;; Repairing the link restores the scoped contents on the next call.
        (make-symbolic-link shared-file (concat root "AGENTS.md") t)
        (gptel--handle-wait fsm)
        (should (string-match-p "SHARED-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))))))

(ert-deftest gptel-agent-instructions-symlink-remote-target-never-connects ()
  (gptel-agent-instructions-test--with-project
    (make-symbolic-link "/ssh:example:/tmp/rules.md" (concat root "remote-link"))
    (make-symbolic-link "remote-link" (concat root "AGENTS.md") t)
    (let ((file-name-handler-alist
           (cons (cons "\\`/ssh:" (lambda (operation &rest _)
                                    (if (eq operation 'file-remote-p) "/ssh:example:"
                                      (ert-fail (format "Unexpected remote access: %s" operation)))))
                 file-name-handler-alist)))
      (cl-letf (((symbol-function 'display-warning) #'ignore))
        (setq fsm (gptel-agent-instructions-test--request parent))
        (gptel-agent-workers-test--tool fsm "Read" (list :file_path (concat root "AGENTS.md")))))
    (let ((record (car (plist-get (gptel-fsm-info fsm) :agent-instructions-sent))))
      (should (string-match-p "remote sources" (plist-get record :error)))
      (should-not (plist-get record :text)))))

(ert-deftest gptel-agent-instructions-cached-provider-state-cannot-retain-old-block ()
  (gptel-agent-instructions-test--with-project
    (setq fsm (gptel-agent-instructions-test--request parent))
    (with-current-buffer parent (gptel-agent-toggle-project-instructions -1))
    (dolist (key '(:cachedContent :conversation :previous_response_id))
      (setf (plist-get (plist-get (gptel-fsm-info fsm) :data) key) "retained")
      (should-error (gptel--handle-wait fsm))
      (setf (plist-get (plist-get (gptel-fsm-info fsm) :data) key) nil))
    (should (= 1 (length requests)))))

(ert-deftest gptel-agent-instructions-api-requests-without-pretool-handler ()
  (gptel-agent-instructions-test--with-project
    (with-current-buffer parent
      (setq fsm (gptel-request "Work here" :buffer parent :callback #'ignore)))
    (should-not (assq 'TPRE (gptel-fsm-handlers fsm)))
    (gptel-agent-workers-test--tool fsm "Read" (list :file_path (concat front "AGENTS.md")))
    (should (string-match-p "FRONT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
    (gptel-agent-workers-test--tool fsm "Read" (list :file_path (concat back "AGENTS.md")))
    (should-not (string-match-p "FRONT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
    (should (= 1 (gptel-agent-workers-test--count "BACK-INSTRUCTION"
                                                 (gptel-agent-instructions-test--payload fsm))))))

(ert-deftest gptel-agent-instructions-search-targets-discover-only-selected-chains ()
  (gptel-agent-instructions-test--with-project
    (setq fsm (gptel-agent-instructions-test--request parent))
    (gptel-agent-workers-test--tool fsm "Glob" (list :root_directory front :pattern "*.md"))
    (let ((deadline (+ (float-time) 10)))
      (while (and (eq (gptel-fsm-state fsm) 'TOOL) (< (float-time) deadline))
        (accept-process-output nil 0.01))
      (should-not (eq (gptel-fsm-state fsm) 'TOOL)))
    (should (string-match-p "FRONT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
    (should-not (string-match-p "BACK-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
    (gptel-agent-workers-test--tool fsm "Grep" (list :path (concat root "old.txt") :regex "old"))
    (let ((deadline (+ (float-time) 10)))
      (while (and (eq (gptel-fsm-state fsm) 'TOOL) (< (float-time) deadline))
        (accept-process-output nil 0.01))
      (should-not (eq (gptel-fsm-state fsm) 'TOOL)))
    (should-not (string-match-p "FRONT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
    (should (= 2 (length (buffer-local-value 'gptel-agent-instructions--registry parent))))))

(ert-deftest gptel-agent-instructions-pathinfo-and-rmdir-respect-scopes ()
  (gptel-agent-instructions-test--with-project
    (make-directory (concat back "empty"))
    (setq fsm (gptel-agent-instructions-test--request parent))
    (gptel-agent-workers-test--tool fsm "PathInfo" (list :path (concat front "new.java")))
    (should (string-match-p "FRONT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
    (gptel-agent-workers-test--tool fsm "Rmdir" (list :directory_path (concat back "empty")))
    (should (file-directory-p (concat back "empty")))
    (should (string-match-p "BACK-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
    (gptel-agent-workers-test--tool fsm "Rmdir" (list :directory_path (concat back "empty")))
    (should-not (file-exists-p (concat back "empty")))))

(provide 'gptel-agent-instructions-test)
;;; gptel-agent-instructions-test.el ends here
