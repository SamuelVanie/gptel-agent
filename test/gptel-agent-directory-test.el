;;; gptel-agent-directory-test.el --- Working directory changes -*- lexical-binding: t; -*-

(require 'gptel-agent-instructions-test)

(defun gptel-agent-directory-test--request (owner)
  "Start a request in OWNER with ChangeDirectory available."
  (with-current-buffer owner
    (push (gptel-get-tool "ChangeDirectory") gptel-tools))
  (gptel-agent-instructions-test--request owner))

(defun gptel-agent-directory-test--ask (fsm directory &optional reason)
  "Request DIRECTORY in FSM and return the pending approval calls."
  (let ((callback (plist-get (gptel-fsm-info fsm) :callback)) pending)
    (setf (plist-get (gptel-fsm-info fsm) :callback)
          (lambda (response info)
            (if (eq (car-safe response) 'tool-call)
                (setq pending (cdr response))
              (funcall callback response info))))
    (gptel-agent-instructions-test--tool
     fsm "ChangeDirectory" (list :directory directory :reason (or reason "Inspect the other project")))
    pending))

(ert-deftest gptel-agent-directory-approved-change-refreshes-all-providers ()
  (dolist (backend (list (gptel-make-openai "move-openai" :host "example.invalid" :key "unused" :models '(test))
                        (gptel-make-openai-responses "move-responses" :key "unused" :models '(test))
                        (gptel-make-anthropic "move-anthropic" :key "unused" :models '(test))
                        (gptel-make-gemini "move-gemini" :key "unused" :models '(test))
                        (gptel-make-ollama "move-ollama" :models '(test))))
    (let ((gptel-agent-instructions-test--backend backend))
      (gptel-agent-instructions-test--with-project
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
          (with-current-buffer parent (cd front))
          (setq fsm (gptel-agent-directory-test--request parent))
          ;; Outside targets fail with actionable recovery guidance.
          (gptel-agent-instructions-test--tool fsm "Read" (list :file_path (concat back "AGENTS.md")))
          (should (string-match-p "Use ChangeDirectory" (gptel-agent-instructions-test--payload fsm)))
          (let ((calls (gptel-agent-directory-test--ask fsm "../backend")))
            (should (= (length calls) 1))
            (should (eq (gptel-fsm-state fsm) 'TOOL))
            (should (equal (plist-get (cadar calls) :directory) back))
            (should (equal (buffer-local-value 'default-directory parent) front))
            ;; Approval can be displayed/executed in another buffer.
            (with-temp-buffer
              (setq default-directory root)
              (gptel--accept-tool-calls calls)
              (should (equal default-directory root)))
            (should (equal (buffer-local-value 'default-directory parent) back))
            (should (equal (buffer-local-value 'gptel-agent-instructions--boundary parent) back))
            (let ((sent (plist-get (gptel-fsm-info fsm) :agent-instructions-sent)))
              (should (equal (mapcar (lambda (record) (plist-get record :text)) sent)
                             '("BACK-INSTRUCTION"))))
            (gptel-agent-instructions-test--tool fsm "Write" '(:file_path "new.txt" :content "approved"))
            (should (file-exists-p (concat back "new.txt")))
            (should-not (file-exists-p (concat front "new.txt")))))))))

(ert-deftest gptel-agent-directory-confirmation-survives-settings-and-grants ()
  (dolist (setting '(nil auto t))
    (dolist (instructions '(nil t))
      (gptel-agent-instructions-test--with-project
        (with-current-buffer parent
          (cd front)
          (setq gptel-confirm-tool-calls setting
                gptel-agent-use-project-instructions instructions
                gptel-pre-tool-call-functions (list (lambda (_) '(:confirm nil)))))
        (setq fsm (gptel-agent-directory-test--request parent))
        ;; Simulate a skill pre-approval without mutating the registry template.
        (let ((grant (gptel--copy-tool (gptel-get-tool "ChangeDirectory"))))
          (setf (gptel-tool-confirm grant) nil
                (plist-get (gptel-fsm-info fsm) :tools) (list grant)))
        (should (gptel-agent-directory-test--ask fsm "../backend"))
        (should (equal (buffer-local-value 'default-directory parent) front))))))

(ert-deftest gptel-agent-directory-invalid-targets-never-ask-or-move ()
  (dolist (directory '(nil :null "" 42 "missing" "../old.txt" "/ssh:host:/tmp/"))
    (gptel-agent-instructions-test--with-project
      (with-current-buffer parent (cd front))
      (setq fsm (gptel-agent-directory-test--request parent))
      (should-not (gptel-agent-directory-test--ask fsm directory))
      (should (equal (buffer-local-value 'default-directory parent) front))
      (should-not (eq (gptel-fsm-state fsm) 'TOOL))
      (should (string-match-p "Error:" (gptel-agent-instructions-test--payload fsm)))))
  (gptel-agent-instructions-test--with-project
    (setq fsm (gptel-agent-directory-test--request parent))
    (should-not (gptel-agent-directory-test--ask fsm back "  "))
    (should (equal (buffer-local-value 'default-directory parent) root))))

(ert-deftest gptel-agent-directory-denial-and-abort-invalidate-approval ()
  (dolist (action '(deny abort manual-cd removed-target redirected-target))
    (gptel-agent-instructions-test--with-project
      (with-current-buffer parent (cd front))
      (setq fsm (gptel-agent-directory-test--request parent))
      (let* ((calls (gptel-agent-directory-test--ask fsm back))
             (function (gptel-tool-function (caar calls))))
        (pcase action
          ('deny
           (with-current-buffer parent (gptel--reject-tool-calls calls))
           (should (string-match-p "denied by the user" (gptel-agent-instructions-test--payload fsm))))
          ('abort (gptel--fsm-transition fsm 'ABRT))
          ('manual-cd (with-current-buffer parent (cd root)))
          ('removed-target (delete-directory back t))
          ('redirected-target
           (delete-directory back t)
           (make-symbolic-link front (directory-file-name back))))
        (should-error (funcall function back "Inspect the other project"))
        (should (equal (buffer-local-value 'default-directory parent)
                       (if (eq action 'manual-cd) root front)))))))

(ert-deftest gptel-agent-directory-mixed-batch-never-reaches-edit-hooks ()
  (gptel-agent-instructions-test--with-project
    (with-current-buffer parent (cd front))
    (setq fsm (gptel-agent-directory-test--request parent))
    (let* ((info (gptel-fsm-info fsm))
           (calls (list (list :id "move" :name "ChangeDirectory" :args (list :directory back :reason "Work there"))
                        (list :id "write" :name "Write" :args '(:file_path "new.txt" :content "wrong project"))))
           hooks)
      (with-current-buffer parent
        (setq-local gptel-pre-tool-call-functions (list (lambda (_) (setq hooks t)))))
      (gptel--parse-response
       (plist-get info :backend)
       (if (gptel-openai-responses-p (plist-get info :backend))
           `(:output ,(vconcat
                       (mapcar (lambda (call)
                                 `(:type "function_call" :call_id ,(plist-get call :id)
                                         :name ,(plist-get call :name)
                                         :arguments ,(gptel--json-encode (plist-get call :args))))
                               calls)))
         `(:choices [(:message (:role "assistant" :content :null
                                     :tool_calls ,(vconcat
                                                   (mapcar
                                                    (lambda (call)
                                                      `(:id ,(plist-get call :id) :type "function"
                                                            :function (:name ,(plist-get call :name)
                                                                             :arguments ,(gptel--json-encode (plist-get call :args)))))
                                                    calls))))])) info)
      (gptel--fsm-transition fsm 'TYPE)
      (gptel--fsm-transition fsm)
      (should-not hooks)
      (should-not (file-exists-p (concat front "new.txt")))
      (should-not (file-exists-p (concat back "new.txt")))
      (should (equal (buffer-local-value 'default-directory parent) front))
      (should (string-match-p "Call ChangeDirectory alone" (gptel-agent-instructions-test--payload fsm)))
      (should (eq (gptel-fsm-state fsm) 'WAIT)))))

(ert-deftest gptel-agent-directory-approval-preview-names-owner-and-reason ()
  (gptel-agent-instructions-test--with-project
    (with-current-buffer parent (cd front))
    (with-temp-buffer
      (gptel-agent-directory--preview (list back "Read sibling API") (list :buffer parent))
      (dolist (text (list (buffer-name parent) front back "Read sibling API"))
        (should (string-match-p (regexp-quote text) (buffer-string)))))))

(ert-deftest gptel-agent-directory-worker-approval-moves-only-worker ()
  (gptel-agent-instructions-test--with-project
    (gptel-agent-workers-test--with-chat
      (with-current-buffer parent
        (cd front)
        (setq-local gptel-agent-preset '(:tools ("ChangeDirectory"))))
      (let* ((run (gptel-agent-workers-test--launch fsm))
             (child (gptel-agent--run-child-buffer run))
             (worker (gptel-agent--run-fsm run)))
        (gptel-agent-workers-test--tool
         worker "ChangeDirectory" '(:directory "../backend" :reason "Read the sibling API"))
        (with-current-buffer parent
          (let* ((overlay (cl-find-if
                           (lambda (ov) (overlay-get ov 'gptel-tool))
                           (overlays-in (point-min) (point-max))))
                 (calls (overlay-get overlay 'gptel-tool)))
            (should (equal (gptel-tool-name (caar calls)) "ChangeDirectory"))
            (should (string-match-p (regexp-quote (buffer-name child)) (buffer-string)))
            (should (equal (buffer-local-value 'default-directory child) front))
            (gptel--accept-tool-calls calls overlay)
            (should (equal default-directory front))))
        (should (equal (buffer-local-value 'default-directory child) back))
        (gptel-agent-workers-test--answer worker "Done")))))

(ert-deftest gptel-agent-directory-api-without-pre-tool-handler-still-asks ()
  (gptel-agent-instructions-test--with-project
    (with-current-buffer parent
      (cd front)
      (push (gptel-get-tool "ChangeDirectory") gptel-tools)
      (setq fsm (gptel-request "Move" :buffer parent :stream nil :callback #'ignore
                               :transforms '(gptel-agent-instructions--transform))))
    (should-not (assq 'TPRE (gptel-fsm-handlers fsm)))
    (let ((calls (gptel-agent-directory-test--ask fsm back)))
      (should calls)
      (should (equal (buffer-local-value 'default-directory parent) front))
      (gptel--accept-tool-calls calls)
      (should (equal (buffer-local-value 'default-directory parent) back)))))

(ert-deftest gptel-agent-directory-discussion-approval-and-cancellation ()
  (dolist (approve '(nil t))
    (gptel-agent-instructions-test--with-project
      (gptel-agent-discussion-test--with-chat
        (with-current-buffer parent (cd front))
        (setq chat (gptel-agent-discussion-test--spawn fsm parent))
        (let* ((child (gptel-agent-discussion--chat-buffer chat))
               (worker (gptel-agent-discussion--chat-fsm chat)))
          (push (gptel-get-tool "ChangeDirectory") (plist-get (gptel-fsm-info worker) :tools))
          (gptel-agent-workers-test--tool
           worker "ChangeDirectory" (list :directory back :reason "Read the sibling project"))
          (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
                 (calls (gptel-agent-discussion--interaction-calls pending))
                 (overlay (car (gptel-agent-discussion--interaction-overlays pending))))
            (should (equal (buffer-local-value 'default-directory child) front))
            (with-current-buffer child
              (if approve (gptel--accept-tool-calls calls overlay)
                (gptel--reject-tool-calls calls overlay)))
            (should (equal (buffer-local-value 'default-directory child) (if approve back front)))
            (should (equal (buffer-local-value 'default-directory parent) front))
            (unless approve
              (should (eq (gptel-fsm-state worker) 'ABRT))
              (gptel--accept-tool-calls calls)
              (should (equal (buffer-local-value 'default-directory child) front)))))))))

(ert-deftest gptel-agent-directory-cd-refreshes-active-request-all-providers ()
  (dolist (backend (list (gptel-make-openai "cd-openai" :key "unused" :models '(test))
                        (gptel-make-openai-responses "cd-responses" :key "unused" :models '(test))
                        (gptel-make-anthropic "cd-anthropic" :key "unused" :models '(test))
                        (gptel-make-gemini "cd-gemini" :key "unused" :models '(test))
                        (gptel-make-ollama "cd-ollama" :models '(test))))
    (let ((gptel-agent-instructions-test--backend backend))
      (dolist (project '(nil t))
        (gptel-agent-instructions-test--with-project
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&rest _) (when project (cons 'transient root)))))
            (with-current-buffer parent (cd front))
            (setq fsm (gptel-agent-instructions-test--request parent))
            ;; Select a target, as an ordinary tool call would, before cd.
            (gptel-agent-instructions-test--tool fsm "Glob" (list :pattern "*"))
            (let ((submitted (car requests)))
              (with-current-buffer parent (cd "../backend"))
              (gptel--handle-wait fsm)
              (let ((payload (gptel-agent-instructions-test--payload fsm)))
                (should (string-match-p "BACK-INSTRUCTION" payload))
                (should-not (string-match-p "FRONT-INSTRUCTION" payload))
                (should (= 1 (gptel-agent-workers-test--count
                              "<gptel-project-instructions>" payload)))
                (should (eq (not (null (string-match-p "ROOT-INSTRUCTION" payload))) project)))
              (should (string-match-p "FRONT-INSTRUCTION" (prin1-to-string submitted))))
            (should (equal (buffer-local-value 'gptel-agent-instructions--boundary parent)
                           (if project root back)))))))))

(ert-deftest gptel-agent-directory-cd-between-projects-and-turns ()
  (gptel-agent-instructions-test--with-project
    (cl-letf (((symbol-function 'project-current)
               (lambda (_prompt directory)
                 (cons 'transient (if (string-prefix-p front directory) front back)))))
      (with-current-buffer parent (cd front))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (gptel-agent-instructions-test--tool fsm "Glob" (list :pattern "*"))
      (gptel--fsm-transition fsm 'DONE)
      (with-current-buffer parent (cd back))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (should (equal (buffer-local-value 'gptel-agent-instructions--boundary parent) back))
      (should (string-match-p "BACK-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
      (should-not (string-match-p "FRONT-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
      (gptel-agent-instructions-test--tool fsm "Write" '(:file_path "new.txt" :content "new project"))
      (should (file-exists-p (concat back "new.txt")))
      (should-not (file-exists-p (concat front "new.txt"))))))

(ert-deftest gptel-agent-directory-cd-defers-pending-relative-write ()
  (dolist (instructions '(nil t))
    (gptel-agent-instructions-test--with-project
      (unless instructions
        (dolist (dir (list root front back)) (delete-file (concat dir "AGENTS.md"))))
      (with-current-buffer parent (cd front))
      (setq fsm (gptel-agent-instructions-test--request parent))
      (let ((write (gptel-tool-function
                    (cl-find "Write" (plist-get (gptel-fsm-info fsm) :tools)
                             :key #'gptel-tool-name :test #'equal))))
        ;; This closure could have been retained by an approval UI.
        (with-current-buffer parent (cd back))
        (should (string-match-p "Deferred before staging" (funcall write "new.txt" "stale")))
        (should-not (file-exists-p (concat front "new.txt")))
        (should-not (file-exists-p (concat back "new.txt")))
        (gptel--handle-wait fsm)
        (funcall write "new.txt" "reconsidered")
        (should (equal (with-temp-buffer
                         (insert-file-contents (concat back "new.txt")) (buffer-string))
                       "reconsidered"))))))

(ert-deftest gptel-agent-directory-agent-launch-inherits-or-selects-own-project ()
  (dolist (background '(nil t))
    (dolist (directory '(nil "" :null "../backend"))
      (gptel-agent-instructions-test--with-project
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
          (gptel-agent-workers-test--with-chat
            (with-current-buffer parent
              (cd front)
              (setq-local gptel-agent-preset '(:tools ("Read" "Bash"))))
            (let* ((tool (cl-find "Agent" (plist-get (gptel-fsm-info fsm) :tools)
                                  :key #'gptel-tool-name :test #'equal))
                   (expected (if (equal directory "../backend") back front))
                   result)
              ;; Dispatch from an unrelated buffer: relative paths belong to the caller.
              (with-temp-buffer
                (setq default-directory temporary-file-directory)
                (apply (gptel-tool-function tool) (lambda (text) (setq result text))
                       (gptel--map-tool-args
                        tool (list :subagent_type "researcher" :description "Inspect directory"
                                   :prompt "Inspect this project" :directory directory
                                   :background background))))
              (let* ((run (car (hash-table-values gptel-agent--runs)))
                     (child (gptel-agent--run-child-buffer run))
                     (worker (gptel-agent--run-fsm run))
                     (payload (gptel-agent-instructions-test--payload worker)))
                (should (equal (buffer-local-value 'default-directory child) expected))
                (should (equal (buffer-local-value 'gptel-agent-instructions--boundary child) expected))
                (should (equal (buffer-local-value 'default-directory parent) front))
                (should (string-match-p (if (equal expected back) "BACK-INSTRUCTION" "FRONT-INSTRUCTION") payload))
                (should-not (string-match-p (if (equal expected back) "FRONT-INSTRUCTION" "BACK-INSTRUCTION") payload))
                (if background (should (string-match-p "run_id:" result)) (should-not result))
                ;; Exercise actual shell execution in the selected child directory.
                (let (output)
                  (with-current-buffer child
                    (gptel-agent--execute-bash (lambda (text) (setq output text)) "pwd" "Working directory"))
                  (let ((deadline (+ (float-time) 5)))
                    (while (and (not output) (< (float-time) deadline))
                      (accept-process-output nil 0.01)))
                  (should (string-match-p (regexp-quote (directory-file-name expected)) (or output ""))))
                (gptel-agent-workers-test--answer worker "Done")))))))))

(ert-deftest gptel-agent-directory-agent-followup-retains-or-changes-directory ()
  (gptel-agent-instructions-test--with-project
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (gptel-agent-followup-test--with-chat
        (with-current-buffer parent (cd root))
        (gptel-agent--task deliver "researcher" "First" "Work" parent-fsm snapshot nil nil nil nil front)
        (let* ((first (car requests))
               (run (plist-get (gptel-fsm-info first) :context))
               (session (gptel-agent--run-session run))
               (id (gptel-agent--session-id session))
               (child (gptel-agent--run-child-buffer run)))
          (gptel-agent-followup-test--finish first "Done")
          (with-current-buffer parent (cd back))
          (dolist (directory '(nil "" :null "../backend"))
            (gptel-agent--task deliver "researcher" "Follow-up" "Continue"
                               parent-fsm snapshot nil nil id nil directory)
            (let* ((next (car requests))
                   (expected (if (equal directory "../backend") back front)))
              (should (eq (plist-get (gptel-fsm-info next) :buffer) child))
              (should (equal (buffer-local-value 'default-directory child) expected))
              (should (string-match-p (if (equal expected back) "BACK-INSTRUCTION" "FRONT-INSTRUCTION")
                                      (gptel-agent-instructions-test--payload next)))
              (should-not (string-match-p (if (equal expected back) "FRONT-INSTRUCTION" "BACK-INSTRUCTION")
                                          (gptel-agent-instructions-test--payload next)))
              (gptel-agent-followup-test--finish next "Done")))
          ;; Invalid follow-ups must not consume the retained session.
          (let ((count (length requests)))
            (gptel-agent--task deliver "researcher" "Invalid" "Continue"
                               parent-fsm snapshot nil nil id nil "missing")
            (should (string-prefix-p "Error:" (car received)))
            (should (= count (length requests)))
            (should (gptel-agent--session-resumable session))
            (should (equal (buffer-local-value 'default-directory child) back))))))))

(ert-deftest gptel-agent-directory-invalid-agent-launch-does-not-create-worker ()
  (gptel-agent-instructions-test--with-project
    (gptel-agent-followup-test--with-chat
      (with-current-buffer parent (cd root))
      (dolist (directory '("missing" "old.txt" 42))
        (gptel-agent--task deliver "researcher" "Invalid" "Work"
                           parent-fsm snapshot nil nil nil t directory)
        (should (string-prefix-p "Error:" (car received)))
        (should-not requests)
        (should-not (buffer-local-value 'gptel-agent--sessions parent))
        (should (= 0 (hash-table-count gptel-agent--runs)))))))

(ert-deftest gptel-agent-directory-discussion-selects-independent-project ()
  (dolist (directory '(nil "" :null "../backend"))
    (gptel-agent-instructions-test--with-project
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (gptel-agent-discussion-test--with-chat
          (with-current-buffer parent (cd front))
          (gptel-agent-workers-test--tool
           fsm "Discussion" (list :title "Directory test" :prompt "Work here" :directory directory))
          (setq chat (car (buffer-local-value 'gptel-agent-discussion--children parent)))
          (let* ((child (gptel-agent-discussion--chat-buffer chat))
                 (expected (if (equal directory "../backend") back front))
                 (payload (gptel-agent-instructions-test--payload (gptel-agent-discussion--chat-fsm chat))))
            (should (equal (buffer-local-value 'default-directory child) expected))
            (should (equal (buffer-local-value 'gptel-agent-instructions--boundary child) expected))
            (should (equal (buffer-local-value 'default-directory parent) front))
            (should (string-match-p (if (equal expected back) "BACK-INSTRUCTION" "FRONT-INSTRUCTION") payload))
            (should-not (string-match-p (if (equal expected back) "FRONT-INSTRUCTION" "BACK-INSTRUCTION") payload))))))))

(provide 'gptel-agent-directory-test)
;;; gptel-agent-directory-test.el ends here
