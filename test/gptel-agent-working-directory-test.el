;;; gptel-agent-working-directory-test.el --- Working directories -*- lexical-binding: t; -*-

(require 'gptel-agent-instructions-test)

(ert-deftest gptel-agent-working-directory-cd-refreshes-active-request-all-providers ()
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

(ert-deftest gptel-agent-working-directory-cd-between-projects-and-turns ()
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

(ert-deftest gptel-agent-working-directory-cd-defers-pending-relative-write ()
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

(ert-deftest gptel-agent-working-directory-agent-launch-inherits-or-selects-own-project ()
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

(ert-deftest gptel-agent-working-directory-agent-followup-retains-or-changes-directory ()
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

(ert-deftest gptel-agent-working-directory-invalid-agent-launch-does-not-create-worker ()
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

(ert-deftest gptel-agent-working-directory-discussion-selects-independent-project ()
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

(provide 'gptel-agent-working-directory-test)
;;; gptel-agent-working-directory-test.el ends here
