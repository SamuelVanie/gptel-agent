;;; gptel-agent-access-test.el --- Outside file approvals -*- lexical-binding: t; -*-

(require 'gptel-agent-instructions-test)

(cl-defmacro gptel-agent-access-test--with-project (&body body)
             (declare (indent 0))
             `(gptel-agent-instructions-test--with-project
               (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
                        (with-current-buffer parent (cd front))
                        (gptel-agent-instructions-test--write (concat back "data.txt") "OUTSIDE-CONTENT")
                        ,@body)))

(defun gptel-agent-access-test--ask (fsm name args)
  "Submit NAME/ARGS to FSM and return its pending approvals."
  (let ((callback (plist-get (gptel-fsm-info fsm) :callback)) pending)
    (unwind-protect
        (progn
          (setf (plist-get (gptel-fsm-info fsm) :callback)
                (lambda (response info &rest extra)
                  (if (eq (car-safe response) 'tool-call)
                      (setq pending (cdr response))
                    (apply callback response info extra))))
          (gptel-agent-instructions-test--tool fsm name args)
          pending)
      (setf (plist-get (gptel-fsm-info fsm) :callback) callback))))

(ert-deftest gptel-agent-access-read-one-call-all-providers ()
  (dolist (backend (list (gptel-make-openai "access-openai" :key "unused" :models '(test))
                         (gptel-make-openai-responses "access-responses" :key "unused" :models '(test))
                         (gptel-make-anthropic "access-anthropic" :key "unused" :models '(test))
                         (gptel-make-gemini "access-gemini" :models '(test))
                         (gptel-make-ollama "access-ollama" :models '(test))))
    (let ((gptel-agent-instructions-test--backend backend))
      (gptel-agent-access-test--with-project
       (setq fsm (gptel-agent-instructions-test--request parent))
       (let ((calls (gptel-agent-access-test--ask fsm "Read" '(:file_path "../backend/data.txt"))))
         (should (= (length calls) 1))
         (should (equal (plist-get (cadar calls) :file_path) (concat back "data.txt")))
         (should-not (string-match-p "BACK-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
         (should-not (string-match-p "OUTSIDE-CONTENT" (gptel-agent-instructions-test--payload fsm)))
         (with-temp-buffer
           (setq default-directory root)
           (gptel--accept-tool-calls calls)
           (should (equal default-directory root)))
         (should (string-match-p "OUTSIDE-CONTENT" (gptel-agent-instructions-test--payload fsm)))
         (should (string-match-p "BACK-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
         (should (equal (buffer-local-value 'default-directory parent) front))
         (should (equal (buffer-local-value 'gptel-agent-instructions--boundary parent) front))
         (should-error (apply (gptel-tool-function (caar calls))
                              (gptel--map-tool-args (caar calls) (cadar calls))))
         ;; Neither the same path nor another outside call inherits approval.
         (should (gptel-agent-access-test--ask fsm "Read" '(:file_path "../backend/data.txt"))))))))

(ert-deftest gptel-agent-access-settings-and-skill-grants-do-not-bypass ()
  (dolist (setting '(nil auto t))
    (dolist (instructions '(nil t))
      (gptel-agent-access-test--with-project
       (with-current-buffer parent
         (setq gptel-confirm-tool-calls setting
               gptel-agent-use-project-instructions instructions
               gptel-pre-tool-call-functions (list (lambda (_) '(:confirm nil)))))
       (setq fsm (gptel-agent-instructions-test--request parent))
       (let ((grant (gptel--copy-tool (gptel-get-tool "Read"))))
         (setf (gptel-tool-confirm grant) nil
               (plist-get (gptel-fsm-info fsm) :tools) (list grant)))
       (should (gptel-agent-access-test--ask fsm "Read" (list :file_path (concat back "data.txt"))))))))

(ert-deftest gptel-agent-access-write-discovers-instructions-after-approval ()
  (gptel-agent-access-test--with-project
   (setq fsm (gptel-agent-instructions-test--request parent))
   (let* ((path (concat back "new.txt"))
          (args (list :file_path path :content "new"))
          (calls (gptel-agent-access-test--ask fsm "Write" args)))
     (should calls)
     (should-not (file-exists-p path))
     (gptel--accept-tool-calls calls)
     (should-not (file-exists-p path))
     (should (string-match-p "Deferred before staging" (gptel-agent-instructions-test--payload fsm)))
     (should (string-match-p "BACK-INSTRUCTION" (gptel-agent-instructions-test--payload fsm)))
     (setq calls (gptel-agent-access-test--ask fsm "Write" args))
     (should calls)
     (gptel--accept-tool-calls calls)
     (should (file-exists-p path))
     (should (equal (buffer-local-value 'default-directory parent) front)))))

(ert-deftest gptel-agent-access-review-does-not-stage-until-approved ()
  (gptel-agent-access-test--with-project
   (with-current-buffer parent
     (setq gptel-agent-use-project-instructions nil
           gptel-confirm-tool-calls 'auto
           gptel-pre-tool-call-functions '(smv/gptel-ediff-tool-call)))
   (setq fsm (gptel-agent-instructions-test--request parent))
   (let* ((path (concat back "new.txt"))
          (calls (gptel-agent-access-test--ask fsm "Write" (list :file_path path :content "new"))))
     (should calls)
     (should-not (file-exists-p path))
     (should-not (buffer-local-value 'gptel-agent-review--queue parent))
     (gptel--accept-tool-calls calls)
     (should (buffer-local-value 'gptel-agent-review--queue parent))
     (should (file-exists-p path)))))

(ert-deftest gptel-agent-access-stale-approvals-cannot-write ()
  (dolist (action '(abort directory symlink arguments))
    (gptel-agent-access-test--with-project
     (with-current-buffer parent (setq gptel-agent-use-project-instructions nil))
     (make-symbolic-link back (concat front "link"))
     (setq fsm (gptel-agent-instructions-test--request parent))
     (let* ((calls (gptel-agent-access-test--ask fsm "Write" '(:file_path "link/new.txt" :content "new")))
            (tool (caar calls))
            (values (gptel--map-tool-args tool (cadar calls))))
       (should calls)
       (pcase action
         ('abort (gptel--fsm-transition fsm 'ABRT))
         ('directory (with-current-buffer parent (cd root)))
         ('symlink (delete-file (concat front "link"))
                   (make-symbolic-link root (concat front "link")))
         ('arguments (setcar values (concat back "other.txt"))))
       (should-error (apply (gptel-tool-function tool) values))
       (should-not (file-exists-p (concat back "new.txt")))
       (should-not (file-exists-p (concat root "new.txt")))
       (should-not (file-exists-p (concat back "other.txt")))))))

(ert-deftest gptel-agent-access-async-searches-ask-and-complete ()
  (dolist (name '("Glob" "Grep"))
    (gptel-agent-access-test--with-project
     (setq fsm (gptel-agent-instructions-test--request parent))
     (let ((calls (gptel-agent-access-test--ask
                   fsm name (if (equal name "Glob")
                                (list :root_directory back :pattern "*.txt")
                              (list :path back :regex "OUTSIDE-CONTENT")))))
       (should calls)
       (gptel--accept-tool-calls calls)
       (let ((deadline (+ (float-time) 10)))
         (while (and (eq (gptel-fsm-state fsm) 'TOOL) (< (float-time) deadline))
           (accept-process-output nil 0.02)))
       (should (eq (gptel-fsm-state fsm) 'WAIT))
       (should (string-match-p "data.txt" (gptel-agent-instructions-test--payload fsm)))
       (should (equal (buffer-local-value 'default-directory parent) front))))))

(ert-deftest gptel-agent-access-api-without-pre-tool-still-asks ()
  (gptel-agent-access-test--with-project
   (with-current-buffer parent
     (setq fsm (gptel-request "Read" :buffer parent :callback #'ignore
                              :transforms '(gptel-agent-instructions--transform))))
   (should-not (assq 'TPRE (gptel-fsm-handlers fsm)))
   (let ((calls (gptel-agent-access-test--ask fsm "Read" (list :file_path (concat back "data.txt")))))
     (should calls)
     (gptel--accept-tool-calls calls)
     (should (string-match-p "OUTSIDE-CONTENT" (gptel-agent-instructions-test--payload fsm))))))

(ert-deftest gptel-agent-access-mixed-batch-runs-local-call-while-outside-waits ()
  (gptel-agent-access-test--with-project
   (with-current-buffer parent (setq gptel-agent-use-project-instructions nil))
   (setq fsm (gptel-agent-instructions-test--request parent))
   (let* ((info (gptel-fsm-info fsm)) pending
          (calls (list (list :id "local" :name "Write" :args (list :file_path "local.txt" :content "local"))
                       (list :id "outside" :name "Write" :args (list :file_path "../backend/new.txt" :content "outside")))))
     (setf (plist-get info :callback)
           (lambda (response _info)
             (when (eq (car-safe response) 'tool-call) (setq pending (cdr response)))))
     (gptel--parse-response
      (plist-get info :backend)
      (if (gptel-openai-responses-p (plist-get info :backend))
          `(:output ,(vconcat (mapcar (lambda (call)
                                        `(:type "function_call" :call_id ,(plist-get call :id)
                                                :name ,(plist-get call :name)
                                                :arguments ,(gptel--json-encode (plist-get call :args))))
                                      calls)))
        `(:choices [(:message (:role "assistant" :content :null
                                     :tool_calls ,(vconcat
                                                   (mapcar (lambda (call)
                                                             `(:id ,(plist-get call :id) :type "function"
                                                                   :function (:name ,(plist-get call :name)
                                                                                    :arguments ,(gptel--json-encode (plist-get call :args)))))
                                                           calls))))])) info)
     (gptel--fsm-transition fsm 'TYPE)
     (gptel--fsm-transition fsm)
     (should (= (length pending) 1))
     (should (file-exists-p (concat front "local.txt")))
     (should-not (file-exists-p (concat back "new.txt")))
     (with-current-buffer parent (gptel--reject-tool-calls pending))
     (should-not (file-exists-p (concat back "new.txt")))
     (gptel--fsm-transition fsm 'ABRT)
     (gptel--accept-tool-calls pending)
     (should-not (file-exists-p (concat back "new.txt"))))))

(ert-deftest gptel-agent-access-batch-edit-inherited-review-hooks-wait ()
  (gptel-agent-access-test--with-project
   (with-current-buffer parent
     (setq gptel-agent-use-project-instructions nil gptel-confirm-tool-calls 'auto
           gptel-pre-tool-call-functions '(t)))
   (setq fsm (gptel-agent-instructions-test--request parent))
   (let ((calls (gptel-agent-access-test--ask
                 fsm "Edit" (list :mode "batch" :root_directory root
                                  :operations [(:operation "write" :file_path "frontend/local.txt" :content "local")
                                               (:operation "write" :file_path "backend/new.txt" :content "outside")]))))
     (should calls)
     (should-not (buffer-local-value 'gptel-agent-review--queue parent))
     (should-not (file-exists-p (concat front "local.txt")))
     (should-not (file-exists-p (concat back "new.txt")))
     (gptel--accept-tool-calls calls)
     (should (file-exists-p (concat front "local.txt")))
     (should (file-exists-p (concat back "new.txt"))))))

(ert-deftest gptel-agent-access-other-file-tools-use-single-call-approval ()
  (dolist (name '("PathInfo" "Mkdir" "Rmdir" "Insert" "Edit"))
    (gptel-agent-access-test--with-project
     (with-current-buffer parent (setq gptel-agent-use-project-instructions nil))
     (make-directory (concat back "empty"))
     (setq fsm (gptel-agent-instructions-test--request parent))
     (let* ((args (pcase name
                    ("PathInfo" (list :path (concat back "data.txt")))
                    ("Mkdir" (list :directory_path (concat back "new/nested")))
                    ("Rmdir" (list :directory_path (concat back "empty")))
                    ("Insert" (list :file_path (concat back "data.txt") :line_number -1 :new_str "added"))
                    ("Edit" (list :mode "replace" :file_path (concat back "data.txt")
                                  :old_str "OUTSIDE-CONTENT" :new_str "changed"))))
            (calls (gptel-agent-access-test--ask fsm name args)))
       (should calls)
       (gptel--accept-tool-calls calls)
       (should (eq (gptel-fsm-state fsm) 'WAIT))
       (should (equal (buffer-local-value 'default-directory parent) front))
       (pcase name
         ("Mkdir" (should (file-directory-p (concat back "new/nested"))))
         ("Rmdir" (should-not (file-exists-p (concat back "empty"))))
         ((or "Insert" "Edit")
          (should (string-match-p (if (equal name "Insert") "added" "changed")
                                  (with-temp-buffer
                                    (insert-file-contents (concat back "data.txt"))
                                    (buffer-string))))))))))

(ert-deftest gptel-agent-access-worker-approval-stays-in-worker-directory ()
  (gptel-agent-access-test--with-project
   (gptel-agent-workers-test--with-chat
    (with-current-buffer parent
      (cd front)
      (setq-local gptel-agent-preset '(:tools ("Read"))))
    (let* ((run (gptel-agent-workers-test--launch fsm))
           (child (gptel-agent--run-child-buffer run))
           (worker (gptel-agent--run-fsm run)))
      (gptel-agent-workers-test--tool worker "Read" '(:file_path "../backend/data.txt"))
      (with-current-buffer parent
        (let* ((overlay (cl-find-if (lambda (ov) (overlay-get ov 'gptel-tool))
                                    (overlays-in (point-min) (point-max))))
               (calls (overlay-get overlay 'gptel-tool)))
          (should (equal (gptel-tool-name (caar calls)) "Read"))
          (gptel--accept-tool-calls calls overlay)))
      (should (equal (buffer-local-value 'default-directory child) front))
      (should (equal (buffer-local-value 'default-directory parent) front))
      (should (string-match-p "OUTSIDE-CONTENT" (gptel-agent-instructions-test--payload worker)))
      (gptel-agent-workers-test--answer worker "Done")))))

(ert-deftest gptel-agent-access-discussion-approval-and-cancellation ()
  (dolist (approve '(nil t))
    (gptel-agent-access-test--with-project
     (gptel-agent-discussion-test--with-chat
      (with-current-buffer parent (cd front))
      (setq chat (gptel-agent-discussion-test--spawn fsm parent))
      (let* ((child (gptel-agent-discussion--chat-buffer chat))
             (worker (gptel-agent-discussion--chat-fsm chat))
             (path (concat back "new.txt")))
        (with-current-buffer child (setq gptel-agent-use-project-instructions nil))
        (push (gptel-get-tool "Write") (plist-get (gptel-fsm-info worker) :tools))
        (gptel-agent-workers-test--tool worker "Write" (list :file_path path :content "new"))
        (let* ((pending (car (gptel-agent-discussion--chat-pending chat)))
               (calls (gptel-agent-discussion--interaction-calls pending))
               (overlay (car (gptel-agent-discussion--interaction-overlays pending))))
          (should calls)
          (should-not (file-exists-p path))
          (with-current-buffer child
            (if approve (gptel--accept-tool-calls calls overlay)
              (gptel--reject-tool-calls calls overlay)))
          (should (eq (file-exists-p path) approve))
          (should (equal (buffer-local-value 'default-directory child) front))
          (should (equal (buffer-local-value 'default-directory parent) front))
          (unless approve
            (should (eq (gptel-fsm-state worker) 'ABRT))
            (gptel--accept-tool-calls calls)
            (should-not (file-exists-p path)))))))))

(provide 'gptel-agent-access-test)
;;; gptel-agent-access-test.el ends here
