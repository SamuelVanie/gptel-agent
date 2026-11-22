;;; gptel-agent-session-tools-test.el --- Durable tool identities -*- lexical-binding: t; -*-

(require 'gptel-agent-session-manager-test)

(ert-deftest gptel-agent-session-tools-standalone-keeps-selected-category ()
  (gptel-agent-session-test--with-store
    (let* ((gptel--known-tools (copy-tree gptel--known-tools))
           (intended (gptel-make-tool :name "session_same_name" :category "session-intended"
                                     :description "Selected tool" :args nil :confirm t
                                     :function (lambda () "intended")))
           (_other (gptel-make-tool :name "session_same_name" :category "session-other"
                                   :description "Other tool" :args nil
                                   :function (lambda () "other")))
           (source (gptel-agent-session-test--buffer " *tool-source*" "Prompt"))
           (file (expand-file-name "tools.eld" gptel-agent-session-directory)))
      (with-current-buffer source (setq-local gptel-tools (list intended)))
      (gptel-agent-session-export-buffer source file)
      (let* ((restored (gptel-agent-session-import-buffer file))
             (tool (car (buffer-local-value 'gptel-tools restored))))
        (should (eq tool intended))
        (should (gptel-tool-confirm tool))
        (should (equal (funcall (gptel-tool-function tool)) "intended"))))))

(ert-deftest gptel-agent-session-tools-default-category-and-legacy-records ()
  (gptel-agent-session-test--with-store
    (let* ((gptel--known-tools (copy-tree gptel--known-tools))
           (tool (gptel-make-tool :name "session_uncategorized" :description "Default category"
                                 :args nil :function #'ignore))
           (source (gptel-agent-session-test--buffer " *default-tool-source*" "Prompt")))
      (with-current-buffer source (setq-local gptel-tools (list tool)))
      (let ((record (gptel-agent-session-buffer-record source)))
        (should (equal (plist-get record :tools) '(("misc" "session_uncategorized"))))
        (should (eq tool (car (buffer-local-value
                              'gptel-tools (gptel-agent-session-buffer-from-record record)))))
        ;; Existing archives carry bare tool names rather than qualified paths.
        (setf (plist-get record :tools) '("session_uncategorized"))
        (should (eq tool (car (buffer-local-value
                              'gptel-tools (gptel-agent-session-buffer-from-record record)))))))))

(ert-deftest gptel-agent-session-tools-missing-category-never-substitutes ()
  (gptel-agent-session-test--with-store
    (let* ((gptel--known-tools (copy-tree gptel--known-tools))
           (intended (gptel-make-tool :name "session_same_name" :category "session-intended"
                                     :description "Selected tool" :args nil :function #'ignore))
           (_other (gptel-make-tool :name "session_same_name" :category "session-other"
                                   :description "Other tool" :args nil :function #'ignore))
           (source (gptel-agent-session-test--buffer " *missing-tool-source*" "Prompt"))
           warnings)
      (with-current-buffer source (setq-local gptel-tools (list intended)))
      (let ((record (gptel-agent-session-buffer-record source)))
        (setq gptel--known-tools (assoc-delete-all "session-intended" gptel--known-tools))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type text &rest _) (push text warnings))))
          (let ((restored (gptel-agent-session-buffer-from-record record)))
            (should-not (buffer-local-value 'gptel-tools restored))))
        (should (= 1 (length warnings)))
        (should (string-match-p "Missing saved tool.*session-intended.*session_same_name"
                                (car warnings)))))))

(ert-deftest gptel-agent-session-tools-invalid-references-are-rejected ()
  (gptel-agent-session-test--with-store
    (let* ((source (gptel-agent-session-test--buffer " *invalid-tool-source*" "Prompt"))
           (record (gptel-agent-session-buffer-record source)))
      (dolist (reference '(nil ("gptel-agent") ("gptel-agent" "Read" "extra")
                          ("gptel-agent" . "Read") ("gptel-agent" Read)
                          ["gptel-agent" "Read"]))
        (should-error (gptel-agent-session-buffer-from-record
                       (plist-put (copy-sequence record) :tools (list reference))))))))

(ert-deftest gptel-agent-session-tools-family-restores-each-category ()
  (gptel-agent-session-test--with-store
    (let* ((gptel--known-tools (copy-tree gptel--known-tools))
           (first (gptel-make-tool :name "session_same_name" :category "session-first"
                                  :description "First tool" :args nil :function #'ignore))
           (second (gptel-make-tool :name "session_same_name" :category "session-second"
                                   :description "Second tool" :args nil :function #'ignore))
           (parent (gptel-agent-session-test--buffer " *tool-parent*" "Parent"))
           (child (gptel-agent-session-manager-test--child parent " *tool-child*")) id)
      (with-current-buffer parent (setq-local gptel-tools (list first)))
      (with-current-buffer child (setq-local gptel-tools (list second)))
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (setq id (gptel-agent-session-manager-test--id parent))
      (gptel-agent-session-test--forget child parent)
      (setq parent (gptel-agent-session-restore id)
            child (gptel-agent-discussion--chat-buffer
                   (car (buffer-local-value 'gptel-agent-discussion--children parent))))
      (should (eq first (car (buffer-local-value 'gptel-tools parent))))
      (should (eq second (car (buffer-local-value 'gptel-tools child)))))))

(ert-deftest gptel-agent-session-tools-restored-send-discovers-workers-in-workspace ()
  (gptel-agent-session-test--with-store
    (let* ((gptel-agent-dirs '("agents/"))
           (gptel-agent-skill-dirs nil)
           (gptel-agent-use-project-instructions nil)
           (gptel--known-presets (copy-tree gptel--known-presets))
           gptel-agent--registered-agents gptel-agent--additional-subagents
           gptel-agent--agents gptel-agent--agent-files gptel-agent--file-cache
           (alpha (expand-file-name "alpha/" gptel-agent-session-directory))
           (bravo (expand-file-name "bravo/" gptel-agent-session-directory))
           (empty (expand-file-name "empty/" gptel-agent-session-directory))
           (file (expand-file-name "chat.eld" gptel-agent-session-directory))
           (source (gptel-agent-session-test--buffer " *workspace-source*" "Review the task."))
           record request)
      (make-directory empty)
      (dolist (project (list alpha bravo))
        (let ((name (file-name-nondirectory (directory-file-name project))))
          (make-directory (expand-file-name "agents/" project) t)
          (with-temp-file (expand-file-name "agents/worker.md" project)
            (insert "---\nname: " name "-worker\ndescription: " name "\n---\n" name))))
      (with-current-buffer source
        (setq default-directory alpha)
        (gptel-agent-update)
        (setq-local gptel-tools (list (gptel-get-tool "Agent"))
                    gptel-use-tools t gptel-use-curl t gptel-stream nil
                    gptel-system-prompt "Saved custom prompt")
        (setq record (gptel-agent-session-buffer-record source))
        (gptel-agent-session-export-buffer source file))
      (with-temp-buffer
        (setq default-directory bravo)
        (gptel-agent-update))
      (dolist (kind '(standalone external empty))
        (let ((chat (if (eq kind 'standalone)
                        (gptel-agent-session-import-buffer file)
                      (gptel-agent-session-buffer-from-record record t))))
          (with-current-buffer chat
            (should (eq gptel-agent-session-managed-externally (not (eq kind 'standalone))))
            (should (equal gptel-system-prompt "Saved custom prompt"))
            (should (equal default-directory alpha))
            ;; A host can set the node workspace after importing its record.
            (when (eq kind 'empty) (setq default-directory empty))
            (setq request nil)
            (cl-letf (((symbol-function 'gptel-curl-get-response)
                       (lambda (fsm) (setq request fsm)))
                      ((symbol-function 'gptel--url-get-response)
                       (lambda (&rest _) (ert-fail "Unexpected URL transport"))))
              (gptel-send))
            (should request)
            (let* ((tools (plist-get (gptel-fsm-info request) :tools))
                   (agent (cl-find "Agent" tools :key #'gptel-tool-name :test #'equal)))
              (if (eq kind 'empty)
                  (should-not agent)
                (should (equal (plist-get (car (gptel-tool-args agent)) :enum)
                               ["alpha-worker"]))))
            ;; First use in a restored chat must leave other callers' registry alone.
            (should (equal (mapcar #'car gptel-agent--agents) '("bravo-worker")))))))))

(provide 'gptel-agent-session-tools-test)
;;; gptel-agent-session-tools-test.el ends here
