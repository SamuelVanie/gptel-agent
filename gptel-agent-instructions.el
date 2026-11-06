;;; gptel-agent-instructions.el --- Scoped project instructions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Conversation-owned AGENTS.md discovery.  Prompt transforms capture request
;; configuration, never instruction text.  The WAIT adapter replaces only the
;; generated system payload before *each* transport invocation.  The TPRE adapter
;; runs before review hooks, which can themselves stage/apply edits; TOOL covers
;; API requests without TPRE.  Execution
;; wrappers recheck after delayed approval.  No tool arguments are replayed.
;; These two adapters are needed because gptel has no per-send transform hook
;; and its pre-tool hooks neither receive the FSM nor short-circuit later hooks.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'gptel)
(require 'gptel-agent-edit)

(defvar gptel-agent--result-file-scope)

(defcustom gptel-agent-use-project-instructions t
  "Whether gptel conversations discover scoped AGENTS.md instructions.
This applies regardless of the selected preset or available tools.  The
global value supplies the default for all conversations; a buffer-local
value overrides it for that conversation.  Changes take effect at the next
outgoing call; already submitted calls retain their snapshot."
  :type 'boolean :group 'gptel-agent)

(defcustom gptel-agent-project-instructions-max-bytes (* 128 1024)
  "Maximum size of one AGENTS.md file.  Larger files are reported, not truncated."
  :type 'natnum :group 'gptel-agent)

(defvar-local gptel-agent-instructions--registry nil
  "Private alist of discovered paths and immutable content records.
Records contain :path, :scope, :text, :version, and optionally :error.")
(defvar-local gptel-agent-instructions--boundary nil
  "Canonical discovery boundary owned by this conversation.")
(defvar-local gptel-agent-instructions--working-directory nil
  "Canonical working directory used to establish the current boundary.")
(defvar-local gptel-agent-instructions--targets nil
  "Directories most recently targeted by this conversation.")
(defvar-local gptel-agent-instructions--sent nil
  "Owned snapshot of instruction records supplied by the latest call.")
(defvar gptel-agent-instructions--request nil
  "Dynamically bound FSM while preparing or executing its file tools.")

(defun gptel-agent-instructions--info (fsm)
  "Return FSM's info with stable places for instruction metadata.
`plist-put' may prepend a new property, so always save the resulting head."
  (let ((info (gptel-fsm-info fsm)))
    (dolist (key '(:agent-instructions-context
                  :agent-instructions-directory :agent-instructions-sent-directory
                  :agent-instructions-started :agent-instructions-targets
                  :agent-instructions-sent :agent-instructions-rendering
                  :agent-instructions-wrapped :agent-instructions-tool-batch))
      (unless (plist-member info key) (setq info (plist-put info key nil))))
    (setf (gptel-fsm-info fsm) info)))

(defun gptel-agent-instructions--configuration (&optional fsm)
  "Return the effective instruction preference for FSM or the current buffer."
  (let ((owner (and fsm (plist-get (gptel-fsm-info fsm) :buffer))))
    (with-current-buffer (or owner (current-buffer))
      (and gptel-agent-use-project-instructions t))))

(defun gptel-agent-instructions--enabled-p (fsm)
  "Whether FSM should manage instructions in its owning conversation."
  (and (not (plist-get (gptel-fsm-info fsm) :agent-instructions-skip))
       (not (bound-and-true-p gptel-agent--result-file-scope))
       (gptel-agent-instructions--configuration fsm)))

(defun gptel-agent-instructions--transform (fsm)
  "Capture explicit file context for FSM.
Do not change the base system prompt, including function/list prompts."
  (let ((info (gptel-agent-instructions--info fsm)))
    (with-current-buffer (if (bufferp (plist-get info :data))
                             (plist-get info :data) (current-buffer))
      (when gptel-use-context
        (setf (plist-get info :agent-instructions-context)
              (delq nil
                    (mapcar (lambda (entry)
                              (cond ((buffer-live-p (car entry))
                                     (buffer-file-name (car entry)))
                                    ((stringp (car entry)) (car entry))))
                            gptel-context)))))))

(defun gptel-agent-instructions--skip (fsm)
  "Exclude the compaction request FSM from managed instruction delivery."
  (setf (plist-get (gptel-fsm-info fsm) :agent-instructions-skip) t))

(defun gptel-agent-instructions--inside-p (path directory)
  "Whether canonical PATH is DIRECTORY or lies within it."
  (or (equal (directory-file-name path) (directory-file-name directory))
      (string-prefix-p (file-name-as-directory directory) path)))

(defun gptel-agent-instructions--initialize (&optional fsm)
  "Refresh this conversation's boundary after a working-directory change.
Also reset FSM's discovery targets when supplied.  Keep its last sent
snapshot and rendering until the next send, so pending edits still require
fresh instructions and the old managed prompt block can be removed."
  (when (file-remote-p default-directory)
    (error "Project instructions do not support remote working directories"))
  (let ((directory (file-name-as-directory (file-truename default-directory))))
    (unless (and gptel-agent-instructions--boundary
                 (equal directory gptel-agent-instructions--working-directory))
      (let ((boundary
             (file-name-as-directory
              (file-truename (if-let* ((project (project-current nil default-directory)))
                                 (project-root project) default-directory)))))
        (setq gptel-agent-instructions--boundary boundary
              gptel-agent-instructions--working-directory directory
              gptel-agent-instructions--registry nil
              gptel-agent-instructions--targets nil
              gptel-agent-instructions--sent nil)))
    (when fsm
      (let ((info (gptel-agent-instructions--info fsm)))
        (unless (equal directory (plist-get info :agent-instructions-directory))
          (setf (plist-get info :agent-instructions-directory) directory
                (plist-get info :agent-instructions-started) nil
                (plist-get info :agent-instructions-targets) nil))))))

(defun gptel-agent-instructions--directory (path &optional directory)
  "Resolve PATH to a canonical scope, treating it as a DIRECTORY if non-nil.
Internal symlinks work; remote paths and escapes from the boundary fail closed."
  (unless (stringp path) (error "Instruction target must be a file path"))
  (when (file-remote-p path) (error "Project instructions do not support remote targets: %s" path))
  (gptel-agent-instructions--initialize)
  (let* ((path (file-truename (expand-file-name path)))
         (dir (file-name-as-directory (if directory path (file-name-directory path)))))
    (unless (and (gptel-agent-instructions--inside-p path gptel-agent-instructions--boundary)
                 (gptel-agent-instructions--inside-p dir gptel-agent-instructions--boundary))
      (error "Instruction target is outside this conversation's boundary (%s): %s"
             gptel-agent-instructions--boundary path))
    dir))

(defun gptel-agent-instructions--load (path)
  "Refresh instruction PATH from disk, replacing its private registry record.
Missing files remove old records.  Read failures replace stale text with an
explicit error; no partial contents are ever treated as instructions."
  (let ((old (cdr (assoc path gptel-agent-instructions--registry))) record)
    (condition-case err
        (when (or (file-exists-p path) (file-symlink-p path))
          (when (file-symlink-p path)
            (error "Symlinked instruction files are not supported; use a regular AGENTS.md"))
          (unless (gptel-agent-instructions--inside-p
                   (file-truename path) gptel-agent-instructions--boundary)
            (error "Instruction symlink escapes discovery boundary"))
          (unless (and (file-regular-p path) (file-readable-p path))
            (error "Instruction file is not a readable regular file"))
          (when (> (file-attribute-size (file-attributes path))
                   gptel-agent-project-instructions-max-bytes)
            (error "Instruction file exceeds %d bytes" gptel-agent-project-instructions-max-bytes))
          (let ((text (with-temp-buffer
                        ;; Bound the read too, in case the file grew after stat.
                        (insert-file-contents path nil 0 (1+ gptel-agent-project-instructions-max-bytes))
                        (buffer-string))))
            (when (> (string-bytes text) gptel-agent-project-instructions-max-bytes)
              (error "Instruction file exceeds size limit"))
            (setq record (list :path path :scope (file-name-directory path)
                               :text text :version (secure-hash 'sha256 text)))))
      (error
       (let ((reason (error-message-string err)))
         (setq record (list :path path :scope (file-name-directory path)
                            :error reason :version (list 'error reason)))
         (unless (equal (plist-get old :error) reason)
           (display-warning 'gptel-agent (format "%s: %s" path reason) :warning)))))
    (setq gptel-agent-instructions--registry
          (cons (cons path record)
                (assoc-delete-all path gptel-agent-instructions--registry)))
    ;; Removed files are not discovered sources, but will be checked again
    ;; whenever their directory chain is selected.
    (unless record
      (setq gptel-agent-instructions--registry
            (assoc-delete-all path gptel-agent-instructions--registry)))
    record))

(defun gptel-agent-instructions--discover (directory)
  "Refresh DIRECTORY's chain and return records in root-to-leaf order."
  (let ((dir directory) paths)
    (while (and dir (gptel-agent-instructions--inside-p dir gptel-agent-instructions--boundary))
      (push (expand-file-name "AGENTS.md" dir) paths)
      (setq dir (unless (equal dir gptel-agent-instructions--boundary)
                  (file-name-directory (directory-file-name dir)))))
    (delq nil (mapcar #'gptel-agent-instructions--load paths))))

(defun gptel-agent-instructions--records (directories)
  "Refresh DIRECTORIES and return their deduplicated, ordered instruction chains."
  (let (records)
    (dolist (dir (sort (copy-sequence directories) #'string-lessp))
      (dolist (record (gptel-agent-instructions--discover dir))
        (unless (cl-find (plist-get record :path) records
                         :key (lambda (r) (plist-get r :path)) :test #'equal)
          (push record records))))
    (nreverse records)))

(defun gptel-agent-instructions--format (records)
  "Format scoped RECORDS as one replaceable instruction block."
  (when records
    (concat
     "<gptel-project-instructions>\n"
     "Apply each source only to files within its directory scope. "
     "More specific scopes override conflicting parent rules; other parent rules remain. "
     "Explicit user instructions take precedence over these project instructions.\n\n"
     (mapconcat (lambda (record)
                  (format "Source: %s\nDirectory scope: %s\n%s"
                          (plist-get record :path) (plist-get record :scope)
                          (or (plist-get record :text)
                              (concat "UNAVAILABLE: " (plist-get record :error)
                                      ". Resolve this before modifying this scope."))))
                records "\n\n")
     "\n</gptel-project-instructions>")))

(defun gptel-agent-instructions--restore (data rendering)
  "Remove only managed RENDERING from DATA, returning DATA's updated plist.
Used before every send and before saving a retained worker's conversation."
  (when rendering
    (let ((key (plist-get rendering :key)))
      (if (eq key :messages)
          (setf (plist-get data :messages)
                (vconcat (cl-remove (plist-get rendering :message)
                                    (append (plist-get data :messages) nil) :test #'equal :count 1)))
        (if (plist-get rendering :present)
            (setq data (plist-put data key (copy-tree (plist-get rendering :base) t)))
          (cl-remf data key)))))
  data)

(defun gptel-agent-instructions--render (info text)
  "Replace INFO's generated payload block with TEXT, preserving tool history."
  (let* ((data (gptel-agent-instructions--restore
                (copy-tree (plist-get info :data) t)
                (plist-get info :agent-instructions-rendering)))
         (backend (plist-get info :backend)) rendering)
    ;; Stateful provider context could implicitly retain obsolete blocks, even
    ;; when this call is disabling a previously supplied managed block.
    (when (and (or text (plist-get info :agent-instructions-rendering))
               (cl-some (lambda (key) (plist-get data key))
                        '(:previous_response_id :conversation :cachedContent)))
      (error "Project instructions require full-context requests; remove provider continuation parameters"))
    (when text
      (let ((gptel-model (plist-get info :model)))
        (when (gptel--model-capable-p 'nosystem)
          (error "Project instructions require a model that supports system messages")))
      (let* ((key (cond ((plist-member data :input) :instructions)
                        ((plist-member data :contents) :systemInstruction)
                        ((and (fboundp 'gptel-anthropic-p) (gptel-anthropic-p backend)) :system)
                        ((plist-member data :messages) :messages)
                        (t (error "Unsupported project-instruction payload for %s" backend))))
             (base (plist-get data key)))
        (setq rendering (list :key key :present (and (plist-member data key) t)
                              :base (unless (eq key :messages) (copy-tree base t))))
        (pcase key
          (:instructions (setf (plist-get data key) (if base (concat base "\n\n" text) text)))
          (:system (setf (plist-get data key)
                         (vconcat (if (stringp base) (list (list :type "text" :text base)) base)
                                  (list (list :type "text" :text text)))))
          (:systemInstruction
           (let ((value (copy-tree base t)))
             (setf (plist-get value :parts)
                   (vconcat (plist-get value :parts) (list (list :text text))))
             (setf (plist-get data key) value)))
          (:messages
           (let ((message (list :role "system" :content text)) (messages (append base nil)) prefix)
             (while (member (plist-get (car messages) :role) '("system" "developer"))
               (push (pop messages) prefix))
             (setf (plist-get rendering :message) message
                   (plist-get data key) (vconcat (nreverse prefix) (list message) messages)))))
        (when (eq key :instructions) (setf (plist-get data :store) :json-false))))
    (setf (plist-get info :data) data
          (plist-get info :agent-instructions-rendering) rendering)))

(defun gptel-agent-instructions--live-p (fsm)
  "Whether FSM still has a live owner and has not been cancelled or completed."
  (and (buffer-live-p (plist-get (gptel-fsm-info fsm) :buffer))
       (not (memq (gptel-fsm-state fsm) '(ABRT DONE ERRS)))))

(defun gptel-agent-instructions--before-send (original fsm)
  "Rebuild FSM's managed payload immediately before ORIGINAL sends it."
  (when (gptel-agent-instructions--live-p fsm)
    (let ((info (gptel-agent-instructions--info fsm)))
      (with-current-buffer (plist-get info :buffer)
        (let (records)
          (when (gptel-agent-instructions--enabled-p fsm)
            (gptel-agent-instructions--initialize fsm)
            (unless (plist-get info :agent-instructions-started)
              (dolist (path (mapcar #'car gptel-agent-instructions--registry))
                (gptel-agent-instructions--load path))
              ;; Startup discovery is independent of the targets selected for
              ;; delivery (explicit context may point into a sibling scope).
              (gptel-agent-instructions--discover
               (gptel-agent-instructions--directory default-directory t))
              (let ((context (or (plist-get info :agent-instructions-context)
                                 (when buffer-file-name (list buffer-file-name)))))
                (setf (plist-get info :agent-instructions-targets)
                      (or (mapcar #'gptel-agent-instructions--directory context)
                          gptel-agent-instructions--targets
                          (list (gptel-agent-instructions--directory default-directory t))))))
            (setq records (gptel-agent-instructions--records
                           (or (plist-get info :agent-instructions-targets)
                               (list (gptel-agent-instructions--directory default-directory t))))))
          (gptel-agent-instructions--render info (gptel-agent-instructions--format records))
          (setf (plist-get info :agent-instructions-started) t
                (plist-get info :agent-instructions-sent-directory)
                gptel-agent-instructions--working-directory
                (plist-get info :agent-instructions-sent) (copy-tree records t))
          (setq gptel-agent-instructions--sent (copy-tree records t))
          (gptel-agent-instructions--wrap-tools fsm)
          (funcall original fsm))))))

(defun gptel-agent-instructions--tool-targets (name args)
  "Return canonical directories targeted by file tool NAME and ARGS."
  (gptel-agent-instructions--initialize)
  ;; Check before the edit parser invokes file-directory-p/file-truename,
  ;; either of which can open a TRAMP connection for a remote path.
  (dolist (key '(:path :file_path :root_directory :directory_path))
    (let ((path (plist-get args key)))
      (when (and (stringp path) (file-remote-p path))
        (error "Project instructions do not support remote targets: %s" path))))
  (if (member name '("Glob" "Grep" "PathInfo" "Mkdir" "Rmdir"))
      (let ((path (pcase name
                    ("Glob" (or (plist-get args :root_directory) "."))
                    ((or "Mkdir" "Rmdir") (plist-get args :directory_path))
                    (_ (plist-get args :path)))))
        (list (gptel-agent-instructions--directory
               path (or (member name '("Glob" "Mkdir" "Rmdir")) (file-directory-p path)))))
    (delete-dups
     (mapcar #'gptel-agent-instructions--directory
             (pcase name
               ("Read" (list (plist-get args :file_path)))
               ((or "Edit" "Insert" "Write") (gptel-agent-edit-targets name args)))))))

(defun gptel-agent-instructions--signature (records directory)
  "Return supplied RECORDS' path/version pairs applicable to DIRECTORY."
  (sort (cl-loop for record in records
                 when (gptel-agent-instructions--inside-p directory (plist-get record :scope))
                 collect (cons (plist-get record :path) (plist-get record :version)))
        (lambda (a b) (string-lessp (car a) (car b)))))

(defun gptel-agent-instructions--check (fsm directories)
  "Return steering text if DIRECTORIES require a new decision by FSM's model."
  (let* ((info (gptel-fsm-info fsm))
         (records (gptel-agent-instructions--records directories))
         (sent (plist-get info :agent-instructions-sent)))
    (when (or (and directories (plist-get info :agent-instructions-sent-directory)
                   (not (equal (plist-get info :agent-instructions-sent-directory)
                               gptel-agent-instructions--working-directory)))
              (cl-some (lambda (r) (plist-get r :error)) records)
              (cl-some (lambda (dir)
                         (not (equal (gptel-agent-instructions--signature records dir)
                                     (gptel-agent-instructions--signature sent dir)))) directories))
      (format (concat "Deferred before staging or writing. Working directory or project instructions changed, or instructions were not "
                      "supplied for targets: %s. Applicable sources: %s. "
                      "Read the updated scoped instructions on the next call, reconsider, and propose "
                      "a new operation. The old arguments will not be replayed.")
              (string-join directories ", ")
              (if records (mapconcat (lambda (r) (plist-get r :path)) records ", ")
                "none (previous sources were removed)")))))

(defun gptel-agent-instructions--select (fsm directories)
  "Select DIRECTORIES for FSM's next call and its owner's next turn."
  (when directories
    (setq gptel-agent-instructions--targets (copy-sequence directories))
    (setf (plist-get (gptel-fsm-info fsm) :agent-instructions-targets)
          (copy-sequence directories))))

(defun gptel-agent-instructions--read-ack (args)
  "Return an acknowledgement for a managed Read of AGENTS.md in ARGS."
  (let* ((path (expand-file-name (plist-get args :file_path)))
         (target (file-truename path)))
    (when (or (equal (file-name-nondirectory path) "AGENTS.md")
              (equal (file-name-nondirectory target) "AGENTS.md"))
      (let* ((dir (gptel-agent-instructions--directory
                   (file-name-directory (if (equal (file-name-nondirectory path) "AGENTS.md") path target)) t))
             (source (expand-file-name "AGENTS.md" dir))
             (record (gptel-agent-instructions--load source)))
        (cond ((null record) (format "Instruction source no longer exists: %s" source))
              ((plist-get record :error) (format "Cannot load %s: %s" source (plist-get record :error)))
              (t (format "Loaded project instruction source %s (scope %s). Contents are supplied in the next scoped instruction block; inspect with gptel-agent-describe-project-instructions."
                         source dir)))))))

(defconst gptel-agent-instructions--file-tools '("Read" "Glob" "Grep" "PathInfo" "Edit" "Insert" "Write" "Mkdir" "Rmdir"))
(defconst gptel-agent-instructions--mutating-tools '("Edit" "Insert" "Write" "Mkdir" "Rmdir"))

(defun gptel-agent-instructions--pre-tool (original fsm)
  "Discover all FSM targets and defer stale operations before ORIGINAL's hooks."
  (when (gptel-agent-instructions--live-p fsm)
    (let ((info (gptel-agent-instructions--info fsm)))
      (setf (plist-get info :agent-instructions-tool-batch) (plist-get info :tool-use))
      (with-current-buffer (plist-get info :buffer)
        (when (gptel-agent-instructions--enabled-p fsm)
          (gptel-agent-instructions--initialize fsm)
          (let (directories mutation-directories calls failures)
            (dolist (call (plist-get info :tool-use))
              (when (and (not (plist-get call :result))
                         (member (plist-get call :name) gptel-agent-instructions--file-tools))
                (push call calls)
                (condition-case err
                    (let ((dirs (gptel-agent-instructions--tool-targets
                                 (plist-get call :name) (plist-get call :args))))
                      (setq directories (append directories dirs))
                      (when (member (plist-get call :name) gptel-agent-instructions--mutating-tools)
                        (setq mutation-directories (append mutation-directories dirs))))
                  (error (push (cons call (error-message-string err)) failures)))))
            (setq directories (delete-dups directories))
            (gptel-agent-instructions--select fsm directories)
            (gptel-agent-instructions--records directories)
            (let ((steering (gptel-agent-instructions--check fsm (delete-dups mutation-directories))))
              (dolist (call (nreverse calls))
                (let* ((name (plist-get call :name))
                       (result (or (cdr (assq call failures))
                                   (cond ((equal name "Read")
                                          (gptel-agent-instructions--read-ack (plist-get call :args)))
                                         ((member name gptel-agent-instructions--mutating-tools) steering)))))
                  (when result
                    (when (assq call failures)
                      (setf (plist-get call :error) t))
                    (gptel--process-tool-call
                     fsm (cl-find name (plist-get info :tools) :key #'gptel-tool-name :test #'equal)
                     call result)))))))
        (let ((gptel-agent-instructions--request fsm))
          (funcall original fsm))))))

(defun gptel-agent-instructions--tool-use (original fsm)
  "Check FSM's tool batch before ORIGINAL if no TPRE handler checked it."
  (let ((info (gptel-fsm-info fsm)))
    (if (eq (plist-get info :tool-use) (plist-get info :agent-instructions-tool-batch))
        (funcall original fsm)
      (gptel-agent-instructions--pre-tool original fsm))))

(defun gptel-agent-instructions--guard-directories (fsm directories)
  "Recheck DIRECTORIES before staging/writing; signal when FSM must reconsider."
  (when (gptel-agent-instructions--enabled-p fsm)
    (gptel-agent-instructions--initialize fsm)
    (gptel-agent-instructions--select
     fsm (delete-dups (append (plist-get (gptel-fsm-info fsm) :agent-instructions-targets)
                              directories)))
    (when-let* ((reason (gptel-agent-instructions--check fsm directories)))
      (error "%s" reason))))

(defun gptel-agent-instructions--before-prepare (name args)
  "Guard NAME/ARGS even when another pre-tool hook changed the proposal."
  (when-let* ((fsm gptel-agent-instructions--request))
    (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
      (when (gptel-agent-instructions--enabled-p fsm)
        (gptel-agent-instructions--guard-directories
         fsm (gptel-agent-instructions--tool-targets name args))))))

(defun gptel-agent-instructions--before-apply (changes &rest _)
  "Check saved instruction versions again before applying CHANGES."
  (when-let* ((fsm gptel-agent-instructions--request))
    (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
      (when (gptel-agent-instructions--enabled-p fsm)
        (gptel-agent-instructions--guard-directories
         fsm (delete-dups (mapcar (lambda (c) (gptel-agent-instructions--directory (plist-get c :path)))
                                  changes)))))))

(defun gptel-agent-instructions--wrap-tools (fsm)
  "Install execution guards owned by FSM, including after delayed approvals."
  (let ((info (gptel-agent-instructions--info fsm)))
    (when (gptel-agent-instructions--enabled-p fsm)
      (setf (plist-get info :tools)
            (mapcar
             (lambda (tool)
               (if (or (not (member (gptel-tool-name tool) gptel-agent-instructions--file-tools))
                       (memq (gptel-tool-function tool) (plist-get info :agent-instructions-wrapped))) tool
                 (let* ((copy (gptel--copy-tool tool))
                        (function (gptel-tool-function tool)) (name (gptel-tool-name tool)))
                   (setf (gptel-tool-function copy)
                         (lambda (&rest values)
                           (if (not (gptel-agent-instructions--live-p fsm))
                               "Cancelled: instruction request no longer has a live owner."
                             (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
                               (let ((gptel-agent-instructions--request fsm) args result)
                                 (cl-mapc (lambda (spec value)
                                            (setq args (plist-put args (intern (concat ":" (plist-get spec :name))) value)))
                                          (gptel-tool-args tool) values)
                                 (when (gptel-agent-instructions--enabled-p fsm)
                                   (gptel-agent-instructions--initialize fsm)
                                   (let ((dirs (gptel-agent-instructions--tool-targets name args)))
                                     (gptel-agent-instructions--select
                                      fsm (delete-dups
                                           (append (plist-get (gptel-fsm-info fsm) :agent-instructions-targets) dirs)))
                                     (setq result (gptel-agent-instructions--check fsm dirs))
                                     (unless (member name gptel-agent-instructions--mutating-tools)
                                       (setq result (and (equal name "Read")
                                                         (gptel-agent-instructions--read-ack args))))))
                                 (or result (apply function values)))))))
                   (push (gptel-tool-function copy) (plist-get info :agent-instructions-wrapped))
                   copy)))
             (plist-get info :tools))))))

;;;###autoload
(defun gptel-agent-toggle-project-instructions (&optional arg)
  "Toggle project instructions locally.
With ARG, enable if positive and disable otherwise."
  (interactive "P")
  (setq-local gptel-agent-use-project-instructions
              (if arg (> (prefix-numeric-value arg) 0)
                (not (gptel-agent-instructions--configuration))))
  (message "Project instructions %s for this conversation at the next model call"
           (if gptel-agent-use-project-instructions "enabled" "disabled")))

;;;###autoload
(defun gptel-agent-describe-project-instructions ()
  "Show this conversation's discovered sources and latest delivered snapshot."
  (interactive)
  (let ((enabled (gptel-agent-instructions--configuration))
        (boundary gptel-agent-instructions--boundary)
        (records (copy-tree gptel-agent-instructions--registry t))
        (sent (copy-tree gptel-agent-instructions--sent t))
        (owner (buffer-name)))
    (with-help-window "*gptel project instructions*"
      (princ (format "Conversation: %s\nEnabled: %s\nDiscovery boundary: %s\n\nDiscovered sources:\n"
                     owner (if enabled "yes" "no") (or boundary "not discovered")))
      (dolist (entry records)
        (princ (format "\n%s\nScope: %s\nVersion: %s\n%s\n"
                       (car entry) (plist-get (cdr entry) :scope)
                       (plist-get (cdr entry) :version)
                       (or (plist-get (cdr entry) :text) (plist-get (cdr entry) :error)))))
      (princ "\nLatest request actually received:\n")
      (princ (or (gptel-agent-instructions--format sent) "No managed instructions.\n")))))

(add-hook 'gptel-prompt-transform-functions #'gptel-agent-instructions--transform t)
;; Also capture final @preset/context settings when callers supply their own
;; transform list.  This runs in the prompt buffer and never alters its text.
(advice-add 'gptel--realize-query :before #'gptel-agent-instructions--transform)
(advice-add 'gptel--handle-wait :around #'gptel-agent-instructions--before-send)
(advice-add 'gptel--handle-pre-tool :around #'gptel-agent-instructions--pre-tool)
(advice-add 'gptel--handle-tool-use :around #'gptel-agent-instructions--tool-use)
(advice-add 'gptel-agent-edit-prepare :before #'gptel-agent-instructions--before-prepare)
(advice-add 'gptel-agent-edit-apply :before #'gptel-agent-instructions--before-apply)

(provide 'gptel-agent-instructions)
;;; gptel-agent-instructions.el ends here
