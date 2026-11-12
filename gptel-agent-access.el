;;; gptel-agent-access.el --- Approval for outside paths -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Outside file operations use the ordinary tool approval callback.  Hold their
;; pre-tool hooks too: review hooks can read, stage and apply files themselves.
;; Approval belongs to one call unless the user explicitly grants outside
;; access to its owning conversation, until revoked or its directory changes.

;;; Code:

(require 'gptel-agent-instructions)

(defvar gptel-agent-access--approved nil
  "Exact canonical paths authorized while executing one outside operation.")

(defvar gptel-agent-access--remember nil
  "Non-nil while the user accepts calls and grants future outside access.")

(defvar-local gptel-agent-access--grant nil
  "Working directory and its canonical path when outside access was granted.
This conversation-only grant is neither inherited nor saved to disk.")

(defun gptel-agent-access--granted-p (&rest _)
  "Whether this conversation still has a valid outside-access grant.
Also refresh the grant after instruction boundary initialization."
  (when gptel-agent-access--grant
    (unless (and (equal (car gptel-agent-access--grant) default-directory)
                 (equal (cdr gptel-agent-access--grant)
                        (file-truename default-directory)))
      (setq gptel-agent-access--grant nil)))
  (and gptel-agent-access--grant t))

;;;###autoload
(defun gptel-agent-allow-outside-access (&optional calls ov)
  "Accept pending CALLS and allow future outside paths in their owning chats.
OV is the native tool approval overlay.  Only a valid outside file-call
approval can grant access.  Each grant lasts until its chat buffer closes,
its working directory changes, or `gptel-agent-revoke-outside-access' runs.
Ordinary tool confirmations and file reviews still apply."
  (interactive (pcase-let ((`(,calls . ,ov)
                           (get-char-property-and-overlay (point) 'gptel-tool)))
                 (list calls ov)))
  (unless calls (user-error "No pending tool calls at point"))
  (let ((gptel-agent-access--remember t))
    (gptel--accept-tool-calls calls ov)))

;;;###autoload
(defun gptel-agent-revoke-outside-access ()
  "Require outside file-call approvals again in the current conversation."
  (interactive)
  (setq gptel-agent-access--grant nil)
  (message "Outside file operations in %s require approval again" (buffer-name)))

(defun gptel-agent-access--check-paths (paths)
  "Require approval for outside PATHS before preparation or application."
  (unless (bound-and-true-p gptel-agent--result-file-scope)
    (gptel-agent-instructions--initialize)
    (dolist (path paths)
      (setq path (file-truename path))
      (unless (or (gptel-agent-instructions--inside-p path gptel-agent-instructions--boundary)
                  (gptel-agent-access--granted-p)
                  (member path gptel-agent-access--approved))
        (error "Outside file operation requires approval; submit a fresh tool call: %s" path)))))

(defun gptel-agent-access--paths (name args)
  "Return canonical local paths for file tool NAME and ARGS."
  (dolist (key '(:path :file_path :root_directory :directory_path))
    (when-let* ((path (plist-get args key)))
      (when (and (stringp path) (file-remote-p path))
        (error "File tools do not support remote targets: %s" path))))
  (mapcar
   (lambda (path)
     (unless (and (stringp path) (not (string-empty-p path)))
       (error "File tool requires a nonempty target path"))
     (let ((target (let (file-name-handler-alist)
                     (file-truename (expand-file-name path)))))
       (when (file-remote-p target)
         (error "File tools do not support remote targets: %s" target))
       target))
   (pcase name
     ((or "Edit" "Insert" "Write") (gptel-agent-edit-targets name args))
     ("Read"
      (let ((path (plist-get args :file_path)))
        ;; Shared instruction symlinks retain their containing local scope.
        (list (if (and gptel-agent-use-project-instructions (stringp path)
                       (equal (file-name-nondirectory path) "AGENTS.md"))
                  (file-name-directory (expand-file-name path)) path))))
     ("Glob" (list (or (plist-get args :root_directory) ".")))
     ((or "Mkdir" "Rmdir") (list (plist-get args :directory_path)))
     (_ (list (plist-get args :path))))))

(defun gptel-agent-access--outside-p (name args)
  "Whether NAME/ARGS requires outside access approval in this conversation."
  (when (and (member name gptel-agent-instructions--file-tools)
             (not (bound-and-true-p gptel-agent--result-file-scope)))
    (gptel-agent-instructions--initialize)
    ;; Validate targets even when outside access has already been granted.
    (let ((paths (gptel-agent-access--paths name args)))
      (and (not (gptel-agent-access--granted-p))
           (cl-some (lambda (path)
                      (not (gptel-agent-instructions--inside-p
                            path gptel-agent-instructions--boundary)))
                    paths)))))

(defun gptel-agent-access--scan (fsm)
  "Mark outside calls in FSM before instruction discovery and review hooks."
  (let ((info (gptel-fsm-info fsm)))
    (with-current-buffer (plist-get info :buffer)
      (dolist (call (plist-get info :tool-use))
        (unless (plist-get call :result)
          (condition-case err
              (when (gptel-agent-access--outside-p
                     (plist-get call :name) (plist-get call :args))
                (dolist (key '(:confirm :agent-access-pending :agent-access-used))
                  (unless (plist-member call key) (nconc call (list key nil))))
                (setf (plist-get call :confirm) t
                      (plist-get call :agent-access-pending) t)
                ;; Absolute arguments keep the normal preview unambiguous,
                ;; including when a worker's approval appears in its parent.
                (let ((args (copy-tree (plist-get call :args) t)))
                  (dolist (key '(:path :file_path :root_directory :directory_path))
                    (when-let* ((path (plist-get args key))
                                ((and (stringp path) (not (string-empty-p path)))))
                      (setf (plist-get args key) (expand-file-name path))))
                  (gptel--inject-tool-call (plist-get info :backend) (plist-get info :data)
                                           call (list :args args))
                  (setf (plist-get call :args) args)))
            (error
             (gptel--process-tool-call
              fsm (cl-find (plist-get call :name) (plist-get info :tools)
                           :key #'gptel-tool-name :test #'equal)
              call (concat "Error: " (error-message-string err))))))))))

(defun gptel-agent-access--pre-tool (original fsm)
  "Hold outside calls' hooks while ORIGINAL prepares FSM."
  (gptel-agent-access--scan fsm)
  (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
    (let* ((hooks (cl-mapcan (lambda (hook)
                               (if (eq hook t) (copy-sequence (default-value 'gptel-pre-tool-call-functions))
                                 (list hook)))
                             gptel-pre-tool-call-functions))
           (gptel-pre-tool-call-functions
            (mapcar (lambda (hook)
                      (lambda (call)
                        (unless (gptel-agent-access--outside-p
                                 (plist-get call :name) (plist-get call :args))
                          (funcall hook call))))
                    hooks)))
      (funcall original fsm))))

(defun gptel-agent-access--run-hooks (fsm name args)
  "Run deferred hooks for approved NAME/ARGS in FSM; return an early result."
  (let* ((info (gptel-fsm-info fsm))
         (input (list :name name :args args :buffer (buffer-name)
                      :backend (plist-get info :backend) :model (plist-get info :model)))
         result)
    (run-hook-wrapped
     'gptel-pre-tool-call-functions
     (lambda (hook)
       (let ((reply (funcall hook (copy-tree input t))))
         (when (or (plist-get reply :stop) (plist-get reply :block))
           (error "%s" (or (plist-get reply :stop-reason)
                           (plist-get reply :block) "Operation stopped by a tool hook")))
         (when (or (and (plist-get reply :name) (not (equal name (plist-get reply :name))))
                   (and (plist-get reply :args) (not (equal args (plist-get reply :args)))))
           (error "Approved operation changed in a tool hook; submit a fresh call"))
         (setq result (plist-get reply :result)))))
    result))

(defun gptel-agent-access--approval (fsm pending call)
  "Wrap PENDING's tool with a single-use approval for FSM's CALL."
  (let* ((tool (car pending)) (copy (gptel--copy-tool tool))
         (name (gptel-tool-name tool))
         (args (copy-tree (cadr pending) t))
         (values (gptel--map-tool-args tool args))
         (owner (plist-get (gptel-fsm-info fsm) :buffer))
         (directory (buffer-local-value 'default-directory owner))
         (source (file-truename directory))
         (paths (with-current-buffer owner (gptel-agent-access--paths name args)))
         used)
    (setf (gptel-tool-function copy)
          (lambda (&rest supplied)
            (let* ((callback (and (gptel-tool-async tool) (pop supplied)))
                   (result
                    (progn
                      (unless (and (not used) (not (plist-get call :agent-access-used))
                                   (gptel-agent-instructions--live-p fsm)
                                   (eq (gptel-fsm-state fsm) 'TOOL)
                                   (memq call (plist-get (gptel-fsm-info fsm) :tool-use))
                                   (not (plist-get call :result)))
                        (error "This file-operation approval is no longer pending"))
                      (setq used t)
                      (setf (plist-get call :agent-access-used) t)
                      (with-current-buffer owner
                        (unless (and (equal supplied values) (equal default-directory directory)
                                     (equal (file-truename directory) source)
                                     (equal (gptel-agent-access--paths name args) paths))
                          (error "Approved operation or target changed; submit a fresh call"))
                        ;; Remember only after validating the live approval,
                        ;; but before instruction discovery can defer the call.
                        (when gptel-agent-access--remember
                          (setq gptel-agent-access--grant (cons directory source)))
                        (let ((gptel-agent-access--approved paths)
                              (gptel-agent-instructions--request fsm))
                          (when (gptel-agent-instructions--enabled-p fsm)
                            (let ((dirs (gptel-agent-instructions--tool-targets name args)))
                              (gptel-agent-instructions--select fsm dirs)
                              (gptel-agent-instructions--records dirs)
                              (when (member name gptel-agent-instructions--mutating-tools)
                                (gptel-agent-instructions--guard-directories fsm dirs))))
                          (or (gptel-agent-access--run-hooks fsm name args)
                              (if callback
                                  (progn (apply (gptel-tool-function tool) callback supplied)
                                         :agent-access-async)
                                (apply (gptel-tool-function tool) supplied))))))))
              (if callback
                  (unless (eq result :agent-access-async) (funcall callback result))
                result))))
    (cons copy (cdr pending))))

(defun gptel-agent-access--tool-use (original fsm)
  "Require per-call outside approval when ORIGINAL dispatches FSM."
  (gptel-agent-access--scan fsm)
  (let* ((info (gptel-fsm-info fsm)) (callback (plist-get info :callback)))
    (unwind-protect
        (progn
          (setf (plist-get info :callback)
                (lambda (response state &rest extra)
                  (when (eq (car-safe response) 'tool-call)
                    (setq response
                          (cons 'tool-call
                                (mapcar
                                 (lambda (pending)
                                   (let ((call (cl-find-if
                                                (lambda (item)
                                                  (and (plist-get item :agent-access-pending)
                                                       (equal (plist-get item :name) (gptel-tool-name (car pending)))
                                                       (eq (plist-get item :args) (cadr pending))))
                                                (plist-get info :tool-use))))
                                     (if call (gptel-agent-access--approval fsm pending call) pending)))
                                 (cdr response)))))
                  (apply callback response state extra)))
          (funcall original fsm))
      (setf (plist-get info :callback) callback))))

(defun gptel-agent-access--display (original calls info &optional minibuffer)
  "Add a conversation grant action when ORIGINAL displays outside CALLS.
INFO belongs to the requesting chat, even for workers shown in a parent.
MINIBUFFER requests the native minibuffer approval interface."
  (if (not (cl-some (lambda (call) (plist-get call :agent-access-pending))
                   (plist-get info :tool-use)))
      (funcall original calls info minibuffer)
    (let* ((position (plist-get info :position))
           (tracking (plist-get info :tracking-marker))
           (owner (plist-get info :buffer))
           (label (format "Run tools + allow all outside paths for %s"
                          (buffer-name owner))))
      (with-current-buffer (marker-buffer position)
        (if (or minibuffer buffer-read-only
                (get-char-property (max (point-min) (1- (or tracking position))) 'read-only))
            (pcase (car (read-multiple-choice
                        (format "%s wants to run %s. "
                                (gptel-backend-name (plist-get info :backend))
                                (mapconcat (lambda (call) (gptel-tool-name (car call))) calls ", "))
                        `((?y "Run tools once") (?a ,label)
                          (?n "Cancel (resumable)") (?i "Inspect or edit"))))
              (?y (gptel--accept-tool-calls calls))
              (?a (gptel-agent-allow-outside-access calls))
              (?n (gptel--reject-tool-calls))
              (?i (gptel--inspect-tool-calls calls info)))
          (let* ((prompt (funcall original calls info minibuffer))
                 (ov (cl-find-if (lambda (ov) (memq prompt (overlay-get ov 'prompt)))
                                 (overlays-in (point-min) (point-max)))))
            (when (and (overlayp prompt) ov)
              (overlay-put
               prompt 'before-string
               (concat (overlay-get prompt 'before-string)
                       (buttonize label
                                  (lambda (_)
                                    (gptel-agent-allow-outside-access
                                     (overlay-get ov 'gptel-tool) ov)))
                       " " (propertize "C-c C-a" 'face 'help-key-binding) "\n\n")))
            prompt))))))

(define-key gptel-tool-call-actions-map (kbd "C-c C-a") #'gptel-agent-allow-outside-access)
(advice-add 'gptel-agent-instructions--initialize :after #'gptel-agent-access--granted-p)
(advice-add 'gptel--display-tool-calls :around #'gptel-agent-access--display)
(advice-add 'gptel--handle-pre-tool :around #'gptel-agent-access--pre-tool '((depth . -10)))
(advice-add 'gptel--handle-tool-use :around #'gptel-agent-access--tool-use '((depth . -10)))

(provide 'gptel-agent-access)
;;; gptel-agent-access.el ends here
