;;; gptel-agent-access.el --- Per-call approval for outside paths -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Outside file operations use the ordinary tool approval callback.  Hold their
;; pre-tool hooks too: review hooks can read, stage and apply files themselves.
;; Approval belongs to one call, never to a directory or a later request.

;;; Code:

(require 'gptel-agent-instructions)

(defvar gptel-agent-access--approved nil
  "Exact canonical paths authorized while executing one outside operation.")

(defun gptel-agent-access--check-paths (paths)
  "Require approval for outside PATHS before preparation or application."
  (unless (bound-and-true-p gptel-agent--result-file-scope)
    (gptel-agent-instructions--initialize)
    (dolist (path paths)
      (setq path (file-truename path))
      (unless (or (gptel-agent-instructions--inside-p path gptel-agent-instructions--boundary)
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
  "Whether NAME/ARGS targets a path outside this conversation's boundary."
  (when (and (member name gptel-agent-instructions--file-tools)
             (not (bound-and-true-p gptel-agent--result-file-scope)))
    (gptel-agent-instructions--initialize)
    (cl-some (lambda (path)
               (not (gptel-agent-instructions--inside-p
                     path gptel-agent-instructions--boundary)))
             (gptel-agent-access--paths name args))))

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

(advice-add 'gptel--handle-pre-tool :around #'gptel-agent-access--pre-tool '((depth . -10)))
(advice-add 'gptel--handle-tool-use :around #'gptel-agent-access--tool-use '((depth . -10)))

(provide 'gptel-agent-access)
;;; gptel-agent-access.el ends here
