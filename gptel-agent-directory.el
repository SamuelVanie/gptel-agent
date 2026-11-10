;;; gptel-agent-directory.el --- Approved working directory changes -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; ChangeDirectory uses gptel's ordinary approval UI, but always asks.  Each
;; approval owns one request, call, source directory and canonical destination.

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'subr-x)
(require 'gptel-agent-path)
(require 'gptel-agent-instructions)

(declare-function gptel-agent--confirm-overlay "gptel-agent-tools")

(defun gptel-agent-directory--resolve (directory)
  "Validate DIRECTORY and return its canonical local directory name."
  (unless (and (stringp directory) (not (string-empty-p directory)))
    (error "ChangeDirectory requires a nonempty directory"))
  ;; Check before any filesystem probe could establish a TRAMP connection.
  (when (file-remote-p (expand-file-name directory))
    (error "ChangeDirectory supports local directories only"))
  (file-name-as-directory
   (file-truename (gptel-agent-path-working-directory directory))))

(defun gptel-agent-directory--pre-tool (original fsm)
  "Defer mixed directory-change batches before ORIGINAL can run file hooks."
  (let* ((info (gptel-fsm-info fsm))
         (calls (plist-get info :tool-use)))
    (if (and (length> calls 1)
             (cl-find "ChangeDirectory" calls
                      :key (lambda (call) (plist-get call :name)) :test #'equal))
        (dolist (call calls)
          (unless (plist-get call :result)
            (gptel--process-tool-call
             fsm (cl-find (plist-get call :name) (plist-get info :tools)
                          :key #'gptel-tool-name :test #'equal)
             call
             "Deferred: Call ChangeDirectory alone and wait for approval and updated project instructions before requesting other tools. No operations in this batch were run.")))
      (funcall original fsm))))

(defun gptel-agent-directory--prepare (fsm call tool)
  "Bind TOOL to FSM and CALL's approved source and destination."
  (let* ((info (gptel-fsm-info fsm))
         (owner (plist-get info :buffer))
         (args (plist-get call :args)))
    (with-current-buffer owner
      (let* ((source default-directory)
             (source-target (file-truename source))
             (destination (gptel-agent-directory--resolve (plist-get args :directory)))
             (reason (plist-get args :reason))
             (copy (gptel--copy-tool tool))
             used)
        (unless (and (stringp reason) (not (string-empty-p (string-trim reason))))
          (error "ChangeDirectory requires a reason for the user's approval"))
        ;; Show exactly the absolute destination that approval will authorize.
        (setq args (plist-put (copy-sequence args) :directory destination))
        (gptel--inject-tool-call (plist-get info :backend) (plist-get info :data)
                                call (list :args args))
        (setf (plist-get call :args) args
              (gptel-tool-function copy)
              (lambda (directory explanation)
                (unless (and (not used)
                             (gptel-agent-instructions--live-p fsm)
                             (eq (gptel-fsm-state fsm) 'TOOL)
                             (memq call (plist-get (gptel-fsm-info fsm) :tool-use))
                             (not (plist-get call :result)))
                  (error "This directory-change approval is no longer pending"))
                (setq used t)
                (with-current-buffer owner
                  (unless (and (equal default-directory source)
                               (equal (file-truename source) source-target))
                    (error "Working directory changed while approval was pending; request ChangeDirectory again"))
                  (unless (and (equal directory destination) (equal explanation reason)
                               (equal (gptel-agent-directory--resolve directory) destination))
                    (error "Directory-change destination or reason changed; request ChangeDirectory again"))
                  (cd destination)
                  (format "Working directory changed from %s to %s. Subsequent tools use this directory. Review the updated project instructions before continuing."
                          source default-directory))))
        (setf (plist-get info :tools) (cons copy (remq tool (plist-get info :tools)))
              (plist-get call :agent-directory-prepared) t)))))

(defun gptel-agent-directory--tool-use (original fsm)
  "Require approval for directory changes before ORIGINAL dispatches FSM."
  (gptel-agent-directory--pre-tool
   (lambda (request)
     (let* ((info (gptel-fsm-info request))
            (call (cl-find "ChangeDirectory" (plist-get info :tool-use)
                           :key (lambda (item) (plist-get item :name)) :test #'equal))
            (tool (cl-find "ChangeDirectory" (plist-get info :tools)
                           :key #'gptel-tool-name :test #'equal)))
       (when (and call tool (not (plist-get call :result)))
         ;; Keep the call's cons identity: plist-put may prepend absent keys.
         (dolist (key '(:confirm :agent-directory-prepared :error))
           (unless (plist-member call key) (nconc call (list key nil))))
         (condition-case err
             (progn
               (unless (plist-get call :agent-directory-prepared)
                 (gptel-agent-directory--prepare request call tool))
               ;; Per-call confirmation overrides global and skill grants.
               (setf (plist-get call :confirm) t))
           (error
            (setf (plist-get call :error) t)
            (gptel--process-tool-call request tool call
                                      (concat "Error: " (error-message-string err))))))
       (funcall original request)))
   fsm))

(defun gptel-agent-directory--preview (values info)
  "Show the directory change in VALUES for request INFO."
  (let ((from (point))
        (owner (plist-get info :buffer)))
    (insert (format "Change working directory for %s\nFrom: %s\nTo: %s\nReason: %s\n\n"
                    (if (buffer-live-p owner) (buffer-name owner) "agent")
                    (if (buffer-live-p owner)
                        (buffer-local-value 'default-directory owner) default-directory)
                    (car values) (cadr values)))
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent-directory--reject (original &optional calls overlay)
  "Report refused directory changes in CALLS after ORIGINAL clears OVERLAY."
  (prog1 (funcall original calls overlay)
    ;; Managed discussions already reject and abort the owning request.
    (unless (and (overlayp overlay)
                 (overlay-get overlay 'gptel-agent-discussion-interaction))
      (dolist (call calls)
        (when (equal (gptel-tool-name (car call)) "ChangeDirectory")
          (funcall (nth 2 call)
                   "Directory change denied by the user. The working directory is unchanged. Do not access that project without permission."))))))

(gptel-make-tool
 :name "ChangeDirectory"
 :function (lambda (&rest _)
             (error "ChangeDirectory requires a pending request-owned approval"))
 :description "Request the user's permission to change this agent's working directory.
Use when work requires reading or writing outside the current project boundary,
or moving to another project. Supply an existing local directory and explain why.
Always waits for explicit approval, even when other tool confirmations are off.
Call this tool alone. After approval, project instructions refresh on the next
model call; reconsider the operation using those instructions and the new directory.
Only this agent/chat moves; other chats, workers and running processes keep their
directories. A refusal does not authorize access. Do not use Bash or Eval to
bypass a boundary error. To return, request ChangeDirectory with the old directory."
 :args '((:name "directory" :type string
          :description "Existing local directory. Absolute or relative to this agent's current directory.")
         (:name "reason" :type string
          :description "Explain to the user why work in this directory is needed."))
 :category "gptel-agent" :confirm t :include t)

(setf (alist-get "ChangeDirectory" gptel--tool-preview-alist nil nil #'equal)
      (list #'gptel-agent-directory--preview))
(advice-add 'gptel--handle-pre-tool :around #'gptel-agent-directory--pre-tool)
(advice-add 'gptel--handle-tool-use :around #'gptel-agent-directory--tool-use)
(advice-add 'gptel--reject-tool-calls :around #'gptel-agent-directory--reject)

(provide 'gptel-agent-directory)
;;; gptel-agent-directory.el ends here
