;;; gptel-agent-review-aside.el --- Private chats about file proposals -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Fork the conversation into an ordinary gptel buffer for review questions.
;; The fork owns its transcript, request state and settings.  It never sends
;; feedback to the author or changes a file's review decision.

;;; Code:

(require 'gptel-agent-review)

(declare-function gptel-agent--copy-parent-request-config "gptel-agent-tools")
(declare-function gptel-agent-instructions--transform "gptel-agent-instructions")
(declare-function smv/gptel-ediff-open "gptel-ediff-prehook")
(defvar gptel-agent-instructions--targets)

(defvar-local gptel-agent-review-aside--queue nil)
(defvar-local gptel-agent-review-aside--path nil)
(defvar-local gptel-agent-review-aside--fsm nil)
(defvar-local gptel-agent-review-aside--draft-start nil)

(defvar gptel-agent-review-aside-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'gptel-agent-review-aside-send)
    (define-key map (kbd "C-c C-r") #'gptel-agent-review-aside-return)
    (define-key map (kbd "C-c C-q") #'quit-window)
    (define-key map (kbd "C-c C-k") #'gptel-abort)
    map))

(define-minor-mode gptel-agent-review-aside-mode
  "An independent review chat.  Send follow-ups with `gptel-send'.
\\<gptel-agent-review-aside-mode-map>\\[gptel-agent-review-aside-return] returns to the file's review;
\\[quit-window] hides the aside.  Its transcript is never sent to the main chat."
  :lighter " Review aside")

(defun gptel-agent-review-aside-send ()
  "Send the aside's prompt, retaining it for retry if startup fails."
  (interactive)
  (when (gptel-agent-review--live-p gptel-agent-review-aside--fsm)
    (user-error "The aside is still answering; wait or abort its request first"))
  (condition-case err
      (gptel-send)
    ((error quit)
     (when (gptel-agent-review--live-p gptel-agent-review-aside--fsm)
       (gptel-abort (current-buffer))
       ;; Setup can fail before a transport exists for `gptel-abort' to stop.
       (setf (gptel-fsm-state gptel-agent-review-aside--fsm) 'ERRS))
     (signal (car err) (cdr err)))))

(defun gptel-agent-review-aside--draft-p ()
  "Whether the aside contains an unsent follow-up."
  (let* ((info (and gptel-agent-review-aside--fsm
                    (gptel-fsm-info gptel-agent-review-aside--fsm)))
         (start (or (plist-get info :tracking-marker)
                    (plist-get info :position)
                    gptel-agent-review-aside--draft-start)))
    (and (markerp start) (eq (marker-buffer start) (current-buffer))
         (gptel--trim-prefixes (buffer-substring-no-properties start (point-max))))))

(defun gptel-agent-review-aside-return ()
  "Return to the file's live Ediff without deciding its review."
  (interactive)
  (let* ((queue gptel-agent-review-aside--queue)
         (owner (and queue (gptel-agent-review--queue-owner queue)))
         (file (and queue
                    (cl-find gptel-agent-review-aside--path
                             (gptel-agent-review--queue-files queue)
                             :key (lambda (entry) (plist-get entry :path)) :test #'equal))))
    (unless (and file (buffer-live-p owner)
                 (eq queue (buffer-local-value 'gptel-agent-review--queue owner)))
      (user-error "This file review has finished"))
    (quit-window)
    (smv/gptel-ediff-open queue file)))

(defun gptel-agent-review-aside--isolate (fsm)
  "Keep FSM an explanation-only request, independent of provider sessions."
  (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
    (when (gptel-agent-review--live-p gptel-agent-review-aside--fsm)
      (user-error "The aside is still answering; wait or abort its request first"))
    (setq gptel-agent-review-aside--fsm fsm))
  (setq-local gptel-use-tools nil gptel-tools nil)
  ;; Strip provider-level tools and continuation IDs too, after all settings
  ;; have been merged.  Never mutate the parent's backend/model parameters.
  (let ((handlers (copy-tree (gptel-fsm-handlers fsm))))
    (push #'gptel-agent-review-aside--prepare (alist-get 'WAIT handlers))
    (setf (gptel-fsm-handlers fsm) handlers)))

(defun gptel-agent-review-aside--prepare (fsm)
  "Remove tools and remote conversation continuation from FSM's payload."
  (let* ((info (gptel-fsm-info fsm))
         (data (copy-tree (plist-get info :data) t)))
    (dolist (key '(:tools :tool_choice :toolChoice :tool_config :toolConfig
                  :conversation :previous_response_id :cachedContent))
      (cl-remf data key))
    (setf (plist-get info :tools) nil
          (plist-get info :data) data)))

(defun gptel-agent-review-aside--kill ()
  "Abort only this aside's request when its buffer is killed."
  (gptel-abort (current-buffer)))

(defun gptel-agent-review-aside--create (queue file)
  "Create an independent chat for QUEUE's FILE, copying the owner's context."
  (let* ((owner (gptel-agent-review--queue-owner queue))
         (path (plist-get file :path))
         (fsm (buffer-local-value 'gptel--fsm-last owner))
         (info (and fsm (gptel-fsm-info fsm)))
         (snapshot
          (with-current-buffer owner
            (save-excursion
              (goto-char (point-max))
              (let ((mark-active nil)) (gptel--create-prompt-buffer (point))))))
         (buffer (generate-new-buffer
                  (format "*Review aside: %s — %s*" (file-name-nondirectory path)
                          (buffer-name owner))))
         ready)
    (unwind-protect
        (with-current-buffer buffer
          ;; The source prompt is already flattened (including Org topics).
          ;; Do not reinterpret its headings or file snapshots as Org branches
          ;; or GPTEL_* configuration properties in this independent chat.
          (funcall (if (fboundp 'markdown-mode) #'markdown-mode #'text-mode))
          (gptel-agent--copy-parent-request-config owner buffer)
          ;; The author's request may use a one-shot preset.  Its captured settings
          ;; include the resolved system prompt and the actual model/backend.
          (dolist (entry (plist-get info :discussion-config))
            (set (make-local-variable (car entry)) (copy-tree (cdr entry))))
          (setq default-directory (buffer-local-value 'default-directory owner))
          (setq-local gptel-agent-instructions--targets
                      (delete-dups
                       (cons (file-name-directory path)
                             (copy-sequence
                              (or (plist-get info :agent-instructions-targets)
                                  (buffer-local-value 'gptel-agent-instructions--targets owner))))))
          (when (functionp gptel-system-prompt)
            (let* ((prompt gptel-system-prompt)
                   (parts (with-current-buffer owner (gptel--parse-directive prompt 'raw))))
              (setq-local gptel-system-prompt (if (cdr parts) parts (car parts)))))
          ;; Reuse context and scoped instructions, without rerunning agent
          ;; presets, tool localizers or parent-specific transforms/hooks.
          (setq-local gptel-tools nil gptel-use-tools nil
                      gptel-prompt-transform-functions
                      '(gptel--transform-add-context
                        gptel-agent-instructions--transform
                        gptel-agent-review-aside--isolate)
                      gptel-pre-tool-call-functions nil
                      gptel-post-tool-call-functions nil)
          (insert-buffer-substring snapshot)
          (goto-char (point-max))
          (insert "\n\n")
          (set-text-properties (- (point) 2) (point) nil)
          (gptel-mode 1)
          (setq-local gptel-agent--child-chat-p t)
          (gptel-agent-review-aside-mode 1)
          (setq-local gptel-agent-review-aside--draft-start (point-marker))
          (setq-local header-line-format
                      (append (if (listp header-line-format) header-line-format
                                (list header-line-format))
                              '("  Aside: C-c RET send · C-c C-r review · C-c C-q hide")))
          (setq-local gptel-agent-review-aside--queue queue
                      gptel-agent-review-aside--path path)
          (add-hook 'kill-buffer-hook #'gptel-agent-review-aside--kill nil t)
          (setq ready t)
          buffer)
      (kill-buffer snapshot)
      (unless ready (kill-buffer buffer)))))

(defun gptel-agent-review-aside (queue file question &optional context)
  "Ask QUESTION about FILE in QUEUE in an independent chat.
FILE is an immutable snapshot of the revision the user was viewing.
CONTEXT identifies the selected side and lines.  Empty QUESTION opens the
existing aside without sending.  Further questions reuse its own history."
  (let* ((path (plist-get file :path))
         (buffer (cdr (assoc path (gptel-agent-review--queue-asides queue)))))
    (unless (buffer-live-p buffer)
      (unless (buffer-live-p (gptel-agent-review--queue-owner queue))
        (user-error "The original conversation has been closed"))
      (setq buffer (gptel-agent-review-aside--create queue file))
      (setf (alist-get path (gptel-agent-review--queue-asides queue) nil nil #'equal) buffer))
    (let ((window (display-buffer-in-side-window
                   buffer '((side . bottom) (slot . 1) (window-height . 0.35)))))
      (when (window-live-p window) (select-window window)))
    (with-current-buffer buffer
      (unless (string-empty-p (string-trim question))
        (when (gptel-agent-review--live-p gptel-agent-review-aside--fsm)
          (user-error "The aside is still answering; wait or abort its request first"))
        (when (gptel-agent-review-aside--draft-p)
          (user-error "The aside has an unsent draft; send it with C-c RET or clear it first"))
        (goto-char (point-max))
        (let ((start (point)))
          (insert
           "\n\n" (gptel-prompt-prefix-string)
           (format (concat "Review aside about %s (revision %d).\n"
                           "This is a separate explanatory discussion. Answer the question; "
                           "do not modify files or treat it as review feedback or a refusal. "
                           "The review decision is unchanged. The snapshots below are the "
                           "revision being discussed, even if work continues elsewhere.\n\n"
                           "Original file%s:\n%s\n\nProposed file%s:\n%s\n\n%sQuestion: %s")
                   path (plist-get file :revision)
                   (if (null (plist-get file :before)) " (absent)" "")
                   (or (plist-get file :before) "")
                   (if (null (plist-get file :after)) " (deleted)" "")
                   (or (plist-get file :after) "")
                   (if context (concat "Selected context" context "\n\n") "") question))
          ;; An unfinished streamed parent response may have sticky role props.
          (set-text-properties start (point) nil))
        (let ((mark-active nil)) (gptel-agent-review-aside-send))))
    buffer))

(provide 'gptel-agent-review-aside)
;;; gptel-agent-review-aside.el ends here
