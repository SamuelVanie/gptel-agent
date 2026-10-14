;;; gptel-ediff-prehook.el --- Review gptel file edits with Ediff -*- lexical-binding: t; -*-

;;; Commentary:
;; Load after gptel-agent.  N/P switch files, f selects a file, n/p switch
;; hunks, H adds feedback, R rejects, and q accepts once every file was seen.
;; Preparation and execution use exactly the same file changes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ediff)
(require 'seq)

(declare-function lsp-mode "lsp-mode")
(declare-function gptel-agent-edit-prepare "gptel-agent-edit")
(declare-function gptel-agent-edit-apply "gptel-agent-edit")
(defvar gptel-confirm-tool-calls)

(defvar smv/gptel-ediff-tool-names '("Edit" "Write" "Insert")
  "Tool names whose calls should be previewed via ediff.")

(defvar smv/gptel-ediff-reject-key "R"
  "Key bound in the ediff control buffer to reject the proposed change.")

(defvar smv/gptel-ediff-comment-key "H"
  "Key bound in ediff review buffers to add a region-specific rejection comment.")

(defvar-local smv/gptel-ediff-review-mode nil
  "Buffer-local flag enabling temporary gptel ediff review bindings.")


(defvar smv/gptel-ediff-inhibit-lsp nil
  "Non-nil while configuring temporary gptel Ediff preview buffers.")

(defun smv/gptel-ediff--inhibit-lsp-advice (original &rest args)
  "Skip ORIGINAL LSP startup while creating gptel preview buffers."
  (unless smv/gptel-ediff-inhibit-lsp
    (apply original args)))

(with-eval-after-load 'lsp-mode
  (advice-add 'lsp
              :around #'smv/gptel-ediff--inhibit-lsp-advice)
  (advice-add 'lsp-deferred
              :around #'smv/gptel-ediff--inhibit-lsp-advice))


(defun smv/gptel-ediff--set-mode-for (buffer path)
  "Set BUFFER's major mode based on PATH without retaining an LSP workspace."
  (when (and buffer path (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((bfn buffer-file-name)
            (smv/gptel-ediff-inhibit-lsp t))
        (setq buffer-file-name path)
        (unwind-protect
            (progn
              (ignore-errors
                (set-auto-mode t))
              (when (bound-and-true-p lsp-mode)
                (lsp-mode -1)))
          (setq buffer-file-name bfn))))))

(defun smv/gptel-ediff--select-main-window ()
  "Select a non-side, non-dedicated window in the current frame.
If none exists, split one off the frame root."
  (let ((current (selected-window)))
    (when (or (window-parameter current 'window-side)
              (window-dedicated-p current))
      (let ((target (seq-find (lambda (w)
                                (and (not (window-parameter w 'window-side))
                                     (not (window-dedicated-p w))))
                              (window-list nil 'no-mini))))
        (if target
            (select-window target)
          (select-window (split-window (frame-root-window) nil 'below)))))))

(defun smv/gptel-ediff--line-range (beg end)
  "Return a human-readable line or line range for BEG and END in current buffer."
  (let ((start (line-number-at-pos beg t))
        (finish (line-number-at-pos end t)))
    (if (= start finish)
        (format "line %d" start)
      (format "lines %d-%d" start finish))))

(defun smv/gptel-ediff--format-region-comments (comments)
  "Format region-specific rejection COMMENTS for the final refusal message."
  (concat
   "The user added comments to the proposed changes, should be taken into account before proceeding:\n\n"
   (mapconcat
    (lambda (entry)
      (let ((index (plist-get entry :index))
            (source (format "%s (%s)" (plist-get entry :path)
                            (plist-get entry :source)))
            (range (plist-get entry :range))
            (text (plist-get entry :text))
            (comment (plist-get entry :comment)))
        (format "%d. from %s, %s:\n%s\n\nComment:\n%s"
                index source range text comment)))
    comments
    "\n\n")))

(defun smv/gptel-ediff--buffer-local-set-key (key command)
  "Bind KEY to COMMAND in the current buffer without changing shared mode maps."
  (setq-local smv/gptel-ediff-review-mode t)
  (unless (local-variable-p 'minor-mode-overriding-map-alist)
    (setq-local minor-mode-overriding-map-alist
                (copy-tree minor-mode-overriding-map-alist)))
  (let ((map (or (cdr (assq 'smv/gptel-ediff-review-mode
                            minor-mode-overriding-map-alist))
                 (let ((new-map (make-sparse-keymap)))
                   (push (cons 'smv/gptel-ediff-review-mode new-map)
                         minor-mode-overriding-map-alist)
                   new-map))))
    (define-key map (kbd key) command)))

(defun smv/gptel-ediff--comment (path source comments)
  "Record feedback for PATH and SOURCE in the mutable COMMENTS list."
  (let* ((has-region (use-region-p))
         (beg (if has-region (region-beginning) (line-beginning-position)))
         (end (if has-region (region-end) (line-end-position)))
         (range (smv/gptel-ediff--line-range beg (if has-region (max beg (1- end)) end)))
         (text (buffer-substring-no-properties beg end))
         (comment (read-string (format "Comment for %s, %s: " path range))))
    (unless (string-empty-p (string-trim comment))
      (setcar comments
              (append (car comments)
                      (list (list :index (1+ (length (car comments)))
                                  :path path :source source :range range
                                  :text text :comment comment))))
      (message "Added feedback %d; use R to send it to the agent."
               (length (car comments))))))

(defun smv/gptel-ediff--review (changes)
  "Review prepared CHANGES as one navigable Ediff session.
Return nil on acceptance or a rejection reason.  No target files are written.
Quitting requires viewing every file, and feedback must be sent using R."
  (let* ((count (length changes))
         (pairs (make-vector count nil))
         (seen (make-vector count nil))
         (hunks (make-vector count -1))
         (comments (list nil))
         (saved-config (current-window-configuration))
         ;; Whitespace-only edits must remain visible, regardless of a user's
         ;; usual diff filters.  These bindings apply only to this review.
         (ediff-diff-options "")
         (index 0) decision reason control resume)
    (unwind-protect
        (progn
          (cl-loop for change in changes for i from 0 do
                   (let* ((path (plist-get change :path))
                          (original (generate-new-buffer
                                     (format "*gptel-original: %s*" path)))
                          (proposed (generate-new-buffer
                                     (format "*gptel-proposed: %s*" path))))
                     (aset pairs i (cons original proposed))
                     (dolist (entry (list (cons original :before) (cons proposed :after)))
                       (with-current-buffer (car entry)
                         (insert (or (plist-get change (cdr entry)) ""))
                         (smv/gptel-ediff--set-mode-for (current-buffer) path)
                         (setq buffer-read-only t)
                         (set-buffer-modified-p nil)))))
          (while (not decision)
            (let* ((change (nth index changes))
                   (path (plist-get change :path))
                   (pair (aref pairs index))
                   (next index)
                   (navigating nil)
                   (active nil)
                   (depth (recursion-depth))
                   (finish
                    (lambda ()
                      (when (buffer-live-p control)
                        (with-current-buffer control
                          (aset hunks index ediff-current-difference)
                          (ediff-really-quit nil)))))
                   (navigate
                    (lambda (target)
                      (setq next (mod target count) navigating t)
                      (funcall finish)))
                   (reject
                    (lambda ()
                      (interactive)
                      (setq reason
                            (if (car comments)
                                (smv/gptel-ediff--format-region-comments (car comments))
                              (read-string "Reject all proposed changes — reason: "))
                            decision 'rejected)
                      (funcall finish)))
                   (accept
                    (lambda ()
                      (interactive)
                      (cond
                       ((car comments)
                        (user-error "Feedback is pending; use R to send it to the agent"))
                       ((seq-position seen nil)
                        (message "Review this file before accepting the entire change set")
                        (funcall navigate (seq-position seen nil)))
                       (t (setq decision 'accepted) (funcall finish)))))
                   (select-file
                    (lambda ()
                      (interactive)
                      (let* ((choices
                              (cl-loop for c in changes for i from 0
                                       collect (cons (format "%d. %s%s" (1+ i)
                                                             (plist-get c :path)
                                                             (if (aref seen i) "" " [unseen]"))
                                                     i)))
                             (choice (completing-read "Review file: " choices nil t)))
                        (funcall navigate (cdr (assoc choice choices)))))))
              (setq resume (lambda ()
                             (when (and active (= (recursion-depth) (1+ depth)))
                               (exit-recursive-edit))))
              (smv/gptel-ediff--select-main-window)
              (setq control
                    (ediff-buffers
                     (car pair) (cdr pair)
                     (list (lambda ()
                             (setq-local ediff-brief-help-message-function
                                         (lambda ()
                                           (format " n/p: change   N/P: file   f: choose file\n q: accept all (visits unseen files first)\n %s: reject all   %s: comment in A/B   ?: Ediff help"
                                                   smv/gptel-ediff-reject-key
                                                   smv/gptel-ediff-comment-key)))))))
              (aset seen index t)
              (with-current-buffer control
                ;; This hook belongs only to this Ediff control buffer.  Quitting
                ;; an unrelated Ediff must never release our recursive edit.
                (add-hook 'ediff-after-quit-hook-internal resume nil t)
                (setq-local ediff-ignore-similar-regions nil)
                (setq-local header-line-format
                            (format "%d/%d %s%s"
                                    (1+ index) count path
                                    (cond ((null (plist-get change :before)) " [new file]")
                                          ((null (plist-get change :after)) " [delete file]")
                                          (t ""))))
                (smv/gptel-ediff--buffer-local-set-key "q" accept)
                (smv/gptel-ediff--buffer-local-set-key smv/gptel-ediff-reject-key reject)
                (smv/gptel-ediff--buffer-local-set-key "N"
                                                       (lambda () (interactive) (funcall navigate (1+ index))))
                (smv/gptel-ediff--buffer-local-set-key "P"
                                                       (lambda () (interactive) (funcall navigate (1- index))))
                (smv/gptel-ediff--buffer-local-set-key "f" select-file)
                (smv/gptel-ediff--buffer-local-set-key
                 smv/gptel-ediff-comment-key
                 (lambda () (interactive) (message "Select a region in A or B, then press H")))
                (when (> ediff-number-of-differences 0)
                  (ediff-jump-to-difference
                   (1+ (max 0 (min (aref hunks index) (1- ediff-number-of-differences)))))))
              (dolist (entry (list (cons (car pair) "original file")
                                   (cons (cdr pair) "proposed changes")))
                (let ((source (cdr entry)))
                  (with-current-buffer (car entry)
                    (setq-local header-line-format
                                (format "%d/%d %s — %s%s" (1+ index) count path source
                                        (cond ((null (plist-get change :before)) " [new file]")
                                              ((null (plist-get change :after)) " [delete file]")
                                              (t ""))))
                    (smv/gptel-ediff--buffer-local-set-key
                     smv/gptel-ediff-comment-key
                     (lambda () (interactive) (smv/gptel-ediff--comment path source comments))))))
              (setq active t)
              (unwind-protect (recursive-edit) (setq active nil))
              ;; An unexpected recursive exit must not count as acceptance.
              (when (or (buffer-live-p control) (and (not decision) (not navigating)))
                (setq decision 'rejected reason "Ediff review was interrupted."))
              (setq index next)))
          (when (eq decision 'accepted)
            ;; The preview is a snapshot, not an editor.  Ediff commands can
            ;; override read-only buffers, so verify both sides before applying.
            (cl-loop for change in changes for pair across pairs do
                     (dolist (entry (list (cons (car pair) :before) (cons (cdr pair) :after)))
                       (unless (and (buffer-live-p (car entry))
                                    (equal (with-current-buffer (car entry)
                                             (save-restriction
                                               (widen)
                                               (buffer-substring-no-properties (point-min) (point-max))))
                                           (or (plist-get change (cdr entry)) "")))
                         (error "Preview buffers were edited; ask the agent to regenerate the proposal"))))))
      (when (buffer-live-p control)
        (with-current-buffer control
          (remove-hook 'ediff-after-quit-hook-internal resume t)
          (ignore-errors (ediff-really-quit nil)))
        (when (buffer-live-p control) (kill-buffer control)))
      (cl-loop for pair across pairs do
               (dolist (buffer (list (car pair) (cdr pair)))
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer (set-buffer-modified-p nil))
                   (kill-buffer buffer))))
      (set-window-configuration saved-config))
    (unless (eq decision 'accepted) (or reason "Review cancelled."))))

(defun smv/gptel-ediff-tool-call (plist)
  "Review Edit/Write/Insert calls in PLIST using the shared file edit engine.
Apply the exact reviewed snapshots and return their result through gptel's
:result hook API.  Failed previews and aborted reviews always block the call."
  (when (and gptel-confirm-tool-calls
             (member (plist-get plist :name) smv/gptel-ediff-tool-names))
    (condition-case err
        (let* ((_ (require 'gptel-agent-edit))
               (changes (gptel-agent-edit-prepare (plist-get plist :name) (plist-get plist :args)))
               (rejection (smv/gptel-ediff--review changes)))
          (if rejection
              (list :block (concat "The user rejected the proposed changes. Adjust accordingly.\n"
                                   rejection))
            (list :confirm nil :result (gptel-agent-edit-apply changes))))
      (quit (list :block "The user cancelled Ediff review. No changes were applied."))
      (error (list :block (concat "Ediff review could not complete: " (error-message-string err)))))))

(add-hook 'gptel-pre-tool-call-functions #'smv/gptel-ediff-tool-call)

(provide 'gptel-ediff-prehook)
;;; gptel-ediff-prehook.el ends here
