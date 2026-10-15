;;; gptel-ediff-prehook.el --- Review gptel file edits with Ediff -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded automatically by gptel-agent-tools.  File tools stage proposals in
;; a nonblocking review queue.  Ediff runs only when the user opens a proposal.
;; The original side stays fixed while the proposed side updates in place.
;; H submits comments immediately; R refuses this file, C cancels all files.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ediff)
(require 'seq)
(require 'gptel-agent-review)

(declare-function lsp-mode "lsp-mode")
(declare-function gptel-agent-edit-prepare "gptel-agent-edit")
(declare-function gptel-agent-edit-apply "gptel-agent-edit")
(defvar gptel-confirm-tool-calls)

(defvar smv/gptel-ediff-tool-names '("Edit" "Write" "Insert")
  "File tools previewed via Ediff; Edit also supports mixed operation batches.")

(defvar smv/gptel-ediff-reject-key "R"
  "Key bound in the ediff control buffer to reject the proposed change.")

(defvar smv/gptel-ediff-comment-key "H"
  "Key bound in Ediff review buffers to send a region-specific comment.")

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


(cl-defstruct (smv/gptel-ediff--view (:constructor smv/gptel-ediff--make-view))
  queue file original proposed control revision before after config closing updating)

(defun smv/gptel-ediff--fill (buffer text)
  "Replace preview BUFFER with TEXT, retaining its approximate line position."
  (with-current-buffer buffer
    (let ((line (line-number-at-pos)) (column (current-column)) (inhibit-read-only t))
      (save-restriction
        (widen)
        ;; Preserve window starts, marks and unchanged regions while updating.
        (replace-region-contents (point-min) (point-max) (lambda () (or text "")))
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column column))
      (set-buffer-modified-p nil))))

(defun smv/gptel-ediff--validate-view (view)
  "Verify VIEW still contains the displayed snapshots, not manual edits."
  (dolist (pair (list (cons (smv/gptel-ediff--view-original view) (smv/gptel-ediff--view-before view))
                      (cons (smv/gptel-ediff--view-proposed view) (smv/gptel-ediff--view-after view))))
    (unless (and (buffer-live-p (car pair))
                 (equal (with-current-buffer (car pair)
                          (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))
                        (or (cdr pair) "")))
      (user-error "Preview buffers were edited; close with Q and reopen the file"))))

(defun smv/gptel-ediff-validate (queue)
  "Ensure QUEUE's open view shows the current revision without manual edits."
  (when-let* ((view (gptel-agent-review--queue-view queue)))
    (smv/gptel-ediff--validate-view view)
    (unless (= (smv/gptel-ediff--view-revision view)
               (plist-get (smv/gptel-ediff--view-file view) :revision))
      (user-error "The file has newer changes; refresh or reopen its review"))))

(defun smv/gptel-ediff--headers (view)
  "Display the file revision and decision throughout VIEW."
  (let* ((file (smv/gptel-ediff--view-file view))
         (header (format "%s — revision %d — %s%s"
                         (plist-get file :path) (plist-get file :revision) (plist-get file :status)
                         (cond ((null (plist-get file :before)) " [new file]")
                               ((null (plist-get file :after)) " [delete file]") (t "")))))
    (dolist (buffer (list (smv/gptel-ediff--view-control view)
                          (smv/gptel-ediff--view-original view) (smv/gptel-ediff--view-proposed view)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (setq-local header-line-format header))))))

(defun smv/gptel-ediff-refresh (queue)
  "Refresh QUEUE's active Ediff when the file's cumulative revision changes."
  (when-let* ((view (gptel-agent-review--queue-view queue))
              (control (smv/gptel-ediff--view-control view))
              ((buffer-live-p control))
              ((not (smv/gptel-ediff--view-updating view))))
    (let ((file (smv/gptel-ediff--view-file view)))
      (setf (smv/gptel-ediff--view-updating view) t)
      (unwind-protect
          ;; Diff subprocesses can run callbacks that enqueue another revision.
          ;; Finish each snapshot before recomputing the most recent one.
          (while (/= (smv/gptel-ediff--view-revision view) (plist-get file :revision))
            (smv/gptel-ediff--validate-view view)
            (let ((revision (plist-get file :revision))
                  (before (plist-get file :before)) (after (plist-get file :after))
                  (recenter (symbol-function 'ediff-recenter)))
              (save-selected-window
                (unless (equal before (smv/gptel-ediff--view-before view))
                  (smv/gptel-ediff--fill (smv/gptel-ediff--view-original view) before))
                (smv/gptel-ediff--fill (smv/gptel-ediff--view-proposed view) after)
                ;; Recompute overlays without Ediff rebuilding the window layout.
                (cl-letf (((symbol-function 'ediff-recenter)
                           (lambda (&rest args)
                             (unless (eq (current-buffer) control)
                               (apply recenter args)))))
                  (with-current-buffer control
                    (let ((ediff-diff-options "")) (ediff-update-diffs)))))
              (setf (smv/gptel-ediff--view-revision view) revision
                    (smv/gptel-ediff--view-before view) before
                    (smv/gptel-ediff--view-after view) after)))
        (setf (smv/gptel-ediff--view-updating view) nil))
      (smv/gptel-ediff--headers view))))

(defun smv/gptel-ediff--destroy (view)
  "Release VIEW's temporary buffers and restore its window configuration."
  (let ((queue (smv/gptel-ediff--view-queue view)))
    (when (eq view (gptel-agent-review--queue-view queue))
      (setf (gptel-agent-review--queue-view queue) nil)
      (dolist (buffer (list (smv/gptel-ediff--view-original view) (smv/gptel-ediff--view-proposed view)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (set-window-configuration (smv/gptel-ediff--view-config view)))))

(defun smv/gptel-ediff-close (queue)
  "Close QUEUE's active Ediff without deciding or applying its changes."
  (when-let* ((view (gptel-agent-review--queue-view queue))
              ((not (smv/gptel-ediff--view-closing view))))
    (setf (smv/gptel-ediff--view-closing view) t)
    (let ((control (smv/gptel-ediff--view-control view)))
      (when (buffer-live-p control)
        (with-current-buffer control
          (setf (plist-get (smv/gptel-ediff--view-file view) :hunk) ediff-current-difference)
          (condition-case err (ediff-really-quit nil)
            (error (message "Closing file review: %s" (error-message-string err)))))
        (when (buffer-live-p control) (kill-buffer control))))
    (smv/gptel-ediff--destroy view)))

(defun smv/gptel-ediff--navigate (view offset)
  "Switch from VIEW by OFFSET files in the live review list."
  (let* ((queue (smv/gptel-ediff--view-queue view))
         (files (gptel-agent-review--queue-files queue))
         (index (cl-position (smv/gptel-ediff--view-file view) files)))
    (smv/gptel-ediff-open queue (nth (mod (+ index offset) (length files)) files))))

(defun smv/gptel-ediff--comment (view source)
  "Submit immediate feedback on the selected region of VIEW's SOURCE buffer."
  (let* ((file (smv/gptel-ediff--view-file view))
         (revision (smv/gptel-ediff--view-revision view))
         (has-region (use-region-p))
         (beg (if has-region (region-beginning) (line-beginning-position)))
         (end (if has-region (region-end) (line-end-position)))
         (range (smv/gptel-ediff--line-range beg (if has-region (max beg (1- end)) end)))
         ;; Capture context before read-string: network callbacks can refresh
         ;; the preview while the user is still composing the comment.
         (context (format " (%s), %s:\n%s" source range (buffer-substring-no-properties beg end)))
         (comment (read-string (format "Comment for %s, %s: " (plist-get file :path) range))))
    (gptel-agent-review--comment (smv/gptel-ediff--view-queue view) file comment context revision)))

(defun smv/gptel-ediff-open (queue file)
  "Open FILE's cumulative diff from QUEUE and return immediately.
Updates reuse the same buffers and control window; no recursive edit is used."
  (smv/gptel-ediff-close queue)
  (let* ((path (plist-get file :path))
         (view (smv/gptel-ediff--make-view
                :queue queue :file file :config (current-window-configuration)
                :original (generate-new-buffer (format "*gptel-original: %s*" path))
                :proposed (generate-new-buffer (format "*gptel-proposed: %s*" path))
                :revision (plist-get file :revision)
                :before (plist-get file :before) :after (plist-get file :after)))
         (ediff-diff-options "") ready)
    (setf (gptel-agent-review--queue-view queue) view)
    (unwind-protect
        (progn
          (dolist (pair (list (cons (smv/gptel-ediff--view-original view) (smv/gptel-ediff--view-before view))
                              (cons (smv/gptel-ediff--view-proposed view) (smv/gptel-ediff--view-after view))))
            (smv/gptel-ediff--fill (car pair) (cdr pair))
            (smv/gptel-ediff--set-mode-for (car pair) path)
            (with-current-buffer (car pair) (setq buffer-read-only t)))
          (smv/gptel-ediff--select-main-window)
          (setf (smv/gptel-ediff--view-control view)
                (ediff-buffers (smv/gptel-ediff--view-original view) (smv/gptel-ediff--view-proposed view)))
          (with-current-buffer (smv/gptel-ediff--view-control view)
            (setq-local ediff-keep-variants t)
            (setq-local ediff-ignore-similar-regions nil)
            (setq-local ediff-brief-help-message-function
                        (lambda () " n/p: change   N/P: file   f: choose file\n q: accept file   Q: hide   R: refuse file   C: cancel all\n H: submit comment immediately (also in A/B)   ?: help"))
            (add-hook 'ediff-after-quit-hook-internal (lambda () (smv/gptel-ediff--destroy view)) nil t)
            (smv/gptel-ediff--buffer-local-set-key
             "q" (lambda ()
                   (interactive)
                   (smv/gptel-ediff-validate queue)
                   (when (eq (plist-get file :status) 'feedback)
                     (user-error "Feedback submitted; wait for a revision, or use Q to leave review"))
                   (when (eq (plist-get file :status) 'pending)
                     (setf (plist-get file :status) 'accepted))
                   (smv/gptel-ediff-close queue)
                   (gptel-agent-review--refresh queue)))
            (smv/gptel-ediff--buffer-local-set-key "Q" (lambda () (interactive) (smv/gptel-ediff-close queue)))
            (smv/gptel-ediff--buffer-local-set-key
             smv/gptel-ediff-reject-key
             (lambda () (interactive)
               (gptel-agent-review--cancel queue file (read-string "Refusal feedback (optional): "))))
            (smv/gptel-ediff--buffer-local-set-key
             "C" (lambda () (interactive)
                   (with-current-buffer (gptel-agent-review--queue-owner queue) (gptel-agent-review-cancel-all))))
            (smv/gptel-ediff--buffer-local-set-key
             "C-]" (lambda () (interactive)
                     (gptel-agent-review--cancel queue file "Review cancelled by user.")
                     (smv/gptel-ediff-close queue)))
            (smv/gptel-ediff--buffer-local-set-key "N" (lambda () (interactive) (smv/gptel-ediff--navigate view 1)))
            (smv/gptel-ediff--buffer-local-set-key "P" (lambda () (interactive) (smv/gptel-ediff--navigate view -1)))
            (smv/gptel-ediff--buffer-local-set-key
             "f" (lambda () (interactive)
                   (let* ((files (gptel-agent-review--queue-files queue))
                          (paths (mapcar (lambda (f) (cons (plist-get f :path) f)) files)))
                     (smv/gptel-ediff-open queue (cdr (assoc (completing-read "Review file: " paths nil t) paths))))))
            (smv/gptel-ediff--buffer-local-set-key
             smv/gptel-ediff-comment-key
             (lambda () (interactive)
               (let ((revision (smv/gptel-ediff--view-revision view)))
                 (gptel-agent-review--comment queue file (read-string "Review comment: ") nil revision))))
            (when (> ediff-number-of-differences 0)
              (ediff-jump-to-difference (1+ (max 0 (min (plist-get file :hunk) (1- ediff-number-of-differences)))))))
          (dolist (pair (list (cons (smv/gptel-ediff--view-original view) "original file")
                              (cons (smv/gptel-ediff--view-proposed view) "proposed changes")))
            (let ((source (cdr pair)))
              (with-current-buffer (car pair)
                (smv/gptel-ediff--buffer-local-set-key
                 smv/gptel-ediff-comment-key (lambda () (interactive) (smv/gptel-ediff--comment view source))))))
          (smv/gptel-ediff--headers view)
          (smv/gptel-ediff-refresh queue)
          (setq ready t))
      (unless ready (smv/gptel-ediff-close queue)))
    (smv/gptel-ediff--view-control view)))

(defun smv/gptel-ediff-tool-call (plist)
  "Stage file tool PLIST without waiting for review, or read staged content.
Return an immediate :result so gptel can continue with more proposals."
  (let* ((name (plist-get plist :name))
         (owner (gptel-agent-review--owner (plist-get plist :buffer)))
         (queue (buffer-local-value 'gptel-agent-review--queue owner)))
    (condition-case err
        (cond
         ((equal name "Read")
          (gptel-agent-review-read (plist-get plist :args) owner))
         ((and (or gptel-confirm-tool-calls queue)
               (member name smv/gptel-ediff-tool-names))
          (list :confirm nil :result
                (gptel-agent-review-enqueue name (plist-get plist :args) (plist-get plist :buffer)))))
      (quit (list :block "Proposal preparation cancelled. No changes were applied."))
      (error
       (let ((reason (concat "File review could not prepare proposal: " (error-message-string err))))
         (message "%s" reason)
         (list :block reason))))))

(add-hook 'gptel-pre-tool-call-functions #'smv/gptel-ediff-tool-call)

(provide 'gptel-ediff-prehook)
;;; gptel-ediff-prehook.el ends here
