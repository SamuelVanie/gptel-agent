;;; gptel-agent-history.el --- Restorable file snapshots -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Keep before/after copies of file-tool changes outside the project.  History
;; survives conversation buffers and Emacs restarts, until temporary files are
;; removed.  Restores refuse to overwrite later edits and are themselves saved.

;;; Code:

(require 'gptel-agent-edit)
(require 'tabulated-list)
(require 'diff)

(defcustom gptel-agent-history-directory
  (expand-file-name (format "gptel-agent-history-%s/" (user-uid)) temporary-file-directory)
  "Directory for persistent before/after copies of agent file changes.
The system may clean its temporary directory.  Choose a permanent directory
if history must be retained indefinitely.  Snapshots are not pruned by gptel."
  :type 'directory :group 'gptel)

(defvar gptel-agent-history-before-restore-hook nil
  "Functions called with file changes before restoring a history entry.")

(defvar-local gptel-agent-history-show-cancelled nil
  "Whether to show cancelled changes and cancellation/recovery entries.")

(defvar-local gptel-agent-history--displayed-files nil
  "Alist of snapshot directories and their currently displayed file entries.")

(defun gptel-agent-history--id (directory)
  "Return DIRECTORY's persistent batch identifier."
  (file-name-nondirectory (directory-file-name directory)))

(defun gptel-agent-history--short-id (id)
  "Return a compact display label for batch ID, retaining it in help text."
  (propertize (concat "#" (car (last (split-string id "-" t))))
              'help-echo id))

(defun gptel-agent-history--record (changes)
  "Save CHANGES before any write, returning the new snapshot directory."
  (make-directory gptel-agent-history-directory t)
  (when (or (file-symlink-p (directory-file-name gptel-agent-history-directory))
            (not (equal (file-attribute-user-id (file-attributes gptel-agent-history-directory))
                        (user-uid))))
    (error "History directory must be owned by the current user and not a symlink"))
  (set-file-modes gptel-agent-history-directory #o700)
  (let ((directory (make-temp-file
                    (expand-file-name (format-time-string "%Y%m%dT%H%M%S-")
                                      gptel-agent-history-directory) t))
        (coding-system-for-write 'utf-8-emacs)
        (index 0) files)
    (dolist (change changes)
      (let ((entry (list :path (file-truename (plist-get change :path))
                         :coding (plist-get change :coding) :modes (plist-get change :modes))))
        (when (plist-get change :kind)
          (setq entry (append entry (list :kind (plist-get change :kind)
                                          :reverses (plist-get change :reverses)))))
        (dolist (side '(:before :after))
          (when (plist-get change side)
            (let ((name (format "%d-%s" index (substring (symbol-name side) 1))))
              (with-temp-file (expand-file-name name directory) (insert (plist-get change side)))
              (set-file-modes (expand-file-name name directory) #o600)
              (setq entry (plist-put entry side name)))))
        (push entry files)
        (cl-incf index)))
    (with-temp-file (expand-file-name "manifest.el" directory)
      (let ((print-length nil) (print-level nil))
        (prin1 (list :time (format-time-string "%Y-%m-%d %H:%M:%S.%3N")
                     :files (nreverse files)) (current-buffer))))
    (set-file-modes (expand-file-name "manifest.el" directory) #o600)
    directory))

(defun gptel-agent-history--complete (directory)
  "Mark DIRECTORY as applied, without turning a successful write into failure."
  (condition-case err
      (with-temp-file (expand-file-name "applied" directory))
    (error (message "Files saved; history completion marker failed in %s: %s"
                    directory (error-message-string err)))))

(defun gptel-agent-history--manifest (directory)
  "Read DIRECTORY's snapshot metadata without evaluating it."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "manifest.el" directory))
    (goto-char (point-min))
    (read (current-buffer))))

(defun gptel-agent-history--content (directory name)
  "Read snapshot NAME in DIRECTORY; nil represents a nonexistent file."
  (when name
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-emacs))
        (insert-file-contents (expand-file-name name directory)))
      (buffer-string))))

(defun gptel-agent-history--entries ()
  "Return completed snapshot directories, newest first."
  (when (file-directory-p gptel-agent-history-directory)
    (sort (cl-remove-if-not
           (lambda (directory) (file-exists-p (expand-file-name "applied" directory)))
           (directory-files gptel-agent-history-directory t directory-files-no-dot-files-regexp))
          (lambda (a b)
            (time-less-p (file-attribute-modification-time (file-attributes b))
                         (file-attribute-modification-time (file-attributes a)))))))

(defun gptel-agent-history--catalog ()
  "Read completed batches and annotate their files with active cancellations.
Links are per file, so refusing one file never hides another in the same batch.
Reversing a cancellation makes its original changes visible again.  No saved
manifests are modified; legacy entries without links remain ordinary changes."
  (let ((catalog (mapcar (lambda (directory)
                           (cons directory (gptel-agent-history--manifest directory)))
                         (gptel-agent-history--entries)))
        (incoming (make-hash-table :test #'equal))
        (active (make-hash-table :test #'equal)))
    (dolist (batch catalog)
      (let ((id (gptel-agent-history--id (car batch))))
        (dolist (file (plist-get (cdr batch) :files))
          (dolist (target (plist-get file :reverses))
            (push (cons id (plist-get file :path))
                  (gethash (cons target (plist-get file :path)) incoming))))))
    (cl-labels ((active-p (key)
                  (let ((cached (gethash key active 'unknown)))
                    (if (not (eq cached 'unknown)) cached
                      ;; Guard against cycles in damaged metadata as well.
                      (puthash key nil active)
                      (puthash key (not (cl-some #'active-p (gethash key incoming))) active)))))
      (dolist (batch catalog)
        (let ((id (gptel-agent-history--id (car batch))))
          (setf (plist-get (cdr batch) :files)
                (mapcar (lambda (file)
                          (plist-put file :cancelled-by
                                     (mapcar #'car
                                             (cl-remove-if-not
                                              #'active-p (gethash (cons id (plist-get file :path)) incoming)))))
                        (plist-get (cdr batch) :files))))))
    catalog))

(defun gptel-agent-history--hidden-p (file)
  "Whether FILE is hidden in the default history view."
  (or (plist-get file :kind) (plist-get file :cancelled-by)))

(defun gptel-agent-history--files (directory)
  "Return DIRECTORY's displayed files in history, or all files outside it."
  (if (derived-mode-p 'gptel-agent-history-mode)
      (or (cdr (assoc directory gptel-agent-history--displayed-files))
          (user-error "This batch is hidden; use c to show cancelled changes"))
    (plist-get (gptel-agent-history--manifest directory) :files)))

(defun gptel-agent-history--deletion-group (directory)
  "Return DIRECTORY and all batches connected by cancellation/recovery links.
Follow links in both directions so deleting either side never leaves an
orphaned cancellation or makes a cancelled change appear active again."
  (let* ((catalog (gptel-agent-history--catalog))
         (directory (directory-file-name (expand-file-name directory)))
         (links (make-hash-table :test #'equal))
         (selected (make-hash-table :test #'equal))
         (pending (list (gptel-agent-history--id directory))))
    (unless (assoc directory catalog)
      (user-error "Select a completed history snapshot"))
    (dolist (batch catalog)
      (let ((id (gptel-agent-history--id (car batch))))
        (dolist (file (plist-get (cdr batch) :files))
          (dolist (target (plist-get file :reverses))
            (push target (gethash id links))
            (push id (gethash target links))))))
    (while pending
      (let ((id (pop pending)))
        (unless (gethash id selected)
          (puthash id t selected)
          (setq pending (append (gethash id links) pending)))))
    (cl-loop for (batch . _) in catalog
             when (gethash (gptel-agent-history--id batch) selected)
             collect batch)))

;;;###autoload
(defun gptel-agent-history-delete (directory)
  "Permanently delete DIRECTORY's saved history and all linked batches.
Cancellation and recovery links are followed in both directions, including
shared cancellations of several revisions or files.  Delete whole batches,
even if the current history filter hides some of their files.
This only removes saved snapshots; working files are not changed.
Ask for confirmation when called interactively."
  (interactive
   (list (if (derived-mode-p 'gptel-agent-history-mode)
             (or (tabulated-list-get-id) (user-error "Select a snapshot first"))
           (completing-read "Delete history snapshot: "
                            (gptel-agent-history--entries) nil t))))
  (let ((directories (gptel-agent-history--deletion-group directory)))
    (when (or (not (called-interactively-p 'any))
              (yes-or-no-p
               (format "Permanently delete %d history batch(es), including linked cancellations/recoveries? "
                       (length directories))))
      (dolist (snapshot directories)
        (delete-directory snapshot t))
      (when (derived-mode-p 'gptel-agent-history-mode) (gptel-agent-history-refresh))
      (message "Deleted %d history batch(es)" (length directories)))))

;;;###autoload
(defun gptel-agent-history-restore (directory)
  "Undo the snapshot in DIRECTORY, preserving later or unsaved user edits.
Restore newer entries first if a file has changed since this snapshot.
Restoring also records a new snapshot, so the restore can itself be undone.
In the history browser, restore only the files visible under the current filter."
  (interactive
   (list (if (derived-mode-p 'gptel-agent-history-mode)
             (or (tabulated-list-get-id)
                 (user-error "Select a visible batch; use c to show cancelled changes"))
           (completing-read "Restore changes from snapshot: "
                            (gptel-agent-history--entries) nil t))))
  (let ((changes
         (mapcar
          (lambda (file)
            (list :path (plist-get file :path) :coding (plist-get file :coding)
                  :modes (plist-get file :modes)
                  :kind (if (eq (plist-get file :kind) 'cancellation) 'recovery 'cancellation)
                  :reverses (list (gptel-agent-history--id directory))
                  :before (gptel-agent-history--content directory (plist-get file :after))
                  :after (gptel-agent-history--content directory (plist-get file :before))))
          (gptel-agent-history--files directory))))
    (run-hook-with-args 'gptel-agent-history-before-restore-hook changes)
    (gptel-agent-edit-apply changes)
    (when (derived-mode-p 'gptel-agent-history-mode) (gptel-agent-history-refresh))
    (message "Restored %d file(s); the restore is also saved in history" (length changes))))

(defun gptel-agent-history-diff ()
  "Display the selected snapshot's changes to a file."
  (interactive)
  (let* ((directory (or (tabulated-list-get-id) (user-error "Select a snapshot first")))
         (files (gptel-agent-history--files directory))
         (path (completing-read "File: " (mapcar (lambda (f) (plist-get f :path)) files) nil t))
         (file (cl-find path files :key (lambda (f) (plist-get f :path)) :test #'equal)))
    (let ((before (generate-new-buffer " *history-before*"))
          (after (generate-new-buffer " *history-after*")))
      (unwind-protect
          (progn
            (with-current-buffer before
              (insert (or (gptel-agent-history--content directory (plist-get file :before)) "")))
            (with-current-buffer after
              (insert (or (gptel-agent-history--content directory (plist-get file :after)) "")))
            (diff before after "-u" t))
        (kill-buffer before)
        (kill-buffer after)))))

(defvar gptel-agent-history-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'gptel-agent-history-diff)
    (define-key map (kbd "r") #'gptel-agent-history-restore)
    (define-key map (kbd "d") #'gptel-agent-history-delete)
    (define-key map (kbd "c") #'gptel-agent-history-toggle-cancelled)
    map))

(define-derived-mode gptel-agent-history-mode tabulated-list-mode "Agent History"
  "Browse saved agent changes.
RET inspects, r undoes, d deletes history, c shows cancellations."
  (setq tabulated-list-format [("Batch" 8 t) ("Time" 24 t) ("Status" 20 t)
                               ("Related batches" 24 t) ("Files" 0 t)])
  (add-hook 'tabulated-list-revert-hook #'gptel-agent-history-refresh nil t)
  (tabulated-list-init-header))

(defun gptel-agent-history-toggle-cancelled ()
  "Toggle cancelled changes and their cancellation/recovery entries."
  (interactive)
  (setq gptel-agent-history-show-cancelled (not gptel-agent-history-show-cancelled))
  (gptel-agent-history-refresh))

(defun gptel-agent-history--status (files)
  "Describe the status of a batch containing FILES."
  (let ((cancelled (cl-count-if (lambda (file) (plist-get file :cancelled-by)) files)))
    (cond ((= cancelled (length files)) (propertize "[-] Cancelled" 'face 'shadow))
          ((> cancelled 0) (propertize "[~] Partly cancelled" 'face 'warning))
          ((eq (plist-get (car files) :kind) 'cancellation)
           (propertize "[x] Cancellation" 'face 'warning))
          ((eq (plist-get (car files) :kind) 'recovery)
           (propertize "[+] Recovery" 'face 'success))
          (t "Change"))))

(defun gptel-agent-history--relations (file)
  "Describe FILE's linked cancellations using batch labels."
  (string-join
   (delq nil
         (list (when (plist-get file :cancelled-by)
                 (concat "by " (mapconcat #'gptel-agent-history--short-id
                                           (plist-get file :cancelled-by) ", ")))
               (when (plist-get file :reverses)
                 (concat "of " (mapconcat #'gptel-agent-history--short-id
                                           (plist-get file :reverses) ", ")))))
   "; "))

(defun gptel-agent-history-refresh ()
  "Refresh the saved file history."
  (setq tabulated-list-entries nil
        gptel-agent-history--displayed-files nil)
  (dolist (batch (gptel-agent-history--catalog))
    (let* ((directory (car batch)) (manifest (cdr batch))
           (files (plist-get manifest :files))
           (shown (if gptel-agent-history-show-cancelled files
                    (cl-remove-if #'gptel-agent-history--hidden-p files))))
      (when shown
        (push (cons directory shown) gptel-agent-history--displayed-files)
        (push (list directory
                    (vector (gptel-agent-history--short-id (gptel-agent-history--id directory))
                            (plist-get manifest :time)
                            (gptel-agent-history--status files)
                            (string-join (delete-dups (cl-remove-if #'string-empty-p
                                                                   (mapcar #'gptel-agent-history--relations files))) "; ")
                            (mapconcat
                             (lambda (file)
                               (concat (plist-get file :path)
                                       (when (and gptel-agent-history-show-cancelled
                                                  (plist-get file :cancelled-by))
                                         (propertize (concat " [cancelled " (gptel-agent-history--relations file) "]")
                                                     'face 'shadow))))
                             shown ", ")))
              tabulated-list-entries))))
  (setq tabulated-list-entries (nreverse tabulated-list-entries))
  (setq-local header-line-format
              (format " RET: diff   r: undo shown files   d: delete batch   c: %s cancelled   g: refresh   q: close"
                      (if gptel-agent-history-show-cancelled "hide" "show")))
  (tabulated-list-print t))

;;;###autoload
(defun gptel-agent-history ()
  "Browse restorable agent changes, including those from earlier sessions."
  (interactive)
  (let ((directory gptel-agent-history-directory))
    (pop-to-buffer (get-buffer-create "*Agent file history*"))
    (gptel-agent-history-mode)
    (setq-local gptel-agent-history-directory directory)
    (gptel-agent-history-refresh)))

(provide 'gptel-agent-history)
;;; gptel-agent-history.el ends here
