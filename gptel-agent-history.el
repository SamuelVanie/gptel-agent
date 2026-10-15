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

;;;###autoload
(defun gptel-agent-history-restore (directory)
  "Undo the snapshot in DIRECTORY, preserving later or unsaved user edits.
Restore newer entries first if a file has changed since this snapshot.
Restoring also records a new snapshot, so the restore can itself be undone."
  (interactive
   (list (or (and (derived-mode-p 'gptel-agent-history-mode) (tabulated-list-get-id))
             (completing-read "Restore changes from snapshot: "
                              (gptel-agent-history--entries) nil t))))
  (let ((changes
         (mapcar
          (lambda (file)
            (list :path (plist-get file :path) :coding (plist-get file :coding)
                  :modes (plist-get file :modes)
                  :before (gptel-agent-history--content directory (plist-get file :after))
                  :after (gptel-agent-history--content directory (plist-get file :before))))
          (plist-get (gptel-agent-history--manifest directory) :files))))
    (run-hook-with-args 'gptel-agent-history-before-restore-hook changes)
    (gptel-agent-edit-apply changes)
    (when (derived-mode-p 'gptel-agent-history-mode) (gptel-agent-history-refresh))
    (message "Restored %d file(s); the restore is also saved in history" (length changes))))

(defun gptel-agent-history-diff ()
  "Display the selected snapshot's changes to a file."
  (interactive)
  (let* ((directory (or (tabulated-list-get-id) (user-error "Select a snapshot first")))
         (files (plist-get (gptel-agent-history--manifest directory) :files))
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
    map))

(define-derived-mode gptel-agent-history-mode tabulated-list-mode "Agent History"
  "Browse saved agent changes.  RET shows a diff; r restores an entry."
  (setq tabulated-list-format [("Time" 24 t) ("Files" 0 t)])
  (add-hook 'tabulated-list-revert-hook #'gptel-agent-history-refresh nil t)
  (tabulated-list-init-header)
  (setq-local header-line-format " RET: inspect file diff   r: restore batch   g: refresh   q: close"))

(defun gptel-agent-history-refresh ()
  "Refresh the saved file history."
  (setq tabulated-list-entries
        (mapcar (lambda (directory)
                  (let ((manifest (gptel-agent-history--manifest directory)))
                    (list directory
                          (vector (plist-get manifest :time)
                                  (mapconcat (lambda (f) (plist-get f :path))
                                             (plist-get manifest :files) ", ")))))
                (gptel-agent-history--entries)))
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
