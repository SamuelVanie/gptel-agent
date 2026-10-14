;;; gptel-agent-edit.el --- Prepare and apply file edits -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The tools and Ediff review share this implementation.  Preparation never
;; writes to the target files; a failed hunk invalidates the entire proposal.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun gptel-agent-edit--snapshot (path &optional must-exist)
  "Read PATH into a change plist.  Require an existing file if MUST-EXIST."
  (setq path (expand-file-name path))
  (when (file-directory-p path)
    (error "Cannot edit a directory as a file: %s" path))
  (when-let* ((buffer (find-buffer-visiting path)))
    (when (buffer-modified-p buffer)
      (error "Save or revert the unsaved buffer before editing %s" path)))
  (when (and must-exist (not (file-exists-p path)))
    (error "File does not exist: %s" path))
  (with-temp-buffer
    (let ((exists (file-exists-p path)))
      (when exists (insert-file-contents path))
      (list :path path :before (and exists (buffer-string))
            :coding buffer-file-coding-system
            :modes (and exists (file-modes path))))))

(defun gptel-agent-edit--normalize-patch (text)
  "Remove optional Markdown fences from unified diff TEXT."
  (when (string-match "\\`[ \t\r\n]*```\\(?:diff\\|patch\\)?[ \t]*\n" text)
    (setq text (substring text (match-end 0)))
    (unless (string-match "\n[ \t]*```[ \t\r\n]*\\'" text)
      (error "Unclosed diff code fence"))
    (setq text (substring text 0 (1+ (match-beginning 0)))))
  (if (string-suffix-p "\n" text) text (concat text "\n")))

(defun gptel-agent-edit--header-path (header)
  "Read a unified diff HEADER filename, allowing tab-separated timestamps."
  (save-match-data
    (let ((path (car (split-string header "\t"))))
      (when (string-prefix-p "\"" path)
        (setq path (car (read-from-string path))))
      (unless (and (stringp path) (not (string-empty-p path)))
        (error "Invalid diff filename: %s" header))
      path)))

(defun gptel-agent-edit--hunks (text)
  "Return hunks from TEXT with corrected counts, including one-line headers.
Count every context, added and removed line, even empty lines and lines
starting with additional plus or minus signs.  Never count file headers."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (hunks)
      (while (not (eobp))
        (unless (looking-at
                 "@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? +\\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@[^\n]*\n")
          (error "Malformed unified diff hunk header"))
        (let ((old-start (match-string 1)) (new-start (match-string 2))
              (begin (match-end 0)) (old-count 0) (new-count 0))
          (goto-char begin)
          (while (and (not (eobp)) (memq (char-after) '(?\s ?- ?+ ?\\)))
            (pcase (char-after)
              (?\s (cl-incf old-count) (cl-incf new-count))
              (?- (cl-incf old-count))
              (?+ (cl-incf new-count))
              (?\\ (unless (looking-at-p "\\\\ No newline at end of file$")
                     (error "Invalid diff newline marker"))))
            (forward-line 1))
          (when (= (+ old-count new-count) 0)
            (error "Empty diff hunk"))
          (push (concat (format "@@ -%s,%d +%s,%d @@\n"
                                old-start old-count new-start new-count)
                        (buffer-substring-no-properties begin (point)))
                hunks)
          ;; Only metadata may follow a hunk.  A missing prefix is an error,
          ;; rather than a silently truncated edit.
          (unless (or (eobp) (looking-at-p "@@ "))
            (error "Invalid unified diff line: %s"
                   (buffer-substring (line-beginning-position)
                                     (line-end-position))))))
      (unless hunks (error "No unified diff hunks found"))
      (apply #'concat (nreverse hunks)))))

(defun gptel-agent-edit--sections (text)
  "Parse unified diff TEXT into (OLD-PATH NEW-PATH HUNKS) sections."
  (with-temp-buffer
    (insert (gptel-agent-edit--normalize-patch text))
    (goto-char (point-min))
    (when (re-search-forward
           "^\\(?:GIT binary patch\\|Binary files \\|rename from \\|rename to \\|copy from \\|copy to \\|old mode \\|new mode \\)" nil t)
      (error "Edit supports text changes, not binary files, renames, copies or mode changes"))
    (goto-char (point-min))
    (while (re-search-forward "^new file mode \\([0-9]+\\)$" nil t)
      (unless (equal (match-string 1) "100644")
        (error "Edit cannot create special or executable files")))
    (goto-char (point-min))
    (let (headers sections boundaries)
      (while (re-search-forward "^--- \\([^\n]+\\)\n\\+\\+\\+ \\([^\n]+\\)\n@@ " nil t)
        (push (list (match-beginning 0) (- (match-end 0) 3)
                    (gptel-agent-edit--header-path (match-string 1))
                    (gptel-agent-edit--header-path (match-string 2)))
              headers))
      (setq headers (nreverse headers))
      (unless headers
        (error "Expected a unified diff with --- / +++ file headers and @@ hunks"))
      ;; Every Git/Index section must have a text patch.  Otherwise a malformed
      ;; later file could be dropped while an earlier file is applied.
      (goto-char (point-min))
      (while (re-search-forward "^\\(?:diff --git \\|Index: \\)" nil t)
        (push (match-beginning 0) boundaries))
      (setq boundaries (nreverse boundaries))
      (cl-loop for tail on boundaries
               for start = (car tail)
               for end = (or (cadr tail) (point-max)) do
               (unless (cl-find-if (lambda (h) (and (>= (car h) start) (< (car h) end))) headers)
                 (error "Diff file section has no valid headers and hunks")))
      (while headers
        (pcase-let* ((`(,_ ,start ,old ,new) (pop headers))
                     (end (if headers (caar headers) (point-max))))
          (when-let* ((boundary (cl-find-if (lambda (b) (and (> b start) (< b end))) boundaries)))
            (setq end boundary))
          (push (list old new
                      (gptel-agent-edit--hunks
                       (buffer-substring-no-properties start end)))
                sections)))
      (nreverse sections))))

(defun gptel-agent-edit--target (old new directory)
  "Resolve OLD and NEW diff paths relative to DIRECTORY without guessing."
  (let* ((git-prefixes (or (and (string-prefix-p "a/" old)
                                (string-prefix-p "b/" new))
                           (and (equal old "/dev/null") (string-prefix-p "b/" new))
                           (and (equal new "/dev/null") (string-prefix-p "a/" old))))
         (old (if (and git-prefixes (string-prefix-p "a/" old)) (substring old 2) old))
         (new (if (and git-prefixes (string-prefix-p "b/" new)) (substring new 2) new))
         (name (if (equal new "/dev/null") old new)))
    (unless (or (equal old new) (equal old "/dev/null") (equal new "/dev/null"))
      (error "Renames are not supported by Edit: %s -> %s" old new))
    (when (or (file-name-absolute-p name)
              (member ".." (split-string name "/"))
              (equal name "/dev/null"))
      (error "Diff paths must be relative to path: %s" name))
    (let ((target (expand-file-name name directory)))
      (unless (file-in-directory-p (file-truename target) (file-truename directory))
        (error "Diff path escapes the target directory: %s" name))
      target)))

(defun gptel-agent-edit--patch (change hunks)
  "Apply HUNKS to a temporary copy of CHANGE and return the resulting text."
  (unless (executable-find "patch") (error "Command `patch' is unavailable"))
  (let* ((directory (make-temp-file "gptel-edit-" t))
         (target (expand-file-name "target" directory))
         (patch-file (expand-file-name "change.patch" directory))
         (default-directory (file-name-as-directory directory))
         (coding-system-for-write 'utf-8-unix)
         (coding-system-for-read 'utf-8-unix))
    (unwind-protect
        (progn
          (with-temp-file target (insert (or (plist-get change :before) "")))
          (with-temp-file patch-file
            (insert "--- target\n+++ target\n" hunks))
          (with-temp-buffer
            (let ((status (process-file "patch" patch-file t nil
                                        "--batch" "--forward" "--fuzz=0"
                                        "--no-backup-if-mismatch" "target")))
              (unless (eq status 0)
                (error "Cannot apply diff to %s (status %s):\n%s"
                       (plist-get change :path) status (string-trim (buffer-string)))))
            (erase-buffer)
            (insert-file-contents target)
            (buffer-string)))
      (delete-directory directory t))))

(defun gptel-agent-edit--prepare-diff (path text)
  "Prepare all file changes in unified diff TEXT under PATH."
  (let* ((path (expand-file-name path))
         (directory (file-directory-p path))
         (sections (gptel-agent-edit--sections text))
         changes)
    (unless (file-readable-p path) (error "Path is not readable: %s" path))
    (when (and (not directory) (> (length sections) 1))
      (error "Use a directory path for a diff affecting multiple files"))
    (dolist (section sections)
      (pcase-let* ((`(,old ,new ,hunks) section)
                   (target (if directory (gptel-agent-edit--target old new path) path))
                   (existing (cl-find target changes :key (lambda (c) (plist-get c :path))
                                      :test #'equal))
                   (change (or existing (gptel-agent-edit--snapshot target
                                                                    (not (equal old "/dev/null")))))
                   (before (if existing (plist-get existing :after)
                             (plist-get change :before))))
        (when (and (equal old "/dev/null") before)
          (error "Diff would create an existing file: %s" target))
        (let ((after (gptel-agent-edit--patch (list :path target :before before) hunks)))
          (when (equal new "/dev/null")
            (unless (string-empty-p after)
              (error "Deletion diff leaves content in %s" target))
            (setq after nil))
          (setf (plist-get change :after) after))
        (unless existing (push change changes))))
    (nreverse changes)))

(defun gptel-agent-edit-prepare (name args)
  "Return file change plists for tool NAME and argument plist ARGS.
Each has :path, :before and :after.  Nil content means a missing file;
an empty string means an existing empty file.  Signal invalid or no-op edits."
  (let* ((path (plist-get args :path)) changes)
    (unless (stringp path) (error "Required argument `path' must be a string"))
    (setq changes
          (pcase name
            ("Edit"
             (let ((old (plist-get args :old_str)) (new (plist-get args :new_str))
                   (diff (plist-get args :diff)))
               (unless (stringp new) (error "Required argument `new_str' must be a string"))
               (if (and diff (not (eq diff :json-false)))
                   (gptel-agent-edit--prepare-diff path new)
                 (unless (and (stringp old) (not (string-empty-p old)))
                   (error "String replacement requires a nonempty `old_str'"))
                 (let ((change (gptel-agent-edit--snapshot path t)))
                   (with-temp-buffer
                     (insert (plist-get change :before))
                     (goto-char (point-min))
                     (let ((case-fold-search nil))
                       (unless (search-forward old nil t)
                         (error "Could not find old_str in %s" path))
                       (let ((start (match-beginning 0)))
                         (when (save-excursion
                                 (goto-char (1+ start)) (search-forward old nil t))
                           (error "old_str is not unique in %s; add context or use a diff" path)))
                       ;; Searching for duplicates changes the match data.
                       (goto-char (point-min)) (search-forward old)
                       (replace-match new t t))
                     (setf (plist-get change :after) (buffer-string)))
                   (list change)))))
            ("Insert"
             (let ((line (plist-get args :line_number)) (new (plist-get args :new_str))
                   (change (gptel-agent-edit--snapshot path t)))
               (unless (and (integerp line) (>= line -1) (stringp new))
                 (error "Insert requires line_number >= -1 and a string new_str"))
               (with-temp-buffer
                 (insert (plist-get change :before))
                 (if (= line -1) (goto-char (point-max))
                   (goto-char (point-min)) (forward-line line))
                 (insert new)
                 (unless (or (string-suffix-p "\n" new) (eobp)) (insert "\n"))
                 (setf (plist-get change :after) (buffer-string)))
               (list change)))
            ("Write"
             (let ((filename (plist-get args :filename)) (content (plist-get args :content)))
               (unless (and (stringp filename) (stringp content))
                 (error "Write requires string filename and content"))
               (let ((change (gptel-agent-edit--snapshot (expand-file-name filename path))))
                 (setf (plist-get change :after) content)
                 (list change))))
            (_ (error "Unsupported file tool: %s" name))))
    (setq changes (cl-remove-if
                   (lambda (c) (equal (plist-get c :before) (plist-get c :after))) changes))
    (unless changes (error "No changes: the proposed content is identical to the original"))
    changes))

(defun gptel-agent-edit--write (change content)
  "Write CONTENT for CHANGE, or remove the file when CONTENT is nil."
  (let ((path (plist-get change :path)))
    (if (null content)
        (when (file-exists-p path) (delete-file path))
      (make-directory (file-name-directory path) t)
      (with-temp-buffer
        (insert content)
        (let ((coding-system-for-write (plist-get change :coding)))
          (write-region (point-min) (point-max) path nil 'silent)))
      (when (plist-get change :modes) (set-file-modes path (plist-get change :modes))))))

(defun gptel-agent-edit-apply (changes)
  "Apply prepared CHANGES after verifying every original is still current.
Refresh unmodified visiting buffers.  Restore originals on a write error."
  (dolist (change changes)
    (let ((current (gptel-agent-edit--snapshot (plist-get change :path))))
      (unless (equal (plist-get current :before) (plist-get change :before))
        (error "File changed since preview; review again: %s" (plist-get change :path)))))
  (let (written)
    (condition-case err
        (dolist (change changes)
          (push change written)
          (gptel-agent-edit--write change (plist-get change :after)))
      ((error quit)
       (let (failures)
         (dolist (change written)
           (condition-case restore-error
               (gptel-agent-edit--write change (plist-get change :before))
             (error (push (error-message-string restore-error) failures))))
         (if failures
             (error "%s; restoring originals also failed: %s"
                    (error-message-string err) (string-join failures "; "))
           (signal (car err) (cdr err)))))))
  (dolist (change changes)
    (when-let* ((buffer (find-buffer-visiting (plist-get change :path))))
      (with-current-buffer buffer
        (if (plist-get change :after)
            (condition-case err (revert-buffer t t t)
              (error (message "Saved %s, but buffer refresh failed: %s"
                              (plist-get change :path) (error-message-string err))))
          (clear-visited-file-modtime)))))
  (format "Applied changes to %d file(s):\n%s" (length changes)
          (mapconcat (lambda (c) (plist-get c :path)) changes "\n")))

(provide 'gptel-agent-edit)
;;; gptel-agent-edit.el ends here
