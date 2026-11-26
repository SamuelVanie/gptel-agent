;;; gptel-agent-edit.el --- Prepare and apply file edits -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The tools and Ediff review share this implementation.  Preparation never
;; writes to the target files; a failed hunk invalidates the entire proposal.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel-agent-path)

(declare-function gptel-agent-history--record "gptel-agent-history" (changes))
(declare-function gptel-agent-history--complete "gptel-agent-history" (directory))

(defvar gptel-agent-edit--staged-changes nil
  "Dynamically bound table of file changes while preparing proposals.
Keys are resolved entry paths; values retain the original :before and
latest :after.  Each side is text, nil for absence, or (symlink . TARGET)
with the link's literal destination.")

(defvar gptel-agent-edit--preflight-changes nil
  "Owning review's staged entries while preflighting a review tool batch.
Disk execution and final validation use the physical filesystem.")

(defvar gptel-agent-edit--preflight-disk-hooks nil
  "Whether the pre-tool batch also has hooks that may open the disk path.")

(defun gptel-agent-edit--staged-read (path)
  "Return the pending review entry that a Read of PATH would use."
  (when gptel-agent-edit--preflight-changes
    (let ((gptel-agent-edit--staged-changes gptel-agent-edit--preflight-changes))
      (gethash (gptel-agent-edit--file-path path) gptel-agent-edit--staged-changes))))

(defun gptel-agent-edit--entry-path (path)
  "Return absolute PATH with its parent canonicalized and its leaf intact."
  (unless (file-name-absolute-p path) (setq path (expand-file-name path)))
  (let ((parent (let (file-name-handler-alist)
                  (file-truename (file-name-directory path)))))
    (when (file-remote-p parent) (error "File tools require a local path: %s" parent))
    (concat (file-name-as-directory parent) (file-name-nondirectory path))))

(defun gptel-agent-edit--check-staged-directory (directory &optional seen)
  "Check that each component of DIRECTORY is still a directory when staged.
Follow directory aliases too, rejecting cycles tracked in SEEN."
  (when (and gptel-agent-edit--staged-changes
             (> (hash-table-count gptel-agent-edit--staged-changes) 0))
    (let ((path (directory-file-name directory)))
      (while (not (equal path (directory-file-name (file-name-directory path))))
        (let ((entry (gptel-agent-edit--entry-path path)))
          (when-let* ((change (gethash entry gptel-agent-edit--staged-changes)))
            (gptel-agent-path--error entry "parent directory"
                                    (if (plist-get change :after) "file in pending changes"
                                      "entry deleted in pending changes")
                                    "Choose a path whose parent is still a directory."))
          (when (file-symlink-p entry)
            (when (member entry seen) (error "Cyclic directory symlink: %s" entry))
            (gptel-agent-edit--check-staged-directory
             (file-chase-links entry 1) (cons entry seen))))
        (setq path (directory-file-name (file-name-directory path)))))))

(defun gptel-agent-edit--file-path (path &optional entry)
  "Resolve file operand PATH in the staged filesystem.
ENTRY selects the named directory entry, even when it is a symlink."
  (setq path (gptel-agent-path-resolve path 'file-entry))
  (gptel-agent-edit--check-staged-directory (file-name-directory path))
  (let ((path (gptel-agent-edit--entry-path path))
        seen)
    (unless entry
      ;; A staged unlink or recreation shadows the physical link even when
      ;; reached through another link in the same batch.
      (while (and (not (and gptel-agent-edit--staged-changes
                            (gethash path gptel-agent-edit--staged-changes)))
                  (file-symlink-p path))
        (when (member path seen) (error "Cyclic symlink: %s" path))
        (push path seen)
        (let ((next (file-chase-links path 1)))
          (gptel-agent-edit--check-staged-directory (file-name-directory next))
          (setq path (gptel-agent-edit--entry-path next))))
      (let ((staged (and gptel-agent-edit--staged-changes
                         (gethash path gptel-agent-edit--staged-changes))))
        (when (and seen (if staged (null (plist-get staged :after)) (not (file-exists-p path))))
          (gptel-agent-path--error (car (last seen)) "file" "broken symlink"
                                  "Inspect the link with PathInfo and correct file_path."))
        (unless staged (gptel-agent-path-resolve path 'file))))
    path))

(defun gptel-agent-edit--display-content (content)
  "Format CONTENT for a read-only review of a snapshot."
  (if (eq (car-safe content) 'symlink)
      (format "Symbolic link to %S\n" (cdr content))
    content))

(defun gptel-agent-edit--change-label (before after)
  "Describe the entry types in a preview of BEFORE and AFTER."
  (cond ((consp after) " [proposed symbolic link]")
        ((consp before) (if after " [replace symbolic link with file]" " [delete symbolic link]"))
        ((null before) " [new file]")
        ((null after) " [delete file]")
        (t "")))

(defun gptel-agent-edit-stage (changes table)
  "Merge CHANGES into TABLE without modifying the supplied change plists."
  (dolist (change changes)
    (let* ((path (plist-get change :path))
           (merged (copy-sequence (or (gethash path table) change))))
      (setf (plist-get merged :path) path
            (plist-get merged :after) (plist-get change :after))
      (puthash path merged table)))
  table)

(defun gptel-agent-edit--unique-match (regexp path)
  "Return the unique bounds of REGEXP in the current buffer for PATH.
Return nil if absent; reject ambiguous matches, including overlapping ones."
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (when (re-search-forward regexp nil t)
      (let ((bounds (cons (match-beginning 0) (match-end 0))))
        (goto-char (1+ (car bounds)))
        (when (re-search-forward regexp nil t)
          (error "old_str is not unique in %s; read the file and include more surrounding lines" path))
        bounds))))

(defun gptel-agent-edit--line-pattern (text)
  "Match complete lines of TEXT, tolerating leading and trailing whitespace.
Keep line breaks, internal whitespace and case significant."
  (let* ((newline (string-suffix-p "\n" text))
         (lines (split-string (if newline (substring text 0 -1) text) "\n")))
    (concat
     "^[ \t]*"
     (mapconcat (lambda (line)
                  (regexp-quote (string-trim line "[ \t]+" "[ \t]+")))
                lines "[ \t]*\n[ \t]*")
     "[ \t]*" (if newline "\\(?:\n\\|\\'\\)" "$"))))

(defun gptel-agent-edit--indent-width (prefix)
  "Return the indentation width of PREFIX, with tab stops every eight columns."
  (let ((width 0))
    (mapc (lambda (char)
            (setq width (+ width (if (= char ?\t) (- 8 (% width 8)) 1))))
          prefix)
    width))

(defun gptel-agent-edit--base-indent (text)
  "Return the least indentation prefix of the nonblank lines in TEXT."
  (let (prefix width)
    (dolist (line (split-string text "\n"))
      (when (string-match "\\`\\([ \t]*\\)[^ \t]" line)
        (let* ((indent (match-string 1 line))
               (columns (gptel-agent-edit--indent-width indent)))
          (when (or (null width) (< columns width))
            (setq prefix indent width columns)))))
    (or prefix "")))

(defun gptel-agent-edit--reindent (new old actual)
  "Align NEW's indentation with ACTUAL when OLD matched only after trimming.
Preserve relative indentation in NEW and the source block's base prefix."
  (let* ((old-prefix (gptel-agent-edit--base-indent old))
         (prefix (gptel-agent-edit--base-indent actual))
         (old-width (gptel-agent-edit--indent-width old-prefix))
         (width (gptel-agent-edit--indent-width prefix)))
    (if (equal prefix old-prefix) new
      (mapconcat
       (lambda (line)
         (if (not (string-match "\\`\\([ \t]*\\)[^ \t]" line)) line
           (let* ((end (match-end 1))
                  (relative (- (gptel-agent-edit--indent-width (match-string 1 line)) old-width)))
             (concat (if (>= relative 0)
                         (concat prefix (make-string relative ?\s))
                       (make-string (max 0 (+ width relative)) ?\s))
                     (substring line end)))))
       (split-string new "\n") "\n"))))

(defun gptel-agent-edit--replace (change old new)
  "Replace OLD with NEW in CHANGE, using a unique whitespace fallback.
Exact matches take precedence.  Never guess changed words or punctuation."
  (with-temp-buffer
    (insert (plist-get change :before))
    (let* ((path (plist-get change :path))
           (bounds (gptel-agent-edit--unique-match (regexp-quote old) path)))
      (unless bounds
        (setq old (string-replace "\r\n" "\n" old))
        (when (string-match-p "[^ \t\n]" old)
          (setq bounds (gptel-agent-edit--unique-match
                        (gptel-agent-edit--line-pattern old) path)))
        (unless bounds
          (error "Could not find old_str in %s, even ignoring indentation and trailing whitespace. Read the current file and retry with a unique block copied from it" path))
        (setq new (gptel-agent-edit--reindent
                   (string-replace "\r\n" "\n" new) old
                   (buffer-substring-no-properties (car bounds) (cdr bounds)))))
      (goto-char (car bounds))
      (delete-region (car bounds) (cdr bounds))
      (insert new))
    (setf (plist-get change :after) (buffer-string)))
  (list change))

(defun gptel-agent-edit--visiting-buffers (path entry-only)
  "Return local buffers visiting PATH, a resolved entry path.
ENTRY-ONLY matches the final name even through aliases of its parent.
Otherwise also include the buffer visiting the file through a symlink."
  (let ((leaf (file-name-nondirectory path))
        buffers)
    (dolist (buffer (buffer-list))
      (when-let* ((name (buffer-file-name buffer))
                  ((not (file-remote-p name)))
                  ((equal leaf (file-name-nondirectory name)))
                  ((or (equal name path)
                       (equal path (ignore-errors (gptel-agent-edit--entry-path name))))))
        (push buffer buffers)))
    (unless entry-only
      (when-let* ((buffer (find-buffer-visiting path)))
        (cl-pushnew buffer buffers)))
    buffers))

(defun gptel-agent-edit--snapshot (path &optional must-exist entry)
  "Read PATH into a change plist, requiring existence if MUST-EXIST.
When ENTRY is non-nil, snapshot the final symlink itself."
  (setq path (gptel-agent-edit--file-path path entry))
  (if-let* ((staged (and gptel-agent-edit--staged-changes
                         (gethash path gptel-agent-edit--staged-changes))))
      (let ((snapshot (copy-sequence staged)))
        (when (and must-exist (null (plist-get staged :after)))
          (error "File was deleted earlier in this batch: %s" path))
        (setf (plist-get snapshot :before) (plist-get staged :after))
        snapshot)
    (let ((link (and entry (file-symlink-p path))))
      (when (cl-some #'buffer-modified-p (gptel-agent-edit--visiting-buffers path link))
        (error "Save or revert the unsaved buffer before editing %s" path))
      (if link
          (list :path path :before (cons 'symlink link))
        (when must-exist (gptel-agent-path-resolve path 'file t))
        (with-temp-buffer
          (let ((exists (file-exists-p path)))
            (when exists (insert-file-contents path))
            (list :path path :before (and exists (buffer-string))
                  :coding buffer-file-coding-system
                  :modes (and exists (file-modes path)))))))))

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
      (when (or (string-suffix-p "/" path)
                (member (file-name-nondirectory path) '("." "..")))
        (gptel-agent-path--error path "file name in patch header" "directory syntax"
                                "Use a filename without a trailing slash or final . / ..; inspect uncertain targets with PathInfo."))
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
  "Parse unified diff TEXT into (OLD-PATH NEW-PATH HUNKS DELETED-MODE) sections."
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
    (let ((metadata (point-min)) headers sections boundaries)
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
        (pcase-let* ((`(,header ,start ,old ,new) (pop headers))
                     (end (if headers (caar headers) (point-max))))
          (when-let* ((boundary (cl-find-if (lambda (b) (and (> b start) (< b end))) boundaries)))
            (setq end boundary))
          (goto-char metadata)
          (push (list old new
                      (gptel-agent-edit--hunks
                       (buffer-substring-no-properties start end))
                      (when (re-search-forward "^deleted file mode \\([0-9]+\\)$" header t)
                        (match-string 1)))
                sections)
          (setq metadata end)))
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
    (gptel-agent-edit--batch-path name directory nil (equal new "/dev/null"))))

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
          (with-temp-file patch-file
            (insert "--- target\n+++ target\n" hunks))
          (with-temp-buffer
            (let (status)
              ;; Try exact context first.  The original tool allowed patch's
              ;; normal fuzz; try that before also relaxing whitespace.
              ;; Reset the copy because an unsuccessful attempt can apply some
              ;; hunks before rejecting others.
              (catch 'applied
                (dolist (options '(("--fuzz=0") ("--fuzz=2") ("--fuzz=2" "-l")))
                  (with-temp-file target (insert (or (plist-get change :before) "")))
                  (erase-buffer)
                  (setq status (apply #'process-file "patch" patch-file t nil
                                      "--batch" "--forward" "--no-backup-if-mismatch"
                                      (append options '("target"))))
                  (when (eq status 0) (throw 'applied t))))
              (unless (eq status 0)
                (error "Cannot apply diff to %s (status %s). Read the current file and regenerate the diff:\n%s"
                       (plist-get change :path) status (string-trim (buffer-string)))))
            (erase-buffer)
            (insert-file-contents target)
            (buffer-string)))
      (delete-directory directory t))))

(defun gptel-agent-edit--unlink-hunks (link)
  "Return the exact normalized unified deletion hunk for literal LINK."
  (let* ((newline (string-suffix-p "\n" link))
         (lines (split-string (if newline (substring link 0 -1) link) "\n")))
    (concat (format "@@ -1,%d +0,0 @@\n" (length lines))
            (mapconcat (lambda (line) (concat "-" line "\n")) lines "")
            (unless newline "\\ No newline at end of file\n"))))

(defun gptel-agent-edit--stage-table ()
  "Copy the current staged filesystem, or start an empty one."
  (if gptel-agent-edit--staged-changes (copy-hash-table gptel-agent-edit--staged-changes)
    (make-hash-table :test #'equal)))

(defun gptel-agent-edit--prepare-diff (path text directory)
  "Prepare unified diff TEXT at PATH; DIRECTORY explicitly selects a root."
  (let* ((sections (gptel-agent-edit--sections text))
         (gptel-agent-edit--staged-changes (gptel-agent-edit--stage-table))
         (batch-changes (make-hash-table :test #'equal))
         changes)
    (when (and (not directory) (> (length sections) 1))
      (error "Use root_directory for a diff affecting multiple files; file_path selects one file"))
    (dolist (section sections)
      (pcase-let* ((`(,old ,new ,hunks ,deleted-mode) section)
                   (delete (equal new "/dev/null"))
                   (target (if directory (gptel-agent-edit--target old new path)
                             (gptel-agent-edit--file-path path delete)))
                   (change (gptel-agent-edit--snapshot target
                                                       (or (not directory) (not (equal old "/dev/null")))
                                                       t))
                   (before (plist-get change :before)))
        (when (and deleted-mode
                   (not (and delete
                             (if (eq (car-safe before) 'symlink) (equal deleted-mode "120000")
                               (member deleted-mode '("100644" "100755"))))))
          (error "Deletion diff mode %s does not match the entry at %s" deleted-mode target))
        (when (and (equal old "/dev/null") before)
          (error "Diff would create an existing file: %s" target))
        (let ((after
               (if (eq (car-safe before) 'symlink)
                   (if (and delete (equal hunks (gptel-agent-edit--unlink-hunks (cdr before)))) ""
                     (error "Deletion diff must match the exact symbolic link destination: %s" target))
                 (gptel-agent-edit--patch change hunks))))
          (when delete
            (unless (string-empty-p after)
              (error "Deletion diff leaves content in %s" target))
            (setq after nil))
          (setf (plist-get change :after) after))
        (unless (gethash target batch-changes) (push target changes))
        (gptel-agent-edit-stage (list change) batch-changes)
        (gptel-agent-edit-stage (list change) gptel-agent-edit--staged-changes)))
    (mapcar (lambda (target) (gethash target batch-changes)) (nreverse changes))))

(defun gptel-agent-edit--batch-path (path root &optional directory entry)
  "Resolve relative PATH inside ROOT as a DIRECTORY, file or final ENTRY."
  (unless (and (stringp path) (not (string-empty-p path))
               (not (file-name-absolute-p path))
               (not (member ".." (split-string path "/"))))
    (error "Operation path must be relative to the batch directory: %S" path))
  (let* ((absolute (expand-file-name path root))
         (target (if directory
                     (progn
                       (gptel-agent-edit--check-staged-directory absolute)
                       (file-truename (gptel-agent-path-resolve absolute 'directory t)))
                   (gptel-agent-edit--file-path absolute entry)))
         (root (file-name-as-directory (file-truename root))))
    (unless (or (and directory (equal (directory-file-name target) (directory-file-name root)))
                (string-prefix-p root target))
      (error "Operation path escapes the batch directory: %s" path))
    target))

(defun gptel-agent-edit--deletion-p (mode args)
  "Whether MODE with ARGS unlinks its single file operand."
  (or (member mode '("delete" "Delete"))
      (and (equal mode "patch")
           (let ((sections (gptel-agent-edit--sections (plist-get args :new_str))))
             (and (null (cdr sections)) (equal (cadar sections) "/dev/null"))))))

(defun gptel-agent-edit--operation-args (operation root)
  "Translate OPERATION's explicit relative targets under ROOT."
  (let* ((kind (plist-get operation :operation))
         (args (copy-sequence operation)))
    (unless (member kind '("replace" "patch" "insert" "write" "delete"))
      (error "Unknown file operation: %S" kind))
    (dolist (key '(:file_path :root_directory))
      (when-let* ((value (plist-get args key))
                  ((not (equal value ""))))
        (let ((unlink (and (eq key :file_path) (gptel-agent-edit--deletion-p kind args))))
          (setq args (plist-put args key (gptel-agent-edit--batch-path
                                         value root (eq key :root_directory) unlink))))))
    (setq args (plist-put args :mode kind))
    (cons (pcase kind ((or "replace" "patch") "Edit")
                 ("insert" "Insert") ("write" "Write") ("delete" "Delete"))
          args)))

(defun gptel-agent-edit--prepare-operation (operation root)
  "Prepare OPERATION against the current staged contents under ROOT."
  (pcase-let ((`(,name . ,args) (gptel-agent-edit--operation-args operation root)))
    (gptel-agent-edit-prepare name args)))

(defun gptel-agent-edit--prepare-batch (root operations)
  "Prepare ordered OPERATIONS under ROOT without writing to target files."
  (setq root (gptel-agent-path-resolve root 'directory t))
  (unless (and (or (listp operations) (vectorp operations)) (> (length operations) 0))
    (error "Batch mode requires a nonempty operations array"))
  (let ((root (file-name-as-directory (file-truename root)))
        (gptel-agent-edit--staged-changes (gptel-agent-edit--stage-table))
        (batch-changes (make-hash-table :test #'equal))
        (index 0) paths)
    (dolist (operation (append operations nil))
      (cl-incf index)
      (condition-case err
          (dolist (change (gptel-agent-edit--prepare-operation operation root))
            (let* ((path (plist-get change :path))
                   (previous (gethash path batch-changes)))
              (unless previous (push path paths))
              (gptel-agent-edit-stage (list change) batch-changes)
              (gptel-agent-edit-stage (list change) gptel-agent-edit--staged-changes)))
        (error (error "Operation %d: %s" index (error-message-string err)))))
    (mapcar (lambda (path) (gethash path batch-changes)) (nreverse paths))))

(defun gptel-agent-edit--contract (name args)
  "Validate NAME/ARGS and return (MODE TARGET DIRECTORY-P).
Mode is always explicit for Edit.  Empty unused fields are accepted for
providers which require every schema field, but never select a mode."
  (let* ((mode (if (equal name "Edit") (plist-get args :mode) name))
         (file (plist-get args :file_path)) (root (plist-get args :root_directory))
         (operations (plist-get args :operations))
         (has-file (and (stringp file) (not (string-empty-p file))))
         (has-root (and (stringp root) (not (string-empty-p root)))))
    (when (or (plist-member args :path) (plist-member args :filename)
              (plist-member args :diff))
      (error "Use file_path or root_directory and an explicit Edit mode; path/filename/diff are not supported"))
    (if (equal name "Edit")
        (unless (member mode '("replace" "patch" "batch"))
          (error "Edit requires mode: replace, patch, or batch"))
      (unless (member name '("Insert" "Write" "Delete"))
        (error "Unsupported file tool: %s" name)))
    (dolist (value (list file root))
      (unless (or (null value) (stringp value))
        (error "file_path and root_directory must be strings when supplied")))
    (unless (or (null operations) (listp operations) (vectorp operations))
      (error "operations must be an array"))
    (when (and operations (> (length operations) 0) (not (equal mode "batch")))
      (error "operations requires mode=batch"))
    (pcase mode
      ("batch"
       (unless (and has-root (not has-file)
                    (member (plist-get args :old_str) '(nil ""))
                    (member (plist-get args :new_str) '(nil "")))
         (error "Batch mode requires root_directory; omit file_path/old_str/new_str or leave them empty"))
       (unless (and operations (> (length operations) 0))
         (error "Batch mode requires a nonempty operations array")))
      ("patch"
       (unless (and (not (eq has-file has-root))
                    (member (plist-get args :old_str) '(nil "")))
         (error "Patch mode requires exactly one of file_path or root_directory; omit old_str or leave it empty")))
      (_ (unless (and has-file (not has-root))
           (error "%s requires file_path; omit root_directory or leave it empty" mode))))
    (when has-root (gptel-agent-edit--check-staged-directory (expand-file-name root)))
    (list mode
          (gptel-agent-path-resolve (if has-root root file)
                                    (if has-root 'directory 'file-entry) has-root)
          has-root)))

(defun gptel-agent-edit-targets (name args &optional operands)
  "Return resolved file targets of NAME/ARGS without reading their contents.
If supplied, map each batch operation with a file_path to its entry in OPERANDS."
  (let* ((gptel-agent-edit--staged-changes
          (or gptel-agent-edit--staged-changes gptel-agent-edit--preflight-changes))
         (gptel-agent-edit--staged-changes (gptel-agent-edit--stage-table)))
    (delete-dups (gptel-agent-edit--collect-targets name args operands))))

(defun gptel-agent-edit--collect-targets (name args &optional operands)
  "Collect NAME/ARGS targets in order, optionally indexing OPERANDS."
  (pcase-let* ((`(,mode ,path ,directory) (gptel-agent-edit--contract name args)))
    (pcase mode
      ("batch"
       (cl-mapcan
        (lambda (operation)
          (pcase-let* ((`(,op-name . ,op-args) (gptel-agent-edit--operation-args operation path))
                       (targets (gptel-agent-edit--collect-targets op-name op-args)))
            (when (and operands (stringp (plist-get operation :file_path))
                       (not (string-empty-p (plist-get operation :file_path)))
                       (= (length targets) 1))
              (puthash operation (car targets) operands))
            targets))
        (append (plist-get args :operations) nil)))
      ("patch"
       (unless (stringp (plist-get args :new_str))
         (error "Patch mode requires a unified diff in new_str"))
       (let ((sections (gptel-agent-edit--sections (plist-get args :new_str))))
         (when (and (not directory) (cdr sections))
           (error "Use root_directory for a diff affecting multiple files"))
         (cl-loop for (old new _) in sections
                  for delete = (equal new "/dev/null")
                  for target = (if directory (gptel-agent-edit--target old new path)
                                 (gptel-agent-edit--file-path path delete))
                  do (puthash target (list :after (unless delete "")) gptel-agent-edit--staged-changes)
                  collect target)))
      ("Delete"
       (let ((target (gptel-agent-edit--file-path path t)))
         (puthash target '(:after nil) gptel-agent-edit--staged-changes)
         (list target)))
      (_ (let ((target (gptel-agent-edit--file-path path)))
           (puthash target '(:after "") gptel-agent-edit--staged-changes)
           (list target))))))

(defun gptel-agent-edit--goto-insertion (line)
  "Move to the insertion point for LINE in the original text."
  (if (= line -1)
      (goto-char (point-max))
    (goto-char (point-min))
    (forward-line line)))

(defun gptel-agent-edit-prepare (name args)
  "Prepare NAME/ARGS with explicit file and directory contracts."
  (pcase-let* ((`(,mode ,path ,directory) (gptel-agent-edit--contract name args))
              (changes
               (pcase mode
                 ("batch" (gptel-agent-edit--prepare-batch path (plist-get args :operations)))
                 ("patch"
                  (unless (stringp (plist-get args :new_str))
                    (error "Patch mode requires a unified diff in new_str"))
                  (gptel-agent-edit--prepare-diff path (plist-get args :new_str) directory))
                 ("replace"
                  ;; Check the target before suggesting changes to text arguments.
                  (let ((change (gptel-agent-edit--snapshot path t))
                        (old (plist-get args :old_str)) (new (plist-get args :new_str)))
                    (unless (and (stringp old) (not (string-empty-p old)) (stringp new))
                      (error "Replacement requires nonempty old_str copied from Read, and string new_str; use Write for a new file"))
                    (gptel-agent-edit--replace change old new)))
                 ("Insert"
                  (let ((line (plist-get args :line_number)) (new (plist-get args :new_str))
                        (change (gptel-agent-edit--snapshot path t)))
                    (unless (and (integerp line) (>= line -1) (stringp new))
                      (error "Insert requires line_number >= -1 and a string new_str"))
                    (with-temp-buffer
                      (insert (plist-get change :before))
                      (gptel-agent-edit--goto-insertion line)
                      (when (and (> line 0) (not (bolp)) (not (string-empty-p new)))
                        (insert "\n"))
                      (insert new)
                      (unless (or (eobp) (string-suffix-p "\n" new)) (insert "\n"))
                      (setf (plist-get change :after) (buffer-string)))
                    (list change)))
                 ("Write"
                  (unless (stringp (plist-get args :content))
                    (error "Write requires complete string content; use an empty string for an empty file"))
                  (let ((change (gptel-agent-edit--snapshot path)))
                    (setf (plist-get change :after) (plist-get args :content))
                    (list change)))
                 ("Delete"
                  (let ((change (gptel-agent-edit--snapshot path t t)))
                    (setf (plist-get change :after) nil)
                    (list change))))))
    (setq changes (cl-remove-if
                   (lambda (c) (equal (plist-get c :before) (plist-get c :after))) changes))
    (unless changes (error "No changes: the proposed content is identical to the original"))
    (gptel-agent-edit--validate-file-parents changes)
    changes))

(defun gptel-agent-edit--validate-file-parents (changes)
  "Reject CHANGES that require a file target to also be a directory."
  (let ((files (make-hash-table :test #'equal)))
    (dolist (change changes)
      (when (plist-get change :after)
        (puthash (plist-get change :path) t files)))
    (maphash
     (lambda (path _)
       (let ((parent (file-name-directory path)))
         (while (and parent (not (equal parent (file-name-directory (directory-file-name parent)))))
           (when (gethash (directory-file-name parent) files)
             (gptel-agent-path--error parent "parent directory" "file created by this proposal"
                                     "Choose distinct file and directory paths; no changes were applied."))
           (setq parent (file-name-directory (directory-file-name parent))))))
     files)))

(defvar gptel-agent-edit--write-state nil
  "Progress of the active write if it fails: nil, `removed' or `writing'.")

(defun gptel-agent-edit--entry-version (path)
  "Return the identity and modification metadata of the entry at PATH."
  (when-let* ((attrs (file-attributes path 'integer)))
    (list (file-attribute-file-identifier attrs) (file-attribute-type attrs)
          (file-attribute-size attrs) (file-attribute-modification-time attrs)
          (file-attribute-status-change-time attrs))))

(defun gptel-agent-edit--write (change content)
  "Install CONTENT from CHANGE's expected :before entry, even during rollback."
  (let ((path (plist-get change :path)) (before (plist-get change :before)))
    (gptel-agent-path-resolve path 'file-entry)
    (gptel-agent-edit--check-entry change)
    (when (and before (or (null content) (consp before) (consp content)))
      (delete-file path)
      (setq gptel-agent-edit--write-state 'removed))
    (cond
     ((consp content)
      ;; Never replace an entry installed between deletion and creation.
      (make-symbolic-link (cdr content) path))
     ((stringp content)
      (let ((create (not (stringp before))))
        (make-directory (file-name-directory path) t)
        (with-temp-buffer
          (insert content)
          (let ((coding-system-for-write (plist-get change :coding))
                (previous gptel-agent-edit--write-state))
            ;; Other write failures can leave a partially written regular file.
            (setq gptel-agent-edit--write-state 'writing)
            (condition-case err
                (write-region (point-min) (point-max) path nil 'silent nil (and create 'excl))
              (file-already-exists
               (setq gptel-agent-edit--write-state previous)
               (signal (car err) (cdr err))))))
        (when (and create (plist-get change :modes))
          (set-file-modes path (plist-get change :modes))))))))

(defun gptel-agent-edit--check-entry (change)
  "Reject a changed parent, type or symlink destination for CHANGE."
  (let* ((path (plist-get change :path))
         (before (plist-get change :before))
         (link (file-symlink-p path)))
    (unless (and (equal path (gptel-agent-edit--entry-path path))
                 (cond (link (equal before (cons 'symlink link)))
                       ((null before) (not (file-exists-p path)))
                       (t (and (stringp before) (file-regular-p path)))))
      (error "File changed since preview; review again: %s" path))))

(defun gptel-agent-edit-validate (changes)
  "Check CHANGES against disk and reject text the file encoding cannot save.
Validate every file before history is recorded or working files are written,
including unsaved visiting buffers.  Return changes with current permissions
for existing files and the supplied permissions for files to be recreated."
  (gptel-agent-edit--validate-file-parents changes)
  (let (gptel-agent-edit--staged-changes validated)
    (dolist (change changes (nreverse validated))
      ;; `write-region' silently substitutes unsupported characters when its
      ;; coding system is forced.  Undecided encodings are different: ASCII
      ;; and new files may still choose a suitable encoding for their text.
      (when-let* ((content (plist-get change :after))
                  ((stringp content))
                  (coding (plist-get change :coding))
                  ((not (eq (coding-system-type coding) 'undecided)))
                  (position (unencodable-char-position
                             0 (length content) coding nil content)))
        (error "Cannot encode U+%04X in %s using %s; use compatible text or explicitly change the file encoding before retrying. No changes were applied"
               (aref content position) (plist-get change :path) coding))
      (gptel-agent-edit--check-entry change)
      (let ((current (gptel-agent-edit--snapshot (plist-get change :path) nil t)))
        (unless (equal (plist-get current :before) (plist-get change :before))
          (error "File changed since preview; review again: %s" (plist-get change :path)))
        (push (if-let* ((modes (plist-get current :modes)))
                  (plist-put (copy-sequence change) :modes modes)
                change)
              validated)))))

(defun gptel-agent-edit--refresh-buffer (path &optional entry-only)
  "Refresh unmodified buffers visiting PATH after a disk change.
ENTRY-ONLY restricts updates to visitors of the named entry.  Detach
previous contents when the entry is now a link, without loading its target."
  (dolist (buffer (gptel-agent-edit--visiting-buffers path entry-only))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless (buffer-modified-p)
          (condition-case err
              (cond
               ((and entry-only (file-symlink-p path))
                (set-visited-file-name nil t)
                (set-buffer-modified-p nil)
                (message "Symbolic link at %s; %s no longer visits that path" path (buffer-name)))
               ((not (file-exists-p path)) (clear-visited-file-modtime))
               (t (revert-buffer t t t)))
            (error (message "File %s changed, but buffer refresh failed: %s"
                            path (error-message-string err)))))))))

(defun gptel-agent-edit-apply (changes &optional return-snapshot)
  "Snapshot and apply CHANGES after verifying every original is current.
Refresh unmodified visiting buffers.  Restore originals on a write error.
Return a result message, or the snapshot directory when RETURN-SNAPSHOT
is non-nil."
  (require 'gptel-agent-history)
  (setq changes (gptel-agent-edit-validate changes))
  (let ((snapshot (gptel-agent-history--record changes))
        gptel-agent-edit--write-state current written)
    (condition-case err
        (dolist (change changes)
          (setq current change gptel-agent-edit--write-state nil)
          (gptel-agent-edit--write change (plist-get change :after))
          (push (list change (plist-get change :after)
                      (gptel-agent-edit--entry-version (plist-get change :path)))
                written)
          (setq current nil))
      ((error quit)
       (let (failures)
         ;; A failed exclusive create did not acquire the competing entry.
         ;; If we already unlinked the original, only its empty slot is ours.
         (when (and current gptel-agent-edit--write-state)
           (let ((version (and (eq gptel-agent-edit--write-state 'writing)
                               (gptel-agent-edit--entry-version (plist-get current :path)))))
             (push (list current (and version (plist-get current :after)) version) written)))
         (dolist (item written)
           (pcase-let* ((`(,change ,expected ,version) item)
                        (path (plist-get change :path))
                        (original (plist-get change :before))
                        (inverse (copy-sequence change)))
             (condition-case restore-error
                 (progn
                   (unless (equal version (gptel-agent-edit--entry-version path))
                     (error "Entry changed during rollback: %s" path))
                   (setf (plist-get inverse :before) expected
                         (plist-get inverse :after) original)
                   (gptel-agent-edit--write inverse original))
               (error (push (error-message-string restore-error) failures)))))
         (if failures
             (error "%s; restoring originals also failed: %s; recover from snapshots in %s"
                    (error-message-string err) (string-join failures "; ") snapshot)
           (signal (car err) (cdr err))))))
    (gptel-agent-history--complete snapshot)
    (dolist (change changes)
      (gptel-agent-edit--refresh-buffer
       (plist-get change :path) (or (consp (plist-get change :before)) (consp (plist-get change :after)))))
    (if return-snapshot snapshot
      (format "Applied changes to %d file(s):\n%s" (length changes)
              (mapconcat (lambda (c) (plist-get c :path)) changes "\n")))))

(provide 'gptel-agent-edit)
;;; gptel-agent-edit.el ends here
