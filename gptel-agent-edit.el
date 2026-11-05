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
Keys are canonical paths; values retain the original :before and latest :after.")

(defun gptel-agent-edit-stage (changes table)
  "Merge CHANGES into TABLE without modifying the supplied change plists."
  (dolist (change changes)
    (let* ((path (file-truename (plist-get change :path)))
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

(defun gptel-agent-edit--snapshot (path &optional must-exist)
  "Read PATH into a change plist.  Require an existing file if MUST-EXIST."
  (setq path (gptel-agent-path-resolve path 'file))
  (when gptel-agent-edit--staged-changes
    (setq path (file-truename path))
    (let ((parent (file-name-directory path)))
      (while (and parent (not (equal parent (file-name-directory (directory-file-name parent)))))
        (when (plist-get (gethash (directory-file-name parent) gptel-agent-edit--staged-changes) :after)
          (gptel-agent-path--error parent "parent directory" "file in pending changes"
                                  "Choose a different path; this parent is already staged as a file."))
        (setq parent (file-name-directory (directory-file-name parent))))))
  (if-let* ((staged (and gptel-agent-edit--staged-changes
                         (gethash path gptel-agent-edit--staged-changes))))
      (let ((snapshot (copy-sequence staged)))
        (when (and must-exist (null (plist-get staged :after)))
          (error "File was deleted earlier in this batch: %s" path))
        (setf (plist-get snapshot :before) (plist-get staged :after))
        snapshot)
    (when-let* ((buffer (find-buffer-visiting path)))
      (when (buffer-modified-p buffer)
        (error "Save or revert the unsaved buffer before editing %s" path)))
    (when must-exist (gptel-agent-path-resolve path 'file t))
    (with-temp-buffer
      (let ((exists (file-exists-p path)))
        (when exists (insert-file-contents path))
        (list :path path :before (and exists (buffer-string))
              :coding buffer-file-coding-system
              :modes (and exists (file-modes path)))))))

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
    (let ((target (gptel-agent-path-resolve (expand-file-name name directory) 'file)))
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

(defun gptel-agent-edit--prepare-diff (path text directory)
  "Prepare unified diff TEXT at PATH; DIRECTORY explicitly selects a root."
  (let* ((path (gptel-agent-path-resolve path (if directory 'directory 'file)))
         (sections (gptel-agent-edit--sections text))
         changes)
    (unless (or (and (file-readable-p path) (or (not directory) (file-directory-p path)))
                (and gptel-agent-edit--staged-changes
                     (plist-get (gethash (file-truename path) gptel-agent-edit--staged-changes) :after)))
      (gptel-agent-path-resolve path (if directory 'directory 'file) t)
      (error "Path is not readable: %s; inspect permissions with PathInfo" path))
    (when (and (not directory) (> (length sections) 1))
      (error "Use root_directory for a diff affecting multiple files; file_path selects one file"))
    (dolist (section sections)
      (pcase-let* ((`(,old ,new ,hunks) section)
                   (target (if directory (gptel-agent-edit--target old new path) path))
                   (target (if gptel-agent-edit--staged-changes (file-truename target) target))
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

(defun gptel-agent-edit--batch-path (path root &optional directory)
  "Resolve relative PATH inside ROOT, explicitly expecting a DIRECTORY or file."
  (unless (and (stringp path) (not (string-empty-p path))
               (not (file-name-absolute-p path))
               (not (member ".." (split-string path "/"))))
    (error "Operation path must be relative to the batch directory: %S" path))
  (let ((target (file-truename
                 (gptel-agent-path-resolve (expand-file-name path root)
                                           (if directory 'directory 'file) directory))))
    (unless (or (equal (directory-file-name target) (directory-file-name root))
                (file-in-directory-p target root))
      (error "Operation path escapes the batch directory: %s" path))
    target))

(defun gptel-agent-edit--operation-args (operation root)
  "Translate OPERATION's explicit relative targets under ROOT."
  (let* ((kind (plist-get operation :operation))
         (args (copy-sequence operation)))
    (unless (member kind '("replace" "patch" "insert" "write" "delete"))
      (error "Unknown file operation: %S" kind))
    (dolist (key '(:file_path :root_directory))
      (when-let* ((value (plist-get args key))
                  ((not (equal value ""))))
        (setq args (plist-put args key (gptel-agent-edit--batch-path
                                       value root (eq key :root_directory))))))
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
        (gptel-agent-edit--staged-changes
         (if gptel-agent-edit--staged-changes
             (copy-hash-table gptel-agent-edit--staged-changes)
           (make-hash-table :test #'equal)))
        (batch-changes (make-hash-table :test #'equal))
        (index 0) paths)
    (dolist (operation (append operations nil))
      (cl-incf index)
      (condition-case err
          (dolist (change (gptel-agent-edit--prepare-operation operation root))
            (let* ((path (file-truename (plist-get change :path)))
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
    (list mode
          (gptel-agent-path-resolve (if has-root root file)
                                    (if has-root 'directory 'file) has-root)
          has-root)))

(defun gptel-agent-edit-targets (name args)
  "Return validated file targets of NAME/ARGS without staging edits."
  (pcase-let* ((`(,mode ,path ,directory) (gptel-agent-edit--contract name args)))
    (pcase mode
      ("batch"
       (delete-dups
        (cl-mapcan
         (lambda (operation)
           (pcase-let ((`(,op-name . ,op-args)
                         (gptel-agent-edit--operation-args operation path)))
             (gptel-agent-edit-targets op-name op-args)))
         (append (plist-get args :operations) nil))))
      ("patch"
       (unless (stringp (plist-get args :new_str))
         (error "Patch mode requires a unified diff in new_str"))
       (let ((sections (gptel-agent-edit--sections (plist-get args :new_str))))
         (if directory
             (mapcar (lambda (section)
                       (gptel-agent-edit--target (nth 0 section) (nth 1 section) path)) sections)
           (list path))))
      (_ (list path)))))

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
                      (if (= line -1) (goto-char (point-max))
                        (goto-char (point-min)) (forward-line line))
                      (insert new)
                      (unless (or (string-suffix-p "\n" new) (eobp)) (insert "\n"))
                      (setf (plist-get change :after) (buffer-string)))
                    (list change)))
                 ("Write"
                  (unless (stringp (plist-get args :content))
                    (error "Write requires complete string content; use an empty string for an empty file"))
                  (let ((change (gptel-agent-edit--snapshot path)))
                    (setf (plist-get change :after) (plist-get args :content))
                    (list change)))
                 ("Delete"
                  (let ((change (gptel-agent-edit--snapshot path t)))
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
        (puthash (file-truename (plist-get change :path)) t files)))
    (maphash
     (lambda (path _)
       (let ((parent (file-name-directory path)))
         (while (and parent (not (equal parent (file-name-directory (directory-file-name parent)))))
           (when (gethash (directory-file-name parent) files)
             (gptel-agent-path--error parent "parent directory" "file created by this proposal"
                                     "Choose distinct file and directory paths; no changes were applied."))
           (setq parent (file-name-directory (directory-file-name parent))))))
     files)))

(defun gptel-agent-edit--write (change content)
  "Write CONTENT for CHANGE, or remove the file when CONTENT is nil."
  (let ((path (gptel-agent-path-resolve (plist-get change :path) 'file)))
    (if (null content)
        (when (file-exists-p path) (delete-file path))
      (make-directory (file-name-directory path) t)
      (with-temp-buffer
        (insert content)
        (let ((coding-system-for-write (plist-get change :coding)))
          (write-region (point-min) (point-max) path nil 'silent)))
      (when (plist-get change :modes) (set-file-modes path (plist-get change :modes))))))

(defun gptel-agent-edit-validate (changes)
  "Check CHANGES against disk, including unsaved visiting buffers."
  (gptel-agent-edit--validate-file-parents changes)
  (let (gptel-agent-edit--staged-changes)
    (dolist (change changes)
      (let ((current (gptel-agent-edit--snapshot (plist-get change :path))))
        (unless (equal (plist-get current :before) (plist-get change :before))
          (error "File changed since preview; review again: %s" (plist-get change :path)))))))

(defun gptel-agent-edit--refresh-buffer (path)
  "Refresh an unmodified buffer visiting PATH after an on-disk change."
  (when-let* ((buffer (find-buffer-visiting path)))
    (with-current-buffer buffer
      (unless (buffer-modified-p)
        (if (file-exists-p path)
            (condition-case err (revert-buffer t t t)
              (error (message "File %s changed, but buffer refresh failed: %s"
                              path (error-message-string err))))
          (clear-visited-file-modtime))))))

(defun gptel-agent-edit-apply (changes &optional return-snapshot)
  "Snapshot and apply CHANGES after verifying every original is current.
Refresh unmodified visiting buffers.  Restore originals on a write error.
Return a result message, or the snapshot directory when RETURN-SNAPSHOT
is non-nil."
  (require 'gptel-agent-history)
  (gptel-agent-edit-validate changes)
  (let ((snapshot (gptel-agent-history--record changes)) written)
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
             (error "%s; restoring originals also failed: %s; recover from snapshots in %s"
                    (error-message-string err) (string-join failures "; ") snapshot)
           (signal (car err) (cdr err))))))
    (gptel-agent-history--complete snapshot)
    (dolist (change changes)
      (gptel-agent-edit--refresh-buffer (plist-get change :path)))
    (if return-snapshot snapshot
      (format "Applied changes to %d file(s):\n%s" (length changes)
              (mapconcat (lambda (c) (plist-get c :path)) changes "\n")))))

(provide 'gptel-agent-edit)
;;; gptel-agent-edit.el ends here
