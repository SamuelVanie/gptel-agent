;;; gptel-agent-session-store.el --- Committed family checkpoints -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Immutable chat versions are visible only through an atomically replaced
;; family manifest.  Header indexing never reads archived transcripts.  Legacy
;; flat snapshots remain readable until their known family is migrated.
;; Callers own the directory writer lock for every mutation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst gptel-agent-session-store--id-regexp "[a-f0-9]\\{64\\}")
(defvar gptel-agent-session-store--cache-directory nil)
(defvar gptel-agent-session-store--cache-input nil)
(defvar gptel-agent-session-store--cache nil)

(defun gptel-agent-session-store--directory (directory)
  "Return a canonical local DIRECTORY without creating it."
  (when (file-remote-p directory) (error "Session storage must be local"))
  (file-name-as-directory (file-truename (expand-file-name directory))))

(defun gptel-agent-session-store--check-id (id)
  "Validate filesystem-safe session ID and return it."
  (unless (and (stringp id)
               (string-match-p (concat "\\`" gptel-agent-session-store--id-regexp "\\'") id))
    (error "Invalid session identifier: %S" id))
  id)

(defun gptel-agent-session-store--read (file &optional header-only)
  "Read data in FILE without evaluation; HEADER-ONLY bounds the read."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-emacs) (read-circle nil))
      (insert-file-contents file nil 0 (and header-only 8192))
      (goto-char (point-min))
      (read (current-buffer)))))

(defun gptel-agent-session-store--relative-file (id file &optional legacy)
  "Validate ID's relative payload FILE; LEGACY permits a flat snapshot."
  (gptel-agent-session-store--check-id id)
  (unless (and (stringp file)
               (or (and legacy (equal file (concat id ".eld")))
                   (string-match-p
                    (concat "\\`snapshots/" id "--"
                            gptel-agent-session-store--id-regexp "\\.eld\\'") file)))
    (error "Invalid snapshot reference for %s: %S" id file))
  file)

(defun gptel-agent-session-store--check-header (record id &optional legacy)
  "Validate RECORD as ID's metadata, optionally allowing LEGACY references."
  (unless (and (proper-list-p record) (equal id (plist-get record :id))
               (stringp (plist-get record :name))
               (numberp (plist-get record :updated)))
    (error "Invalid session header for %s" id))
  (gptel-agent-session-store--check-id id)
  (when (plist-get record :parent)
    (gptel-agent-session-store--check-id (plist-get record :parent)))
  (gptel-agent-session-store--relative-file id (plist-get record :file) legacy)
  record)

(defun gptel-agent-session-store--check-manifest (manifest root &optional directory)
  "Validate MANIFEST for ROOT, checking payloads when DIRECTORY is non-nil."
  (gptel-agent-session-store--check-id root)
  (unless (and (proper-list-p manifest) (equal (plist-get manifest :version) 2)
               (equal root (plist-get manifest :root))
               (proper-list-p (plist-get manifest :members))
               (plist-get manifest :members))
    (error "Invalid family checkpoint for %s" root))
  (gptel-agent-session-store--check-id (plist-get manifest :generation))
  (let ((members (make-hash-table :test #'equal))
        (children (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal)))
    (dolist (record (plist-get manifest :members))
      (let ((id (plist-get record :id)))
        (gptel-agent-session-store--check-header record id)
        (when (gethash id members) (error "Duplicate checkpoint member %s" id))
        (puthash id record members)
        (push id (gethash (plist-get record :parent) children))
        (when (and directory
                   (not (file-regular-p (expand-file-name (plist-get record :file) directory))))
          (error "Incomplete checkpoint %s: missing snapshot for %s" root id))))
    (unless (gethash root members) (error "Checkpoint has no root snapshot: %s" root))
    (when (plist-get (gethash root members) :parent)
      (error "Checkpoint root %s has a parent" root))
    (maphash (lambda (id record)
               (unless (or (equal id root) (gethash (plist-get record :parent) members))
                 (error "Checkpoint %s omits parent of %s" root id))) members)
    (let ((pending (list root)))
      (while pending
        (let ((id (pop pending)))
          (when (gethash id seen) (error "Cyclic checkpoint ownership in %s" root))
          (puthash id t seen)
          (dolist (child (gethash id children)) (push child pending)))))
    (unless (= (hash-table-count members) (hash-table-count seen))
      (error "Disconnected or cyclic checkpoint ownership in %s" root)))
  manifest)

(defun gptel-agent-session-store--index (directory &optional refresh)
  "Index DIRECTORY's committed headers, optionally forcing REFRESH.
Return a plist with :families, :entries and :owners hash tables.  :superseded
maps hidden legacy IDs to their committed root, for safe cleanup.  Staged
versions are never advertised.  Invalid manifests fail rather than exposing
a partial family."
  (let* ((input (file-name-as-directory (expand-file-name directory)))
         (directory (if (equal input gptel-agent-session-store--cache-input)
                        gptel-agent-session-store--cache-directory
                      (gptel-agent-session-store--directory directory))))
    (if (and (not refresh) (equal directory gptel-agent-session-store--cache-directory)
             gptel-agent-session-store--cache)
        (progn (setq gptel-agent-session-store--cache-input input)
               gptel-agent-session-store--cache)
      (let ((families (make-hash-table :test #'equal))
            (entries (make-hash-table :test #'equal))
            (owners (make-hash-table :test #'equal))
            (superseded (make-hash-table :test #'equal))
            (legacy (make-hash-table :test #'equal))
            (legacy-families (make-hash-table :test #'equal))
            (resolved (make-hash-table :test #'equal))
            (pattern (concat "\\`" gptel-agent-session-store--id-regexp "\\.eld\\'"))
            (manifests-directory (expand-file-name "families/" directory)))
        (when (file-directory-p manifests-directory)
          (dolist (file (directory-files manifests-directory t pattern))
            (let* ((root (file-name-base file))
                   (manifest (gptel-agent-session-store--check-manifest
                              (gptel-agent-session-store--read file) root)))
              (puthash root manifest families)
              (dolist (record (plist-get manifest :members))
                (let ((id (plist-get record :id)))
                  (when (gethash id entries) (error "Session %s belongs to multiple checkpoints" id))
                  (puthash id record entries)
                  (puthash id root owners))))))
        (when (file-directory-p directory)
          (dolist (file (directory-files directory t pattern))
            (let* ((id (file-name-base file)) (owner (gethash id owners)))
              (if owner (puthash id owner superseded)
                (let ((record (gptel-agent-session-store--read file t)))
                  (unless (equal (plist-get record :version) 1)
                    (error "Unsupported legacy session snapshot %s" id))
                  (setq record (plist-put record :file (file-name-nondirectory file)))
                  (gptel-agent-session-store--check-header record id t)
                  (puthash id record legacy))))))
        ;; Resolve each legacy ancestry once.  A committed manifest is
        ;; authoritative: leftover flat children cannot fill gaps in it.
        (maphash
         (lambda (id _record)
           (unless (gethash id resolved)
             (let ((cursor id) (visited (make-hash-table :test #'equal)) chain result)
               (while (not result)
                 (cond
                  ((gethash cursor resolved) (setq result (gethash cursor resolved)))
                  ((gethash cursor owners) (setq result (cons 'hidden (gethash cursor owners))))
                  ((gethash cursor visited) (error "Cyclic legacy session ownership at %s" cursor))
                  (t
                   (puthash cursor t visited)
                   (push cursor chain)
                   (let ((parent (plist-get (gethash cursor legacy) :parent)))
                     (if (and parent (or (gethash parent legacy) (gethash parent owners)))
                         (setq cursor parent)
                       (setq result (cons 'legacy cursor)))))))
               (dolist (member chain) (puthash member result resolved))))) legacy)
        (maphash (lambda (id record)
                   (let ((resolution (gethash id resolved)))
                     (if (eq (car resolution) 'hidden)
                         (puthash id (cdr resolution) superseded)
                       (puthash id record entries)
                       (puthash id (cdr resolution) owners)
                       (push record (gethash (cdr resolution) legacy-families))))) legacy)
        (setq gptel-agent-session-store--cache-directory directory
              gptel-agent-session-store--cache-input input
              gptel-agent-session-store--cache
              (list :families families :entries entries :owners owners :superseded superseded
                    :legacy-families legacy-families))))))

(defun gptel-agent-session-store--catalog (directory &optional refresh)
  "Return committed headers in DIRECTORY, optionally forcing REFRESH."
  (let (records)
    (maphash (lambda (_ record) (push record records))
             (plist-get (gptel-agent-session-store--index directory refresh) :entries))
    records))

(defun gptel-agent-session-store--entry (directory id)
  "Return ID's committed header in DIRECTORY, or nil."
  (gptel-agent-session-store--check-id id)
  (gethash id (plist-get (gptel-agent-session-store--index directory) :entries)))

(defun gptel-agent-session-store--owner (directory id)
  "Return the known committed family root for ID in DIRECTORY, or nil."
  (gptel-agent-session-store--check-id id)
  (gethash id (plist-get (gptel-agent-session-store--index directory) :owners)))

(defun gptel-agent-session-store--family (directory root)
  "Return ROOT's committed member headers in DIRECTORY, root first."
  (gptel-agent-session-store--check-id root)
  (let* ((index (gptel-agent-session-store--index directory))
         (manifest (gethash root (plist-get index :families))))
    (if manifest
        (cons (gethash root (plist-get index :entries))
              (cl-remove root (plist-get manifest :members)
                         :key (lambda (record) (plist-get record :id)) :test #'equal))
      (when-let* ((members (gethash root (plist-get index :legacy-families))))
        (cons (gethash root (plist-get index :entries))
              (cl-remove root members :key (lambda (record) (plist-get record :id)) :test #'equal))))))

(defun gptel-agent-session-store--file (directory id)
  "Return ID's committed payload path, or its legacy path if not committed."
  (let ((entry (gptel-agent-session-store--entry directory id)))
    (expand-file-name (or (plist-get entry :file) (concat id ".eld")) directory)))

(defun gptel-agent-session-store--ensure-subdirectory (directory name)
  "Create private NAME beneath DIRECTORY for a mutation."
  (when (file-remote-p directory) (error "Session storage must be local"))
  ;; Preserve the caller's path spelling (notably /var versus /private/var on
  ;; macOS), so file-relative-name against its configured store stays inside it.
  (let ((subdirectory (expand-file-name name directory)))
    (when (file-symlink-p (directory-file-name subdirectory))
      (error "Session storage subdirectory must not be a symlink: %s" subdirectory))
    (make-directory subdirectory t)
    (set-file-modes subdirectory #o700)
    subdirectory))

(defun gptel-agent-session-store--version-file (directory id generation)
  "Return a new immutable payload path for ID and GENERATION in DIRECTORY."
  (gptel-agent-session-store--check-id id)
  (gptel-agent-session-store--check-id generation)
  (expand-file-name (concat id "--" generation ".eld")
                    (gptel-agent-session-store--ensure-subdirectory directory "snapshots/")))

(defun gptel-agent-session-store--commit (directory root generation members)
  "Publish MEMBERS for ROOT and GENERATION atomically in DIRECTORY.
Every payload must already exist.  Reused versions may have older generation
IDs.  The previous checkpoint remains usable if preparing or renaming the
new manifest fails.  This does not prune old or in-flight snapshot versions."
  (setq directory (gptel-agent-session-store--directory directory))
  (let* ((index (gptel-agent-session-store--index directory))
         (manifest (list :version 2 :root root :generation generation :members (copy-tree members)))
         (known (make-hash-table :test #'equal)))
    (gptel-agent-session-store--check-manifest manifest root directory)
    (dolist (record members)
      (let* ((id (plist-get record :id)) (owner (gethash id (plist-get index :owners))))
        (puthash id t known)
        (when (and owner (not (equal owner root)))
          (error "Session %s already belongs to family %s" id owner))))
    (dolist (record (gptel-agent-session-store--family directory root))
      (unless (gethash (plist-get record :id) known)
        (error "Checkpoint cannot omit previously saved discussion %s" (plist-get record :id))))
    (let* ((manifests-directory (gptel-agent-session-store--ensure-subdirectory directory "families/"))
           (file (expand-file-name (concat root ".eld") manifests-directory))
           (temporary (make-temp-file (expand-file-name ".checkpoint-" manifests-directory)))
           (coding-system-for-write 'utf-8-emacs)
           (print-length nil) (print-level nil) (print-circle nil)
           (print-escape-newlines t) (print-escape-control-characters t))
      (unwind-protect
          (progn
            (set-file-modes temporary #o600)
            (with-temp-buffer
              (prin1 manifest (current-buffer))
              (insert "\n")
              (write-region (point-min) (point-max) temporary nil 'silent))
            (rename-file temporary file t)
            ;; Update only this family's index: independent families do not
            ;; rescan one another's manifests on every commit.
            (puthash root manifest (plist-get index :families))
            (remhash root (plist-get index :legacy-families))
            (dolist (record (plist-get manifest :members))
              (let ((id (plist-get record :id)))
                (puthash id record (plist-get index :entries))
                (puthash id root (plist-get index :owners))
                (when (file-exists-p (expand-file-name (concat id ".eld") directory))
                  (puthash id root (plist-get index :superseded)))))
            manifest)
        (when (file-exists-p temporary) (delete-file temporary))))))

(defun gptel-agent-session-store--prune (directory &optional protected-files)
  "Remove unreferenced versions in DIRECTORY, except PROTECTED-FILES.
PROTECTED-FILES are absolute paths or store-relative paths in active
transactions.  Also remove legacy snapshots superseded by manifests.
Return the number removed.  Call only while holding the writer lock."
  (setq directory (gptel-agent-session-store--directory directory))
  (let* ((index (gptel-agent-session-store--index directory))
         (keep (make-hash-table :test #'equal))
         (snapshots (expand-file-name "snapshots/" directory))
         (pattern (concat "\\`" gptel-agent-session-store--id-regexp "--"
                          gptel-agent-session-store--id-regexp "\\.eld\\'"))
         (removed 0))
    (maphash (lambda (_ record)
               (puthash (expand-file-name (plist-get record :file) directory) t keep))
             (plist-get index :entries))
    (dolist (file protected-files)
      (puthash (file-truename (expand-file-name file directory)) t keep))
    (when (file-directory-p snapshots)
      (when (file-symlink-p (directory-file-name snapshots))
        (error "Refusing to prune symlinked snapshot directory"))
      (dolist (file (directory-files snapshots t pattern))
        (unless (gethash file keep) (delete-file file) (cl-incf removed))))
    (maphash (lambda (id _)
               (let ((file (expand-file-name (concat id ".eld") directory)))
                 (when (and (file-exists-p file) (not (gethash file keep)))
                   (delete-file file) (cl-incf removed))))
             (plist-get index :superseded))
    removed))

(defun gptel-agent-session-store--obsolete (directory old-members new-members)
  "Return superseded payload paths after OLD-MEMBERS become NEW-MEMBERS.
No directory scan is performed.  The caller can delete these incrementally
after the new manifest has committed.  Legacy files for migrated members are
also returned when present."
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (let ((keep (make-hash-table :test #'equal))
        (obsolete (make-hash-table :test #'equal)))
    (dolist (record new-members)
      (puthash (expand-file-name (plist-get record :file) directory) t keep))
    (dolist (record old-members)
      (let ((file (expand-file-name (plist-get record :file) directory)))
        (unless (gethash file keep) (puthash file t obsolete))))
    (dolist (record new-members)
      (let ((file (expand-file-name (concat (plist-get record :id) ".eld") directory)))
        (when (and (file-exists-p file) (not (gethash file keep)))
          (puthash file t obsolete))))
    (let (files)
      (maphash (lambda (file _) (push file files)) obsolete)
      files)))

(defun gptel-agent-session-store--files (directory ids)
  "Return stored files belonging to the families containing IDS in DIRECTORY.
Include manifests, committed payloads, obsolete versions and legacy files,
so deleting a family cannot expose an older or staged copy afterward."
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (let* ((index (gptel-agent-session-store--index directory))
         (roots (make-hash-table :test #'equal))
         (members (make-hash-table :test #'equal))
         (files (make-hash-table :test #'equal))
         (snapshots (expand-file-name "snapshots/" directory)))
    (dolist (id ids)
      (gptel-agent-session-store--check-id id)
      (puthash id t members)
      (when-let* ((root (gethash id (plist-get index :owners)))) (puthash root t roots)))
    (maphash (lambda (id root) (when (gethash root roots) (puthash id t members)))
             (plist-get index :owners))
    (maphash (lambda (id root) (when (gethash root roots) (puthash id t members)))
             (plist-get index :superseded))
    (maphash (lambda (root _)
               (let ((file (expand-file-name (concat "families/" root ".eld") directory)))
                 (when (file-exists-p file) (puthash file t files)))) roots)
    (maphash (lambda (id _)
               (dolist (file (list (expand-file-name (concat id ".eld") directory)
                                   (gptel-agent-session-store--file directory id)))
                 (when (file-exists-p file) (puthash file t files)))) members)
    (when (file-directory-p snapshots)
      (dolist (file (directory-files snapshots t
                                     (concat "\\`" gptel-agent-session-store--id-regexp "--"
                                             gptel-agent-session-store--id-regexp "\\.eld\\'")))
        (when (gethash (substring (file-name-nondirectory file) 0 64) members)
          (puthash file t files))))
    (let (result)
      (maphash (lambda (file _) (push file result)) files)
      result)))

(defun gptel-agent-session-store--forget (directory)
  "Invalidate cached headers for DIRECTORY after an external mutation."
  (when (equal (gptel-agent-session-store--directory directory)
               gptel-agent-session-store--cache-directory)
    (gptel-agent-session-store--reset)))

(defun gptel-agent-session-store--reset ()
  "Forget the in-memory storage index."
  (setq gptel-agent-session-store--cache-directory nil
        gptel-agent-session-store--cache-input nil
        gptel-agent-session-store--cache nil))

(provide 'gptel-agent-session-store)
;;; gptel-agent-session-store.el ends here
