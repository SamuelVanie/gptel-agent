;;; gptel-agent-path.el --- Filesystem path contracts -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'subr-x)

(define-error 'gptel-agent-path-error "Invalid filesystem target")

(defun gptel-agent-path-type (path)
  "Return the filesystem type of PATH, following symlinks."
  (setq path (directory-file-name path))
  (cond ((file-directory-p path) "directory")
        ((file-regular-p path) "file")
        ((file-exists-p path) "special file")
        ((file-symlink-p path) "broken symlink")
        (t "missing")))

(defun gptel-agent-path--error (path expected actual guidance)
  "Report PATH's EXPECTED and ACTUAL types with recovery GUIDANCE."
  (signal 'gptel-agent-path-error
          (list (format "Path: %s; expected: %s; actual: %s. %s"
                        path expected actual guidance))))

(defun gptel-agent-path-resolve (path expected &optional must-exist)
  "Resolve PATH and validate EXPECTED type without changing the filesystem.
EXPECTED is `file', `directory', or `any'.  MUST-EXIST requires existence.
File syntax is checked before canonicalization can hide a trailing slash."
  (unless (and (stringp path) (not (string-empty-p path)))
    (gptel-agent-path--error (format "%S" path) expected "empty or invalid path"
                            "Supply a nonempty path; use PathInfo to inspect a target."))
  (let* ((resolved (expand-file-name path))
         (actual (gptel-agent-path-type resolved)))
    (when (and (eq expected 'file) (equal actual "directory"))
      (gptel-agent-path--error
       resolved "file" actual
       "Changing file content cannot fix this. Use PathInfo or Glob to inspect it and correct file_path. Rmdir can remove an accidental empty directory."))
    (when (and (eq expected 'file)
               (or (string-suffix-p "/" path)
                   (member (file-name-nondirectory path) '("." ".."))))
      (gptel-agent-path--error
       resolved "file path without a trailing slash or final . / .." actual
       "Supply the complete file_path including its filename. No directories were created."))
    (when (and (not (eq expected 'any))
               (not (member actual (list "missing" (symbol-name expected)))))
      (gptel-agent-path--error resolved expected actual
                              "Use PathInfo to inspect the target and choose a path of the required type."))
    (when (and must-exist (equal actual "missing"))
      (gptel-agent-path--error
       resolved expected actual
       (if (eq expected 'file)
           "Use Write to create a new file with complete content; Read, replacement and Insert require an existing file."
         "Choose an existing directory, or use Mkdir to create it recursively.")))
    ;; A missing leaf can still have an existing non-directory ancestor.
    (let ((parent (file-name-directory (directory-file-name resolved))))
      (while (and parent (not (equal parent (file-name-directory (directory-file-name parent))))
                  (equal (gptel-agent-path-type parent) "missing"))
        (setq parent (file-name-directory (directory-file-name parent))))
      (when (and parent (not (file-directory-p parent)))
        (gptel-agent-path--error parent "parent directory" (gptel-agent-path-type parent)
                                "A parent component blocks this path. Inspect it with PathInfo and correct the target.")))
    resolved))

(provide 'gptel-agent-path)
;;; gptel-agent-path.el ends here
