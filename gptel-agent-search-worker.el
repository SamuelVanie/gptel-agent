;;; gptel-agent-search-worker.el --- Isolated search fallbacks -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Loaded by a clean batch Emacs, never in the interactive search path.
;; Keep this file independent of gptel and the user's configuration.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defun gptel-agent-search-worker--record (value)
  "Write one JSON protocol record for VALUE."
  (princ (json-encode value))
  (princ "\n"))

(defun gptel-agent-search-worker-walk (root depth limit)
  "Enumerate ROOT to DEPTH, examining at most LIMIT directory entries.
Emit lstat modification times and followed types, without following directory
symlinks.  Bound even one wide directory's allocation before sorting."
  (let ((pending (list (cons root 1))) (count 0) (status "complete"))
    (condition-case err
        (catch 'limited
          (while pending
            (pcase-let ((`(,directory . ,level) (pop pending)))
              (dolist (path (directory-files directory t directory-files-no-dot-files-regexp
                                             t (1+ (- limit count))))
                (when (>= count limit)
                  (setq status "incomplete: traversal entry limit")
                  (throw 'limited nil))
                (cl-incf count)
                (let* ((attrs (file-attributes path 'integer))
                       (link (file-symlink-p path))
                       (kind (cond ((file-directory-p path) "directory")
                                   ((file-regular-p path) "file")
                                   ((file-exists-p path) "special file")
                                   (link "broken symlink")
                                   (t (error "Entry disappeared: %s" path)))))
                  (unless (and (equal (file-name-nondirectory path) ".git")
                               (equal kind "directory"))
                    (gptel-agent-search-worker--record
                     `((entry . [,path ,kind ,link ,(car (time-convert (file-attribute-modification-time attrs) 1000000000))])))
                    (when (and (equal kind "directory") (not link)
                               (or (null depth) (< level depth)))
                      (push (cons path (1+ level)) pending))))))))
      (error (setq status (concat "incomplete: traversal failed: " (error-message-string err)))))
    (gptel-agent-search-worker--record `((status . ,status) (examined . ,count)))))

(defun gptel-agent-search-worker-grep (regex files context)
  "Search FILES for native Emacs REGEX with CONTEXT, using bounded reads.
Write filename-NUL-line records compatible with grep -H -n -Z.  Skip lines
over 65536 bytes with a failing exit status, rather than matching fragments."
  (let ((case-fold-search nil) (coding-system-for-read 'no-conversion)
        (coding-system-for-write 'utf-8-unix) skipped)
    (condition-case err
        (progn
          (string-match regex "")
          (dolist (file files)
            (let ((offset 0) (carry "") (line 1) (before nil)
                  (after 0) (last-emitted 0) skipping eof)
              (cl-labels
                  ((emit (number text match)
                     (when (> number last-emitted)
                       (princ file) (princ "\0")
                       (princ (format "%d%c%s\n" number (if match ?: ?-) text))
                       (setq last-emitted number)))
                   (consume ()
                     (if skipping
                         (setq skipped t before nil after 0)
                       (let* ((text (decode-coding-string carry 'utf-8-unix))
                              (match (string-match-p regex text)))
                         (cond (match
                                (dolist (item (reverse before)) (emit (car item) (cdr item) nil))
                                (emit line text t)
                                (setq after context))
                               ((> after 0) (emit line text nil) (cl-decf after)))
                         (push (cons line text) before)
                         (when (> (length before) context)
                           (setq before (cl-subseq before 0 context)))))
                     (setq carry "" skipping nil)
                     (cl-incf line)))
                (while (not eof)
                  (let* ((bytes (with-temp-buffer
                                  (set-buffer-multibyte nil)
                                  (insert-file-contents-literally file nil offset (+ offset 65536))
                                  (buffer-string)))
                         (size (length bytes)) (pos 0))
                    (setq eof (< size 65536))
                    (cl-incf offset size)
                    (while (< pos size)
                      (let* ((newline (string-match "\n" bytes pos)) (end (or newline size)))
                        (unless skipping
                          (if (> (+ (length carry) (- end pos)) 65536)
                              (setq carry "" skipping t)
                            (setq carry (concat carry (substring bytes pos end)))))
                        (when newline (consume))
                        (setq pos (if newline (1+ newline) end))))
                    (when (and eof (or skipping (not (string-empty-p carry)))) (consume)))))))
          (when skipped
            (princ "Skipped lines exceeding 65536 bytes" 'external-debugging-output)
            (kill-emacs 2)))
      (error
       (princ (error-message-string err) 'external-debugging-output)
       (kill-emacs 2)))))

(provide 'gptel-agent-search-worker)
;;; gptel-agent-search-worker.el ends here
