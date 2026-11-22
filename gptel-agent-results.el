;;; gptel-agent-results.el --- Bounded saved tool results -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Account for active spools and completed result files together.  Only files
;; created through the private storage helpers are tracked or deleted here.

;;; Code:

(require 'cl-lib)

(declare-function gptel-agent--check-tool-result "gptel-agent-tools" (file identity))
(declare-function gptel-agent--write-tool-result "gptel-agent-tools" (file identity start end &optional append))
(declare-function gptel-agent--delete-tool-result "gptel-agent-tools" (file identity))

(defcustom gptel-agent-result-file-limit (* 8 1024 1024)
  "Maximum UTF-8 bytes saved per tool result, including a running Bash log.
Keep a prefix and explicitly report incomplete coverage after this limit.
Captured at Bash launch, or when other results are created; zero retains
no output bytes."
  :type 'natnum :group 'gptel-agent)

(defcustom gptel-agent-result-total-limit (* 64 1024 1024)
  "Maximum UTF-8 bytes retained across saved results and active Bash logs.
Old completed results are evicted first.  Active spools and results in use
by an explorer are protected; when they fill the budget, new output is
truncated instead.  This is an Emacs-instance limit, shared by all chats."
  :type 'natnum :group 'gptel-agent)

(defcustom gptel-agent-result-count-limit 128
  "Maximum number of active and completed saved result files.
Old completed results are evicted to admit new logs.  If all slots are
protected, persistence fails explicitly.  Zero disables saved results."
  :type 'natnum :group 'gptel-agent)

(defcustom gptel-agent-result-retention-seconds 3600
  "Idle lifetime in seconds of completed saved results.
Pruning runs on access and about once a minute while results exist.
Active spools and files pinned by a running explorer do not expire.
Must be a positive finite number.  Killing the owning conversation removes
its files immediately unless an explorer is still using them."
  :type 'number :group 'gptel-agent)

(defvar gptel-agent--saved-tool-results (make-hash-table :test #'equal)
  "Finished result paths and their extraction contracts.")
(defvar gptel-agent--result-resources (make-hash-table :test #'equal)
  "Private result paths and their resource accounting plists.
Entries hold filesystem identity, owning conversation, bytes, file limit,
active state, explorer pins and access time.  Never retain request FSMs.")
(defvar gptel-agent--results-timer nil
  "Timer that expires idle completed results.")

(defun gptel-agent--results-validate-limits ()
  "Reject invalid settings before acquiring resources."
  (dolist (option '(gptel-agent-result-file-limit
                    gptel-agent-result-total-limit
                    gptel-agent-result-count-limit))
    (unless (natnump (symbol-value option))
      (error "%s must be a nonnegative integer" option)))
  (unless (and (numberp gptel-agent-result-retention-seconds)
               (> gptel-agent-result-retention-seconds 0)
               (< gptel-agent-result-retention-seconds 1.0e+INF))
    (error "gptel-agent-result-retention-seconds must be positive and finite")))

(defun gptel-agent--result-resource (file)
  "Return the resource accounting entry for FILE, if any."
  (gethash file gptel-agent--result-resources))

(defun gptel-agent--result-protected-p (entry)
  "Whether ENTRY is an active spool or has a live explorer pin."
  (setf (plist-get entry :pins)
        (cl-delete-if-not #'buffer-live-p (plist-get entry :pins)))
  (or (plist-get entry :active) (plist-get entry :pins)))

(defun gptel-agent--results-bytes ()
  "Return total bytes accounted for in all result files."
  (let ((total 0))
    (maphash (lambda (_file entry) (cl-incf total (plist-get entry :bytes)))
             gptel-agent--result-resources)
    total))

(defun gptel-agent--result-forget (file)
  "Release FILE and its contract, deleting only the original private file.
Callers must ensure FILE is no longer in use.  Changed or unrelated paths
are left untouched by the secure storage helper.  If deletion of the original
file fails, keep charging its space and retry during pruning."
  (when-let* ((entry (gethash file gptel-agent--result-resources)))
    (let ((identity (plist-get entry :identity)))
      (gptel-agent--delete-tool-result file identity)
      (if (condition-case nil
              (progn (gptel-agent--check-tool-result file identity) t)
            (file-error nil))
          (progn
            ;; A write may have failed after writing part of its bounded chunk.
            ;; Keep charging actual bytes if cleanup also fails.
            (setf (plist-get entry :bytes)
                  (file-attribute-size (file-attributes file 'integer))
                  (plist-get entry :active) nil
                  (plist-get entry :delete-pending) t)
            (message "Could not delete saved tool result; retaining its quota: %s" file))
        (remhash file gptel-agent--result-resources))))
  (remhash file gptel-agent--saved-tool-results)
  (when (and (zerop (hash-table-count gptel-agent--result-resources))
             (timerp gptel-agent--results-timer))
    (cancel-timer gptel-agent--results-timer)
    (setq gptel-agent--results-timer nil)))

(defun gptel-agent--results-prune (&optional needed-bytes needed-files)
  "Expire idle files, then make room for NEEDED-BYTES and NEEDED-FILES.
Evict oldest completed unpinned results first.  Active or pinned files are
never removed here, so callers must check whether admission succeeded."
  (gptel-agent--results-validate-limits)
  (let ((now (float-time)) candidates expired)
    (maphash
     (lambda (file entry)
       (unless (gptel-agent--result-protected-p entry)
         (if (or (plist-get entry :delete-pending)
                 (not (buffer-live-p (plist-get entry :owner)))
                 (>= (- now (plist-get entry :access))
                     gptel-agent-result-retention-seconds))
             (push file expired)
           (push (cons file (plist-get entry :access)) candidates))))
     gptel-agent--result-resources)
    (dolist (file expired) (gptel-agent--result-forget file))
    (setq candidates (sort candidates (lambda (a b) (< (cdr a) (cdr b)))))
    (while (and candidates
                (or (> (+ (or needed-files 0)
                          (hash-table-count gptel-agent--result-resources))
                       gptel-agent-result-count-limit)
                    (> (+ (or needed-bytes 0) (gptel-agent--results-bytes))
                       gptel-agent-result-total-limit)))
      (gptel-agent--result-forget (caar candidates))
      (setq candidates (cdr candidates)))))

(defun gptel-agent--results-sweep ()
  "Timer entry point for expiring completed results."
  (condition-case err
      (gptel-agent--results-prune)
    (error (message "Could not prune saved tool results: %s"
                    (error-message-string err)))))

(defun gptel-agent--result-register (file identity owner &optional limit)
  "Track newly created empty FILE with IDENTITY and live conversation OWNER.
LIMIT overrides the captured per-file byte limit.  Signal if no file slot
is available; the caller must delete its just-created empty file on error."
  (gptel-agent--results-prune 0 1)
  (unless (buffer-live-p owner) (error "Saved result owner is unavailable"))
  (when (gethash file gptel-agent--result-resources)
    (error "Saved result is already registered"))
  (unless (< (hash-table-count gptel-agent--result-resources)
             gptel-agent-result-count-limit)
    (error "Saved result file quota is full; active logs or explorers retain all slots"))
  (unless (natnump (or limit gptel-agent-result-file-limit))
    (error "Saved result byte limit must be a nonnegative integer"))
  (gptel-agent--check-tool-result file identity)
  (unless (zerop (file-attribute-size (file-attributes file 'integer)))
    (error "A new saved result must be empty"))
  (let ((entry (list :identity identity :owner owner :bytes 0
                     :limit (or limit gptel-agent-result-file-limit)
                     :active t :pins nil :access (float-time) :truncated nil :capture-note nil
                     :delete-pending nil)))
    (puthash file entry gptel-agent--result-resources)
    (with-current-buffer owner
      (add-hook 'kill-buffer-hook #'gptel-agent--results-owner-cleanup nil t))
    (unless (timerp gptel-agent--results-timer)
      (setq gptel-agent--results-timer
            (run-at-time 60 60 #'gptel-agent--results-sweep)))
    entry))

(defun gptel-agent--result-prefix (text bytes)
  "Return the longest whole-character prefix of TEXT fitting UTF-8 BYTES."
  (let ((low 0) (high (length text)))
    (while (< low high)
      (let* ((mid (/ (+ low high 1) 2))
             (size (string-bytes
                    (encode-coding-string (substring text 0 mid) 'utf-8-unix))))
        (if (<= size bytes) (setq low mid) (setq high (1- mid)))))
    (substring text 0 low)))

(defun gptel-agent--result-append (file text)
  "Append a bounded UTF-8 prefix of TEXT to tracked FILE.
Return (:bytes TOTAL :truncated FLAG :written CHARACTERS).  Once a prefix
is truncated, later chunks are discarded even if capacity becomes free;
the result can therefore never contain an undisclosed gap."
  (let* ((entry (or (gethash file gptel-agent--result-resources)
                    (error "Saved result is no longer registered")))
         (identity (plist-get entry :identity)))
    ;; Validate even after reaching the cap: a substituted file must not be
    ;; published or read merely because no more bytes need to be written.
    (gptel-agent--check-tool-result file identity)
    (unless (plist-get entry :active) (error "Saved result is already complete"))
    (let* ((bytes (string-bytes (encode-coding-string text 'utf-8-unix)))
           (wanted (min bytes (max 0 (- (plist-get entry :limit)
                                        (plist-get entry :bytes)))))
           (written 0))
      (unless (plist-get entry :truncated)
        (gptel-agent--results-prune wanted 0)
        (let* ((room (min wanted (max 0 (- gptel-agent-result-total-limit
                                          (gptel-agent--results-bytes)))))
               (prefix (if (= room bytes) text
                         (gptel-agent--result-prefix text room)))
               (stored (string-bytes (encode-coding-string prefix 'utf-8-unix))))
          (when (> stored 0)
            (let ((coding-system-for-write 'utf-8-unix)
                  (write-region-inhibit-fsync t))
              (gptel-agent--write-tool-result file identity prefix nil t))
            (cl-incf (plist-get entry :bytes) stored))
          (setq written (length prefix))
          (when (< written (length text))
            (setf (plist-get entry :truncated) t))))
      (setf (plist-get entry :access) (float-time))
      (list :bytes (plist-get entry :bytes)
            :truncated (plist-get entry :truncated) :written written))))

(defun gptel-agent--result-publish (file query tool chars lines &optional truncated capture-note)
  "Finish FILE and register its QUERY contract for TOOL, CHARS and LINES.
CHARS and LINES describe observed output, including discarded output.
TRUNCATED marks a discarded suffix.  CAPTURE-NOTE describes other incomplete
capture, such as a command stopped by its execution deadline."
  (let ((entry (or (gethash file gptel-agent--result-resources)
                   (error "Saved result is no longer registered"))))
    (gptel-agent--check-tool-result file (plist-get entry :identity))
    (setf (plist-get entry :active) nil
          (plist-get entry :access) (float-time)
          (plist-get entry :capture-note) capture-note
          (plist-get entry :truncated) (or truncated (plist-get entry :truncated)))
    (puthash file
             (list :query query :tool tool :chars chars :lines lines
                   :owner (plist-get entry :owner)
                   :identity (plist-get entry :identity)
                   :bytes (plist-get entry :bytes)
                   :capture-note capture-note
                   :truncated (plist-get entry :truncated))
             gptel-agent--saved-tool-results)))

(defun gptel-agent--result-touch (file)
  "Check FILE's identity and renew its idle lifetime, returning its entry."
  (gptel-agent--results-prune)
  (let ((entry (or (gethash file gptel-agent--result-resources)
                   (error "Saved result expired or was evicted; rerun the producing tool"))))
    (gptel-agent--check-tool-result file (plist-get entry :identity))
    (setf (plist-get entry :access) (float-time))
    entry))

(defun gptel-agent--result-pin (file buffer)
  "Protect FILE while the explorer in live BUFFER uses it."
  (unless (buffer-live-p buffer) (error "Result explorer buffer is unavailable"))
  (let ((entry (gptel-agent--result-touch file)))
    (cl-pushnew buffer (plist-get entry :pins))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook #'gptel-agent--results-unpin-buffer nil t))))

(defun gptel-agent--result-unpin (file buffer)
  "Release BUFFER's pin on FILE and apply any deferred cleanup."
  (when-let* ((entry (gethash file gptel-agent--result-resources)))
    (setf (plist-get entry :pins) (delq buffer (plist-get entry :pins))
          (plist-get entry :access) (float-time))
    (when (and (not (buffer-live-p (plist-get entry :owner)))
               (not (gptel-agent--result-protected-p entry)))
      (gptel-agent--result-forget file))))

(defun gptel-agent--results-unpin-buffer ()
  "Release all pins held by the current explorer buffer."
  (let (files)
    (maphash (lambda (file entry)
               (when (memq (current-buffer) (plist-get entry :pins))
                 (push file files)))
             gptel-agent--result-resources)
    (dolist (file files) (gptel-agent--result-unpin file (current-buffer)))))
(put 'gptel-agent--results-unpin-buffer 'permanent-local-hook t)

(defun gptel-agent--results-owner-cleanup ()
  "Release the closing conversation's files, deferring pinned result deletion."
  (let (files)
    (maphash
     (lambda (file entry)
       (when (eq (plist-get entry :owner) (current-buffer))
         ;; A kill-buffer hook runs before buffer-live-p becomes nil.
         (setf (plist-get entry :owner) nil)
         (when-let* ((metadata (gethash file gptel-agent--saved-tool-results)))
           (setf (plist-get metadata :owner) nil))
         (unless (cl-some #'buffer-live-p (plist-get entry :pins))
           (push file files))))
     gptel-agent--result-resources)
    (dolist (file files) (gptel-agent--result-forget file))))
(put 'gptel-agent--results-owner-cleanup 'permanent-local-hook t)

(defun gptel-agent--results-exit-cleanup ()
  "Delete only tracked private result files during orderly Emacs exit."
  (let (files)
    (maphash (lambda (file _entry) (push file files)) gptel-agent--result-resources)
    (dolist (file files) (gptel-agent--result-forget file))))
(add-hook 'kill-emacs-hook #'gptel-agent--results-exit-cleanup)

(provide 'gptel-agent-results)
;;; gptel-agent-results.el ends here
