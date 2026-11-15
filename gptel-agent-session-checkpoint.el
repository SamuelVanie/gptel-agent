;;; gptel-agent-session-checkpoint.el --- Bounded family checkpoints -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Stage immutable chat versions incrementally.  Only a complete family
;; manifest makes them visible.  Later edits form the next generation; a
;; continuously streaming family need not become quiet before publishing.
;;; Code:
(require 'cl-lib)
(require 'gptel-agent-session-store)

(defvar gptel-agent-session--transactions (make-hash-table :test #'equal))
(defvar gptel-agent-session--family-members (make-hash-table :test #'equal))
(defvar gptel-agent-session--member-roots (make-hash-table :test #'equal))
(defvar gptel-agent-session--membership-revisions (make-hash-table :test #'equal))
(defvar gptel-agent-session--queued (make-hash-table :test #'equal))
(defvar gptel-agent-session--write-file nil)
(defvar-local gptel-agent-session--revision 0)
(defvar-local gptel-agent-session--staged-revision 0)
(put 'gptel-agent-session--revision 'permanent-local t)
(put 'gptel-agent-session--staged-revision 'permanent-local t)

(cl-defstruct (gptel-agent-session-checkpoint--transaction
               (:constructor gptel-agent-session-checkpoint--make))
  root generation entries required pending old membership-revision (attempt 0) error)

(defvar gptel-agent-session-directory)
(defvar gptel-agent-session--buffers)
(defvar gptel-agent-session--dirty)
(defvar gptel-agent-session--queue)
(defvar gptel-agent-session--queue-tail)
(defvar gptel-agent-session--id)
(defvar gptel-agent-session--parent-id)
(defvar gptel-agent-session--last-save)
(defvar gptel-agent-session--last-attempt)
(defvar gptel-agent-session--error)
(defvar gptel-agent-session-batch-size)
(defvar gptel-agent-session-time-budget)
(defvar gptel-agent-session-save-interval)
(declare-function gptel-agent-session--new-id "gptel-agent-session")
(declare-function gptel-agent-session--snapshot "gptel-agent-session")
(declare-function gptel-agent-session--write "gptel-agent-session")
(declare-function gptel-agent-session--assert-writer "gptel-agent-session")
(declare-function gptel-agent-session--eligible-p "gptel-agent-session")
(declare-function gptel-agent-session--ensure-timer "gptel-agent-session")
(declare-function gptel-agent-session--stop-if-clean "gptel-agent-session")

(defun gptel-agent-session-checkpoint--reset ()
  "Release in-memory generations after flushing or before loading a store."
  (setq gptel-agent-session--transactions (make-hash-table :test #'equal)
        gptel-agent-session--family-members (make-hash-table :test #'equal)
        gptel-agent-session--member-roots (make-hash-table :test #'equal)
        gptel-agent-session--membership-revisions (make-hash-table :test #'equal)
        gptel-agent-session--queued (make-hash-table :test #'equal)))

(defun gptel-agent-session-checkpoint--load ()
  "Index committed family membership once on enabling session mode."
  (gptel-agent-session-checkpoint--reset)
  (let ((index (gptel-agent-session-store--index gptel-agent-session-directory t)))
    (maphash
     (lambda (id root)
       (puthash id root gptel-agent-session--member-roots)
       (unless (gethash root gptel-agent-session--family-members)
         (puthash root (make-hash-table :test #'equal) gptel-agent-session--family-members))
       (puthash id t (gethash root gptel-agent-session--family-members)))
     (plist-get index :owners))))

(defun gptel-agent-session-checkpoint--register (buffer)
  "Update BUFFER's family membership without scanning unrelated chats."
  (with-current-buffer buffer
    (let* ((id gptel-agent-session--id)
           (old (gethash id gptel-agent-session--member-roots))
           (root (if gptel-agent-session--parent-id
                     (or (gethash gptel-agent-session--parent-id gptel-agent-session--member-roots)
                         old
                         gptel-agent-session--parent-id)
                   (or old id))))
      (unless (gethash root gptel-agent-session--family-members)
        (puthash root (make-hash-table :test #'equal) gptel-agent-session--family-members))
      ;; A new child is initially enabled as an ordinary gptel buffer before
      ;; its owner is attached.  Move that temporary root and its descendants.
      (when (and old (not (equal old root)))
        (if (equal old id)
            (progn
              (maphash (lambda (member _)
                         (puthash member root gptel-agent-session--member-roots)
                         (puthash member t (gethash root gptel-agent-session--family-members)))
                       (gethash old gptel-agent-session--family-members))
              (remhash old gptel-agent-session--family-members))
          (remhash id (gethash old gptel-agent-session--family-members))))
      (puthash id root gptel-agent-session--member-roots)
      (unless (gethash id (gethash root gptel-agent-session--family-members))
        (puthash root (1+ (gethash root gptel-agent-session--membership-revisions 0))
                 gptel-agent-session--membership-revisions))
      (when (and old (not (equal old root)))
        (puthash root (1+ (gethash root gptel-agent-session--membership-revisions 0))
                 gptel-agent-session--membership-revisions))
      (puthash id t (gethash root gptel-agent-session--family-members))
      root)))

(defun gptel-agent-session-checkpoint--enqueue (item)
  "Queue BUFFER or family ID ITEM once, in constant time."
  (unless (gethash item gptel-agent-session--queued)
    (puthash item t gptel-agent-session--queued)
    (let ((cell (list item)))
      (if gptel-agent-session--queue-tail
          (setcdr gptel-agent-session--queue-tail cell)
        (setq gptel-agent-session--queue cell))
      (setq gptel-agent-session--queue-tail cell)))
  (gptel-agent-session--ensure-timer))

(defun gptel-agent-session-checkpoint--extend (transaction)
  "Include any newly created members in TRANSACTION before publication."
  (let* ((root (gptel-agent-session-checkpoint--transaction-root transaction))
         (revision (gethash root gptel-agent-session--membership-revisions 0)))
    (unless (eql revision (gptel-agent-session-checkpoint--transaction-membership-revision transaction))
      (let (added)
        (maphash
         (lambda (id _)
           (unless (gethash id (gptel-agent-session-checkpoint--transaction-required transaction))
             (puthash id t (gptel-agent-session-checkpoint--transaction-required transaction))
             (push id added)))
         (gethash root gptel-agent-session--family-members))
        (setf (gptel-agent-session-checkpoint--transaction-pending transaction)
              (nconc (gptel-agent-session-checkpoint--transaction-pending transaction) (nreverse added))))
      (setf (gptel-agent-session-checkpoint--transaction-membership-revision transaction) revision))))

(defun gptel-agent-session-checkpoint--begin (buffer)
  "Return BUFFER's current generation, creating one from committed versions."
  (let* ((root (gptel-agent-session-checkpoint--register buffer))
         (existing (gethash root gptel-agent-session--transactions)))
    (or existing
        (let* ((old (gptel-agent-session-store--family gptel-agent-session-directory root))
               (entries (make-hash-table :test #'equal))
               (required (make-hash-table :test #'equal)) pending
               (transaction (gptel-agent-session-checkpoint--make
                             :root root :generation (gptel-agent-session--new-id)
                             :entries entries :required required :old old
                             :membership-revision (gethash root gptel-agent-session--membership-revisions 0))))
          (dolist (entry old) (puthash (plist-get entry :id) entry entries))
          (maphash
           (lambda (id _)
             (puthash id t required)
             (let ((entry (gethash id entries)) (live (gethash id gptel-agent-session--buffers)))
               (when (or (null entry)
                         (not (string-prefix-p "snapshots/" (plist-get entry :file)))
                         (and (buffer-live-p live) (gethash live gptel-agent-session--dirty)))
                 (push id pending))))
           (gethash root gptel-agent-session--family-members))
          (setf (gptel-agent-session-checkpoint--transaction-pending transaction) (nreverse pending))
          (puthash root transaction gptel-agent-session--transactions)
          transaction))))

(defun gptel-agent-session-checkpoint--stage (transaction id)
  "Write one immutable version for ID into TRANSACTION, preserving later edits."
  (let* ((buffer (gethash id gptel-agent-session--buffers))
         (live (gptel-agent-session--eligible-p buffer))
         (revision (and live (buffer-local-value 'gptel-agent-session--revision buffer)))
         (root (gptel-agent-session-checkpoint--transaction-root transaction))
         (old (gethash id (gptel-agent-session-checkpoint--transaction-entries transaction)))
         (file (gptel-agent-session-store--version-file
                gptel-agent-session-directory id (gptel-agent-session--new-id)))
         (gptel-agent-session--write-file file)
         record header)
    (if live
        (with-current-buffer buffer
          (setq gptel-agent-session--last-attempt (float-time)
                record (gptel-agent-session--snapshot))
          (when (equal id root) (setf (plist-get record :parent) nil))
          (gptel-agent-session--write record)
          (setq header (list :id id :parent (plist-get record :parent)
                             :name (truncate-string-to-width (plist-get record :name) 160)
                             :updated (plist-get record :updated)))
          (setq gptel-agent-session--last-save (float-time) gptel-agent-session--error nil
                gptel-agent-session--staged-revision revision)
          (when (= revision gptel-agent-session--revision)
            (remhash buffer gptel-agent-session--dirty)))
      ;; A closed legacy chat still belongs to its family.  Migrate its data
      ;; without opening a buffer or replaying any request.
      (unless old (error "No live buffer or committed version for family member %s" id))
      (let ((coding-system-for-read 'utf-8-emacs) (read-circle nil))
        (with-temp-buffer
          (insert-file-contents (expand-file-name (plist-get old :file) gptel-agent-session-directory))
          (goto-char (point-min)) (read (current-buffer))
          (setq record (read (current-buffer)))))
      (when (equal id root) (setf (plist-get record :parent) nil))
      (gptel-agent-session--write record)
      (setq header (copy-sequence old))
      (setf (plist-get header :parent) (plist-get record :parent)))
    (setf (plist-get header :file)
          (concat "snapshots/" (file-name-nondirectory file)))
    (puthash id header (gptel-agent-session-checkpoint--transaction-entries transaction))
    (let ((pending (gptel-agent-session-checkpoint--transaction-pending transaction)))
      (setf (gptel-agent-session-checkpoint--transaction-pending transaction)
            (if (equal id (car pending)) (cdr pending) (delete id pending))))
    t))

(defun gptel-agent-session-checkpoint--publish (transaction)
  "Publish TRANSACTION if all known family members have a staged version."
  (gptel-agent-session-checkpoint--extend transaction)
  (unless (gptel-agent-session-checkpoint--transaction-pending transaction)
    (let ((root (gptel-agent-session-checkpoint--transaction-root transaction))
          (inhibit-quit t) entries)
      (maphash (lambda (_ entry) (push entry entries))
               (gptel-agent-session-checkpoint--transaction-entries transaction))
      (gptel-agent-session--assert-writer)
      (gptel-agent-session-store--commit
       gptel-agent-session-directory root
       (gptel-agent-session-checkpoint--transaction-generation transaction) entries)
      (remhash root gptel-agent-session--transactions)
      ;; Only changed obsolete versions are collected, never a store-wide scan
      ;; on each save.  A crash during cleanup leaves harmless unused files.
      (dolist (file (gptel-agent-session-store--obsolete
                     gptel-agent-session-directory
                     (gptel-agent-session-checkpoint--transaction-old transaction) entries))
        (ignore-errors (delete-file file)))
      (maphash (lambda (id _)
                 (when-let* ((buffer (gethash id gptel-agent-session--buffers))
                             ((buffer-live-p buffer)))
                   (with-current-buffer buffer (setq gptel-agent-session--last-save (float-time)))
                   (when (gethash buffer gptel-agent-session--dirty)
                     (gptel-agent-session-checkpoint--enqueue buffer))))
               (gethash root gptel-agent-session--family-members))
      t)))

(defun gptel-agent-session-checkpoint--save-buffer (buffer)
  "Stage BUFFER before it dies; keep the transaction even after unregistering."
  (let ((transaction (gptel-agent-session-checkpoint--begin buffer)))
    (condition-case err
        (progn
          (gptel-agent-session-checkpoint--stage transaction
                                                 (buffer-local-value 'gptel-agent-session--id buffer))
          (unless (gptel-agent-session-checkpoint--publish transaction)
            (gptel-agent-session-checkpoint--enqueue
             (gptel-agent-session-checkpoint--transaction-root transaction)))
          t)
      (error
       (message "Family checkpoint failed: %s" (error-message-string err))
       (gptel-agent-session-checkpoint--enqueue
        (gptel-agent-session-checkpoint--transaction-root transaction))
       nil))))

(defun gptel-agent-session-checkpoint--drain (&optional all)
  "Stage a bounded batch, or flush entry revisions with ALL.
Failures remain queued without retrying in a tight loop."
  (let ((started (float-time)) (examined 0) (saved 0)
        (blocked (make-hash-table :test #'equal))
        (targets (make-hash-table :test #'eq)) deferred)
    (when all
      (maphash (lambda (buffer _)
                 (when (buffer-live-p buffer)
                   (puthash buffer (buffer-local-value 'gptel-agent-session--revision buffer) targets)))
               gptel-agent-session--dirty))
    (cl-labels ((defer (item)
                  (unless (gethash item gptel-agent-session--queued)
                    (puthash item t gptel-agent-session--queued)
                    (push item deferred))))
      (unwind-protect
          (while (and gptel-agent-session--queue
                      (or all (and (< examined (max 1 gptel-agent-session-batch-size))
                                   (or (= examined 0)
                                       (< (- (float-time) started) gptel-agent-session-time-budget)))))
            (let* ((item (pop gptel-agent-session--queue))
                   (buffer (and (bufferp item) item))
                   (root (if buffer
                             (and (buffer-live-p buffer)
                                  (gethash (buffer-local-value 'gptel-agent-session--id buffer)
                                           gptel-agent-session--member-roots))
                           item))
                   (transaction (gethash root gptel-agent-session--transactions)))
              (unless gptel-agent-session--queue (setq gptel-agent-session--queue-tail nil))
              (remhash item gptel-agent-session--queued)
              (cond
               ((gethash root blocked) (defer item))
               ((and buffer (or (not (gethash buffer gptel-agent-session--dirty))
                                (not (gptel-agent-session--eligible-p buffer))))
                (remhash buffer gptel-agent-session--dirty))
               ((and (null transaction) (null buffer)))
               ((and (not all) buffer (null transaction)
                     (< (- started (max (buffer-local-value 'gptel-agent-session--last-save buffer)
                                        (buffer-local-value 'gptel-agent-session--last-attempt buffer)))
                        gptel-agent-session-save-interval))
                (cl-incf examined) (defer item))
               (t
                (cl-incf examined)
                (condition-case err
                    (progn
                      (unless transaction
                        (setq transaction (gptel-agent-session-checkpoint--begin buffer)
                              root (gptel-agent-session-checkpoint--transaction-root transaction)))
                      (if (and (not all)
                               (> (gptel-agent-session-checkpoint--transaction-attempt transaction) 0)
                               (< (- started (gptel-agent-session-checkpoint--transaction-attempt transaction))
                                  gptel-agent-session-save-interval))
                          (progn (puthash root t blocked) (defer root))
                        (gptel-agent-session-checkpoint--extend transaction)
                        (when-let* ((id (car (gptel-agent-session-checkpoint--transaction-pending transaction))))
                          (gptel-agent-session-checkpoint--stage transaction id)
                          (cl-incf saved))
                        (if (gptel-agent-session-checkpoint--publish transaction)
                            (when all
                              (let (unfinished)
                                (maphash
                                 (lambda (id _)
                                   (when-let* ((live (gethash id gptel-agent-session--buffers))
                                               (target (gethash live targets))
                                               ((buffer-live-p live)))
                                     (when (< (buffer-local-value 'gptel-agent-session--staged-revision live) target)
                                       (setq unfinished t))))
                                 (gethash root gptel-agent-session--family-members))
                                (unless unfinished (puthash root t blocked))))
                          (gptel-agent-session-checkpoint--enqueue root))))
                  ((error quit)
                   (if transaction
                       (progn
                         (setf (gptel-agent-session-checkpoint--transaction-attempt transaction) (float-time))
                         (unless (equal (error-message-string err)
                                        (gptel-agent-session-checkpoint--transaction-error transaction))
                           (message "Family checkpoint failed: %s" (error-message-string err)))
                         (setf (gptel-agent-session-checkpoint--transaction-error transaction) (error-message-string err))
                         (puthash root t blocked)
                         (defer root))
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (setq gptel-agent-session--last-attempt (float-time))
                         (unless (equal gptel-agent-session--error (error-message-string err))
                           (message "Family checkpoint could not start: %s" (error-message-string err)))
                         (setq gptel-agent-session--error (error-message-string err)))
                       (puthash root t blocked)
                       (defer buffer)))
                   (when (eq (car err) 'quit) (signal 'quit nil))))))))
        (dolist (item (nreverse deferred))
          (remhash item gptel-agent-session--queued)
          (gptel-agent-session-checkpoint--enqueue item))))
    (gptel-agent-session--stop-if-clean)
    saved))

(provide 'gptel-agent-session-checkpoint)
;;; gptel-agent-session-checkpoint.el ends here
