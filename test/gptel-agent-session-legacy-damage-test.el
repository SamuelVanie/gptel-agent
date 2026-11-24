;;; gptel-agent-session-legacy-damage-test.el --- Isolate damaged v1 records -*- lexical-binding: t; -*-

(require 'gptel-agent-session-checkpoint-test)

(defun gptel-agent-session-legacy-damage-test--write (id parent)
  "Write a flat v1 record for ID with PARENT and return its path."
  (gptel-agent-session--write-record
   (list :version 1 :id id :parent parent :name id :buffer-name id
         :directory default-directory :mode 'text-mode :point 1 :updated 0
         :text "Legacy text" :backend "session-test")
   (expand-file-name (concat id ".eld") gptel-agent-session-directory)))

(ert-deftest gptel-agent-session-legacy-isolates-unreadable-and-cyclic-families ()
  (gptel-agent-session-test--with-store
    (let* ((good (gptel-agent-session-test--buffer " *healthy family*" "Manifest text"))
           (unreadable (make-string 64 ?a)) (child (make-string 64 ?b))
           (cycle-a (make-string 64 ?c)) (cycle-b (make-string 64 ?d))
           (cycle-child (make-string 64 ?e)) (legacy (make-string 64 ?f))
           (bad-file (expand-file-name (concat unreadable ".eld") gptel-agent-session-directory)))
      (gptel-agent-session-mode 1)
      (gptel-agent-session-save)
      (let ((good-id (gptel-agent-session-manager-test--id good)) live-child)
        (gptel-agent-session-mode -1)
        (kill-buffer good)
        (with-temp-file bad-file (insert "("))
        (dolist (entry (list (list child unreadable) (list cycle-a cycle-b)
                             (list cycle-b cycle-a) (list cycle-child cycle-b)
                             (list legacy nil)))
          (apply #'gptel-agent-session-legacy-damage-test--write entry))
        (setq live-child (gptel-agent-session-test--buffer " *already open legacy child*" "Live edits"))
        (with-current-buffer live-child
          (setq gptel-agent-session--id child gptel-agent-session--parent-id unreadable))
        (gptel-agent-session-mode 1)
        (should (equal (sort (list good-id legacy) #'string<)
                       (sort (mapcar (lambda (e) (plist-get e :id))
                                     (gptel-agent-session--catalog)) #'string<)))
        (dolist (id (list unreadable child cycle-a cycle-b cycle-child))
          (let ((err (should-error (gptel-agent-session-restore id) :type 'user-error)))
            (should (string-search (if (member id (list unreadable child))
                                       (file-truename bad-file) "Cyclic")
                                   (error-message-string err)))))
        (setq good (gptel-agent-session-restore good-id))
        (gptel-agent-session-checkpoint-test--append good " still works")
        (should (= 1 (gptel-agent-session-save)))
        (should (gethash live-child gptel-agent-session--dirty))
        (should (equal "(" (with-temp-buffer (insert-file-contents bad-file) (buffer-string))))
        (should (equal "Manifest text still works" (gptel-agent-session-checkpoint-test--text good)))
        (should (equal "Legacy text" (with-current-buffer (gptel-agent-session-restore legacy)
                                       (buffer-string))))
        (dolist (id (list unreadable child cycle-a cycle-b cycle-child))
          (should (file-exists-p (expand-file-name (concat id ".eld") gptel-agent-session-directory))))
        ;; Repairing the flat root makes its child reachable on the next scan.
        (gptel-agent-session-legacy-damage-test--write unreadable nil)
        (should (member child (mapcar (lambda (e) (plist-get e :id))
                                      (gptel-agent-session--catalog))))
        (gptel-agent-session-save)
        (should (equal "Live edits" (gptel-agent-session-checkpoint-test--text live-child)))
        (should (equal "Legacy text" (plist-get (gptel-agent-session--read unreadable) :text)))
        (should-not (gethash live-child gptel-agent-session--dirty))
        (should (zerop (hash-table-count gptel-agent-session--transactions)))))))

(ert-deftest gptel-agent-session-legacy-orphan-waits-for-manifest-repair ()
  (gptel-agent-session-test--with-store
    (let* ((root (make-string 64 ?a)) (missing (make-string 64 ?b))
           (orphan (make-string 64 ?c)) (leaf (make-string 64 ?d))
           (file (gptel-agent-session-legacy-damage-test--write orphan missing))
           (leaf-file (gptel-agent-session-legacy-damage-test--write leaf orphan))
           (dir (expand-file-name "families/" gptel-agent-session-directory))
           (manifest (expand-file-name (concat root ".eld") dir)))
      (make-directory dir)
      (with-temp-file manifest (insert "("))
      (gptel-agent-session-mode 1)
      (should-not (gptel-agent-session--catalog))
      (dolist (id (list orphan leaf))
        (should (string-search missing (error-message-string
                                        (should-error (gptel-agent-session-restore id)
                                                      :type 'user-error)))))
      (should (file-exists-p file))
      (should (file-exists-p leaf-file))
      ;; Without an unreadable manifest, the old standalone-orphan fallback
      ;; applies again.  Neither record was pruned or migrated in the interim.
      (delete-file manifest)
      (should (= 2 (length (gptel-agent-session--catalog)))))))

(provide 'gptel-agent-session-legacy-damage-test)
;;; gptel-agent-session-legacy-damage-test.el ends here
