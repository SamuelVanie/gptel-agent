;;; gptel-agent-todo-header-test.el --- Todo header ownership -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent)

(defmacro gptel-agent-todo-header-test--with-chat (&rest body)
  "Run BODY in a chat with a native header and a request position."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "User task\n")
     (setq-local gptel-mode t gptel-use-header-line t
                 gptel--fsm-last
                 (gptel-make-fsm :info (list :position (point-marker))))
     (gptel-use-header-line)
     ,@body))

(defun gptel-agent-todo-header-test--write (&optional status active)
  "Write a todo with STATUS and ACTIVE form through the registered tool."
  (funcall (gptel-tool-function (gptel-get-tool "TodoWrite"))
           (vector (list :content "Verify" :activeForm (or active "Verifying")
                         :status (or status "in_progress")))))

(ert-deftest gptel-agent-todo-header-native-status-and-other-post-hooks-survive ()
  (gptel-agent-todo-header-test--with-chat
    (let* ((header header-line-format)
           (info (gptel-fsm-info gptel--fsm-last))
           (other-post-ran nil)
           (other-post (lambda (&rest _) (setq other-post-ran t))))
      (plist-put info :post (list other-post))
      (should (gptel-agent-todo-header-test--write))
      (should (string-match-p "Task: Verifying" (nth 2 header)))
      (gptel--update-status " Streaming" 'success)
      (gptel-agent-todo-header-test--write nil "Checking")
      (should (eq header-line-format header))
      (should (equal (substring-no-properties (nth 1 header)) " Streaming"))
      (should (string-match-p "Task: Checking" (nth 2 header)))
      (should (= (length (plist-get info :post)) 2))
      (gptel--handle-post gptel--fsm-last)
      (should other-post-ran)
      (should (eq (nth 2 header) gptel--header-line-info))
      (should (equal (substring-no-properties (nth 1 header)) " Streaming"))
      (should (equal (plist-get info :post) (list other-post)))
      (should-not gptel-agent--todo-header-cleanup))))

(ert-deftest gptel-agent-todo-header-custom-specs-stay-intact ()
  (dolist (header '(nil "Custom" custom-header-symbol
                   (:eval "Computed") (some-condition "Yes" "No")
                   ("Backend" "Status")
                   ("Backend" "Status" "Custom controls")
                   ("Backend" . "Tail")))
    (ert-info ((format "Header: %S" header))
      (gptel-agent-todo-header-test--with-chat
        (setq-local header-line-format (copy-tree header))
        (let ((installed header-line-format))
          (dolist (status '("in_progress" "completed"))
            (should (gptel-agent-todo-header-test--write status))
            (should (eq header-line-format installed))
            (should (equal header-line-format header))
            (let ((overlay (cl-find-if
                            (lambda (ov) (overlay-get ov 'gptel-agent--todos))
                            (overlays-in (point-min) (point-max)))))
              (should overlay)
              (should (string-match-p
                       (if (equal status "completed") "✓ Verify" "● Verifying")
                       (overlay-get overlay 'after-string))))
            (gptel--handle-post gptel--fsm-last)
            (should (equal header-line-format header))))))))

(ert-deftest gptel-agent-todo-header-disabled-headers-stay-intact ()
  (dolist (setting '(gptel-mode gptel-use-header-line))
    (gptel-agent-todo-header-test--with-chat
      (set (make-local-variable setting) nil)
      (let ((header header-line-format))
        (should (gptel-agent-todo-header-test--write))
        (should (eq header-line-format header))
        (should (eq (nth 2 header) gptel--header-line-info))
        (should-not (plist-get (gptel-fsm-info gptel--fsm-last) :post))))))

(ert-deftest gptel-agent-todo-header-cleanup-uses-owning-buffer ()
  (gptel-agent-todo-header-test--with-chat
    (let ((owner (current-buffer))
          (fsm gptel--fsm-last))
      (gptel-agent-todo-header-test--write)
      (gptel-agent-todo-header-test--with-chat
        (gptel-agent-todo-header-test--write nil "Other task")
        (let ((other-header (copy-tree header-line-format)))
          (gptel--handle-post fsm)
          (should (equal header-line-format other-header))
          (should gptel-agent--todo-header-cleanup)))
      (should (eq (nth 2 (buffer-local-value 'header-line-format owner))
                  gptel--header-line-info))
      (should-not gptel-agent--todo-header-cleanup))))

(ert-deftest gptel-agent-todo-header-cleanup-tolerates-dead-owner ()
  (let (fsm)
    (gptel-agent-todo-header-test--with-chat
      (gptel-agent-todo-header-test--write)
      (setq fsm gptel--fsm-last))
    (gptel-agent-todo-header-test--with-chat
      (let ((header header-line-format))
        (gptel--handle-post fsm)
        (should (eq header-line-format header))
        (should (eq (nth 2 header) gptel--header-line-info))))))

(ert-deftest gptel-agent-todo-header-cleanup-does-not-reinstall-replaced-header ()
  (dolist (replacement '(nil "New header" ("New" "Status" "Controls")))
    (gptel-agent-todo-header-test--with-chat
      (let ((old-header header-line-format))
        (gptel-agent-todo-header-test--write)
        (setq header-line-format replacement)
        (gptel--handle-post gptel--fsm-last)
        (should (eq header-line-format replacement))
        ;; A UI may restore the saved header later; it must not contain a task.
        (should (eq (nth 2 old-header) gptel--header-line-info))))))

(ert-deftest gptel-agent-todo-header-cleanup-respects-new-info-segment ()
  (gptel-agent-todo-header-test--with-chat
    (gptel-agent-todo-header-test--write)
    (setf (nth 2 header-line-format) "Replacement controls")
    (gptel--handle-post gptel--fsm-last)
    (should (equal (nth 2 header-line-format) "Replacement controls"))
    (gptel-agent-todo-header-test--write "completed")
    (should (equal (nth 2 header-line-format) "Replacement controls"))))

(ert-deftest gptel-agent-todo-header-reused-overlay-has-request-local-cleanup ()
  (gptel-agent-todo-header-test--with-chat
    (gptel-agent-todo-header-test--write)
    (let ((old-cleanup (car (plist-get (gptel-fsm-info gptel--fsm-last) :post)))
          (overlays (overlays-in (point-min) (point-max))))
      (setq-local gptel--fsm-last
                  (gptel-make-fsm :info (list :position (point-marker))))
      (gptel-agent-todo-header-test--write nil "New request")
      (should (equal (overlays-in (point-min) (point-max)) overlays))
      (should (= (length (plist-get (gptel-fsm-info gptel--fsm-last) :post)) 1))
      ;; A delayed callback from the previous request must not clear this one.
      (funcall old-cleanup)
      (should (string-match-p "Task: New request" (nth 2 header-line-format)))
      (gptel--handle-post gptel--fsm-last)
      (should (eq (nth 2 header-line-format) gptel--header-line-info)))))

(ert-deftest gptel-agent-todo-header-completion-clears-only-owned-decoration ()
  (gptel-agent-todo-header-test--with-chat
    (gptel-agent-todo-header-test--write)
    (let ((cleanup gptel-agent--todo-header-cleanup))
      (gptel-agent-todo-header-test--write "completed")
      (should (eq (nth 2 header-line-format) gptel--header-line-info))
      (should-not gptel-agent--todo-header-cleanup)
      (should-not (plist-get (gptel-fsm-info gptel--fsm-last) :post))
      (gptel-agent-todo-header-test--write nil "Next task")
      (funcall cleanup)
      (should (string-match-p "Task: Next task" (nth 2 header-line-format)))
      (gptel-agent--write-todo [])
      (should (eq (nth 2 header-line-format) gptel--header-line-info)))))

(ert-deftest gptel-agent-todo-header-clone-does-not-own-source-decoration ()
  (dolist (source-task '(nil t))
    (gptel-agent-todo-header-test--with-chat
      (when source-task (gptel-agent-todo-header-test--write nil "Source task"))
      (let ((source-header header-line-format)
            (source-decoration (nth 2 header-line-format))
            (source-cleanup gptel-agent--todo-header-cleanup)
            (source-info (gptel-fsm-info gptel--fsm-last))
            (source-post (plist-get (gptel-fsm-info gptel--fsm-last) :post))
            (clone (clone-buffer " *todo-header-clone*")))
        (unwind-protect
            (with-current-buffer clone
              (should-not gptel-agent--todo-header-cleanup)
              (should-not (eq header-line-format source-header))
              (should (eq (nth 2 header-line-format) gptel--header-line-info))
              (setq-local gptel--fsm-last
                          (gptel-make-fsm :info (list :position (point-marker))))
              (gptel-agent-todo-header-test--write nil "Clone task")
              (should (string-match-p "Task: Clone task" (nth 2 header-line-format)))
              (gptel--update-status " Clone streaming")
              (gptel--handle-post gptel--fsm-last)
              (should (eq (nth 2 header-line-format) gptel--header-line-info)))
          (kill-buffer clone))
        (should (eq header-line-format source-header))
        (should (eq (nth 2 header-line-format) source-decoration))
        (should (equal (substring-no-properties (nth 1 header-line-format)) " Ready"))
        (should (eq gptel-agent--todo-header-cleanup source-cleanup))
        (should (eq (plist-get source-info :post) source-post))
        (gptel--handle-post gptel--fsm-last)
        (should (eq (nth 2 header-line-format) gptel--header-line-info))))))

(provide 'gptel-agent-todo-header-test)
;;; gptel-agent-todo-header-test.el ends here
