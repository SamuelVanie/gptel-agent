;;; gptel-agent-todo-ui-test.el --- Optional TodoWrite UI integration -*- lexical-binding: t; -*-

;; Add the sibling gptel-ui checkout to load-path to run these tests.  The
;; production code and the remaining TodoWrite tests do not require gptel-ui.

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)
(require 'org)

(defvar gptel-ui-header-details)
(defvar gptel-ui-mode)

(defmacro gptel-agent-todo-ui-test--chat (details &rest body)
  "Run BODY in a native chat with optional UI header DETAILS."
  (declare (indent 1))
  `(progn
     (skip-unless (require 'gptel-ui nil t))
     (let ((gptel-mode-hook nil)
           (gptel-ui-header-details ,details))
       (with-temp-buffer
         (org-mode)
         (setq-local gptel-use-header-line t
                     header-line-format "Original header")
         (cl-letf (((symbol-function 'gptel-request)
                    (lambda (&rest _) (ert-fail "Unexpected model request"))))
           (unwind-protect
               (progn
                 (gptel-mode 1)
                 (insert "User task\n")
                 (setq-local gptel--fsm-last
                             (gptel-make-fsm :info (list :position (point-marker))))
                 ,@body)
             (when gptel-ui-mode (gptel-ui-mode -1))
             (when gptel-mode (gptel-mode -1))))))))

(defun gptel-agent-todo-ui-test--write (&optional status)
  "Write a task with STATUS, defaulting to in progress."
  (gptel-agent--write-todo
   (vector (list :content "Verify" :activeForm "Verifying"
                 :status (or status "in_progress")))))

(defun gptel-agent-todo-ui-test--assert-approval ()
  "Check that the real UI still exposes its clickable pending approval."
  (let* ((header (gptel-ui--header))
         (start (string-match "1 approval" header)))
    (should start)
    (should (eq (lookup-key (get-text-property start 'local-map header)
                           [header-line mouse-1])
                #'gptel-ui--header-next-approval))))

(ert-deftest gptel-agent-todo-ui-status-and-approval-survive ()
  (dolist (details '(nil t))
    (gptel-agent-todo-ui-test--chat details
      (let* ((native header-line-format)
             (info (gptel-fsm-info gptel--fsm-last))
             (tool (gptel-make-tool :name "TodoUIApproval"
                                    :description "A fixture approval"
                                    :function #'ignore :args nil)))
        ;; Exercise the actual native approval overlay, without executing it.
        (plist-put info :backend (gptel--make-backend :name "Todo UI test"))
        (gptel--display-tool-calls (list (list tool nil #'ignore)) info)
        (gptel-ui-mode 1)
        (let ((installed header-line-format)
              (prefix (car header-line-format))
              (source (buffer-string)))
          (should (= (length header-line-format) (if details 3 2)))
          (dolist (status '("in_progress" "completed" "in_progress"))
            (gptel--update-status " Streaming" 'success)
            (should (gptel-agent-todo-ui-test--write status))
            (should (eq header-line-format installed))
            (should (eq (car header-line-format) prefix))
            (should (equal (substring-no-properties (nth 1 header-line-format))
                           " Streaming"))
            (should (= (length header-line-format) (if details 3 2)))
            (when details
              (if (equal status "completed")
                  (should (eq (nth 2 header-line-format) gptel--header-line-info))
                (should (string-match-p "Task: Verifying"
                                        (nth 2 header-line-format)))))
            (gptel-agent-todo-ui-test--assert-approval)
            (should (equal-including-properties source (buffer-string)))
            (should (cl-some (lambda (ov)
                               (and (overlay-get ov 'gptel-agent--todos)
                                    (string-match-p "Task list:"
                                                    (overlay-get ov 'after-string))))
                             (overlays-in (point-min) (point-max)))))
          (gptel--update-status " Ready" 'success)
          (gptel--handle-post gptel--fsm-last)
          (should (eq header-line-format installed))
          (gptel-agent-todo-ui-test--assert-approval)
          (gptel-ui-mode -1)
          (should (eq header-line-format native))
          (should (equal (substring-no-properties (nth 1 header-line-format))
                         " Ready"))
          (should (eq (nth 2 header-line-format) gptel--header-line-info)))))))

(ert-deftest gptel-agent-todo-ui-cleanup-restores-saved-native-segment ()
  (dolist (details '(nil t))
    (gptel-agent-todo-ui-test--chat details
      (let ((native header-line-format))
        (gptel-agent-todo-ui-test--write)
        (should (string-match-p "Task: Verifying" (nth 2 native)))
        (gptel-ui-mode 1)
        (let ((installed header-line-format))
          (gptel--update-status " Streaming" 'success)
          (gptel--handle-post gptel--fsm-last)
          (should (eq header-line-format installed))
          (should (eq (nth 2 native) gptel--header-line-info)))
        (gptel-ui-mode -1)
        (should (eq header-line-format native))
        (should (equal (substring-no-properties (nth 1 header-line-format))
                       " Streaming"))
        (should (eq (nth 2 header-line-format) gptel--header-line-info))))))

(ert-deftest gptel-agent-todo-ui-cleanup-after-disabling-ui ()
  (gptel-agent-todo-ui-test--chat t
    (let ((native header-line-format))
      (gptel-ui-mode 1)
      (gptel-agent-todo-ui-test--write)
      (gptel--update-status " Streaming" 'success)
      (gptel-ui-mode -1)
      (should (eq header-line-format native))
      (gptel--handle-post gptel--fsm-last)
      (should (eq (nth 2 header-line-format) gptel--header-line-info))
      (gptel-ui-mode 1)
      (should (eq (nth 2 header-line-format) gptel--header-line-info))
      (should (equal (substring-no-properties (nth 1 header-line-format))
                     " Streaming")))))

(ert-deftest gptel-agent-todo-ui-cleanup-does-not-reenable-disabled-chat-header ()
  (gptel-agent-todo-ui-test--chat t
    (add-hook 'gptel-mode-hook #'gptel-ui--maybe-enable nil t)
    (gptel-ui-mode 1)
    (gptel-agent-todo-ui-test--write)
    (let ((fsm gptel--fsm-last))
      (gptel-mode -1)
      (should-not gptel-ui-mode)
      (should (equal header-line-format "Original header"))
      (gptel--handle-post fsm)
      (should (equal header-line-format "Original header"))
      (gptel-mode 1)
      (should gptel-ui-mode)
      (should (eq (nth 2 header-line-format) gptel--header-line-info)))))

(provide 'gptel-agent-todo-ui-test)
;;; gptel-agent-todo-ui-test.el ends here
