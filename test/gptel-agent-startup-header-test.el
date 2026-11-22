;;; gptel-agent-startup-header-test.el --- Startup header composition -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent)

;; UI is optional: native startup tests also run without it on load-path.
(defvar gptel-ui-global-mode)
(defvar gptel-ui-header-details)
(defvar gptel-ui-mode-hook)

(defmacro gptel-agent-startup-header-test--with-chat (&rest body)
  "Start an actual agent chat and run BODY in it, without model requests."
  (declare (indent 0))
  `(let ((gptel-backend (gptel--make-backend :name "Startup" :models '(test)))
         (gptel-model 'test)
         (gptel-default-mode 'org-mode)
         (gptel-agent-skill-dirs nil)
         (gptel--known-presets (copy-tree gptel--known-presets))
         (gptel-agent--agents nil)
         (gptel-agent--agent-files nil)
         (gptel-agent--file-cache nil)
         (buffers-before (buffer-list)))
     (save-window-excursion
       (unwind-protect
           (cl-letf (((symbol-function 'gptel-request)
                      (lambda (&rest _) (ert-fail "Unexpected model request"))))
             (gptel-agent temporary-file-directory 'gptel-agent)
             (let ((chat (cl-find-if
                          (lambda (buffer)
                            (and (not (memq buffer buffers-before))
                                 (string-prefix-p "*gptel-agent:" (buffer-name buffer))))
                          (buffer-list))))
               (should (buffer-live-p chat))
               (with-current-buffer chat ,@body)))
         (dolist (buffer (buffer-list))
           (when (and (not (memq buffer buffers-before))
                      (buffer-local-value 'gptel-mode buffer))
             (kill-buffer buffer)))))))

(defun gptel-agent-startup-header-test--selector (header)
  "Evaluate the native agent selector in HEADER."
  (should (eq (caar header) :eval))
  (eval (cadar header) t))

(defun gptel-agent-startup-header-test--click-selector (header)
  "Activate the actual preset button from native HEADER."
  (let* ((text (gptel-agent-startup-header-test--selector header))
         (start (text-property-any 0 (length text) 'button t text)))
    (should start)
    (funcall (get-text-property start 'action text) nil)))

(ert-deftest gptel-agent-startup-native-selector-and-hook-scope ()
  (let ((gptel-mode-hook nil)
        (gptel-use-header-line t)
        (ui-loaded (featurep 'gptel-ui))
        (starts 0))
    (add-hook 'gptel-mode-hook (lambda () (cl-incf starts)))
    (let ((hooks gptel-mode-hook))
      (gptel-agent-startup-header-test--with-chat
        (should (= starts 1))
        (should (eq gptel-mode-hook hooks))
        (should (eq (featurep 'gptel-ui) ui-loaded))
        (should (equal gptel-agent--current-agent "gptel-agent"))
        (should (string-match-p
                 "Startup\\[Agent\\]"
                 (gptel-agent-startup-header-test--selector header-line-format)))
        (let ((controls (nth 2 header-line-format)))
          (dolist (preset '("gptel-plan" "ask" "gptel-agent"))
            (gptel-agent-startup-header-test--click-selector header-line-format)
            (should (equal gptel-agent--current-agent preset)))
          (gptel--update-status " Waiting" 'warning)
          (should (equal (substring-no-properties (nth 1 header-line-format))
                         " Waiting"))
          (should (eq (nth 2 header-line-format) controls)))
        ;; A later ordinary chat must not inherit the startup-only selector.
        (with-current-buffer (gptel (generate-new-buffer-name " *startup-ordinary*"))
          (should-not (string-match-p
                       "\\[Agent\\]"
                       (gptel-agent-startup-header-test--selector header-line-format))))))))

(ert-deftest gptel-agent-startup-native-from-buffer-with-local-mode-hook ()
  (let ((gptel-mode-hook nil)
        (gptel-use-header-line t))
    (with-temp-buffer
      (setq-local gptel-mode-hook '(ignore t))
      (gptel-agent-startup-header-test--with-chat
        (should (string-match-p
                 "\\[Agent\\]"
                 (gptel-agent-startup-header-test--selector header-line-format)))))))

(ert-deftest gptel-agent-startup-native-without-header ()
  (let ((gptel-mode-hook nil)
        (gptel-use-header-line nil)
        (org-mode-hook (list (lambda () (setq-local header-line-format "Custom header")))))
    (gptel-agent-startup-header-test--with-chat
      (should (equal header-line-format "Custom header"))
      (should (equal gptel-agent--current-agent "gptel-agent"))
      (gptel--update-status " Waiting" 'warning)
      (should (equal header-line-format "Custom header"))
      (should (equal (substring-no-properties mode-line-process) " Waiting")))))

(ert-deftest gptel-agent-startup-ui-composes-native-selector-and-approvals ()
  (skip-unless (require 'gptel-ui nil t))
  (dolist (details '(nil t))
    (let* ((gptel-mode-hook nil)
           (gptel-use-header-line t)
           (gptel-ui-global-mode nil)
           (gptel-ui-header-details details)
           (ui-starts 0)
           (gptel-ui-mode-hook (list (lambda () (cl-incf ui-starts))))
           (worker (generate-new-buffer " *startup-hidden-worker*")))
      (unwind-protect
          (progn
            (gptel-ui-global-mode 1)
            (gptel-agent-startup-header-test--with-chat
              (should gptel-ui-mode)
              (should (= ui-starts 1))
              (let* ((native gptel-ui--native-header)
                     (ui-header (if details (caar header-line-format)
                                  (car header-line-format))))
                (should (equal ui-header '(:eval (gptel-ui--header))))
                (should (eq header-line-format gptel-ui--installed-header))
                (should (string-match-p
                         "\\[Agent\\]"
                         (gptel-agent-startup-header-test--selector native)))
                (when details
                  (should (eq (cadar header-line-format) (car native))))
                ;; Use a real native approval in a hidden worker buffer.
                (with-current-buffer worker
                  (org-mode)
                  (insert "Pending worker operation\n")
                  (gptel--display-tool-calls
                   (list (list (gptel-make-tool
                                :name "StartupApproval" :description "Startup fixture"
                                :function #'ignore :args nil)
                               nil #'ignore))
                   (list :position (point-marker) :backend gptel-backend)))
                (should-not (get-buffer-window worker t))
                ;; Evaluate the expression actually installed in the header,
                ;; not just the UI helper that startup previously displaced.
                (should (string-match-p "1 approval" (eval (cadr ui-header) t)))
                (gptel--update-status " Waiting" 'warning)
                (should (equal (substring-no-properties (nth 1 header-line-format))
                               " Waiting"))
                (kill-buffer worker)
                (should-not (string-match-p "approval" (eval (cadr ui-header) t)))
                (gptel-ui-mode -1)
                (should (eq header-line-format native))
                (should (equal (substring-no-properties (nth 1 header-line-format))
                               " Waiting"))
                (gptel-agent-startup-header-test--click-selector header-line-format)
                (should (equal gptel-agent--current-agent "gptel-plan"))
                (should (string-match-p
                         "\\[Plan\\]"
                         (gptel-agent-startup-header-test--selector header-line-format))))))
        (when (buffer-live-p worker) (kill-buffer worker))
        (gptel-ui-global-mode -1)))))

(provide 'gptel-agent-startup-header-test)
;;; gptel-agent-startup-header-test.el ends here
