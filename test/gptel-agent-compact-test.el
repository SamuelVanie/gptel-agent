;;; gptel-agent-compact-test.el --- Compaction regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent)
(require 'gptel-openai)

(cl-defmacro gptel-agent-compact-test--with-chat (&body body)
  "Run BODY with offline transports and an isolated chat buffer."
  (declare (indent 0))
  `(let ((gptel-log-level nil)
         (gptel-post-request-hook nil)
         requests displayed)
     (unwind-protect
         (with-temp-buffer
           (setq-local gptel-backend
                       (gptel-make-openai "compact-regression"
                                          :key "unused" :host "example.invalid"
                                          :models '(test))
                       gptel-model 'test
                       gptel-use-curl t
                       gptel-stream nil
                       gptel-use-context nil
                       gptel-context nil)
           (cl-letf (((symbol-function 'gptel-curl-get-response)
                      (lambda (fsm) (push fsm requests)))
                     ((symbol-function 'gptel--url-get-response)
                      (lambda (fsm) (push fsm requests)))
                     ((symbol-function 'gptel--update-status) #'ignore)
                     ((symbol-function 'display-buffer)
                      (lambda (buffer &rest _)
                        (push (get-buffer buffer) displayed))))
             ,@body))
       (dolist (request requests)
         (when-let* ((summary (plist-get (gptel-fsm-info request) :summary-buffer))
                     ((buffer-live-p summary)))
           (kill-buffer summary))))))

(defun gptel-agent-compact-test--answer (fsm response)
  "Deliver RESPONSE and perform the normal terminal transition for FSM."
  (let ((info (gptel-fsm-info fsm)))
    (gptel--fsm-transition fsm 'TYPE)
    (funcall (plist-get info :callback) response info)
    (gptel--fsm-transition fsm)))

(defun gptel-agent-compact-test--retained-summary (fsm response)
  "Assert FSM failed safely and retained RESPONSE in a usable buffer."
  (let* ((info (gptel-fsm-info fsm))
         (summary (plist-get info :summary-buffer)))
    (should (eq (gptel-fsm-state fsm) 'ERRS))
    (should (plist-get info :error))
    (should (equal (plist-get info :summary) response))
    (should (buffer-live-p summary))
    (with-current-buffer summary
      (should (member (buffer-string) (list response (concat response "\n")))))
    summary))

(ert-deftest gptel-agent-compact-rejects-edits-inside-prefix ()
  (dolist (edit '(insert replace delete))
    (gptel-agent-compact-test--with-chat
      (insert "Earlier conversation\nUnsent original draft\n")
      (let ((fsm (gptel-agent-compact)))
        (goto-char (point-min))
        (forward-line 1)
        (pcase edit
          ('insert (insert "NEW USER EDIT while compaction was in flight\n"))
          ('replace (delete-char 6) (insert "Newest"))
          ('delete (delete-char 6)))
        (let ((edited (buffer-string)))
          (gptel-agent-compact-test--answer fsm "Summary of original text")
          (should (equal (buffer-string) edited))
          (should (memq (gptel-agent-compact-test--retained-summary
                         fsm "Summary of original text")
                        displayed)))))))

(ert-deftest gptel-agent-compact-rejects-edits-inside-region ()
  (gptel-agent-compact-test--with-chat
    (insert "Keep before\nSelected conversation\nKeep after\n")
    (let ((transient-mark-mode t))
      (goto-char (point-min))
      (forward-line 1)
      (set-mark (point))
      (forward-line 1)
      (activate-mark)
      (let ((fsm (gptel-agent-compact)))
        (deactivate-mark)
        (goto-char (mark))
        (forward-char 3)
        (insert "NEW EDIT")
        (let ((edited (buffer-string)))
          (gptel-agent-compact-test--answer fsm "Summary")
          (should (equal (buffer-string) edited))
          (gptel-agent-compact-test--retained-summary fsm "Summary"))))))

(ert-deftest gptel-agent-compact-unchanged-prefix-replaces-only-requested-text ()
  (gptel-agent-compact-test--with-chat
    (insert "Older history\n Unselected draft\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((fsm (gptel-agent-compact)))
      (gptel-agent-compact-test--answer fsm "Summary")
      (should (equal (buffer-string) "Summary\n Unselected draft\n"))
      (should (eq (gptel-fsm-state fsm) 'DONE))
      (should-not (plist-get (gptel-fsm-info fsm) :error))
      (should-not (plist-get (gptel-fsm-info fsm) :summary-buffer))
      (should-not displayed))))

(ert-deftest gptel-agent-compact-prefix-snapshot-matches-word-end ()
  (gptel-agent-compact-test--with-chat
    (insert "Earlier conversation\nDraft")
    (goto-char (point-min))
    (search-forward "conver")
    (let ((fsm (gptel-agent-compact)))
      (gptel-agent-compact-test--answer fsm "Summary\n")
      (should (equal (buffer-string) "Summary\n\nDraft"))
      (should-not (plist-get (gptel-fsm-info fsm) :error)))))

(ert-deftest gptel-agent-compact-preserves-appended-draft-at-end-marker ()
  (gptel-agent-compact-test--with-chat
    (insert "Original conversation\n")
    (let ((fsm (gptel-agent-compact)))
      (insert "New unsent draft\n")
      (gptel-agent-compact-test--answer fsm "Summary")
      (should (equal (buffer-string) "Summary\nNew unsent draft\n"))
      (should-not (plist-get (gptel-fsm-info fsm) :error)))))

(ert-deftest gptel-agent-compact-unchanged-region-preserves-both-sides ()
  (gptel-agent-compact-test--with-chat
    (insert "Before\nSelected conversation\nAfter\n")
    (let ((transient-mark-mode t))
      (goto-char (point-min))
      (forward-line 1)
      (set-mark (point))
      (forward-line 1)
      (activate-mark)
      (let ((fsm (gptel-agent-compact)))
        (gptel-agent-compact-test--answer fsm "Summary\n")
        (should (equal (buffer-string) "Before\nSummary\nAfter\n"))
        (should-not (plist-get (gptel-fsm-info fsm) :error))))))

(ert-deftest gptel-agent-compact-region-follows-outside-edits-and-boundaries ()
  (gptel-agent-compact-test--with-chat
    (insert "Before\nSelected conversation\nAfter\n")
    (let ((transient-mark-mode t))
      (goto-char (point-min))
      (forward-line 1)
      (set-mark (point))
      (forward-line 1)
      (activate-mark)
      (let ((fsm (gptel-agent-compact)))
        (deactivate-mark)
        ;; The endpoint must stay before a new draft at that exact position.
        (insert "New draft\n")
        (goto-char (point-min))
        (insert "Prepended outside\n")
        (search-forward "Before\n")
        ;; Likewise, newly inserted text at the start must not be deleted.
        (insert "New boundary text\n")
        (goto-char (point-max))
        (insert "Appended outside\n")
        (gptel-agent-compact-test--answer fsm "Summary")
        (should (equal (buffer-string)
                       (concat "Prepended outside\nBefore\nNew boundary text\n"
                               "Summary\nNew draft\nAfter\nAppended outside\n")))
        (should-not (plist-get (gptel-fsm-info fsm) :error))))))

(ert-deftest gptel-agent-compact-respects-original-narrowed-prefix ()
  (gptel-agent-compact-test--with-chat
    (insert "Outside prefix\nSelected conversation\nOutside suffix\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((start (point)))
      (forward-line 1)
      (narrow-to-region start (point))
      (let ((fsm (gptel-agent-compact)))
        (widen)
        (goto-char (point-min))
        (insert "New outside prefix\n")
        (gptel-agent-compact-test--answer fsm "Summary")
        (should (equal (buffer-string)
                       "New outside prefix\nOutside prefix\nSummary\nOutside suffix\n"))
        (should-not (plist-get (gptel-fsm-info fsm) :error))))))

(ert-deftest gptel-agent-compact-handles-narrowing-during-request ()
  (gptel-agent-compact-test--with-chat
    (insert "Selected conversation\n Outside suffix\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((fsm (gptel-agent-compact)))
      (narrow-to-region (1+ (point)) (point-max))
      (gptel-agent-compact-test--answer fsm "Summary")
      (should (buffer-narrowed-p))
      (should (equal (buffer-string) "Outside suffix\n"))
      (save-restriction
        (widen)
        (should (equal (buffer-string) "Summary\n Outside suffix\n")))
      (should-not (plist-get (gptel-fsm-info fsm) :error)))))

(ert-deftest gptel-agent-compact-read-only-source-retains-summary ()
  (dolist (read-only-kind '(buffer text))
    (gptel-agent-compact-test--with-chat
      (insert "Original conversation\n")
      (let ((fsm (gptel-agent-compact)))
        (pcase read-only-kind
          ('buffer (setq buffer-read-only t))
          ('text (put-text-property 4 8 'read-only t)))
        (gptel-agent-compact-test--answer fsm "Summary")
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       "Original conversation\n"))
        (gptel-agent-compact-test--retained-summary fsm "Summary")))))

(ert-deftest gptel-agent-compact-read-only-text-outside-selection-is-preserved ()
  (gptel-agent-compact-test--with-chat
    (insert "Selected conversation\n Protected suffix\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((fsm (gptel-agent-compact)))
      (put-text-property (point) (point-max) 'read-only t)
      (gptel-agent-compact-test--answer fsm "Summary")
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "Summary\n Protected suffix\n"))
      (should-not (plist-get (gptel-fsm-info fsm) :error)))))

(ert-deftest gptel-agent-compact-killed-source-retains-summary ()
  (gptel-agent-compact-test--with-chat
    (insert "Original conversation\n")
    (let ((fsm (gptel-agent-compact)))
      (kill-buffer (current-buffer))
      (gptel-agent-compact-test--answer fsm "Summary")
      (should (memq (gptel-agent-compact-test--retained-summary fsm "Summary")
                    displayed)))))

(ert-deftest gptel-agent-compact-replacement-failure-rolls-back-deletion ()
  (gptel-agent-compact-test--with-chat
    (insert "Original conversation\n")
    (let ((fsm (gptel-agent-compact)))
      (add-hook 'before-change-functions
                (lambda (beg end)
                  (when (= beg end) (error "Reject summary insertion")))
                nil t)
      (gptel-agent-compact-test--answer fsm "Summary")
      (should (equal (buffer-string) "Original conversation\n"))
      (gptel-agent-compact-test--retained-summary fsm "Summary"))))

(ert-deftest gptel-agent-compact-post-observes-replacement-outcome-once ()
  (dolist (edited '(nil t))
    (gptel-agent-compact-test--with-chat
      (insert "Original conversation\n")
      (let* (post-results
             (fsm (gptel-agent-compact
                   nil (lambda (info)
                         (push (list (plist-get info :error) (buffer-string))
                               post-results)))))
        (when edited
          (goto-char (point-min))
          (forward-char 3)
          (insert "EDIT"))
        (should-not post-results)
        (gptel-agent-compact-test--answer fsm "Summary")
        (should (= (length post-results) 1))
        (if edited
            (progn
              (should (caar post-results))
              (should (equal (cadar post-results) "OriEDITginal conversation\n")))
          (should-not (caar post-results))
          (should (equal (cadar post-results) "Summary\n")))))))

(ert-deftest gptel-agent-compact-reasoning-does-not-consume-snapshot ()
  (gptel-agent-compact-test--with-chat
    (insert "Original conversation\n")
    (let* ((fsm (gptel-agent-compact))
           (info (gptel-fsm-info fsm)))
      (funcall (plist-get info :callback) '(reasoning . "Planning summary") info)
      (should (equal (buffer-string) "Original conversation\n"))
      (should-not (plist-get info :error))
      (should-not displayed)
      (gptel-agent-compact-test--answer fsm "Summary")
      (should (equal (buffer-string) "Summary\n"))
      (should (eq (gptel-fsm-state fsm) 'DONE)))))

(ert-deftest gptel-agent-compact-reasoning-with-killed-source-is-ignored ()
  (gptel-agent-compact-test--with-chat
    (insert "Original conversation\n")
    (let* ((fsm (gptel-agent-compact))
           (info (gptel-fsm-info fsm)))
      (kill-buffer (current-buffer))
      (funcall (plist-get info :callback) '(reasoning . "Planning summary") info)
      (should-not (plist-get info :error))
      (should-not displayed)
      (gptel-agent-compact-test--answer fsm "Summary")
      (gptel-agent-compact-test--retained-summary fsm "Summary"))))

(ert-deftest gptel-agent-compact-provider-error-reaches-post ()
  (gptel-agent-compact-test--with-chat
    (insert "Original conversation\n")
    (let* (post-results
           (fsm (gptel-agent-compact
                 nil (lambda (info) (push (plist-get info :error) post-results))))
           (info (gptel-fsm-info fsm)))
      (plist-put info :error "Provider unavailable")
      (plist-put info :status "503")
      (gptel-agent-compact-test--answer fsm nil)
      (should (equal (buffer-string) "Original conversation\n"))
      (should (equal post-results '("Provider unavailable")))
      (should-not (plist-get info :summary-buffer))
      (should-not displayed))))

(provide 'gptel-agent-compact-test)
;;; gptel-agent-compact-test.el ends here
