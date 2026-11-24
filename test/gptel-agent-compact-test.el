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

(ert-deftest gptel-agent-compact-sends-all-selected-turns-despite-history-limit ()
  (gptel-agent-compact-test--with-chat
    (setq-local gptel-mode t gptel--num-messages-to-send 1)
    (insert "Unselected start\n")
    (let ((transient-mark-mode t)
          (start (point)))
      (dotimes (i 5)
        (insert (format "Question %s\n" i)
                (propertize (format "Answer %s" i) 'gptel 'response) "\n"))
      (let ((end (point)))
        (insert "Unsent draft")
        (set-mark end)
        (goto-char start)
        (activate-mark)
        (let* ((fsm (gptel-agent-compact))
               (messages (plist-get (plist-get (gptel-fsm-info fsm) :data)
                                    :messages)))
          (should (equal
                   (cl-loop for message across messages
                            unless (equal (plist-get message :role) "system")
                            collect (list (plist-get message :role)
                                          (plist-get message :content)))
                   (cl-loop for i below 5
                            append (list (list "user" (format "Question %s" i))
                                         (list "assistant" (format "Answer %s" i))))))
          (should (equal gptel--num-messages-to-send 1))
          (gptel-agent-compact-test--answer fsm "Summary")
          (should (equal (buffer-string) "Unselected start\nSummary\nUnsent draft")))))))

(ert-deftest gptel-agent-compact-org-topic-keeps-heading-properties-and-other-topic ()
  (dolist (selection '(prefix region crossing-region))
    (gptel-agent-compact-test--with-chat
      (let ((backend gptel-backend))
        (org-mode)
        (setq-local gptel-backend backend gptel-model 'test gptel-use-curl t))
      (let* ((transient-mark-mode t)
             (header (concat "* Old topic\nOther conversation\n* Current topic\n"
                             ":PROPERTIES:\n:GPTEL_TOPIC: current\n:END:\n"))
             (earlier "Earlier within topic\n")
             (selected "** Follow-up\nLatest question\n")
             (suffix " A later draft"))
        (insert header earlier selected suffix)
        (goto-char (- (point-max) (length suffix)))
        (unless (eq selection 'prefix)
          (set-mark (point))
          (if (eq selection 'region)
              (backward-char (length selected))
            (goto-char (point-min)))
          (activate-mark))
        (let* ((fsm (gptel-agent-compact))
               (messages (plist-get (plist-get (gptel-fsm-info fsm) :data)
                                    :messages))
               (turns (cl-loop for message across messages
                               when (equal (plist-get message :role) "user")
                               collect (plist-get message :content))))
          (should (equal turns
                         (list (string-trim-right
                                (if (eq selection 'region) selected
                                  (concat earlier selected))))))
          (gptel-agent-compact-test--answer fsm "Summary")
          (should (equal (buffer-string)
                         (concat header (when (eq selection 'region) earlier)
                                 "Summary\n" suffix))))))))

(ert-deftest gptel-agent-compact-preserves-org-file-properties ()
  (gptel-agent-compact-test--with-chat
    (let ((backend gptel-backend)
          (properties ":PROPERTIES:\n:GPTEL_MODEL: private-setting\n:END:\n"))
      (org-mode)
      (setq-local gptel-backend backend gptel-model 'test gptel-use-curl t)
      (insert properties)
      (should-error (gptel-agent-compact) :type 'user-error)
      (should-not requests)
      (insert "The conversation")
      (let* ((fsm (gptel-agent-compact))
             (messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
        (should (equal (plist-get (aref messages 1) :content) "The conversation"))
        (gptel-agent-compact-test--answer fsm "Summary")
        (should (equal (buffer-string) (concat properties "Summary\n")))))))

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

(defun gptel-agent-compact-test--http-response (fsm data transport)
  "Deliver a successful HTTP response with JSON DATA to FSM via TRANSPORT."
  (with-temp-buffer
    (let ((http-buffer (current-buffer)) (gptel--request-alist nil) receive)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_url callback &rest _)
                   (setq receive callback)
                   http-buffer)))
        (funcall transport fsm))
      (insert "HTTP/1.1 200 OK\r\n\r\n")
      (setq-local url-http-end-of-headers (point-marker)
                  url-http-response-status 200)
      (insert (json-serialize data))
      (goto-char (point-min))
      (funcall receive nil))))

(ert-deftest gptel-agent-compact-http-requires-a-complete-summary ()
  (let ((transport (symbol-function 'gptel--url-get-response)))
    (dolist (sample '(("<think>Reasoning</think>" "stop" nil)
                      ("<think>Unclosed reasoning" "stop" nil)
                      (" \n\t " "stop" nil)
                      (nil "stop" "Separate reasoning")
                      ("Half a sentence" "length" nil)
                      ("Complete summary" "stop" "Separate reasoning")))
      (gptel-agent-compact-test--with-chat
        (insert "Original transcript")
        (pcase-let* ((`(,content ,reason ,reasoning) sample)
                     (message (append (list :role "assistant")
                                      (when content (list :content content))
                                      (when reasoning (list :reasoning_content reasoning))))
                     (choice (list :message message :finish_reason reason))
                     (posts nil)
                     (fsm (gptel-agent-compact
                           nil (lambda (info) (push (plist-get info :error) posts))))
                     (info (gptel-fsm-info fsm)))
          (gptel-agent-compact-test--http-response
           fsm (list :choices (vector choice)) transport)
          (should (= (length posts) 1))
          (if (equal content "Complete summary")
              (progn (should (equal (buffer-string) "Complete summary\n"))
                     (should (eq (gptel-fsm-state fsm) 'DONE))
                     (should-not (car posts)))
            (should (equal (buffer-string) "Original transcript"))
            (should (eq (gptel-fsm-state fsm) 'ERRS))
            (should (stringp (car posts)))
            (if (equal reason "length")
                (progn (should (string-match-p reason (car posts)))
                       (should (memq (gptel-agent-compact-test--retained-summary fsm content)
                                     displayed)))
              (should-not (plist-get info :summary-buffer)))))))))

(ert-deftest gptel-agent-compact-checks-provider-stop-reason ()
  (dolist (reason '("max_tokens" "MAX_TOKENS" "incomplete" "content_filter"
                    "end_turn" "STOP" "stop_sequence" "completed"))
    (gptel-agent-compact-test--with-chat
      (insert "Original transcript")
      (let* ((fsm (gptel-agent-compact))
             (info (gptel-fsm-info fsm)))
        (plist-put info :stop-reason reason)
        (gptel-agent-compact-test--answer fsm "Returned text")
        (if (member reason '("end_turn" "STOP" "stop_sequence" "completed"))
            (progn (should (equal (buffer-string) "Returned text\n"))
                   (should (eq (gptel-fsm-state fsm) 'DONE)))
          (should (equal (buffer-string) "Original transcript"))
          (should (string-match-p reason (plist-get info :error)))
          (gptel-agent-compact-test--retained-summary fsm "Returned text"))))))

(ert-deftest gptel-agent-compact-retains-partial-text-alongside-provider-error ()
  (gptel-agent-compact-test--with-chat
    (insert "Original transcript")
    (let* ((fsm (gptel-agent-compact))
           (info (gptel-fsm-info fsm)))
      (plist-put info :error "Connection interrupted")
      (gptel-agent-compact-test--answer fsm "Partial summary")
      (should (equal (buffer-string) "Original transcript"))
      (should (equal (plist-get info :error) "Connection interrupted"))
      (gptel-agent-compact-test--retained-summary fsm "Partial summary"))))

(provide 'gptel-agent-compact-test)
;;; gptel-agent-compact-test.el ends here
