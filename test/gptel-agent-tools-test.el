;;; gptel-agent-tools-test.el --- Agent supervisor tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent)

(defvar url-http-end-of-headers)
(defvar url-http-content-type)
(defvar url-http-response-status)

(defun gptel-agent-test--agent-prompt (name)
  "Return the built-in agent prompt named NAME."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (format "agents/%s.md" name)
                       (file-name-directory (locate-library "gptel-agent"))))
    (buffer-string)))

(defun gptel-agent-test--saved-result-path (result)
  "Return the quoted result_file path embedded in oversized RESULT."
  (when (string-match "result_file: \\(\"[^\n]+\"\\)" result)
    (car (read-from-string (match-string 1 result)))))

(cl-defmacro gptel-agent-test--with-run ((run fsm received) &body body)
  "Create an owned RUN and FSM, bind callback results to RECEIVED, then run BODY."
  (declare (indent 1))
  `(let* ((parent (generate-new-buffer " *gptel-agent-test-parent*"))
          (child (generate-new-buffer " *gptel-agent-test-child*"))
          (,received nil)
          (,run (gptel-agent--make-run
                 :id "test-run" :state 'created :agent "researcher"
                 :description "test task" :parent-buffer parent
                 :child-buffer child :response "" :response-checkpoint 0
                 :rounds 0 :retries 0 :max-request-rounds nil
                 :callback (lambda (value) (push value ,received))))
          (,fsm (gptel-make-fsm :info (list :context ,run))))
     (setf (gptel-agent--run-fsm ,run) ,fsm)
     (unwind-protect
         (progn ,@body)
       (when (timerp (gptel-agent--run-retry-timer ,run))
         (cancel-timer (gptel-agent--run-retry-timer ,run)))
       (when (buffer-live-p parent) (kill-buffer parent))
       (when (buffer-live-p child) (kill-buffer child)))))

(ert-deftest gptel-agent-transient-errors-are-classified-conservatively ()
  (should (gptel-agent--transient-request-error-p
           '(:status "Curl failure" :error "Curl failed with exit code 56")))
  (should (gptel-agent--transient-request-error-p
           '(:http-status "429" :status "Too many requests")))
  (should (gptel-agent--transient-request-error-p
           '(:http-status "503" :status "Unavailable")))
  (should-not (gptel-agent--transient-request-error-p
               '(:http-status "401" :error "Unauthorized")))
  (should-not (gptel-agent--transient-request-error-p
               '(:http-status "400" :error "Stream must be set to true"))))

(ert-deftest gptel-agent-similar-tool-loop-escalates-to-finalization ()
  (with-temp-buffer
    (gptel-agent--reset-tool-call-counts)
    (let ((run (gptel-agent--make-run :tool-calls 0 :web-tool-calls 0))
          (gptel-agent-max-tool-repetitions 20)
          (gptel-agent-max-similar-tool-calls 4)
          (gptel-agent-max-loop-violations 2)
          results)
      (setq-local gptel-agent--supervised-run run)
      (dolist (suffix '("one" "two" "three" "four" "five"))
        (push (gptel-agent--detect-repetition
               (list :name "WebSearch"
                     :args (list :query
                                 (format "alpha beta gamma %s" suffix))))
              results))
      (setq results (nreverse results))
      (should (plist-get (nth 3 results) :result))
      (should (string-match-p "strategy warning"
                              (plist-get (nth 3 results) :result)))
      (should (plist-get (nth 4 results) :result))
      (should (string-match-p "return the requested final answer"
                              (plist-get (nth 4 results) :result)))
      (should (gptel-agent--run-finalization-reason run))
      (should-not (cl-find-if (lambda (result) (plist-get result :stop))
                              results)))))

(ert-deftest gptel-agent-exact-repetition-forces-finalization ()
  (with-temp-buffer
    (gptel-agent--reset-tool-call-counts)
    (let ((run (gptel-agent--make-run :tool-calls 0 :web-tool-calls 0))
          (gptel-agent-max-tool-repetitions 3)
          (gptel-agent-max-similar-tool-calls nil)
          results)
      (setq-local gptel-agent--supervised-run run)
      (dotimes (_ 5)
        (push (gptel-agent--detect-repetition
               '(:name "WebFetch" :args (:url "https://example.com")))
              results))
      (setq results (nreverse results))
      (should (plist-get (nth 2 results) :result))
      (should (plist-get (nth 4 results) :result))
      (should (string-match-p
               "identical arguments"
               (gptel-agent--run-finalization-reason run)))
      (should-not (cl-find-if (lambda (result) (plist-get result :stop))
                              results)))))

(ert-deftest gptel-agent-web-search-count-is-bounded-and-configurable ()
  (let ((gptel-agent-web-search-default-count 10)
        (gptel-agent-web-search-max-count 30))
    (should (= (gptel-agent--web-search-count nil) 10))
    (should (= (gptel-agent--web-search-count 20) 20))
    (should (= (gptel-agent--web-search-count 100) 30))
    (should (= (gptel-agent--web-search-count 0) 10))))

(ert-deftest gptel-agent-web-search-result-reports-coverage ()
  (let (result)
    (with-temp-buffer
      (let ((url-http-end-of-headers (point-min)))
        (cl-letf (((symbol-function 'libxml-parse-html-region)
                   (lambda (&rest _) 'document))
                  ((symbol-function 'eww-score-readability) #'identity)
                  ((symbol-function 'eww-highest-readability) #'identity)
                  ((symbol-function 'shr-insert-document)
                   (lambda (_dom) (insert "no result links"))))
          (gptel-agent--web-search-eww-callback
           (lambda (value) (setq result (read value)))
           "SamuelVanie/gptel-agent" 10))))
    (should (equal (plist-get result :query) "SamuelVanie/gptel-agent"))
    (should (= (plist-get result :requested-count) 10))
    (should (= (plist-get result :returned-count) 0))
    (should (string-match-p "absence is not proof"
                            (plist-get result :coverage-note)))))

(ert-deftest gptel-agent-ask-without-confirmation-waits-in-parent-buffer ()
  (let* ((parent (generate-new-buffer " *gptel-agent-ask-parent*"))
         (child (generate-new-buffer " *gptel-agent-ask-child*"))
         (run (gptel-agent--make-run
               :state 'waiting-for-tool
               :parent-buffer parent
               :child-buffer child))
         (tool (gptel-get-tool "AskUserQuestion"))
         (call (list :name "AskUserQuestion"
                     :args '(:questions
                             [(:question "Continue?"
                               :choices [(:value "Yes") (:value "No")])])))
         (info (list :backend t :buffer child :tools (list tool)
                     :tool-use (list call)))
         (fsm (gptel-make-fsm
               :state 'TOOL
               :table '((TOOL . ((t . TRET))))
               :handlers nil
               :info info))
         (gptel-confirm-tool-calls nil))
    (unwind-protect
        (progn
          (with-current-buffer child
            (setq-local gptel-agent--supervised-run run)
            (gptel--handle-tool-use fsm))
          ;; The tool remains unresolved until the user confirms a choice.
          (ert-info ("after opening the interaction")
            (should (eq (gptel-fsm-state fsm) 'TOOL)))
          (should-not (plist-get call :result))
          (should-not
           (seq-find (lambda (ov) (overlay-get ov 'gptel-ask))
                     (with-current-buffer child
                       (overlays-in (point-min) (point-max)))))
          (let ((ask-ov
                 (seq-find (lambda (ov) (overlay-get ov 'gptel-ask))
                           (with-current-buffer parent
                             (overlays-in (point-min) (point-max))))))
            (should (overlayp ask-ov))
            (with-current-buffer parent
              (goto-char (overlay-start ask-ov))
              (gptel-agent--ask-confirm-choice))
            ;; Saving the last answer does not submit the response list.
            (ert-info ("after saving the answer")
              (should (eq (gptel-fsm-state fsm) 'TOOL)))
            (should-not (plist-get call :result))
            (with-current-buffer parent
              (goto-char (overlay-start ask-ov))
              (gptel-agent--ask-submit)))
          (should (eq (gptel-fsm-state fsm) 'TRET))
          (should (equal (plist-get call :result)
                         "Q1: Continue?\nR1: Yes")))
      (when (buffer-live-p parent) (kill-buffer parent))
      (when (buffer-live-p child) (kill-buffer child)))))

(ert-deftest gptel-agent-ask-multiple-allows-revising-saved-answers ()
  (with-temp-buffer
    (let (result)
      (gptel-agent--ask-multiple
       (lambda (answers) (setq result answers))
       [(:question "Pick a color"
         :choices [(:value "Red") (:value "Blue")])
        (:question "Enable notifications?"
         :choices [(:value "Yes") (:value "No")])])
      (let ((ask-ov
             (seq-find (lambda (ov) (overlay-get ov 'gptel-ask))
                       (overlays-in (point-min) (point-max)))))
        (should (overlayp ask-ov))
        (should (= (overlay-get ask-ov 'gptel-ask--index) 0))
        (should (eq (lookup-key (overlay-get ask-ov 'keymap) (kbd "M-p"))
                    'gptel-agent--ask-previous-question))
        ;; Save both initial answers.  The interaction remains open.
        (goto-char (overlay-start ask-ov))
        (gptel-agent--ask-confirm-choice)
        (should (= (overlay-get ask-ov 'gptel-ask--index) 1))
        (gptel-agent--ask-select-choice 1)
        (gptel-agent--ask-confirm-choice)
        (should-not result)
        (should (equal (append (overlay-get ask-ov 'gptel-ask--answers) nil)
                       '("Red" "No")))
        ;; Return to the first question and replace its saved answer.
        (gptel-agent--ask-previous-question)
        (should (= (overlay-get ask-ov 'gptel-ask--index) 0))
        (gptel-agent--ask-select-choice 1)
        (should-not (aref (overlay-get ask-ov 'gptel-ask--answers) 0))
        (gptel-agent--ask-confirm-choice)
        (should (= (overlay-get ask-ov 'gptel-ask--index) 1))
        (should (equal (append (overlay-get ask-ov 'gptel-ask--answers) nil)
                       '("Blue" "No")))
        ;; Only the explicit final action resolves the tool call.
        (gptel-agent--ask-submit)
        (should (equal result
                       (concat "Q1: Pick a color\nR1: Blue\n\n"
                               "Q2: Enable notifications?\nR2: No")))
        (should-not (overlay-buffer ask-ov))))))

(ert-deftest gptel-agent-task-prompt-allows-inconclusive-results ()
  (let ((prompt (gptel-agent--prepare-task-prompt "Find the repository")))
    (should (string-prefix-p "Find the repository" prompt))
    (should (string-match-p "negative or inconclusive finding is a valid result"
                            prompt))
    (should (string-match-p "high, medium, or low confidence" prompt))))

(ert-deftest gptel-agent-built-in-prompts-report-impossible-outcomes ()
  (let ((case-fold-search t))
    (dolist (name '("gptel-agent" "gptel-plan" "ask" "executor"
                    "researcher" "introspector"))
      (let ((prompt (gptel-agent-test--agent-prompt name)))
        (should (string-match-p
                 (rx (or "cannot be" "could not be" "not possible"
                         "valid completion"))
                 prompt))
        (should (string-match-p "confidence" prompt))))))

(ert-deftest gptel-agent-built-in-prompts-have-balanced-policy-tags ()
  (dolist (name '("gptel-agent" "gptel-plan" "ask" "executor"
                  "researcher" "introspector" "result-explorer"))
    (with-temp-buffer
      (insert (gptel-agent-test--agent-prompt name))
      (goto-char (point-min))
      (let (stack)
        (while (re-search-forward
                "<\\(/?\\)\\([[:alnum:]_-]+\\)\\(?: [^>]*\\)?>" nil t)
          (let ((closing (equal (match-string 1) "/"))
                (tag (match-string 2)))
            (if closing
                (should (equal (pop stack) tag))
              (push tag stack))))
        (should-not stack)))))

(ert-deftest gptel-agent-plan-prompt-requires-a-self-contained-handoff ()
  (let ((prompt (gptel-agent-test--agent-prompt "gptel-plan")))
    (dolist (requirement '("standalone handoff"
                           "user-provided detail"
                           "current behavior"
                           "desired behavior"
                           "success criteria"
                           "decision log"
                           "resulting behavior"))
      (should (string-match-p requirement prompt)))))

(ert-deftest gptel-agent-all-delegating-prompts-accept-inconclusive-reports ()
  (dolist (name '("gptel-agent" "gptel-plan" "ask" "executor"))
    (should (string-match-p
             "completed delegation outcome"
             (gptel-agent-test--agent-prompt name)))))

(ert-deftest gptel-agent-web-budget-counts-calls-not-callbacks ()
  (with-temp-buffer
    (let ((run (gptel-agent--make-run :tool-calls 0 :web-tool-calls 0))
          (gptel-agent-max-web-tool-calls 2)
          (gptel-agent-max-similar-tool-calls nil)
          (gptel-agent-max-tool-repetitions nil))
      (setq-local gptel-agent--supervised-run run)
      (gptel-agent--reset-tool-call-counts)
      (should-not
       (gptel-agent--detect-repetition
        '(:name "WebSearch" :args (:query "first topic"))))
      (should-not
       (gptel-agent--detect-repetition
        '(:name "WebFetch" :args (:url "https://example.com"))))
      (let ((result
             (gptel-agent--detect-repetition
              '(:name "WebSearch" :args (:query "second topic")))))
        (should (plist-get result :result))
        (should (string-match-p "Tool exploration is now closed"
                                (plist-get result :result)))
        (should (string-match-p "web tool budget"
                                (gptel-agent--run-finalization-reason run))))
      (should (= (gptel-agent--run-web-tool-calls run) 3)))))

(ert-deftest gptel-agent-web-parser-exception-completes-callback-once ()
  (let (results)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (let ((buffer (generate-new-buffer " *web-parser-test*")))
                   (with-current-buffer buffer (funcall callback nil))
                   buffer))))
      (gptel-agent--fetch-with-timeout
       "https://example.test"
       (lambda (_callback) (error "parser exploded"))
       (lambda (result) (push result results))
       "Test fetch"))
    (should (= (length results) 1))
    (should (string-match-p "could not be parsed" (car results)))))

(ert-deftest gptel-agent-web-requests-are-noninteractive ()
  (let (noninteractive result)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _args)
                 (setq noninteractive url-request-noninteractive)
                 (let ((buffer (generate-new-buffer " *web-request-test*")))
                   (with-current-buffer buffer
                     (funcall callback nil))
                   buffer))))
      (gptel-agent--fetch-with-timeout
       "https://example.test"
       (lambda (callback) (funcall callback "ok"))
       (lambda (value) (setq result value))
       "Test fetch"))
    (should noninteractive)
    (should (equal result "ok"))))

(ert-deftest gptel-agent-web-fetch-preserves-json-and-plain-text ()
  (let (result)
    (with-temp-buffer
      (insert "{\"default_branch\":\"master\"}")
      (let ((url-http-end-of-headers (point-min))
            (url-http-content-type "application/json; charset=utf-8"))
        (gptel-agent--web-fetch-callback
         (lambda (value) (setq result value)) nil)))
    (should (equal result "{\"default_branch\":\"master\"}"))))

(ert-deftest gptel-agent-web-fetch-falls-back-to-full-html-render ()
  (let (result)
    (with-temp-buffer
      (insert "<html><body>source</body></html>")
      (let ((url-http-end-of-headers (point-min))
            (url-http-content-type "text/html; charset=utf-8"))
        (cl-letf (((symbol-function 'libxml-parse-html-region)
                   (lambda (&rest _) '(html (body "full"))))
                  ((symbol-function 'eww-score-readability) #'ignore)
                  ((symbol-function 'eww-highest-readability)
                   (lambda (_dom) '(p "tiny")))
                  ((symbol-function 'shr-insert-document)
                   (lambda (dom)
                     (insert (if (eq (car dom) 'p)
                                 "tiny"
                               "Full rendered page content")))))
          (gptel-agent--web-fetch-callback
           (lambda (value) (setq result value)) nil))))
    (should (equal result "Full rendered page content"))))

(ert-deftest gptel-agent-web-fetch-rejects-binary-content-clearly ()
  (let (result)
    (with-temp-buffer
      (insert "%PDF binary data")
      (let ((url-http-end-of-headers (point-min))
            (url-http-content-type "application/pdf"))
        (gptel-agent--web-fetch-callback
         (lambda (value) (setq result value)) nil)))
    (should (string-match-p "unsupported binary content-type application/pdf"
                            result))))

(ert-deftest gptel-agent-web-fetch-bounds-large-responses ()
  (let ((result (gptel-agent--web-fetch-limit (make-string 1500 ?x) 1000)))
    (should (string-prefix-p (make-string 1000 ?x) result))
    (should (string-match-p "truncated 500 remaining characters" result))))

(ert-deftest gptel-agent-web-fetch-dispatches-youtube-urls ()
  (let (generic-fetch video-id result)
    (cl-letf (((symbol-function 'gptel-agent--yt-fetch-watch-page)
               (lambda (callback id)
                 (setq video-id id)
                 (funcall callback "video transcript")))
              ((symbol-function 'gptel-agent--fetch-with-timeout)
               (lambda (&rest _args) (setq generic-fetch t))))
      (gptel-agent--read-url
       (lambda (value) (setq result value))
       "https://www.youtube.com/watch?v=H2qJRnV8ZGA"))
    (should (equal video-id "H2qJRnV8ZGA"))
    (should (equal result "video transcript"))
    (should-not generic-fetch)
    (should (string-match-p
             "YouTube URLs return the video description and transcript"
             (gptel-tool-description (gptel-get-tool "WebFetch"))))))

(ert-deftest gptel-agent-bash-schema-requires-an-extraction-query ()
  (let* ((tool (gptel-get-tool "Bash"))
         (args (gptel-tool-args tool))
         (query (cadr args)))
    (should (equal (mapcar (lambda (arg) (plist-get arg :name)) args)
                   '("command" "query")))
    (should-not (plist-get query :optional))
    (should (string-match-p "exact question"
                            (plist-get query :description)))))

(ert-deftest gptel-agent-oversized-result-is-persisted-with-explorer-contract ()
  (let ((gptel-agent-tool-result-max-chars 40)
        (gptel-agent-tool-result-preview-chars 20)
        (query "Extract every failing test and its reason")
        result-file)
    (unwind-protect
        (with-temp-buffer
          (insert "first line\nsecond line\nthird line containing full evidence\n")
          (let ((full-result (buffer-string)))
            (gptel-agent--truncate-buffer "bash" nil query)
            (let ((result (buffer-string)))
              (setq result-file
                    (gptel-agent-test--saved-result-path result))
              (should result-file)
              (should (string-match-p "subagent_type: \"result-explorer\""
                                      result))
              (should (string-match-p (regexp-quote (format "query: %S" query))
                                      result))
              (should (equal (plist-get
                              (gethash result-file
                                       gptel-agent--saved-tool-results)
                              :query)
                             query))
              (should (equal (with-temp-buffer
                               (insert-file-contents result-file)
                               (buffer-string))
                             full-result)))))
      (when result-file
        (remhash result-file gptel-agent--saved-tool-results)
        (when (file-exists-p result-file)
          (delete-file result-file))))))

(ert-deftest gptel-agent-bash-bounds-async-output-and-saves-the-full-result ()
  (let ((gptel-agent-tool-result-max-chars 100)
        (gptel-agent-tool-result-preview-chars 40)
        result result-file)
    (unwind-protect
        (let ((process
               (gptel-agent--execute-bash
                (lambda (value) (setq result value))
                "for ((i=1; i<=80; i++)); do printf 'record-%03d\\n' \"$i\"; done"
                "Find record 080")))
          (while (and (not result) (process-live-p process))
            (accept-process-output process 0.1))
          (unless result
            (accept-process-output process 0.1))
          (should (string-match-p "Preview" result))
          (setq result-file (gptel-agent-test--saved-result-path result))
          (should result-file)
          (should (string-match-p "record-080"
                                  (with-temp-buffer
                                    (insert-file-contents result-file)
                                    (buffer-string)))))
      (when result-file
        (remhash result-file gptel-agent--saved-tool-results)
        (when (file-exists-p result-file)
          (delete-file result-file))))))

(ert-deftest gptel-agent-result-explorer-tools-are-locked-and-cite-the-file ()
  (let ((query "Find the error") result-file)
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "startup ok\nERROR database unavailable\nshutdown\n")
            (setq result-file (gptel-agent--save-tool-result "bash" query)))
          (let ((gptel-agent--current-agent "result-explorer")
                (gptel-agent--result-file-scope result-file))
            (should (string-match-p
                     (regexp-quote
                      (format "%s:2:ERROR database unavailable" result-file))
                     (gptel-agent--read-scoped-result 2 2)))
            (should (string-match-p
                     (regexp-quote (format "%s:2:" result-file))
                     (gptel-agent--grep-scoped-result "ERROR" t 10))))
          (let ((gptel-agent--current-agent "researcher")
                (gptel-agent--result-file-scope result-file))
            (should-error (gptel-agent--read-scoped-result 1 1)
                          :type 'error)))
      (when result-file
        (remhash result-file gptel-agent--saved-tool-results)
        (when (file-exists-p result-file)
          (delete-file result-file))))))

(ert-deftest gptel-agent-result-explorer-rejects-a-changed-scope-query ()
  (let ((query "Find failures") result-file)
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "failure evidence")
            (setq result-file (gptel-agent--save-tool-result "bash" query)))
          (should-not (gptel-agent--result-explorer-contract-error
                       result-file query))
          (should (string-match-p
                   "does not exactly match"
                   (gptel-agent--result-explorer-contract-error
                    result-file "Explore the repository instead"))))
      (when result-file
        (remhash result-file gptel-agent--saved-tool-results)
        (when (file-exists-p result-file)
          (delete-file result-file))))))

(ert-deftest gptel-agent-result-explorer-dispatch-enforces-the-contract ()
  (let ((query "Find failures") result-file response
        (parent (generate-new-buffer " *gptel-agent-result-parent*")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "failure evidence")
            (setq result-file (gptel-agent--save-tool-result "bash" query)))
          (let ((fsm (gptel-make-fsm :info (list :buffer parent))))
            (gptel-agent--task
             (lambda (value) (setq response value))
             "result-explorer" "Explore result" "ignored prompt"
             fsm '(("result-explorer" :description "Scoped explorer"))
             result-file "Inspect unrelated files"))
          (should (string-match-p "Invalid result-explorer scope contract"
                                  response)))
      (when (buffer-live-p parent)
        (kill-buffer parent))
      (when result-file
        (remhash result-file gptel-agent--saved-tool-results)
        (when (file-exists-p result-file)
          (delete-file result-file))))))

(ert-deftest gptel-agent-web-request-error-includes-response-details ()
  (with-temp-buffer
    (insert "Not found on this branch")
    (let ((url-http-end-of-headers (point-min))
          (url-http-content-type "text/plain")
          (url-http-response-status 404))
      (let ((message (gptel-agent--web-request-error
                      "Fetch for test" '(error http 404))))
        (should (string-match-p "HTTP 404, content-type text/plain" message))
        (should (string-match-p "Not found on this branch" message))))))

(ert-deftest gptel-agent-inspect-link-is-clickable ()
  (let* ((run (gptel-agent--make-run))
         (link (gptel-agent--run-inspect-button run)))
    (should (string-match-p "Inspect sub-agent" link))
    (should (text-property-not-all 0 (length link) 'keymap nil link))))

(ert-deftest gptel-agent-active-child-buffer-kill-requires-confirmation ()
  (let* ((child (generate-new-buffer " *gptel-agent-kill-guard*"))
         (run (gptel-agent--make-run
               :id "kill-guard" :state 'requesting :agent "researcher"
               :child-buffer child)))
    (unwind-protect
        (with-current-buffer child
          (setq-local gptel-agent--supervised-run run)
          (add-hook 'kill-buffer-query-functions
                    #'gptel-agent--confirm-child-buffer-kill nil t)
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
            (should-not (kill-buffer child)))
          (should (buffer-live-p child)))
      (when (buffer-live-p child)
        (with-current-buffer child
          (setq-local kill-buffer-query-functions nil))
        (kill-buffer child)))))

(ert-deftest gptel-agent-killing-active-child-cancels-owned-request ()
  (let* ((parent (generate-new-buffer " *gptel-agent-kill-parent*"))
         (child (generate-new-buffer " *gptel-agent-kill-child*"))
         delivered aborted-buffer
         (run (gptel-agent--make-run
               :id "kill-child" :state 'requesting :agent "researcher"
               :parent-buffer parent :child-buffer child
               :callback (lambda (result) (setq delivered result)))))
    (unwind-protect
        (progn
          (with-current-buffer child
            (setq-local gptel-agent--supervised-run run)
            (add-hook 'kill-buffer-query-functions
                      #'gptel-agent--confirm-child-buffer-kill nil t)
            (add-hook 'kill-buffer-hook
                      #'gptel-agent--handle-child-buffer-kill nil t))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'gptel-agent--run-abort-transport)
                     (lambda (owned-run)
                       (setq aborted-buffer
                             (gptel-agent--run-child-buffer owned-run)))))
            (should (kill-buffer child)))
          (should (eq (gptel-agent--run-state run) 'cancelled))
          (should (eq aborted-buffer child))
          (should (string-match-p "task was cancelled" delivered)))
      (when (buffer-live-p child)
        (with-current-buffer child
          (setq-local kill-buffer-query-functions nil))
        (kill-buffer child))
      (when (buffer-live-p parent) (kill-buffer parent)))))

(ert-deftest gptel-agent-run-finishes-exactly-once ()
  (gptel-agent-test--with-run (run _fsm received)
    (should (gptel-agent--run-finish run 'completed "first" 'done))
    (should-not (gptel-agent--run-finish run 'failed "second" 'late-error))
    (should (eq (gptel-agent--run-state run) 'completed))
    (should (equal received '("first")))
    (should-not (buffer-live-p (gptel-agent--run-child-buffer run)))))

(ert-deftest gptel-agent-timeout-aborts-the-owned-child-request ()
  (gptel-agent-test--with-run (run _fsm received)
    (let (aborted-buffer)
      (cl-letf (((symbol-function 'gptel-abort)
                 (lambda (buffer) (setq aborted-buffer buffer))))
        (gptel-agent--run-finish run 'timed-out "timeout" 'timeout))
      (should (eq aborted-buffer (gptel-agent--run-child-buffer run)))
      (should-not (eq aborted-buffer (gptel-agent--run-parent-buffer run)))
      (should (equal received '("timeout"))))))

(ert-deftest gptel-agent-parent-abort-cancels-child-and-parent-fsm ()
  (gptel-agent-test--with-run (run _child-fsm received)
    (let ((parent-fsm (gptel-make-fsm :state 'TOOL :handlers nil)))
      (setf (gptel-agent--run-parent-fsm run) parent-fsm)
      (puthash (gptel-agent--run-id run) run gptel-agent--runs)
      (cl-letf (((symbol-function 'gptel-agent--run-abort-transport) #'ignore))
        (should (= (gptel-agent--cancel-runs-for-buffer
                    (gptel-agent--run-parent-buffer run))
                   1)))
      (should (eq (gptel-agent--run-state run) 'cancelled))
      (should (eq (gptel-fsm-state parent-fsm) 'ABRT))
      (should-not received))))

(ert-deftest gptel-agent-done-state-owns-completion ()
  (gptel-agent-test--with-run (run fsm received)
    (setf (gptel-agent--run-response run) "final evidence")
    (gptel-agent--handle-task-done fsm)
    (should (eq (gptel-agent--run-state run) 'completed))
    (should (= (length received) 1))
    (should (string-match-p "Researcher result for task: test task"
                            (car received)))
    (should (string-match-p "final evidence" (car received)))))

(ert-deftest gptel-agent-transient-error-schedules-bounded-retry ()
  (gptel-agent-test--with-run (run fsm received)
    (setf (gptel-fsm-info fsm)
          (list :context run :status "Curl failure"
                :error "Curl failed with exit code 56"))
    (setf (gptel-agent--run-response run) "kept partial-duplicate"
          (gptel-agent--run-response-checkpoint run) 4)
    (let ((gptel-agent-max-request-retries 1)
          (gptel-agent-retry-delay 60))
      (gptel-agent--handle-task-error fsm))
    (should (eq (gptel-agent--run-state run) 'retry-wait))
    (should (= (gptel-agent--run-retries run) 1))
    (should (equal (gptel-agent--run-response run) "kept"))
    (should (timerp (gptel-agent--run-retry-timer run)))
    (should-not received)))

(ert-deftest gptel-agent-auth-error-is-terminal-without-retry ()
  (gptel-agent-test--with-run (run fsm received)
    (setf (gptel-fsm-info fsm)
          (list :context run :status "Unauthorized" :http-status "401"
                :error "Invalid API key"))
    (gptel-agent--handle-task-error fsm)
    (should (eq (gptel-agent--run-state run) 'failed))
    (should (= (gptel-agent--run-retries run) 0))
    (should (= (length received) 1))))

(ert-deftest gptel-agent-round-budget-is-terminal ()
  (gptel-agent-test--with-run (run fsm received)
    (setf (gptel-agent--run-rounds run) 1
          (gptel-agent--run-max-request-rounds run) 1)
    (gptel-agent--handle-task-wait fsm)
    (should (eq (gptel-agent--run-state run) 'inconclusive))
    (should (= (length received) 1))
    (should (string-match-p "limit of 1 model/tool rounds" (car received)))
    (should (string-match-p "Confidence: low" (car received)))))

(ert-deftest gptel-agent-finalization-disables-tools-and-injects-synthesis-turn ()
  (gptel-agent-test--with-run (run fsm received)
    (let* ((data '(:input [existing-transcript]
                  :tools [tool-schema] :tool_choice "required"
                  :toolConfig (:mode "AUTO")))
           (info (list :context run :backend 'fake-backend
                       :data data :tools '(tool-spec)))
           injected)
      (setf (gptel-agent--run-description run) "summarize repository"
            (gptel-agent--run-finalization-reason run)
            "the same WebFetch call kept repeating.")
      (cl-letf (((symbol-function 'gptel--parse-list)
                 (lambda (_backend prompts) (list :parsed prompts)))
                ((symbol-function 'gptel--inject-prompt)
                 (lambda (_backend request-data prompt &optional _position)
                   (setq injected (list request-data prompt)))))
        (should (gptel-agent--begin-finalization run info)))
      (should (gptel-agent--run-finalization-requested run))
      (should-not (plist-get info :tools))
      (should-not (plist-member (plist-get info :data) :tools))
      (should-not (plist-member (plist-get info :data) :tool_choice))
      (should-not (plist-member (plist-get info :data) :toolConfig))
      (should injected)
      (should (string-match-p
               "return your final answer"
               (prin1-to-string injected)))
      (with-current-buffer (gptel-agent--run-child-buffer run)
        (should-not gptel-use-tools)
        (should-not gptel-tools))
      ;; Only the model's interpreted response is delivered to the parent.
      (setf (gptel-fsm-info fsm) (list :context run)
            (gptel-agent--run-response run) "interpreted final answer")
      (gptel-agent--handle-task-done fsm)
      (should (eq (gptel-agent--run-state run) 'completed))
      (should (= (length received) 1))
      (should (string-match-p "interpreted final answer" (car received)))
      (should-not (string-match-p "existing-transcript" (car received))))))

(ert-deftest gptel-agent-finalization-gets-a-turn-at-the-round-limit ()
  (gptel-agent-test--with-run (run fsm received)
    (setf (gptel-agent--run-rounds run) 1
         (gptel-agent--run-finalization-requested run) t
          (gptel-agent--run-max-request-rounds run) 1)
    (let (handled)
      (cl-letf (((symbol-function 'gptel--handle-wait)
                 (lambda (_fsm) (setq handled t))))
        (gptel-agent--handle-task-wait fsm))
      (should handled)
      (should (gptel-agent--run-finalization-started run))
      (should (= (gptel-agent--run-rounds run) 2))
      (should-not received))))

(ert-deftest gptel-agent-researcher-prompt-is-local-first-and-metadata-aware ()
  (let ((prompt (gptel-agent-test--agent-prompt "researcher")))
    (should (string-match-p "current workspace is the primary source" prompt))
    (should (string-match-p "default_branch" prompt))
    (should (string-match-p "successful result.*stop signal" prompt))))

(ert-deftest gptel-agent-built-in-supervision-policy-is-role-specific ()
  (let ((researcher (gptel-agent-test--agent-prompt "researcher"))
        (executor (gptel-agent-test--agent-prompt "executor"))
        (introspector (gptel-agent-test--agent-prompt "introspector")))
    (should-not gptel-agent-task-timeout)
    (should-not gptel-agent-max-request-rounds)
    (should (string-match-p "task-timeout: 1200" researcher))
    (should (string-match-p "max-request-rounds: 30" researcher))
    (should-not (string-match-p "task-timeout:" executor))
    (should-not (string-match-p "max-request-rounds:" executor))
    (should-not (string-match-p "task-timeout:" introspector))
    (should-not (string-match-p "max-request-rounds:" introspector))))

(ert-deftest gptel-agent-result-explorer-prompt-is-strict-and-evidence-based ()
  (let ((prompt (gptel-agent-test--agent-prompt "result-explorer")))
    (should (string-match-p "- ResultGrep\n  - ResultRead" prompt))
    (dolist (tool '("Read" "Grep" "Bash" "Agent"))
      (should-not (string-match-p
                   (format "^  - %s$" (regexp-quote tool)) prompt)))
    (should (string-match-p "untrusted data" prompt))
    (should (string-match-p "absolute file path and exact line number"
                            prompt))
    (should (string-match-p "cannot be established" prompt))))

(ert-deftest gptel-agent-tool-schema-is-request-local ()
  (let* ((global-tool (gptel-get-tool "Agent"))
         (global-args (copy-tree (gptel-tool-args global-tool)))
         (fsm (gptel-make-fsm))
         captured local-tool)
    (with-temp-buffer
      (setq-local
       gptel-tools (list global-tool)
       gptel-agent--current-agent nil
       gptel-agent--agent-snapshot
       '(("executor" :description "Executes work" :system "execute")
         ("researcher" :description "Finds evidence" :system "research")))
      (gptel-agent--localize-agent-tool fsm)
      (setq local-tool (car gptel-tools)))
    (let ((local-tool local-tool))
      (should-not (eq local-tool global-tool))
      (should (equal (append (plist-get (car (gptel-tool-args local-tool)) :enum)
                             nil)
                     '("executor" "researcher")))
      (should (equal (mapcar (lambda (arg) (plist-get arg :name))
                             (gptel-tool-args local-tool))
                     '("subagent_type" "description" "prompt"
                       "result_file" "query")))
      (should (equal (gptel-tool-args global-tool) global-args))
      (cl-letf (((symbol-function 'gptel-agent--task)
                 (lambda (&rest args) (setq captured args))))
        (funcall (gptel-tool-function local-tool)
                 #'ignore "executor" "do work" "details"))
      (should (eq (nth 4 captured) fsm))
      (should (equal (mapcar #'car (nth 5 captured))
                     '("executor" "researcher"))))))

(ert-deftest gptel-agent-session-records-its-initial-preset ()
  (let ((session-buffer (generate-new-buffer " *gptel-agent-session-test*"))
        (gptel-use-header-line nil))
    (unwind-protect
        (cl-letf (((symbol-function 'gptel)
                   (lambda (&rest _args) session-buffer))
                  ((symbol-function 'gptel-agent-update) #'ignore)
                  ((symbol-function 'gptel--apply-preset) #'ignore))
          (gptel-agent default-directory 'ask)
          (with-current-buffer session-buffer
            (should (equal gptel-agent--current-agent "ask"))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest gptel-agent-skill-discovery-uses-cached-exact-metadata ()
  (let ((gptel-agent-skill-dirs '("/skills"))
        gptel-agent--skills cached-file cached-full filename-regexp)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_dir) t))
              ((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'directory-files-recursively)
               (lambda (_dir regexp &rest _args)
                 (setq filename-regexp regexp)
                 '("/skills/example/SKILL.md")))
              ((symbol-function 'gptel-agent--cached-read-file)
               (lambda (file full)
                 (setq cached-file file
                       cached-full full)
                 '("example" :description "Example skill")))
              ((symbol-function 'gptel-agent-read-file)
               (lambda (&rest _args)
                 (ert-fail "Skill discovery bypassed the file cache"))))
      (gptel-agent--update-skills))
    (should (equal filename-regexp "SKILL\\.md$"))
    (should (equal cached-file "/skills/example/SKILL.md"))
    (should-not cached-full)
    (should (equal (car (alist-get "example" gptel-agent--skills
                                   nil nil #'string-equal))
                   "/skills/example/"))))

(ert-deftest gptel-agent-project-registry-snapshot-is-isolated ()
  (let ((gptel-agent--agents
         '(("other-project" :description "Wrong project")))
        (source (generate-new-buffer " *gptel-agent-project-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq-local
             gptel-agent--registry-snapshot
             '(("this-project" :description "Right project" :system "local"))))
          (with-temp-buffer
            (setq-local gptel-tools (list (gptel-get-tool "Agent")))
            (let ((fsm (gptel-make-fsm :info (list :buffer source))))
              (gptel-agent--localize-agent-tool fsm))
            (should
             (equal (append
                     (plist-get (car (gptel-tool-args (car gptel-tools))) :enum)
                     nil)
                    '("this-project")))))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest gptel-agent-subagent-configuration-applies-each-layer ()
  (let (events)
    (with-temp-buffer
      (gptel-agent--apply-subagent-configuration
       (list :pre (lambda () (push 'preset-pre events))
             :post (lambda () (push 'preset-post events))
             :stream nil :temperature 0.2)
       (list :pre (lambda () (push 'agent-pre events))
             :post (lambda () (push 'agent-post events))
             :task-timeout 1200 :max-request-rounds 30
             :stream t :temperature 0.7))
      (should (eq gptel-stream t))
      (should (= gptel-temperature 0.7)))
    (should (equal (nreverse events)
                   '(preset-pre preset-post agent-pre agent-post)))))

(ert-deftest gptel-agent-supervision-values-are-per-agent ()
  (let ((agent '(:task-timeout 1200 :max-request-rounds 30)))
    (should (= (gptel-agent--supervision-value
                agent :task-timeout 700) 1200))
    (should (= (gptel-agent--supervision-value
                agent :max-request-rounds 15) 30))
    (should-not (gptel-agent--supervision-value
                 '(:task-timeout nil) :task-timeout 700))
    (should-not (gptel-agent--supervision-value
                 nil :task-timeout nil))
    (should-error
     (gptel-agent--supervision-value
      '(:task-timeout -1) :task-timeout nil)
     :type 'user-error)))

(ert-deftest gptel-agent-unknown-subagent-preset-is-an-error ()
  (should-error
   (gptel-agent--resolve-subagent-preset
   'gptel-agent-test-missing-preset)
   :type 'user-error))

(ert-deftest gptel-agent-named-subagent-preset-applies-parents ()
  (let ((gptel--known-presets (copy-tree gptel--known-presets)))
    (gptel-make-preset 'gptel-agent-test-parent :max-tokens 222)
    (gptel-make-preset 'gptel-agent-test-child
      :parents 'gptel-agent-test-parent :model 'named-preset-model)
    (with-temp-buffer
      (gptel-agent--apply-subagent-configuration
       (gptel-agent--resolve-subagent-preset 'gptel-agent-test-child)
       '(:description "Test agent"))
      (should (eq gptel-model 'named-preset-model))
      (should (= gptel-max-tokens 222)))))

(ert-deftest gptel-agent-task-pins-stream-and-child-buffer ()
  (let* ((parent (generate-new-buffer " *gptel-agent-config-parent*"))
         (parent-fsm (gptel-make-fsm))
         captured requested-prompt run)
    (unwind-protect
        (progn
          (with-current-buffer parent
            (insert "prompt\n")
            (setq-local gptel-stream t
                        gptel-model 'test-model
                        gptel-agent-preset
                        '(:model preset-model :stream nil
                          :temperature 0.2 :max-tokens 321
                          :include-reasoning t :system "Preset system"))
            (setf (gptel-fsm-info parent-fsm)
                  (list :buffer parent :position (point-marker))))
          (cl-letf (((symbol-function 'gptel--update-status) #'ignore)
                    ((symbol-function 'gptel-request)
                     (lambda (prompt &rest args)
                       (setq requested-prompt prompt
                             captured args)
                       (let ((fsm (plist-get args :fsm)))
                         (setf (gptel-fsm-info fsm)
                               (list :buffer (plist-get args :buffer)
                                     :context (plist-get args :context)
                                     :callback (plist-get args :callback)))
                         fsm))))
            (gptel-agent--task
             #'ignore "researcher" "pin config" "perform task" parent-fsm
             '(("researcher" :description "Research" :system "Pinned system"
                :temperature 0.7 :task-timeout 1200
                :max-request-rounds 30))))
          (setq run (plist-get captured :context))
          (should (plist-member captured :stream))
          (should-not (plist-get captured :stream))
          (should (buffer-live-p (plist-get captured :buffer)))
          (should-not (eq (plist-get captured :buffer) parent))
          (should (eq (marker-buffer (plist-get captured :position)) parent))
          (should (equal (plist-get captured :system) "Pinned system"))
          (should (string-match-p
                   "negative or inconclusive finding is a valid result"
                   requested-prompt))
          (should (eq (gptel-agent--run-parent-fsm run) parent-fsm))
          (should (= (gptel-agent--run-task-timeout run) 1200))
          (should (= (gptel-agent--run-max-request-rounds run) 30))
          (with-current-buffer (gptel-agent--run-child-buffer run)
            (should (string-match-p "Sub-agent run: agent-" (buffer-string)))
            (should (string-match-p "perform task" (buffer-string)))
            (should (eq gptel-model 'preset-model))
            (should (= gptel-temperature 0.7))
            (should (= gptel-max-tokens 321))
            (should (eq gptel-include-reasoning t)))
          (should (string-match-p
                   "Inspect sub-agent"
                   (overlay-get (gptel-agent--run-overlay run) 'msg)))
          (should (string-match-p
                   "preset-model"
                   (overlay-get (gptel-agent--run-overlay run) 'msg)))
          (should-not (string-match-p
                       "test-model"
                       (overlay-get (gptel-agent--run-overlay run) 'msg)))
          (let (finalized-run)
            (cl-letf (((symbol-function 'gptel-agent--begin-finalization)
                       (lambda (owned-run _info)
                         (setq finalized-run owned-run))))
              (funcall (plist-get captured :callback)
                       '(tool-result) '(:backend test)))
            (should (eq finalized-run run))))
      (when (and run (gptel-agent--run-active-p run))
        (gptel-agent--run-finish run 'cancelled "cleanup" 'test-cleanup t))
      (when (and run (buffer-live-p (gptel-agent--run-child-buffer run)))
        (kill-buffer (gptel-agent--run-child-buffer run)))
      (when (buffer-live-p parent) (kill-buffer parent)))))

(provide 'gptel-agent-tools-test)
;;; gptel-agent-tools-test.el ends here
