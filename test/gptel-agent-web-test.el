;;; gptel-agent-web-test.el --- Async web lifecycle regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent-web)

(defvar gptel-agent-policy--active-check)

(defun gptel-agent-web-test--wait (predicate &optional seconds)
  "Wait up to SECONDS for PREDICATE while servicing Emacs events."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(cl-defmacro gptel-agent-web-test--with-queue (&body body)
  "Run BODY with an isolated request queue and source buffer."
  (declare (indent 0) (debug t))
  `(let ((gptel-agent--web-active nil)
         (gptel-agent--web-queue nil)
         (gptel-agent-web-max-concurrent-requests 2)
         (gptel-agent-web-max-response-bytes (* 1024 1024))
         (gptel-agent-web-request-timeout 5)
         (source (generate-new-buffer " *web-test-owner*")))
     (unwind-protect
         (with-current-buffer source ,@body)
       (when (buffer-live-p source) (kill-buffer source))
       (mapc #'gptel-agent--web-dispose
             (append gptel-agent--web-active gptel-agent--web-queue)))))

(ert-deftest gptel-agent-web-async-parallelism-and-fifo-drain ()
  (gptel-agent-web-test--with-queue
    (let (started results first second third fourth)
      (cl-letf (((symbol-function 'gptel-agent--web-start)
                 (lambda (job) (setq started (append started (list job))))))
        (setq first (gptel-agent--web-enqueue
                     (lambda (value) (push value results))
                     'fetch "https://example.test/one" nil)
              second (gptel-agent--web-enqueue
                      (lambda (value) (push value results))
                      'fetch "https://example.test/two" nil)
              third (gptel-agent--web-enqueue
                     (lambda (value) (push value results))
                     'fetch "https://example.test/three" nil)
              fourth (gptel-agent--web-enqueue
                      (lambda (value) (push value results))
                      'fetch "https://example.test/four" nil))
        (should (equal started (list first second)))
        (should (= (length gptel-agent--web-active) 2))
        (should (equal gptel-agent--web-queue (list third fourth)))
        (gptel-agent--web-finish second "second")
        (should (equal started (list first second third)))
        (should (= (length gptel-agent--web-active) 2))
        (should (equal gptel-agent--web-queue (list fourth)))
        (gptel-agent--web-finish first "first")
        (should (equal started (list first second third fourth)))
        (gptel-agent--web-finish third "third")
        (gptel-agent--web-finish fourth "fourth")
        (should-not gptel-agent--web-active)
        (should-not gptel-agent--web-queue)
        (should (equal (nreverse results) '("second" "first" "third" "fourth")))))))

(ert-deftest gptel-agent-web-policy-redirects-checked-before-download ()
  (gptel-agent-web-test--with-queue
    (let ((first "https://youtu.be/test")
          (second "https://youtu.be/second")
          (outside "https://elsewhere.example/private")
          checked launched result)
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (_program &rest _) (expand-file-name invocation-name invocation-directory)))
                ((symbol-function 'gptel-agent--web-launch)
                 (lambda (job command _filter sentinel)
                   (let ((url (cadr (member "--url" command))))
                     (push url launched)
                     (should (member "--globoff" command))
                     (should-not (member "--location" command))
                     (with-temp-file (cadr (member "--dump-header" command))
                       (insert "HTTP/1.1 302 Found\r\nLocation: "
                               (cond ((equal url first) "/second")
                                     ((equal url second) outside)
                                     (t (ert-fail "Downloaded denied URL")))
                               "\r\n\r\n"))
                     (setf (gptel-agent--web-job-process job)
                           (make-process :name "mock-http" :command (list (car command) "-Q" "--batch")
                                         :noquery t :sentinel sentinel))))))
        ;; Only the initial call runs in this dynamic binding. Queued hops
        ;; must use the request-owned checker captured by the WebFetch job.
        (let ((gptel-agent-policy--active-check
               (lambda (args)
                 (let ((url (plist-get args :url)))
                   (push url checked) (member url (list first second))))))
          (gptel-agent--read-url (lambda (value) (setq result value)) first))
        (gptel-agent-web-test--wait (lambda () result))
        (should (string-match-p "denied by tool policy" result))
        (should (equal (nreverse checked) (list first second outside)))
        (should (equal (nreverse launched) (list first second)))))))

(defun gptel-agent-web-test--literal-url (path)
  "Fetch literal PATH with real curl, directly and through a redirect."
  (unless (executable-find "curl") (ert-skip "curl is unavailable"))
  (gptel-agent-web-test--with-queue
    (let ((process-environment (copy-sequence process-environment))
          server clients requests)
      (setenv "NO_PROXY" "127.0.0.1")
      (setenv "no_proxy" "127.0.0.1")
      (unwind-protect
          (progn
            (setq server
                  (condition-case err
                      (make-network-process
                       :name "gptel-web-literal-url" :family 'ipv4 :server t
                       :host "127.0.0.1" :service t :noquery t :coding 'binary
                       :log (lambda (_server client _message) (push client clients))
                       :filter
                       (lambda (client data)
                         (let ((request (concat (process-get client 'request) data)))
                           (process-put client 'request request)
                           (when (and (not (process-get client 'answered))
                                      (string-match-p "\r\n\r\n" request))
                             (process-put client 'answered t)
                             (unless (string-match "\\`GET \\([^ ]+\\) HTTP/" request)
                               (ert-fail (format "Unexpected HTTP request: %S" request)))
                             (let* ((target (match-string 1 request))
                                    (redirect (equal target "/redirect"))
                                    (body (if redirect "" (concat "Literal URL: " target))))
                               (push target requests)
                               (process-send-string
                                client
                                (concat (if redirect "HTTP/1.0 302 Found\r\n" "HTTP/1.0 200 OK\r\n")
                                        (when redirect (concat "Location: " path "\r\n"))
                                        "Content-Type: text/plain\r\nConnection: close\r\n"
                                        (format "Content-Length: %d\r\n\r\n" (string-bytes body)) body))
                               (delete-process client))))))
                    (file-error (ert-skip (error-message-string err)))))
            (let* ((base (format "http://127.0.0.1:%d" (process-contact server :service)))
                   (target (concat base path)))
              (dolist (guarded '(nil t))
                (dolist (redirect '(nil t))
                  (let* ((initial (if redirect (concat base "/redirect") target))
                         (expected (if redirect (list "/redirect" path) (list path)))
                         (gptel-agent-policy--active-check
                          (when guarded
                            (lambda (args)
                              (member (plist-get args :url) (list initial target)))))
                         result)
                    (setq requests nil)
                    (gptel-agent--read-url (lambda (value) (setq result value)) initial)
                    (gptel-agent-web-test--wait (lambda () result))
                    (should (equal result (concat "Literal URL: " path)))
                    (should (equal (nreverse requests) expected)))))))
        (dolist (client clients) (when (process-live-p client) (delete-process client)))
        (when (and server (process-live-p server)) (delete-process server))))))

(ert-deftest gptel-agent-web-url-brackets-remain-literal ()
  (gptel-agent-web-test--literal-url "/api?filter[name]=abc"))

(ert-deftest gptel-agent-web-url-braces-never-expand-requests ()
  (gptel-agent-web-test--literal-url "/api?fields={name,email}"))

(ert-deftest gptel-agent-web-async-completes-once-in-owning-buffer ()
  (gptel-agent-web-test--with-queue
    (let (job results)
      (cl-letf (((symbol-function 'gptel-agent--web-start) #'ignore))
        (setq job (gptel-agent--web-enqueue
                   (lambda (value) (push (cons (current-buffer) value) results))
                   'fetch "https://example.test" nil)))
      (with-temp-buffer
        (gptel-agent--web-finish job "ok")
        (gptel-agent--web-finish job "late error")
        (gptel-agent--web-dispose job))
      (should (equal results (list (cons source "ok"))))
      (should (gptel-agent--web-job-done job))
      (should-not (memq (gptel-agent--web-job-timer job) timer-list)))))

(ert-deftest gptel-agent-web-async-source-kill-cancels-queued-and-active ()
  (gptel-agent-web-test--with-queue
    (let ((gptel-agent-web-max-concurrent-requests 1) jobs results)
      (cl-letf (((symbol-function 'gptel-agent--web-start) #'ignore))
        (dotimes (_ 3)
          (push (gptel-agent--web-enqueue
                 (lambda (value) (push value results))
                 'fetch "https://example.test" nil)
                jobs))
        (should (= (length gptel-agent--web-active) 1))
        (should (= (length gptel-agent--web-queue) 2))
        (kill-buffer source)
        (dolist (job jobs)
          (should (gptel-agent--web-job-done job))
          (gptel-agent--web-finish job "stale"))
        (should-not results)
        (should-not gptel-agent--web-active)
        (should-not gptel-agent--web-queue)))))

(ert-deftest gptel-agent-web-async-post-hook-cancels-without-callback ()
  (gptel-agent-web-test--with-queue
    (let ((gptel-agent-web-max-concurrent-requests 1)
          (fsm (gptel-make-fsm :state 'TOOL :info nil))
          jobs results)
      (cl-letf (((symbol-function 'gptel-agent--web-start) #'ignore))
        (dotimes (_ 2)
          (push (gptel-agent--web-enqueue
                 (lambda (value) (push value results))
                 'fetch "https://example.test" nil fsm)
                jobs))
        (should (= (length (plist-get (gptel-fsm-info fsm) :post)) 2))
        (dolist (cleanup (copy-sequence (plist-get (gptel-fsm-info fsm) :post)))
          (funcall cleanup))
        (should-not (plist-get (gptel-fsm-info fsm) :post))
        (should-not gptel-agent--web-active)
        (should-not gptel-agent--web-queue)
        (dolist (job jobs)
          (should (gptel-agent--web-job-done job))
          (gptel-agent--web-finish job "stale"))
        (should-not results)))))

(ert-deftest gptel-agent-web-async-timeout-includes-queue-wait ()
  (gptel-agent-web-test--with-queue
    (let ((gptel-agent-web-max-concurrent-requests 1)
          (gptel-agent-web-request-timeout 0.05)
          jobs results)
      (cl-letf (((symbol-function 'gptel-agent--web-start) #'ignore))
        (dotimes (_ 2)
          (push (gptel-agent--web-enqueue
                 (lambda (value) (push value results))
                 'fetch "https://example.test" nil)
                jobs))
        (gptel-agent-web-test--wait (lambda () (= (length results) 2)) 1)
        (dolist (result results) (should (string-match-p "timed out" result)))
        (dolist (job jobs) (should (gptel-agent--web-job-done job)))
        (should-not gptel-agent--web-active)
        (should-not gptel-agent--web-queue)))))

(ert-deftest gptel-agent-web-async-launch-failure-releases-next-slot ()
  (gptel-agent-web-test--with-queue
    (let ((gptel-agent-web-max-concurrent-requests 1) results job)
      (cl-letf (((symbol-function 'gptel-agent--web-start)
                 (lambda (_job) (error "worker launch failure"))))
        (setq job (gptel-agent--web-enqueue
                   (lambda (value) (push value results))
                   'fetch "https://example.test" nil)))
      (should (= (length results) 1))
      (should (string-match-p "worker launch failure" (car results)))
      (should (gptel-agent--web-job-done job))
      (should-not gptel-agent--web-active)
      (should-not gptel-agent--web-queue))))

(ert-deftest gptel-agent-web-async-callback-error-does-not-redeliver ()
  (gptel-agent-web-test--with-queue
    (let ((calls 0) job)
      (cl-letf (((symbol-function 'gptel-agent--web-start) #'ignore))
        (setq job (gptel-agent--web-enqueue
                   (lambda (_value) (cl-incf calls) (error "callback failed"))
                   'fetch "https://example.test" nil)))
      (should-error (gptel-agent--web-finish job "ok"))
      (gptel-agent--web-finish job "retry")
      (should (= calls 1))
      (should (gptel-agent--web-job-done job))
      (should-not gptel-agent--web-active))))

(ert-deftest gptel-agent-web-async-queued-failure-cannot-suppress-ready-result ()
  (dolist (action '(error kill))
    (gptel-agent-web-test--with-queue
      (let ((gptel-agent-web-max-concurrent-requests 1)
            (starts 0) results first second)
        (cl-letf (((symbol-function 'gptel-agent--web-start)
                   (lambda (_job)
                     (when (> (cl-incf starts) 1) (error "queued startup failed")))))
          (setq first (gptel-agent--web-enqueue
                       (lambda (value) (push (cons 'first value) results))
                       'fetch "https://example.test/first" nil)
                second (gptel-agent--web-enqueue
                        (lambda (value)
                          (push (cons 'second value) results)
                          (if (eq action 'kill)
                              (kill-buffer source)
                            (error "queued callback failed")))
                        'fetch "https://example.test/second" nil))
          (if (eq action 'error)
              (should-error (gptel-agent--web-finish first "completed content"))
            (gptel-agent--web-finish first "completed content"))
          (should (equal (mapcar #'car (reverse results)) '(first second)))
          (should (equal (cdr (assq 'first results)) "completed content"))
          (should (string-match-p "queued startup failed" (cdr (assq 'second results))))
          (should (gptel-agent--web-job-done first))
          (should (gptel-agent--web-job-done second))
          (gptel-agent--web-finish first "late duplicate")
          (should (= (length results) 2))
          (should-not gptel-agent--web-active)
          (should-not gptel-agent--web-queue))))))

(ert-deftest gptel-agent-web-async-stale-filter-cannot-touch-extraction-stage ()
  (gptel-agent-web-test--with-queue
    (let (job results)
      (cl-letf (((symbol-function 'gptel-agent--web-start) #'ignore))
        (setq job (gptel-agent--web-enqueue
                   (lambda (value) (push value results))
                   'fetch "https://example.test" nil)))
      ;; Extraction has released the download buffer and owns another process.
      (setf (gptel-agent--web-job-process job) 'extraction-process)
      (gptel-agent--web-filter job 'obsolete-download "stale bytes")
      (should-not (gptel-agent--web-job-done job))
      (should-not results)
      (gptel-agent--web-finish job "extracted content")
      (should (equal results '("extracted content"))))))

(ert-deftest gptel-agent-web-async-missing-download-buffer-fails-once ()
  (gptel-agent-web-test--with-queue
    (let (job results)
      (cl-letf (((symbol-function 'gptel-agent--web-start) #'ignore))
        (setq job (gptel-agent--web-enqueue
                   (lambda (value) (push value results))
                   'fetch "https://example.test" nil)))
      (gptel-agent--web-filter job nil "bytes without a buffer")
      (should (gptel-agent--web-job-done job))
      (should (= (length results) 1))
      (should (string-match-p "buffer was killed" (car results)))
      (gptel-agent--web-filter job nil "late bytes")
      (should (= (length results) 1)))))

(ert-deftest gptel-agent-web-async-localization-captures-owner-and-search-prefix ()
  (gptel-agent-web-test--with-queue
    (let* ((fsm (gptel-make-fsm :state 'TOOL :info nil))
           (fetch (gptel--make-tool-internal :name "WebFetch" :category "gptel-agent" :function #'ignore))
           (search (gptel--make-tool-internal :name "WebSearch" :category "gptel-agent" :function #'ignore))
           (unrelated (gptel--make-tool-internal :name "Other" :function #'ignore))
           fetched searched)
      (setq-local gptel-tools (list fetch search unrelated)
                  eww-search-prefix "https://search.example.test/find?q=")
      (gptel-agent--localize-web-tools fsm)
      (should-not (eq (car gptel-tools) fetch))
      (should-not (eq (cadr gptel-tools) search))
      (should (eq (caddr gptel-tools) unrelated))
      (should (eq (gptel-tool-function fetch) #'ignore))
      (should (eq (gptel-tool-function search) #'ignore))
      (cl-letf (((symbol-function 'gptel-agent--web-start) #'ignore))
        (setq fetched (funcall (gptel-tool-function (car gptel-tools))
                               #'ignore "https://example.test/article" 2000)
              searched (funcall (gptel-tool-function (cadr gptel-tools))
                                #'ignore "two words" 7)))
      (setq-local eww-search-prefix "https://other.example.test/?q=")
      (should (eq (gptel-agent--web-job-fsm fetched) fsm))
      (should (eq (gptel-agent--web-job-fsm searched) fsm))
      (should (eq (gptel-agent--web-job-source fetched) source))
      (should (eq (gptel-agent--web-job-source searched) source))
      (should (= (gptel-agent--web-job-max-chars fetched) 2000))
      (should (equal (gptel-agent--web-job-url searched)
                     "https://search.example.test/find?q=two%20words"))
      (should (= (plist-get (gptel-agent--web-job-options searched) :count) 7))
      (should (= (length (plist-get (gptel-fsm-info fsm) :post)) 2)))))

(ert-deftest gptel-agent-web-async-youtube-worker-discovers-helper-in-clean-emacs ()
  (gptel-agent-web-test--with-queue
    (let (results job)
      ;; An invalid ID fails before any network call but only after the clean
      ;; worker has found and loaded the separately packaged YouTube helper.
      (setq job (gptel-agent--web-enqueue
                 (lambda (value) (push value results))
                 'youtube "https://youtu.be/test" '(:video-id 42)))
      (gptel-agent-web-test--wait (lambda () results))
      (should (= (length results) 1))
      (should (string-match-p "[Ww]rong type argument" (car results)))
      (should-not (string-match-p "Cannot open load file" (car results)))
      (should (gptel-agent--web-job-done job))
      (should-not (file-exists-p (gptel-agent--web-job-directory job))))))

(ert-deftest gptel-agent-web-async-streaming-input-is-bounded ()
  (gptel-agent-web-test--with-queue
    (let ((gptel-agent-web-max-response-bytes 1024)
          (real-make-process (symbol-function 'make-process))
          results job process)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (apply real-make-process
                          (plist-put args :command '("/bin/sleep" "10"))))))
        (setq job (gptel-agent--web-enqueue
                   (lambda (value) (push value results))
                   'fetch "https://example.test/stream" nil)))
      (setq process (gptel-agent--web-job-process job))
      (should (process-live-p process))
      (gptel-agent--web-filter job process (make-string 1000 ?a))
      (with-current-buffer (gptel-agent--web-job-buffer job)
        (should (= (buffer-size) 1000)))
      ;; No Content-Length is available: the streaming cap must still apply.
      (gptel-agent--web-filter job process (make-string 1000 ?b))
      (should (= (length results) 1))
      (should (string-match-p "\\(?:byte\\|size\\|limit\\)" (car results)))
      (should (gptel-agent--web-job-done job))
      (should-not (process-live-p process))
      (gptel-agent--web-filter job process "late output")
      (should (= (length results) 1)))))

(ert-deftest gptel-agent-web-async-source-kill-stops-process-and-removes-files ()
  (gptel-agent-web-test--with-queue
    (let ((real-make-process (symbol-function 'make-process))
          results job process directory)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (apply real-make-process
                          (plist-put args :command '("/bin/sleep" "10"))))))
        (setq job (gptel-agent--web-enqueue
                   (lambda (value) (push value results))
                   'fetch "https://example.test/slow" nil)))
      (setq process (gptel-agent--web-job-process job)
            directory (gptel-agent--web-job-directory job))
      (should (process-live-p process))
      (should (file-directory-p directory))
      (kill-buffer source)
      (should-not (process-live-p process))
      (should-not (file-exists-p directory))
      (accept-process-output nil 0.02)
      (should-not results))))

(ert-deftest gptel-agent-web-async-obsolete-fsm-stops-process ()
  (gptel-agent-web-test--with-queue
    (let ((real-make-process (symbol-function 'make-process))
          (fsm (gptel-make-fsm :state 'TOOL :info nil))
          results job process)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (apply real-make-process
                          (plist-put args :command '("/bin/sleep" "10"))))))
        (setq job (gptel-agent--web-enqueue
                   (lambda (value) (push value results))
                   'fetch "https://example.test" nil fsm)))
      (setq process (gptel-agent--web-job-process job))
      (setf (gptel-fsm-state fsm) 'DONE)
      (gptel-agent--web-filter job process "obsolete output")
      (should (gptel-agent--web-job-done job))
      (should-not (process-live-p process))
      (should-not (plist-get (gptel-fsm-info fsm) :post))
      (should-not results))))

(ert-deftest gptel-agent-web-async-worker-failure-bounds-diagnostics ()
  (gptel-agent-web-test--with-queue
    (let ((real-make-process (symbol-function 'make-process)) results job)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (apply real-make-process
                          (plist-put args :command
                                     '("/bin/sh" "-c" "printf '%10000s' x >&2; exit 23"))))))
        ;; The YouTube kind goes directly to a worker, without downloading here.
        (setq job (gptel-agent--web-enqueue
                   (lambda (value) (push value results))
                   'youtube "https://youtu.be/test" nil))
        (gptel-agent-web-test--wait (lambda () results)))
      (should (= (length results) 1))
      (should (string-match-p "Web extraction failed (23)" (car results)))
      (should (< (length (car results)) 2200))
      (should (gptel-agent--web-job-done job))
      (should-not (file-exists-p (gptel-agent--web-job-directory job))))))

(ert-deftest gptel-agent-web-async-worker-result-is-bounded-before-reading ()
  (gptel-agent-web-test--with-queue
    (let ((real-make-process (symbol-function 'make-process)) results job)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (let* ((command (plist-get args :command))
                          (form (read (cadr (member "--eval" command))))
                          (result-file (car (last form))))
                     (apply real-make-process
                            (plist-put args :command
                                       (list "/bin/sh" "-c"
                                             "printf '%30000s' x > \"$1\""
                                             "fake-worker" result-file)))))))
        (setq job (gptel-agent--web-enqueue
                   (lambda (value) (push value results))
                   'youtube "https://youtu.be/test" '(:max-chars 1000)))
        (gptel-agent-web-test--wait (lambda () results)))
      (should (= (length results) 1))
      (should (string-match-p "output limit" (car results)))
      (should (< (length (car results)) 1000))
      (should (gptel-agent--web-job-done job))
      (should-not (file-exists-p (gptel-agent--web-job-directory job))))))

(ert-deftest gptel-agent-web-async-extraction-runs-outside-parent-emacs ()
  (skip-unless (and (executable-find "curl") (libxml-available-p)))
  (gptel-agent-web-test--with-queue
    (let ((real-make-process (symbol-function 'make-process))
          (ticks 0) (downloads 0) results jobs timer)
      (unwind-protect
          (cl-letf
              (((symbol-function 'make-process)
                (lambda (&rest args)
                  (let ((command (plist-get args :command)))
                    (when (equal (file-name-nondirectory (car command)) "curl")
                      (cl-incf downloads)
                      (let ((headers (or (cadr (member "--dump-header" command))
                                         (cadr (member "-D" command)))))
                        (should headers)
                        (setq args
                              (plist-put
                               args :command
                               (list
                                "/bin/sh" "-c"
                                (concat
                                 "printf '%s\\r\\n' 'HTTP/1.1 200 OK' "
                                 "'Content-Type: text/html; charset=utf-8' '' > \"$1\"; "
                                 "sleep 0.10; "
                                 "printf '%s' '<!doctype html><html><body><main><h1>Useful result</h1>"
                                 "<p>Static content extracted by the worker.</p></main>"
                                 "<script>window.secret = 42;</script></body></html>'")
                                "fake-curl" headers)))))
                    (apply real-make-process args))))
               ((symbol-function 'libxml-parse-html-region)
                (lambda (&rest _) (ert-fail "HTML parsing ran in parent Emacs")))
               ((symbol-function 'shr-insert-document)
                (lambda (&rest _) (ert-fail "SHR rendering ran in parent Emacs")))
               ((symbol-function 'url-retrieve)
                (lambda (&rest _) (ert-fail "URL transport ran in parent Emacs"))))
            (setq timer (run-at-time 0.005 0.005 (lambda () (cl-incf ticks))))
            (dotimes (i 3)
              (push (gptel-agent--web-enqueue
                     (lambda (value) (push value results))
                     'fetch (format "https://example.test/%d" i) nil)
                    jobs))
            (should (= downloads 2))
            (gptel-agent-web-test--wait (lambda () (= (length results) 3)))
            (should (= downloads 3))
            (should (> ticks 5))
            (dolist (result results)
              (should (string-match-p "Useful result" result))
              (should (string-match-p "Static content extracted by the worker" result))
              (should-not (string-match-p "window.secret" result)))
            (dolist (job jobs)
              (should (gptel-agent--web-job-done job))
              (should-not (file-exists-p (gptel-agent--web-job-directory job))))
            (should-not gptel-agent--web-active)
            (should-not gptel-agent--web-queue))
        (when timer (cancel-timer timer))))))

(provide 'gptel-agent-web-test)
;;; gptel-agent-web-test.el ends here
