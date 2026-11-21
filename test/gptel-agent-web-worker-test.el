;;; gptel-agent-web-worker-test.el --- Web extraction regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent-web-worker)

(ert-deftest gptel-agent-web-worker-static-text-does-not-load-browser-renderers ()
  (cl-letf (((symbol-function 'shr-insert-document)
             (lambda (&rest _) (ert-fail "Unexpected SHR layout")))
            ((symbol-function 'eww-score-readability)
             (lambda (&rest _) (ert-fail "Unexpected EWW scoring"))))
    (let ((text (gptel-agent-web-worker-fetch
               "<html><head><title>Metadata</title></head><body><nav>Menu</nav><main><h1>Guide</h1><p>Hello <strong>world</strong> &amp; friends.</p><script>secretPayload()</script><style>.secret{color:red}</style><div hidden='hidden'>Hidden secret</div><div aria-hidden='true'>A secret</div></main><footer>Legal footer</footer></body></html>"
               "text/html" "https://example.test/guide" 10000)))
      (should (equal text "Guide\nHello world & friends.")))))

(ert-deftest gptel-agent-web-worker-short-static-html-is-content ()
  (should (equal (gptel-agent-web-worker-fetch
                  "<html><body><p>Version 2.0</p><script>analytics()</script></body></html>"
                  "text/html" "https://example.test" 10000)
                 "Version 2.0")))

(ert-deftest gptel-agent-web-worker-hidden-main-does-not-hide-visible-content ()
  (dolist (hidden '("<main hidden='hidden'>Old route</main>"
                    "<main style='display: none'>Old route</main>"
                    "<main aria-hidden='true'>Old route</main>"
                    "<div hidden='hidden'><main>Old route</main></div>"))
    (should (equal
             (gptel-agent-web-worker-fetch
              (concat "<html><body>Surrounding text" hidden
                      "<main>Visible current article content.</main></body></html>")
              "text/html" "https://example.test" 10000)
             "Visible current article content."))))

(ert-deftest gptel-agent-web-worker-hidden-article-does-not-count-as-visible-article ()
  (should (equal
           (gptel-agent-web-worker-fetch
            "<html><body>Surrounding text<article hidden='hidden'>Old article</article><article>Current article.</article></body></html>"
            "text/html" "https://example.test" 10000)
           "Current article.")))

(ert-deftest gptel-agent-web-worker-search-skips-hidden-titles-and-snippets ()
  (dolist (hidden '("<a class='result__a' hidden='hidden' href='https://old.test'>Old title</a>"
                    "<div class='result__snippet' hidden='hidden'>Old snippet</div>"))
    (let* ((body (concat "<html><body>" hidden
                         "<a class='result__a' href='https://current.test'>Current title</a><div class='result__snippet'>Current snippet</div></body></html>"))
           (result (read (gptel-agent-web-worker-search body "query" 1 "https://engine.test/search")))
           (entry (car (plist-get result :results))))
      (should (= (plist-get result :returned-count) 1))
      (should (equal (plist-get entry :url) "https://current.test"))
      (should (equal (plist-get entry :excerpt) "Current title\nCurrent snippet")))))

(ert-deftest gptel-agent-web-worker-text-and-json-preserved ()
  (dolist (pair '(("application/json" . "{\"items\":[],\"loading\":true}")
                  ("text/plain" . "Enable JavaScript\n  This is documentation.")
                  ("application/xml" . "<version>2</version>")))
    (should (equal (gptel-agent-web-worker-fetch
                    (cdr pair) (car pair) "https://example.test" 10000)
                   (cdr pair)))))

(ert-deftest gptel-agent-web-worker-js-shells-give-browser-subagent-advice ()
  (dolist (body '("<html><body><div id='root'></div><script>bundledCode()</script></body></html>"
                  "<html><body><noscript>You need to enable JavaScript to run this app.</noscript><div id='app'></div></body></html>"
                  "<html><body>Loading...</body></html>"
                  "<html><body><h1>Just a moment...</h1><script>challenge()</script></body></html>"
                  "<html><body><h1>Checking your browser</h1><p>Before continuing...</p><script>challenge()</script></body></html>"
                  "<html><body><h1>Product</h1><div id='root'><nav>Home About</nav>Loading...</div><script>bundle()</script></body></html>"
                  "<html><body><nav>Home Products Support</nav></body></html>"
                  "<html><body><h1>Product</h1><div id='__next'></div><script>bundle()</script></body></html>"))
    (let ((text (gptel-agent-web-worker-fetch body "text/html" "https://example.test/app" 10000)))
      (should (string-match-p "agent-browser" text))
      (should (string-match-p "subagent with browser access" text))
      (should (string-match-p "https://example.test/app" text))
      (should-not (string-match-p "bundle()\\|bundledCode()" text)))))

(ert-deftest gptel-agent-web-worker-real-ssr-content-does-not-trigger-shell-warning ()
  (let* ((body (concat "<html><body><div id='__next'><p>"
                       (make-string 2000 ?x) "</p></div><script>bundle()</script></body></html>"))
         (text (gptel-agent-web-worker-fetch body "text/html" "https://example.test" 10000)))
    (should (= (length text) 2000))
    (should-not (string-match-p "agent-browser" text))))

(ert-deftest gptel-agent-web-worker-binary-content-is-rejected ()
  (should (string-match-p "unsupported binary content-type application/pdf"
                          (gptel-agent-web-worker-fetch "%PDF" "application/pdf" nil 1000)))
  (should (string-match-p "NUL bytes"
                          (gptel-agent-web-worker-fetch "a\0b" "text/plain" nil 1000))))

(ert-deftest gptel-agent-web-worker-failed-html-never-exposes-raw-script ()
  (cl-letf (((symbol-function 'libxml-parse-html-region)
             (lambda (&rest _) (error "parser failed"))))
    (let ((text (gptel-agent-web-worker-fetch
                 "<script>contextPollution()</script>" "text/html" "https://example.test" 1000)))
      (should (string-match-p "could not extract HTML" text))
      (should (string-match-p "agent-browser" text))
      (should-not (string-match-p "contextPollution" text)))))

(ert-deftest gptel-agent-web-worker-large-html-and-text-output-are-bounded ()
  (let* ((body (concat "<main><script>" (make-string 300000 ?s)
                       "</script><p>" (make-string 300000 ?x) "</p></main>"))
         (text (gptel-agent-web-worker-fetch body "text/html" nil 1000)))
    (should (string-prefix-p (make-string 1000 ?x) text))
    (should (< (length text) 1300))
    (should (string-match-p "stopped at max_chars" text)))
  (let ((text (gptel-agent-web-worker-fetch (make-string 1500 ?x) "text/plain" nil 1000)))
    (should (string-match-p "truncated 500 remaining characters" text))))

(ert-deftest gptel-agent-web-worker-search-decodes-ddg-and-preserves-coverage ()
  (let* ((body "<html><body><nav><a href='https://duckduckgo.com/settings'>Settings</a></nav><div class='result'><a class='result__a' href='//duckduckgo.com/l/?uddg=https%3A%2F%2Fexample.test%2Fdocs%3Fa%3D1%26b%3D2&amp;rut=tracking'>Documentation</a><a class='result__snippet'>Useful guide</a></div><div class='result'><a class='result__a' href='https://two.test'>Second</a><a class='result__snippet'>More info</a></div></body></html>")
         (result (read (gptel-agent-web-worker-search body "test query" 1 "https://duckduckgo.com/html/?q=test")))
         (entry (car (plist-get result :results))))
    (should (equal (plist-get result :query) "test query"))
    (should (= (plist-get result :requested-count) 1))
    (should (= (plist-get result :returned-count) 1))
    (should (string-match-p "absence is not proof" (plist-get result :coverage-note)))
    (should (equal (plist-get entry :url) "https://example.test/docs?a=1&b=2"))
    (should (equal (plist-get entry :excerpt) "Documentation\nUseful guide"))))

(ert-deftest gptel-agent-web-worker-search-lite-snippets-and-deduplication ()
  (let* ((body "<table><tr><td><a class='result-link' href='https://example.test'>First</a></td></tr><tr><td class='result-snippet'>Snippet</td></tr><tr><td><a class='result-link' href='https://example.test'>Again</a></td></tr></table>")
         (result (read (gptel-agent-web-worker-search body "query" 10 "https://lite.duckduckgo.com/lite/"))))
    (should (= (plist-get result :returned-count) 1))
    (should (equal (plist-get (car (plist-get result :results)) :excerpt) "First\nSnippet"))))

(ert-deftest gptel-agent-web-worker-search-fallback-filters-engine-navigation ()
  (let* ((body "<body><a href='/settings'>Settings</a><a href='/url?q=https%3A%2F%2Fexample.test%2Fpage&amp;other=tracking'>Target</a><a href='javascript:alert(1)'>Invalid</a></body>")
         (result (read (gptel-agent-web-worker-search body "query" 10 "https://engine.test/search"))))
    (should (= (plist-get result :returned-count) 1))
    (should (equal (plist-get (car (plist-get result :results)) :url) "https://example.test/page"))))

(ert-deftest gptel-agent-web-worker-empty-search-suggests-browser-without-claiming-absence ()
  (let ((result (read (gptel-agent-web-worker-search
                       "<body><div id='root'></div><script>bundle()</script></body>"
                       "query" 10 "https://engine.test/search"))))
    (should (= (plist-get result :returned-count) 0))
    (should (string-match-p "agent-browser" (plist-get result :coverage-note)))
    (should (string-match-p "absence is not proof" (plist-get result :coverage-note)))))

(defun gptel-agent-web-worker-test--job (body headers &rest extra)
  "Run a worker job for BODY and HEADERS with EXTRA plist values."
  (let* ((directory (make-temp-file "gptel-web-worker-test-" t))
         (body-file (expand-file-name "body" directory))
         (headers-file (expand-file-name "headers" directory))
         (job-file (expand-file-name "job" directory))
         (result-file (expand-file-name "result" directory)))
    (unwind-protect
        (progn
          (with-temp-file body-file (insert body))
          (with-temp-file headers-file (insert headers))
          (with-temp-file job-file
            (prin1 (append extra
                           (list :kind 'fetch :body-file body-file :headers-file headers-file
                                 :url "https://example.test" :max-chars 10000))
                   (current-buffer)))
          (gptel-agent-web-worker-run job-file result-file)
          (with-temp-buffer
            (insert-file-contents result-file)
            (buffer-string)))
      (delete-directory directory t))))

(ert-deftest gptel-agent-web-worker-job-uses-final-headers-and-decoding ()
  (should (equal
           (gptel-agent-web-worker-test--job
            "{\"text\":\"café\"}"
            "HTTP/1.1 302 Found\r\nContent-Type: text/html\r\n\r\nHTTP/2 200\r\nContent-Type: application/json; charset=utf-8\r\n\r\n")
           "{\"text\":\"café\"}")))

(ert-deftest gptel-agent-web-worker-http-error-includes-clean-excerpt ()
  (let ((text (gptel-agent-web-worker-test--job
               "<body><p>Access denied</p><script>challengeSecret()</script></body>"
               "HTTP/2 403\r\nContent-Type: text/html\r\n\r\n")))
    (should (string-match-p "HTTP 403" text))
    (should (string-match-p "Access denied" text))
    (should (string-match-p "agent-browser" text))
    (should-not (string-match-p "challengeSecret" text))))

(ert-deftest gptel-agent-web-worker-job-bounds-input-before-parsing ()
  (cl-letf (((symbol-function 'gptel-agent-web-worker--parse)
             (lambda (&rest _) (ert-fail "Oversized body reached parser"))))
    (let ((text (gptel-agent-web-worker-test--job
                 (concat "<body>" (make-string 1024 ?x) "privateTail</body>")
                 "HTTP/2 200\r\nContent-Type: text/html\r\n\r\n"
                 :max-bytes 1024)))
      (should (string-match-p "exceeded the byte limit" text))
      (should-not (string-match-p "privateTail" text)))))

(ert-deftest gptel-agent-web-worker-rejects-oversized-header-history ()
  (let ((text (gptel-agent-web-worker-test--job
               "<body>Access denied</body>"
               (concat "HTTP/1.1 302 Found\r\nContent-Type: text/plain\r\nX-Large: "
                       (make-string 65536 ?x)
                       "\r\n\r\nHTTP/2 403\r\nContent-Type: text/html\r\n\r\n"))))
    (should (string-match-p "headers exceeded" text))
    (should-not (string-match-p "<body>" text))))

(ert-deftest gptel-agent-web-worker-keeps-preformatted-code-indentation ()
  (should (equal
           (gptel-agent-web-worker-fetch
            "<main><h1>Example</h1><pre><code>if x:\n  print(x)\n\n  print(2)</code></pre><p>Done.</p></main>"
            "text/html" nil 1000)
           "Example\nif x:\n  print(x)\n\n  print(2)\nDone."))
  (should (equal (gptel-agent-web-worker-fetch
                  "<pre>  indented first line\n    next line</pre>" "text/html" nil 1000)
                 "  indented first line\n    next line")))

(ert-deftest gptel-agent-web-worker-search-output-budget-keeps-valid-plist ()
  (let* ((body (concat "<body>"
                       (mapconcat (lambda (n)
                                    (format "<a class='result__a' href='https://example.test/%d'>%s</a>"
                                            n (make-string 1000 ?x)))
                                  (number-sequence 1 30) "")
                       "</body>"))
         (text (gptel-agent-web-worker-search body "many results" 30 "https://engine.test" 4000))
         (result (read text)))
    (should (<= (length text) 4000))
    (should (< (plist-get result :returned-count) 30))
    (should (> (plist-get result :returned-count) 0))
    (should (string-match-p "omitted to fit" (plist-get result :coverage-note))))
  (should (<= (length (gptel-agent-web-worker-search
                        "<body>No results</body>" (make-string 10000 ?q) 30
                        "https://engine.test" 1000))
               1000)))

(provide 'gptel-agent-web-worker-test)
;;; gptel-agent-web-worker-test.el ends here
