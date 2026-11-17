;;; gptel-agent-web-worker.el --- Isolated web text extraction -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Run in a clean batch Emacs.  This worker deliberately does not load gptel,
;; EWW, or SHR: it extracts bounded text without laying out a browser document.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'url-expand)

(defconst gptel-agent-web-worker--ignored-tags
  '(head script style template svg canvas iframe object embed))

(defconst gptel-agent-web-worker--block-tags
  '(address article aside blockquote br dd div dl dt fieldset figcaption figure
    footer form h1 h2 h3 h4 h5 h6 header hr li main nav ol p pre section table
    tbody td th thead tr ul))

(defun gptel-agent-web-worker--browser-advice (url)
  "Describe the browser fallback for URL without assuming tools exist."
  (concat
   "WebFetch reads static responses and cannot run JavaScript. "
   "If available, use agent-browser or another real browser tool to open "
   (if (and url (not (string-empty-p url))) url "this URL")
   ", wait for the relevant content to render, and extract it. "
   "Prefer delegating this browser work to a subagent with browser access; "
   "ask it to return only the relevant content and source URL to keep browser "
   "snapshots out of the main context. If no browser tool is available, "
   "report this limitation or use an accessible text/API version."))

(defun gptel-agent-web-worker--hidden-p (node)
  "Return non-nil for NODE whose content should not be extracted."
  (or (memq (dom-tag node) gptel-agent-web-worker--ignored-tags)
      (assq 'hidden (dom-attributes node))
      (equal (dom-attr node 'aria-hidden) "true")
      (let ((style (dom-attr node 'style)) (case-fold-search t))
        (and style
             (string-match-p
              "\\(?:display[ \t]*:[ \t]*none\\|visibility[ \t]*:[ \t]*hidden\\)"
              style)))))

(defun gptel-agent-web-worker--text (root limit &optional skip-navigation preserve-whitespace)
  "Extract at most LIMIT characters from ROOT without layout or recursion.
Skip navigation and footers when SKIP-NAVIGATION is non-nil.  Preserve
whitespace in preformatted elements, or throughout when PRESERVE-WHITESPACE
is non-nil.  Return a cons of text and whether the limit stopped traversal."
  (with-temp-buffer
    (let ((pending (list root)) limited)
      (while (and pending (< (buffer-size) limit))
        (let ((node (pop pending)))
          (cond
           ((eq node :break)
            (when (and (> (buffer-size) 0) (not (eq (char-before) ?\n)))
              (insert "\n")))
           ((stringp node)
            (let ((text (if preserve-whitespace node
                          (replace-regexp-in-string "[ \t\r\n\u00a0]+" " " node))))
              (when (and (not preserve-whitespace) (string-prefix-p " " text)
                         (or (bobp) (memq (char-before) '(?\s ?\n))))
                (setq text (substring text 1)))
              (let ((remaining (- limit (buffer-size))))
                (when (> (length text) remaining) (setq limited t))
                (insert (substring text 0 (min (length text) remaining))))))
           ((consp node)
            (unless (or (gptel-agent-web-worker--hidden-p node)
                        (and skip-navigation
                             (memq (dom-tag node) '(nav footer))))
              (let ((block (memq (dom-tag node)
                                gptel-agent-web-worker--block-tags)))
                (when block
                  (when (and (> (buffer-size) 0) (not (eq (char-before) ?\n)))
                    (insert "\n"))
                  (push :break pending))
                (if (and (memq (dom-tag node) '(pre textarea))
                         (not preserve-whitespace))
                    (let ((pre (gptel-agent-web-worker--text
                                node (- limit (buffer-size)) skip-navigation t)))
                      (insert (car pre))
                      (when (cdr pre) (setq limited t)))
                  (dolist (child (reverse (dom-children node)))
                    (push child pending)))))))))
      (cons (if preserve-whitespace (buffer-string)
              (string-trim (buffer-string) "[\r\n]+" "[ \t\r\n]+"))
            (or limited (and pending t))))))

(defun gptel-agent-web-worker--find (root predicate &optional limit)
  "Find nodes under ROOT satisfying PREDICATE, returning at most LIMIT.
Use iterative traversal so deeply nested HTML cannot exhaust Lisp recursion."
  (let ((pending (list root)) found (count 0))
    (while (and pending (or (null limit) (< count limit)))
      (let ((node (pop pending)))
        (when (consp node)
          (when (funcall predicate node)
            (push node found)
            (cl-incf count))
          (unless (gptel-agent-web-worker--hidden-p node)
            (dolist (child (reverse (dom-children node)))
              (when (consp child) (push child pending)))))))
    (nreverse found)))

(defun gptel-agent-web-worker--parse (body)
  "Parse HTML BODY with libxml; never fall back to exposing raw scripts."
  (unless (and (fboundp 'libxml-parse-html-region) (libxml-available-p))
    (error "This Emacs lacks libxml HTML parsing support"))
  (with-temp-buffer
    (insert body)
    (or (libxml-parse-html-region (point-min) (point-max))
        (error "HTML parser returned no document"))))

(defun gptel-agent-web-worker--html-p (content-type body)
  "Return non-nil when CONTENT-TYPE or BODY identifies HTML."
  (let ((case-fold-search t))
    (or (string-match-p "\\(?:text/html\\|application/xhtml+xml\\)" content-type)
        (and (string-empty-p content-type)
             (string-match-p
              "\\`[[:space:]]*\\(?:<!doctype html\\|<html\\|<head\\|<body\\)"
              body)))))

(defun gptel-agent-web-worker--limit (text limit)
  "Bound TEXT to LIMIT with a notice when truncated."
  (if (<= (length text) limit) text
    (concat (substring text 0 limit)
            (format "\n\n[WebFetch truncated %d remaining characters. Fetch a more specific URL or request a larger max_chars value if needed.]"
                    (- (length text) limit)))))

(defun gptel-agent-web-worker-fetch (body content-type url max-chars)
  "Extract bounded static content from BODY with CONTENT-TYPE at URL.
MAX-CHARS bounds returned content.  Browser fallback advice is added for
empty documents or JavaScript application shells, not short text/API replies."
  (let* ((content-type (downcase (or content-type "")))
         (limit (max 1000 (or max-chars 50000)))
         (case-fold-search t))
    (cond
     ((string-match-p
       "\\`\\(?:image/\\|audio/\\|video/\\|font/\\|application/\\(?:pdf\\|zip\\|octet-stream\\)\\)"
       content-type)
      (format "Error: WebFetch received unsupported binary content-type %s. Use a specialized tool for this resource."
              (replace-regexp-in-string "[[:cntrl:]]" "?" content-type)))
     ((string-search "\0" body)
      "Error: WebFetch received NUL bytes; use a specialized tool for this binary resource.")
     ((gptel-agent-web-worker--html-p content-type body)
      (condition-case err
          (let* ((dom (gptel-agent-web-worker--parse body))
                 (main (car (gptel-agent-web-worker--find
                             dom (lambda (node) (eq (dom-tag node) 'main)) 1)))
                 (articles (unless main
                             (gptel-agent-web-worker--find
                              dom (lambda (node) (eq (dom-tag node) 'article)) 2)))
                 (root (or main (and (= (length articles) 1) (car articles)) dom))
                 (extracted (gptel-agent-web-worker--text root limit t))
                 (text (car extracted))
                 (flat-text (replace-regexp-in-string "[[:space:]]+" " " text))
                 (shell
                  (and (< (length text) 800)
                       (or
                        (string-match-p
                         "\\(?:enable\\|turn on\\|requires?\\|need\\).*javascript\\|javascript.*\\(?:disabled\\|required\\|enable\\)\\|\\`\\(?:loading[. ]*\\|please wait[. ]*\\|just a moment[. ]*\\)\\'\\|checking your browser\\|verify you are human\\|performing security verification"
                         flat-text)
                        (and (< (length text) 200)
                             (gptel-agent-web-worker--find
                              dom
                              (lambda (node)
                                (and (member (dom-attr node 'id)
                                             '("root" "app" "__next" "__nuxt"))
                                     (let ((root-text
                                            (car (gptel-agent-web-worker--text node 200 t))))
                                       (or (string-empty-p root-text)
                                           (string-match-p
                                            "\\`\\(?:loading[. \n]*\\|please wait[. \n]*\\)\\'"
                                            (downcase root-text))))))
                              1))))))
            (cond
             ((or (string-empty-p text) shell)
              (concat "WebFetch found "
                      (if shell "only an apparent JavaScript application shell" "no readable static text")
                      ".\n\n"
                      (gptel-agent-web-worker--browser-advice url)
                      (unless (string-empty-p text)
                        (concat "\n\nStatic text:\n" text))))
             ((cdr extracted)
              (concat text "\n\n[WebFetch stopped at max_chars; more static text may be available. Fetch a more specific URL or request a larger max_chars value if needed.]"))
             (t text)))
        (error
         (concat "Error: WebFetch could not extract HTML: "
                 (error-message-string err) ".\n\n"
                 (gptel-agent-web-worker--browser-advice url)))))
     ((string-empty-p (string-trim body))
      (concat "WebFetch received no readable text.\n\n"
              (gptel-agent-web-worker--browser-advice url)))
     (t (gptel-agent-web-worker--limit (string-trim body) limit)))))

(defun gptel-agent-web-worker--class-p (node class)
  "Return non-nil if NODE has the HTML CLASS."
  (member class (split-string (or (dom-attr node 'class) "") "[[:space:]]+" t)))

(defun gptel-agent-web-worker--link (href base)
  "Resolve search HREF against BASE, decoding known redirect links."
  (when (stringp href)
    (let ((case-fold-search t))
      (setq href
            (cond
             ((string-match "[?&]uddg=\\([^&]+\\)" href)
              (decode-coding-string (url-unhex-string (match-string 1 href)) 'utf-8))
             ((string-match "\\`/url[?]\\(?:q\\|url\\)=\\([^&]+\\)" href)
              (decode-coding-string (url-unhex-string (match-string 1 href)) 'utf-8))
             ((string-prefix-p "//" href) (concat "https:" href))
             ((and base (string-prefix-p "/" href))
              (url-expand-file-name href base))
             (t href)))
      (and (string-match-p "\\`https?://" href) href))))

(defun gptel-agent-web-worker-search (body query count url &optional max-chars)
  "Extract COUNT result links from search BODY for QUERY at URL.
Keep the existing WebSearch plist contract.  Prefer actual result title
links over engine navigation, and decode DuckDuckGo's redirect wrappers.
MAX-CHARS bounds the serialized result while preserving a readable plist."
  (let* ((dom (gptel-agent-web-worker--parse body))
         (anchors (gptel-agent-web-worker--find
                   dom (lambda (node) (eq (dom-tag node) 'a)) 4000))
         (titles (cl-remove-if-not
                  (lambda (node)
                    (or (gptel-agent-web-worker--class-p node "result__a")
                        (gptel-agent-web-worker--class-p node "result-link")))
                  anchors))
         (snippets (gptel-agent-web-worker--find
                    dom (lambda (node)
                          (or (gptel-agent-web-worker--class-p node "result__snippet")
                              (gptel-agent-web-worker--class-p node "result-snippet")))
                    100))
         (seen (make-hash-table :test #'equal))
         (engine-host (and url (url-host (url-generic-parse-url url))))
         (count (max 1 (or count 10)))
         (limit (max 1000 (or max-chars 50000)))
         ;; Reserve room for coverage diagnostics and serialization overhead.
         (query (truncate-string-to-width (or query "") (min 1000 (/ limit 8)) nil nil t))
         (remaining (- limit (length (prin1-to-string query)) 800))
         (result-count 0) results limited)
    (dolist (node (or titles anchors))
      (when (< result-count count)
        (let* ((href (gptel-agent-web-worker--link (dom-attr node 'href) url))
               (title (car (gptel-agent-web-worker--text node 1000)))
               (snippet (when (and titles snippets) (pop snippets))))
          (when (and href (not (gethash href seen))
                     (not (string-empty-p title))
                     (or titles
                         (not (equal engine-host (url-host (url-generic-parse-url href))))))
            (puthash href t seen)
            (let* ((entry (list :url href
                                :excerpt (if snippet
                                             (concat title "\n"
                                                     (car (gptel-agent-web-worker--text snippet 1500)))
                                           title)))
                   (size (1+ (length (prin1-to-string entry)))))
              (if (> size remaining)
                  (setq limited t)
                (cl-decf remaining size)
                (cl-incf result-count)
                (push entry results)))))))
    (prin1-to-string
     (list :query query :requested-count count :returned-count result-count
           :coverage-note
           (concat "These are only the top results exposed by the current search page; absence is not proof that the target does not exist."
                   (when limited
                     " Results were omitted to fit the output limit; use a more specific query or request fewer results.")
                   (when (and (zerop result-count) (not limited))
                     (concat " No result links were found. The page may require JavaScript or be an access challenge. "
                             "If available, use agent-browser or another real browser tool for this search. "
                             "Prefer a subagent with browser access that returns only relevant findings and source URLs.")))
           :results (nreverse results)))))

(defun gptel-agent-web-worker--content-type (headers-file)
  "Read the final HTTP block's content type from HEADERS-FILE."
  (plist-get (gptel-agent-web-worker--headers headers-file) :content-type))

(defun gptel-agent-web-worker--headers (headers-file)
  "Read status and content type from HEADERS-FILE's final HTTP block."
  (when headers-file
    (when (> (file-attribute-size (file-attributes headers-file)) 65536)
      (error "Response headers exceeded 65536 bytes; final HTTP status cannot be determined safely"))
    (with-temp-buffer
      (insert-file-contents headers-file nil 0 65536)
      (goto-char (point-min))
      (let ((case-fold-search t) content-type status)
        (while (not (eobp))
          (cond
           ((looking-at "HTTP/[0-9.]+[ \t]+\\([0-9]+\\)")
            (setq content-type nil status (string-to-number (match-string 1))))
           ((looking-at "Content-Type:[ \t]*\\([^\r\n]+\\)")
            (setq content-type (match-string-no-properties 1))))
          (forward-line 1))
        (list :content-type content-type :status status)))))

(declare-function gptel-agent-web-youtube-run "gptel-agent-web-youtube")

(defun gptel-agent-web-worker--execute (job)
  "Extract the content specified by JOB."
  (if (eq (plist-get job :kind) 'youtube)
      (progn
        (require 'gptel-agent-web-youtube)
        (gptel-agent-web-youtube-run job))
    (let* ((max-bytes (max 1024 (or (plist-get job :max-bytes) (* 2 1024 1024))))
           (body-file (plist-get job :body-file))
           (_size-check
            (when (> (file-attribute-size (file-attributes body-file)) max-bytes)
              (error "Response exceeded the byte limit (%d bytes); request a more specific URL or use a browser tool" max-bytes)))
           (headers (gptel-agent-web-worker--headers (plist-get job :headers-file)))
           (status (plist-get headers :status))
           (content-type (or (plist-get job :content-type)
                             (plist-get headers :content-type) ""))
           (body (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally body-file nil 0 max-bytes)
                   (let* ((raw (buffer-string))
                          (case-fold-search t)
                          (coding (and (string-match
                                        "charset=[\"']?\\([^;\"'[:space:]]+\\)"
                                        content-type)
                                       (coding-system-from-name (match-string 1 content-type)))))
                     (decode-coding-string raw (or coding 'utf-8) t))))
           (text
            (if (and status (>= status 400))
                (concat (format "Error: Web request failed (HTTP %d, content-type %s).\n"
                                status (replace-regexp-in-string
                                        "[[:cntrl:]]" "?" content-type))
                        "Response excerpt:\n"
                        (gptel-agent-web-worker--limit
                         (gptel-agent-web-worker-fetch body content-type
                                                       (plist-get job :url) 1000)
                         1000)
                        (when (memq status '(401 403 429 503))
                          (concat "\n\nThe page may require authentication or browser interaction. "
                                  (gptel-agent-web-worker--browser-advice (plist-get job :url)))))
              (pcase (plist-get job :kind)
                ('search (gptel-agent-web-worker-search
                          body (plist-get job :query) (plist-get job :count)
                          (plist-get job :url) (plist-get job :max-chars)))
                ('fetch (gptel-agent-web-worker-fetch
                         body content-type (plist-get job :url)
                         (plist-get job :max-chars)))
                (_ (error "Unknown web extraction job kind"))))))
      text)))

(defun gptel-agent-web-worker-run (job-file result-file)
  "Read JOB-FILE and write bounded UTF-8 text to RESULT-FILE.
The job is a Lisp plist with :kind (fetch or search), :body-file, optional
:content-type or :headers-file, :url, :max-bytes, :max-chars, :query, :count.
This entry point catches extraction errors so the parent always receives
a readable result; it never returns unparsed HTML as a fallback."
  (let ((coding-system-for-write 'utf-8-unix)
        (result
         (condition-case err
             (let ((job (with-temp-buffer
                          (insert-file-contents job-file nil 0 65536)
                          (let ((read-circle nil)) (read (current-buffer))))))
               (gptel-agent-web-worker--execute job))
           (error (concat "Error: Web extraction failed: " (error-message-string err))))))
    (with-temp-file result-file (insert result))))

(provide 'gptel-agent-web-worker)
;;; gptel-agent-web-worker.el ends here
