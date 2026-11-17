;;; gptel-agent-web-youtube.el --- Isolated YouTube extraction -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; This module runs only in the disposable Web worker, never in the UI.
;; Network responses, their lifetime, and the returned transcript are bounded.

;;; Code:
(require 'cl-lib)
(require 'dom)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defconst gptel-agent-web-youtube--browser-advice
  "If available, use agent-browser or another real browser tool to inspect the video page. Prefer delegating browser work to a subagent that has those tools, and ask it to return only relevant content and the source URL.")

(defun gptel-agent-web-youtube--fetch (url job &optional data)
  "Return a bounded decoded response from URL using limits in JOB.
Optional DATA is a UTF-8 JSON POST body.  Waiting happens only in the
isolated worker.  A filter caps bytes before insertion, including redirects;
unexpected compressed bodies are rejected before decompression."
  (unless (and (stringp url) (string-match-p "\\`https?://" url))
    (error "YouTube extraction requires an HTTP(S) URL"))
  (let* ((limit (or (plist-get job :max-bytes) (* 4 1024 1024)))
         (deadline (+ (float-time) (or (plist-get job :timeout) 30)))
         (url-request-method (if data "POST" "GET"))
         (url-request-data data)
         (url-request-noninteractive t)
         (url-show-status nil)
         (url-mime-encoding-string "identity")
         (url-http-attempt-keepalives nil)
         (url-max-redirections 5)
         (url-request-extra-headers
          (append '(("Accept-Language" . "en-US,en;q=0.8"))
                  (when data '(("Content-Type" . "application/json")))))
         (filter (symbol-function 'url-http-generic-filter))
         (received 0) buffers processes done result failure)
    (cl-labels
        ((remember (buffer)
           (when (buffer-live-p buffer)
             (cl-pushnew buffer buffers)
             (when-let* ((process (get-buffer-process buffer)))
               (cl-pushnew process processes))))
         (receive (status)
           (remember (current-buffer))
           (unless done
             (condition-case err
                 (progn
                   (when-let* ((problem (plist-get status :error)))
                     (error "YouTube request failed (HTTP %s): %S"
                            (or url-http-response-status "unknown") problem))
                   (let* ((start (if url-http-end-of-headers
                                     (if (markerp url-http-end-of-headers)
                                         (marker-position url-http-end-of-headers)
                                       url-http-end-of-headers)
                                   (point-min)))
                          (body (buffer-substring-no-properties start (point-max))))
                     (when (> (string-bytes body) limit)
                       (error "YouTube response exceeded the %d-byte download limit" limit))
                     (setq result (decode-coding-string body 'utf-8 t))))
               (error (setq failure err)))
             (setq done t))))
      (unwind-protect
          (cl-letf
              (((symbol-function 'url-http-generic-filter)
                (lambda (process chunk)
                  (remember (process-buffer process))
                  (unless done
                    (condition-case err
                        (progn
                          (cl-incf received (string-bytes chunk))
                          ;; Headers and redirect metadata get a small allowance.
                          (when (> received (+ limit 65536))
                            (error "YouTube response exceeded the %d-byte download limit" limit))
                          (funcall filter process chunk))
                      (error (setq failure err done t))))))
               ((symbol-function 'url-handle-content-transfer-encoding)
                (lambda ()
                  (let ((encoding (mail-fetch-field "content-encoding" nil nil nil t)))
                    (when (and encoding
                               (not (member (downcase (string-trim encoding))
                                            '("" "identity"))))
                      (error "YouTube returned unsupported compressed content: %s" encoding))))))
            (remember (url-retrieve url #'receive nil t t))
            (while (and (not done) (< (float-time) deadline))
              (accept-process-output nil 0.05))
            (unless done (error "YouTube request timed out"))
            (when failure (signal (car failure) (cdr failure)))
            result)
        (dolist (process processes)
          (when (processp process)
            (set-process-filter process #'ignore)
            (set-process-sentinel process #'ignore)
            (when (process-live-p process) (delete-process process))))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))))

(defun gptel-agent-web-youtube--node-text (node)
  "Collect NODE's nested text without repeatedly copying growing strings."
  (let ((pending (list node)) parts)
    (while pending
      (let ((item (pop pending)))
        (cond ((stringp item) (push item parts))
              ((consp item)
               (setq pending (append (dom-children item) pending))))))
    (replace-regexp-in-string "[[:space:]]+" " "
                              (apply #'concat (nreverse parts)))))

(defun gptel-agent-web-youtube--captions (xml max-chars)
  "Format caption XML into timestamped paragraphs, bounded by MAX-CHARS."
  (let* ((dom (with-temp-buffer
                (insert xml)
                (libxml-parse-xml-region (point-min) (point-max))))
         (legacy (eq (dom-tag dom) 'transcript))
         (entries (dom-by-tag dom (if legacy 'text 'p)))
         (paragraph-start nil) (used 0) truncated)
    (with-temp-buffer
      (catch 'full
        (dolist (node entries)
          (let* ((start (/ (string-to-number
                           (or (dom-attr node (if legacy 'start 't)) "0"))
                          (if legacy 1 1000)))
                 (text (string-trim (gptel-agent-web-youtube--node-text node)))
                 (new-paragraph (or (null paragraph-start)
                                    (and (> (- start paragraph-start) 3)
                                         (/= (floor start 30) (floor paragraph-start 30)))))
                 (prefix (if new-paragraph
                             (format "%s[%d:%02d]\n" (if paragraph-start "\n\n" "")
                                     (floor start 60) (mod (floor start) 60))
                           " "))
                 (piece (concat prefix text)))
            (unless (string-empty-p text)
              (when new-paragraph (setq paragraph-start start))
              (if (> (+ used (length piece)) max-chars)
                  (progn
                    (insert (substring piece 0 (max 0 (- max-chars used))))
                    (setq truncated t)
                    (throw 'full nil))
                (insert piece)
                (cl-incf used (length piece)))))))
      (when truncated (insert "\n[Transcript truncated at the requested character limit.]"))
      (string-trim (buffer-string)))))

(defun gptel-agent-web-youtube-run (job)
  "Return the YouTube description and transcript requested by JOB.
JOB provides :video-id, :max-bytes, :max-chars and :timeout.  The owning
worker also enforces an overall deadline across this multi-request flow."
  (let* ((video-id (plist-get job :video-id))
         (limit (min 200000 (max 1000 (or (plist-get job :max-chars) 50000))))
         (page (gptel-agent-web-youtube--fetch
                (format "https://www.youtube.com/watch?v=%s" (url-hexify-string video-id)) job))
         (api-key (and (string-match
                        "\"INNERTUBE_API_KEY\"[[:space:]]*:[[:space:]]*\"\\([a-zA-Z0-9_-]+\\)\""
                        page)
                       (match-string 1 page))))
    (unless api-key
      (error "Could not extract the YouTube player API key. %s"
             gptel-agent-web-youtube--browser-advice))
    (let* ((body (gptel-agent-web-youtube--fetch
                  (format "https://www.youtube.com/youtubei/v1/player?key=%s" api-key) job
                  (encode-coding-string
                   (json-encode
                    `((context . ((client . ((clientName . "ANDROID")
                                             (clientVersion . "20.10.38")))))
                      (videoId . ,video-id))) 'utf-8)))
           (metadata (json-parse-string body :object-type 'plist :array-type 'list
                                        :null-object nil :false-object nil))
           (description (or (plist-get (plist-get metadata :videoDetails) :shortDescription)
                            "No description available."))
           (tracks (plist-get
                    (plist-get (plist-get metadata :captions) :playerCaptionsTracklistRenderer)
                    :captionTracks))
           (english (cl-find-if (lambda (track)
                                  (string-prefix-p "en" (or (plist-get track :languageCode) "")))
                                tracks))
           (transcript
            (cond
             ((null tracks) "No transcript available.")
             ((null english) "No English transcript available.")
             (t
              (condition-case err
                  (let* ((url (plist-get english :baseUrl))
                         (xml (gptel-agent-web-youtube--fetch url job))
                         (captions (gptel-agent-web-youtube--captions xml limit)))
                    (if (string-empty-p captions) "No readable transcript available." captions))
                (error (format "Error fetching transcript: %s" (error-message-string err)))))))
           (result (format "# Description\n\n%s\n\n# Transcript\n\n%s" description transcript)))
      (if (<= (length result) limit) result
        (concat (substring result 0 limit)
                "\n[WebFetch truncated the YouTube result at the requested character limit.]")))))

(provide 'gptel-agent-web-youtube)
;;; gptel-agent-web-youtube.el ends here
