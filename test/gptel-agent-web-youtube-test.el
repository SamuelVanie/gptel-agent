;;; gptel-agent-web-youtube-test.el --- YouTube worker tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent-web-youtube)

(ert-deftest gptel-agent-web-youtube-captions-preserve-entities-and-timestamps ()
  (should
   (equal
    (gptel-agent-web-youtube--captions
     "<transcript><text start='0'>Alpha &amp; beta.</text><text start='1'>More.</text><text start='31.2'>Next &lt;part&gt;.</text></transcript>"
     1000)
    "[0:00]\nAlpha & beta. More.\n\n[0:31]\nNext <part>.")))

(ert-deftest gptel-agent-web-youtube-captions-handle-srv3-and-limit ()
  (should
   (equal (gptel-agent-web-youtube--captions
           "<timedtext><body><p t='125000'><s>Nested </s><s>words</s></p></body></timedtext>"
           1000)
          "[2:05]\nNested words"))
  (let ((result (gptel-agent-web-youtube--captions
                 (concat "<transcript><text start='0'>" (make-string 5000 ?a)
                         "</text></transcript>") 1000)))
    (should (string-prefix-p "[0:00]\n" result))
    (should (< (length result) 1100))
    (should (string-match-p "Transcript truncated" result))))

(ert-deftest gptel-agent-web-youtube-run-preserves-description-and-english-transcript ()
  (let (requests)
    (cl-letf (((symbol-function 'gptel-agent-web-youtube--fetch)
               (lambda (url _job &optional data)
                 (push (cons url data) requests)
                 (cond
                  ((string-match-p (regexp-quote "watch?v=") url) "{\"INNERTUBE_API_KEY\":\"test_key\"}")
                  ((string-match-p "youtubei/v1/player" url)
                   "{\"videoDetails\":{\"shortDescription\":\"A useful description\"},\"captions\":{\"playerCaptionsTracklistRenderer\":{\"captionTracks\":[{\"languageCode\":\"fr\",\"baseUrl\":\"https://example.test/fr\"},{\"languageCode\":\"en\",\"baseUrl\":\"https://example.test/en?fmt=srv3&other=1\"}]}}}")
                  (t "<transcript><text start='0'>Hello world</text></transcript>")))))
      (let ((result (gptel-agent-web-youtube-run '(:video-id "abc" :max-chars 1000))))
        (should (string-match-p "A useful description" result))
        (should (string-match-p "\\[0:00\\]\nHello world" result))
        (should (= (length requests) 3))
        (should (equal (caar requests) "https://example.test/en?fmt=srv3&other=1"))
        (let ((posted (json-parse-string (cdr (cadr requests)) :object-type 'plist)))
          (should (equal (plist-get posted :videoId) "abc")))))))

(ert-deftest gptel-agent-web-youtube-missing-api-key-offers-browser-delegation ()
  (cl-letf (((symbol-function 'gptel-agent-web-youtube--fetch)
             (lambda (&rest _) "<html><body>Enable JavaScript</body></html>")))
    (let ((error (should-error (gptel-agent-web-youtube-run '(:video-id "abc")))))
      (should (string-match-p "agent-browser" (error-message-string error)))
      (should (string-match-p "subagent" (error-message-string error))))))

(ert-deftest gptel-agent-web-youtube-fetch-cleans-response-buffer ()
  (let (buffer noninteractive encoding)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq noninteractive url-request-noninteractive
                       encoding url-mime-encoding-string
                       buffer (generate-new-buffer " *youtube-test-response*"))
                 (with-current-buffer buffer
                   (insert "Hello")
                   (setq-local url-http-end-of-headers (point-min)
                               url-http-response-status 200)
                   (funcall callback nil))
                 buffer)))
      (should (equal (gptel-agent-web-youtube--fetch
                      "https://example.test" '(:max-bytes 1000 :timeout 1)) "Hello")))
    (should noninteractive)
    (should (equal encoding "identity"))
    (should-not (buffer-live-p buffer))))

(ert-deftest gptel-agent-web-youtube-fetch-rejects-large-stream-before-insertion ()
  (let (buffer process inserted)
    (cl-letf (((symbol-function 'url-http-generic-filter)
               (lambda (_process _chunk) (setq inserted t)))
              ((symbol-function 'url-retrieve)
               (lambda (_url _callback &rest _)
                 (setq buffer (generate-new-buffer " *youtube-test-large*")
                       process (make-pipe-process :name "youtube-test" :buffer buffer :noquery t))
                 (url-http-generic-filter process (make-string 70000 ?a))
                 buffer)))
      (let ((error (should-error
                    (gptel-agent-web-youtube--fetch
                     "https://example.test" '(:max-bytes 1000 :timeout 1)))))
        (should (string-match-p "download limit" (error-message-string error)))))
    (should-not inserted)
    (should-not (buffer-live-p buffer))
    (should-not (process-live-p process))))

(ert-deftest gptel-agent-web-youtube-fetch-timeout-cleans-process ()
  (let (buffer process)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _)
                 (setq buffer (generate-new-buffer " *youtube-test-timeout*")
                       process (make-pipe-process :name "youtube-test" :buffer buffer :noquery t))
                 buffer)))
      (should-error (gptel-agent-web-youtube--fetch
                     "https://example.test" '(:max-bytes 1000 :timeout 0.01))))
    (should-not (buffer-live-p buffer))
    (should-not (process-live-p process))))

(provide 'gptel-agent-web-youtube-test)
;;; gptel-agent-web-youtube-test.el ends here
