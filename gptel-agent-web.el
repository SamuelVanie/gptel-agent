;;; gptel-agent-web.el --- Bounded asynchronous Web tools -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Curl downloads bounded response bytes asynchronously.  A clean batch Emacs
;; extracts text in a separate process; HTML never gets rendered in the editor.

;;; Code:
(require 'cl-lib)
(require 'gptel)
(require 'gptel-agent-tool-identity)
(require 'eww)
(require 'url-expand)

(autoload 'gptel-agent-web-worker--headers "gptel-agent-web-worker")
(defvar gptel-agent-policy--active-check)

(defcustom gptel-agent-web-search-default-count 10
  "Default number of results returned by WebSearch."
  :type 'natnum :group 'gptel-agent)
(defcustom gptel-agent-web-search-max-count 30
  "Maximum number of results returned by one WebSearch call."
  :type 'natnum :group 'gptel-agent)
(defcustom gptel-agent-web-request-timeout 30
  "Deadline in seconds for a Web call, including queueing and extraction."
  :type 'natnum :group 'gptel-agent)
(defcustom gptel-agent-web-fetch-max-chars 50000
  "Default maximum characters returned by WebFetch.
The per-call max_chars argument can override this, up to 200000 characters."
  :type 'natnum :group 'gptel-agent)
(defcustom gptel-agent-web-max-concurrent-requests 4
  "Maximum Web calls downloading or extracting concurrently.
Additional calls wait in a FIFO queue.  Both Web tools share this limit."
  :type 'natnum :group 'gptel-agent)
(defcustom gptel-agent-web-max-response-bytes (* 2 1024 1024)
  "Maximum downloaded, decompressed response bytes for one Web request.
Oversized responses fail explicitly instead of parsing a partial document."
  :type 'natnum :group 'gptel-agent)

(defconst gptel-agent--web-worker-file
  (expand-file-name "gptel-agent-web-worker.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(cl-defstruct (gptel-agent--web-job (:constructor gptel-agent--make-web-job))
  callback source fsm cleanup kind url options directory buffer process
  stderr-process timer done max-bytes max-chars timeout check-url (redirects 0) (stderr ""))

(defvar gptel-agent--web-active nil "Web calls occupying a worker slot.")
(defvar gptel-agent--web-queue nil "Web calls waiting for a worker slot.")
(defvar gptel-agent--web-draining nil)
(defvar-local gptel-agent--web-jobs nil "Web calls owned by this buffer.")
(put 'gptel-agent--web-jobs 'permanent-local t)

(defun gptel-agent--web-live-p (job)
  "Whether JOB can still deliver a result to its owner."
  (and (not (gptel-agent--web-job-done job))
       (buffer-live-p (gptel-agent--web-job-source job))
       (or (null (gptel-agent--web-job-fsm job))
           (eq (gptel-fsm-state (gptel-agent--web-job-fsm job)) 'TOOL))))

(defun gptel-agent--web-stop-process (job)
  "Stop JOB's processes without running their sentinels."
  (dolist (process (list (gptel-agent--web-job-process job)
                         (gptel-agent--web-job-stderr-process job)))
    (when (processp process)
      (set-process-filter process #'ignore)
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process))))
  (setf (gptel-agent--web-job-process job) nil
        (gptel-agent--web-job-stderr-process job) nil))

(defun gptel-agent--web-dispose (job)
  "Silently cancel JOB and release its resources and queue slot."
  (unless (gptel-agent--web-job-done job)
    ;; Invalidate first: deleting a process can otherwise reenter its sentinel.
    (setf (gptel-agent--web-job-done job) t)
    (gptel-agent--web-stop-process job)
    (when (timerp (gptel-agent--web-job-timer job))
      (cancel-timer (gptel-agent--web-job-timer job)))
    (when-let* ((fsm (gptel-agent--web-job-fsm job)))
      (setf (plist-get (gptel-fsm-info fsm) :post)
            (remq (gptel-agent--web-job-cleanup job)
                  (plist-get (gptel-fsm-info fsm) :post))))
    (when (buffer-live-p (gptel-agent--web-job-source job))
      (with-current-buffer (gptel-agent--web-job-source job)
        (setq gptel-agent--web-jobs (delq job gptel-agent--web-jobs))))
    (when (buffer-live-p (gptel-agent--web-job-buffer job))
      (kill-buffer (gptel-agent--web-job-buffer job)))
    (when-let* ((directory (gptel-agent--web-job-directory job)))
      (ignore-errors (delete-directory directory t)))
    (setq gptel-agent--web-active (delq job gptel-agent--web-active)
          gptel-agent--web-queue (delq job gptel-agent--web-queue))
    (setf (gptel-agent--web-job-callback job) nil
          (gptel-agent--web-job-cleanup job) nil
          (gptel-agent--web-job-fsm job) nil)
    (gptel-agent--web-drain)))

(defun gptel-agent--webs-cleanup ()
  "Silently cancel active and queued Web calls owned by this buffer."
  ;; Do not start another of this buffer's queued calls during teardown.
  (let ((gptel-agent--web-draining t))
    (mapc #'gptel-agent--web-dispose (copy-sequence gptel-agent--web-jobs)))
  (gptel-agent--web-drain))
(put 'gptel-agent--webs-cleanup 'permanent-local-hook t)

(defun gptel-agent--web-finish (job result)
  "Release JOB and deliver RESULT once, in its source buffer."
  (unless (gptel-agent--web-job-done job)
    (let ((callback (and (gptel-agent--web-live-p job)
                         (gptel-agent--web-job-callback job)))
          (source (gptel-agent--web-job-source job)))
      ;; Deliver this result before a queued job's startup can invoke another
      ;; callback.  Always release the next slot even if this callback fails.
      (unwind-protect
          (let ((gptel-agent--web-draining t))
            (gptel-agent--web-dispose job)
            (when (and callback (buffer-live-p source))
              (with-current-buffer source (funcall callback result))))
        (gptel-agent--web-drain)))))

(defun gptel-agent--web-drain ()
  "Start queued calls immediately while concurrent worker slots are free."
  (unless gptel-agent--web-draining
    (let ((gptel-agent--web-draining t))
      (while (and gptel-agent--web-queue
                  (< (length gptel-agent--web-active)
                     (max 1 gptel-agent-web-max-concurrent-requests)))
        (let ((job (pop gptel-agent--web-queue)))
          (if (not (gptel-agent--web-live-p job))
              (gptel-agent--web-dispose job)
            (push job gptel-agent--web-active)
            (condition-case err
                (gptel-agent--web-start job)
              (error (gptel-agent--web-finish
                      job (concat "Error: Web request could not start: "
                                  (error-message-string err)))))))))))

(defun gptel-agent--web-filter (job process output)
  "Retain downloaded OUTPUT for JOB, bounded before insertion."
  (cond
   ((not (gptel-agent--web-live-p job)) (gptel-agent--web-dispose job))
   ((not (eq process (gptel-agent--web-job-process job))) nil)
   ((not (buffer-live-p (gptel-agent--web-job-buffer job)))
    (gptel-agent--web-finish job "Error: Web download buffer was killed."))
   ((> (+ (buffer-size (gptel-agent--web-job-buffer job))
          (string-bytes output))
       (gptel-agent--web-job-max-bytes job))
    (gptel-agent--web-finish
     job (format "Error: Web response exceeded the %d-byte download limit. Use a more specific URL or raise gptel-agent-web-max-response-bytes."
                 (gptel-agent--web-job-max-bytes job))))
   (t (with-current-buffer (gptel-agent--web-job-buffer job)
        (goto-char (point-max)) (insert output)))))

(defun gptel-agent--web-launch (job command filter sentinel)
  "Launch COMMAND for JOB with FILTER and SENTINEL, bounding diagnostics."
  (let* ((default-directory (gptel-agent--web-job-directory job))
         (stderr
          (make-pipe-process
           :name "gptel-web-stderr" :noquery t :coding 'utf-8-unix
           :filter (lambda (_process text)
                     (unless (gptel-agent--web-job-done job)
                       (let ((room (- 2048 (length (gptel-agent--web-job-stderr job)))))
                         (when (> room 0)
                           (setf (gptel-agent--web-job-stderr job)
                                 (concat (gptel-agent--web-job-stderr job)
                                         (substring text 0 (min room (length text))))))))))))
    (setf (gptel-agent--web-job-stderr-process job) stderr)
    (setf (gptel-agent--web-job-process job)
          (make-process :name "gptel-web" :command command :buffer nil
                        :connection-type 'pipe :coding 'no-conversion :noquery t
                        :stderr stderr :filter filter :sentinel sentinel))))

(defun gptel-agent--web-extract (job)
  "Start isolated extraction of JOB's downloaded response."
  (gptel-agent--web-stop-process job)
  (setf (gptel-agent--web-job-stderr job) "")
  (let* ((directory (gptel-agent--web-job-directory job))
         (body-file (expand-file-name "body" directory))
         (job-file (expand-file-name "job.el" directory))
         (result-file (expand-file-name "result" directory))
         (data (append (list :kind (gptel-agent--web-job-kind job)
                             :url (gptel-agent--web-job-url job)
                             :body-file body-file
                             :headers-file (expand-file-name "headers" directory)
                             :max-bytes (gptel-agent--web-job-max-bytes job)
                             :max-chars (gptel-agent--web-job-max-chars job)
                             :timeout (gptel-agent--web-job-timeout job))
                       (gptel-agent--web-job-options job))))
    (when (buffer-live-p (gptel-agent--web-job-buffer job))
      (with-current-buffer (gptel-agent--web-job-buffer job)
        (let ((coding-system-for-write 'no-conversion))
          (write-region (point-min) (point-max) body-file nil 'silent)))
      (kill-buffer (gptel-agent--web-job-buffer job))
      (setf (gptel-agent--web-job-buffer job) nil))
    (with-temp-file job-file
      (let ((print-length nil) (print-level nil)) (prin1 data (current-buffer))))
    (gptel-agent--web-launch
     job (list (expand-file-name invocation-name invocation-directory)
               "-Q" "--batch" "-L" (file-name-directory gptel-agent--web-worker-file)
               "-l" gptel-agent--web-worker-file
               "--eval" (prin1-to-string
                         `(gptel-agent-web-worker-run ,job-file ,result-file)))
     #'ignore
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (condition-case err
             (if (not (gptel-agent--web-live-p job))
                 (gptel-agent--web-dispose job)
               (if (and (zerop (process-exit-status process))
                        (file-exists-p result-file))
                   (let ((limit (* 4 (+ 4096 (gptel-agent--web-job-max-chars job)))))
                     (when (> (file-attribute-size (file-attributes result-file)) limit)
                       (error "Extraction result exceeded its output limit"))
                     (with-temp-buffer
                       (let ((coding-system-for-read 'utf-8-unix))
                         (insert-file-contents result-file))
                       (gptel-agent--web-finish job (buffer-string))))
                 (gptel-agent--web-finish
                  job (format "Error: Web extraction failed (%s): %s"
                              (process-exit-status process)
                              (gptel-agent--web-job-stderr job)))))
           (error (gptel-agent--web-finish
                   job (concat "Error: Web extraction failed: " (error-message-string err))))))))))

(defun gptel-agent--web-download-done (job)
  "Extract JOB's response or follow a checked HTTP redirect."
  (let* ((check (gptel-agent--web-job-check-url job))
         (headers (and check (gptel-agent-web-worker--headers
                              (expand-file-name "headers" (gptel-agent--web-job-directory job)))))
         (status (plist-get headers :status))
         (location (plist-get headers :location)))
    (if (and status (<= 300 status 399) location)
        (progn
          (when (>= (gptel-agent--web-job-redirects job) 5)
            (error "WebFetch exceeded its redirect limit"))
          (cl-incf (gptel-agent--web-job-redirects job))
          (setf (gptel-agent--web-job-url job)
                (url-expand-file-name location (gptel-agent--web-job-url job)))
          (gptel-agent--web-stop-process job)
          (with-current-buffer (gptel-agent--web-job-buffer job) (erase-buffer))
          (gptel-agent--web-download job))
      (gptel-agent--web-extract job))))

(defun gptel-agent--web-download (job)
  "Start one download for JOB, checking the next URL before launching curl."
  (let ((check (gptel-agent--web-job-check-url job))
        (curl (or (executable-find "curl")
                  (error "Web tools require curl; install it or use an available browser tool"))))
    (when (and check (not (funcall check (list :url (gptel-agent--web-job-url job)))))
      (error "WebFetch target denied by tool policy"))
    (gptel-agent--web-launch
     job
     (append (list curl "--disable" "--silent" "--show-error")
             (if check '("--globoff") '("--location" "--max-redirs" "5"))
             (list "--proto" "=http,https" "--proto-redir" "=http,https"
                   "--compressed" "--max-time" (number-to-string (gptel-agent--web-job-timeout job))
                   "--max-filesize" (number-to-string (gptel-agent--web-job-max-bytes job))
                   "--dump-header" (expand-file-name "headers" (gptel-agent--web-job-directory job))
                   "--header" "Accept: text/html,application/xhtml+xml,application/json,text/plain;q=0.9,*/*;q=0.5"
                   "--header" "Accept-Language: en-US,en;q=0.8"
                   "--url" (gptel-agent--web-job-url job)))
     (lambda (process output) (gptel-agent--web-filter job process output))
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (condition-case err
             (cond
              ((not (gptel-agent--web-live-p job)) (gptel-agent--web-dispose job))
              ((zerop (process-exit-status process)) (gptel-agent--web-download-done job))
              (t (gptel-agent--web-finish
                  job (format "Error: Web download failed (curl %d): %s%s"
                              (process-exit-status process)
                              (gptel-agent--web-job-stderr job)
                              (if (= (process-exit-status process) 63)
                                  " Response exceeded gptel-agent-web-max-response-bytes; use a more specific URL or raise that limit." "")))))
           (error (gptel-agent--web-finish
                   job (concat "Error: Web download failed: " (error-message-string err))))))))))

(defun gptel-agent--web-start (job)
  "Start JOB's download, or its isolated YouTube worker."
  (let ((default-directory temporary-file-directory))
    (setf (gptel-agent--web-job-directory job) (make-temp-file "gptel-web-" t))
    (if (eq (gptel-agent--web-job-kind job) 'youtube)
        (gptel-agent--web-extract job)
      (setf (gptel-agent--web-job-buffer job) (generate-new-buffer " *gptel-web-body*"))
      (with-current-buffer (gptel-agent--web-job-buffer job)
        (set-buffer-multibyte nil) (buffer-disable-undo))
      (gptel-agent--web-download job))))

(defun gptel-agent--web-enqueue (callback kind url options &optional fsm)
  "Queue a bounded Web call of KIND for URL with OPTIONS, owned by FSM.
Deliver via CALLBACK.  Return its job, or nil for invalid arguments."
  (let ((problem
         (cond ((not (and (stringp url) (string-match-p "\\`https?://[^[:space:]]+\\'" url)))
                "Web tools require an http:// or https:// URL.")
               ((and (plist-get options :max-chars)
                     (not (and (integerp (plist-get options :max-chars))
                               (> (plist-get options :max-chars) 0))))
                "max_chars must be a positive integer."))))
    (if problem
        (progn (funcall callback (concat "Error: " problem)) nil)
      (let* ((job (gptel-agent--make-web-job
                   :callback callback :source (current-buffer) :fsm fsm
                   :kind kind :url url :options options
                   :check-url (and (eq kind 'fetch) (bound-and-true-p gptel-agent-policy--active-check))
                   :max-bytes (max 1 gptel-agent-web-max-response-bytes)
                   :max-chars (min 200000 (max 1000 (or (plist-get options :max-chars)
                                                       gptel-agent-web-fetch-max-chars)))
                   :timeout (max 0.01 gptel-agent-web-request-timeout)))
             (cleanup (lambda (&rest _) (gptel-agent--web-dispose job))))
        (setf (gptel-agent--web-job-cleanup job) cleanup)
        (push job gptel-agent--web-jobs)
        (add-hook 'kill-buffer-hook #'gptel-agent--webs-cleanup nil t)
        (when fsm
          (push cleanup (plist-get (gptel-fsm-info fsm) :post)))
        (setf (gptel-agent--web-job-timer job)
              (run-at-time (gptel-agent--web-job-timeout job) nil
                           (lambda ()
                             (gptel-agent--web-finish
                              job (format "Error: Web request timed out after %s seconds (including queueing and extraction)."
                                          (gptel-agent--web-job-timeout job))))))
        (setq gptel-agent--web-queue (nconc gptel-agent--web-queue (list job)))
        (gptel-agent--web-drain)
        job))))

(defun gptel-agent--web-search-count (count)
  "Return a bounded effective WebSearch result COUNT."
  (min (max 1 gptel-agent-web-search-max-count)
       (max 1 (if (and (integerp count) (> count 0)) count
                gptel-agent-web-search-default-count))))

(defun gptel-agent--web-search-eww (callback query &optional count fsm)
  "Search QUERY using `eww-search-prefix', delivering COUNT results to CALLBACK.
FSM optionally owns cancellation.  Download and parsing are asynchronous."
  (gptel-agent--web-enqueue
   callback 'search (concat eww-search-prefix (url-hexify-string query))
   (list :query query :count (gptel-agent--web-search-count count)) fsm))

(defun gptel-agent--yt-video-id (url)
  "Return the video ID if URL is a YouTube video URL."
  (and (stringp url)
       (string-match
        (rx bos "http" (? "s") "://" (? "www.")
            (or "youtu.be/" "youtube.com/watch?v=")
            (group (+ (any alnum "_-"))) (or eos (any "?&#"))) url)
       (match-string 1 url)))

(defun gptel-agent--read-url (callback url &optional max-chars fsm)
  "Fetch URL asynchronously, limiting text to MAX-CHARS, owned by FSM."
  ;; The transcript worker contacts the player and caption endpoints itself.
  ;; Guarded calls fetch the requested page through the checked redirect path.
  (let ((video-id (unless (bound-and-true-p gptel-agent-policy--active-check)
                    (gptel-agent--yt-video-id url))))
    (gptel-agent--web-enqueue callback (if video-id 'youtube 'fetch) url
                              (list :max-chars max-chars :video-id video-id) fsm)))

(defun gptel-agent--localize-web-tools (fsm)
  "Capture FSM in request-local Web tools for cancellation."
  (setq-local gptel-tools
              (mapcar
               (lambda (base)
                 (if (not (member (gptel-agent--builtin-tool-name base) '("WebFetch" "WebSearch"))) base
                   (let ((tool (gptel--copy-tool base)))
                     (setf (gptel-tool-function tool)
                           (if (equal (gptel-tool-name base) "WebFetch")
                               (lambda (callback url &optional max-chars)
                                 (gptel-agent--read-url callback url max-chars fsm))
                             (lambda (callback query &optional count)
                               (gptel-agent--web-search-eww callback query count fsm))))
                     tool))) gptel-tools)))

(provide 'gptel-agent-web)
;;; gptel-agent-web.el ends here
