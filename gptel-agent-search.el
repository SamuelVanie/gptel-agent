;;; gptel-agent-search.el --- Bounded asynchronous repository searches -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'gptel)
(require 'gptel-agent-path)

(defvar gptel-agent-tool-result-max-chars)

(defcustom gptel-agent-search-timeout 30
  "Deadline in seconds for a complete Glob or Grep call, including traversal."
  :type 'natnum :group 'gptel-agent)
(defcustom gptel-agent-search-max-entries 20000
  "Maximum directory entries examined by one Glob or Grep call.
Callers can lower this using max_entries.  Hitting a limit reports incomplete
coverage; dependency/build directories and hidden entries are not excluded."
  :type 'natnum :group 'gptel-agent)
(defcustom gptel-agent-search-max-results 1000
  "Maximum Glob entries or Grep matching lines collected across all files.
Callers can lower this using max_results.  Context can be cut at this limit."
  :type 'natnum :group 'gptel-agent)
(defcustom gptel-agent-search-traversal-max-bytes (* 4 1024 1024)
  "Maximum UTF-8 protocol bytes retained during repository traversal.
This also bounds candidate paths and metadata before sorting or searching."
  :type 'natnum :group 'gptel-agent)

(defconst gptel-agent--repo-worker
  (expand-file-name "gptel-agent-search-worker.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Standalone fallback worker shipped with this package.")

(defconst gptel-agent--repo-python-walk
  "import json, os, stat, sys
root, depth, limit = json.loads(sys.argv[1])
pending = [(root, 1)]
count = 0
status = 'complete'
def emit(value):
    print(json.dumps(value, ensure_ascii=True), flush=True)
try:
    while pending:
        directory, level = pending.pop()
        with os.scandir(directory) as children:
            for entry in children:
                if count >= limit:
                    status = 'incomplete: traversal entry limit'
                    pending.clear()
                    break
                count += 1
                attrs = entry.stat(follow_symlinks=False)
                link = os.readlink(entry.path) if entry.is_symlink() else None
                try:
                    target = entry.stat()
                    kind = ('directory' if stat.S_ISDIR(target.st_mode) else
                            'file' if stat.S_ISREG(target.st_mode) else 'special file')
                except FileNotFoundError:
                    if link is None:
                        raise
                    kind = 'broken symlink'
                if entry.name == '.git' and kind == 'directory':
                    continue
                emit({'entry': [entry.path, kind, link, attrs.st_mtime_ns]})
                if kind == 'directory' and link is None and (depth is None or level < depth):
                    pending.append((entry.path, level + 1))
except OSError as error:
    status = 'incomplete: traversal failed: ' + str(error)
emit({'status': status, 'examined': count})
"
  "Streaming traversal worker; no ignore files or implicit exclusions.")

(defun gptel-agent--repo-select-backend ()
  "Capture local traversal and matching executables for one request."
  (let* ((default-directory temporary-file-directory)
         (python (executable-find "python3"))
         (rg (executable-find "rg"))
         (grep (and (not rg) (executable-find "grep"))))
    (list :walker (if python 'python 'emacs) :walk-program python
          :name (cond (rg 'rg) (grep 'grep) (t 'emacs)) :program (or rg grep)
          :emacs (expand-file-name invocation-name invocation-directory))))

(defun gptel-agent--repo-syntax (name backend)
  "Describe NAME's pattern contract under BACKEND."
  (if (equal name "Glob")
      (format "Traversal backend: %s subprocess. Pattern syntax is always Emacs wildcard-to-regexp basename glob syntax: *, ?, and character classes [...]; case-insensitive using the standard Emacs case table. No / or **; this is a glob, not a regexp."
              (plist-get backend :walker))
    (pcase (plist-get backend :name)
      ('rg "Selected backend: rg (ripgrep). Native ripgrep/Rust regex syntax with Unicode enabled, e.g. ERROR|WARN or \\p{Greek}+. No PCRE2, lookaround, backreferences or multiline mode. Case-sensitive.")
      ('grep "Selected backend: grep -E. Native extended regular expressions (ERE) in the C locale, e.g. ERROR|WARN or [0-9]+. Case-sensitive byte matching; Unicode literals match exact UTF-8 bytes.")
      ('emacs "Selected backend: isolated Emacs subprocess fallback. Native Emacs regular expressions, e.g. ERROR\\|WARN. Case-sensitive, one complete UTF-8 line at a time. Lines exceeding 65536 bytes are skipped with incomplete coverage; no cross-line matching."))))

(defconst gptel-agent--repo-glob-description
  "Find typed entries on disk under root_directory. Include hidden, ignored, dependency and build entries; omit only .git directories. List symlink directories without traversing them. Results are sorted newest first (path order breaks mtime ties); incomplete results describe only the collected subset. depth=1 inspects immediate children; omitted depth allows all depths. ")
(defconst gptel-agent--repo-grep-description
  "Search text on disk at path, returning quoted absolute filenames and line numbers, with optional context. Directory searches include hidden, ignored, dependency and build files, omit .git directories, and never traverse symlink directories. Search regular files as text, including binary bytes. glob is an optional case-insensitive basename glob (*, ?, [...], no / or **), with the same syntax as Glob. depth=1 inspects immediate children; omit for all depths. ")

(defun gptel-agent--repo-tool (base fsm backend)
  "Clone BASE, advertising and capturing BACKEND and owning FSM."
  (let* ((tool (gptel--copy-tool base)) (name (gptel-tool-name base))
         (args (copy-tree (gptel-tool-args base)))
         (syntax (gptel-agent--repo-syntax name backend)))
    (setf (gptel-tool-description tool)
          (concat (if (equal name "Glob") gptel-agent--repo-glob-description
                    gptel-agent--repo-grep-description)
                  syntax
                  (format " Global limits: %d results, %d examined entries, %d traversal bytes, %d output characters, %s seconds. max_results/max_entries can lower these ceilings. Cancellation stops subprocesses. Limits or errors report incomplete coverage; narrow the path/pattern or raise the configured ceilings before retrying."
                          gptel-agent-search-max-results gptel-agent-search-max-entries
                          gptel-agent-search-traversal-max-bytes
                          gptel-agent-tool-result-max-chars gptel-agent-search-timeout))
          (plist-get (car args) :description) (concat syntax " Pattern: 1..4096 UTF-8 bytes, no literal newline or NUL.")
          (gptel-tool-args tool) args
          (gptel-tool-function tool)
          (if (equal name "Glob")
              (lambda (callback pattern &optional root depth type max-results max-entries)
                (gptel-agent--glob callback pattern root depth type max-results max-entries fsm backend))
            (lambda (callback regex path &optional glob context depth max-results max-entries)
              (gptel-agent--grep callback regex path glob context depth max-results max-entries fsm backend))))
    tool))

(defun gptel-agent--localize-repository-search (fsm)
  "Capture repository search contracts before serializing FSM's tools."
  (when (cl-find-if (lambda (tool) (member (gptel-tool-name tool) '("Glob" "Grep"))) gptel-tools)
    (let ((backend (or (plist-get (gptel-fsm-info fsm) :agent-repo-search-backend)
                       (gptel-agent--repo-select-backend))))
      (setf (gptel-fsm-info fsm) (plist-put (gptel-fsm-info fsm) :agent-repo-search-backend backend))
      (setq-local gptel-tools
                  (mapcar (lambda (tool)
                            (if (member (gptel-tool-name tool) '("Glob" "Grep"))
                                (gptel-agent--repo-tool tool fsm backend) tool)) gptel-tools)))))

(defun gptel-agent--refresh-repository-search (fsm)
  "Refresh changed repository backends only at FSM's next request boundary."
  (let* ((info (gptel-fsm-info fsm)) (old (plist-get info :agent-repo-search-backend)))
    (when old
      (let ((backend (gptel-agent--repo-select-backend)))
        (unless (equal old backend)
          (let ((tools (mapcar (lambda (tool)
                                 (if (member (gptel-tool-name tool) '("Glob" "Grep"))
                                     (gptel-agent--repo-tool tool fsm backend) tool))
                               (plist-get info :tools))))
            (setf (plist-get info :tools) tools
                  (plist-get (plist-get info :data) :tools)
                  (gptel--parse-tools (plist-get info :backend) tools)
                  (plist-get info :agent-repo-search-backend) backend)))))))
(advice-add 'gptel--handle-wait :before #'gptel-agent--refresh-repository-search)

(cl-defstruct (gptel-agent--repo (:constructor gptel-agent--make-repo))
  name source callback fsm cleanup backend root regex glob depth type context
  max-results max-entries limit traversal-limit process stderr-process timer timeout
  stage walk-status status done (pending "") (stderr "") (entries nil) (files nil)
  (output nil) (used 0) (count 0) (examined 0) (walk-bytes 0))

(defvar-local gptel-agent--repository-searches nil "Searches owned by this buffer.")
(put 'gptel-agent--repository-searches 'permanent-local t)

(defun gptel-agent--repo-stop-process (job)
  "Stop JOB's directly owned processes without invoking their sentinels."
  (dolist (process (list (gptel-agent--repo-process job) (gptel-agent--repo-stderr-process job)))
    (when (processp process)
      (set-process-filter process #'ignore)
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process)))))

(defun gptel-agent--repo-dispose (job)
  "Invalidate JOB, then release its processes, timers and ownership hooks."
  (unless (gptel-agent--repo-done job)
    (setf (gptel-agent--repo-done job) t)
    (gptel-agent--repo-stop-process job)
    (dolist (timer (list (gptel-agent--repo-timer job) (gptel-agent--repo-timeout job)))
      (when (timerp timer) (cancel-timer timer)))
    (when-let* ((fsm (gptel-agent--repo-fsm job)))
      (setf (plist-get (gptel-fsm-info fsm) :post)
            (remq (gptel-agent--repo-cleanup job) (plist-get (gptel-fsm-info fsm) :post))))
    (when (buffer-live-p (gptel-agent--repo-source job))
      (with-current-buffer (gptel-agent--repo-source job)
        (setq gptel-agent--repository-searches (delq job gptel-agent--repository-searches))))))

(defun gptel-agent--repository-searches-cleanup ()
  "Silently cancel this buffer's repository searches."
  (mapc #'gptel-agent--repo-dispose (copy-sequence gptel-agent--repository-searches)))
(put 'gptel-agent--repository-searches-cleanup 'permanent-local-hook t)

(defun gptel-agent--repo-live-p (job)
  "Whether JOB may still deliver to its owning request."
  (and (not (gptel-agent--repo-done job)) (buffer-live-p (gptel-agent--repo-source job))
       (or (null (gptel-agent--repo-fsm job))
           (eq (gptel-fsm-state (gptel-agent--repo-fsm job)) 'TOOL))))

(defun gptel-agent--repo-newest (a b)
  "Sort metadata records A and B by lstat mtime, breaking ties by path."
  (if (= (nth 3 a) (nth 3 b)) (string< (car a) (car b)) (> (nth 3 a) (nth 3 b))))

(defun gptel-agent--repo-finish (job status)
  "Deliver JOB once with explicit coverage STATUS and release ownership."
  (unless (gptel-agent--repo-done job)
    (when (and (equal status "complete") (gptel-agent--repo-status job))
      (setq status (gptel-agent--repo-status job)))
    (when (> (length status) 2048)
      (setq status (concat (substring status 0 2048) "…")))
    (when (equal (gptel-agent--repo-name job) "Glob")
      (setf (gptel-agent--repo-used job) 0)
      (dolist (entry (sort (gptel-agent--repo-entries job) #'gptel-agent--repo-newest))
        (pcase-let* ((`(,path ,kind ,link ,_) entry)
                     (text (format "[%s%s] %S%s\n" kind (if link " symlink" "") path
                                   (if link (format " -> %S" link) ""))))
          (when (<= (+ (gptel-agent--repo-used job) (length text)) (gptel-agent--repo-limit job))
            (push text (gptel-agent--repo-output job))
            (cl-incf (gptel-agent--repo-used job) (length text))))))
    (let ((result (and (gptel-agent--repo-live-p job)
                       (concat (apply #'concat (nreverse (gptel-agent--repo-output job)))
                               (when (and (equal status "complete") (zerop (gptel-agent--repo-count job)))
                                 "No matching entries.\n")
                               (format "[%s %s; backend=%s; traversal=%s; examined=%d; results=%d. Narrow the search if incomplete.]"
                                       (gptel-agent--repo-name job) status
                                       (if (equal (gptel-agent--repo-name job) "Glob") 'emacs-glob
                                         (plist-get (gptel-agent--repo-backend job) :name))
                                       (plist-get (gptel-agent--repo-backend job) :walker)
                                       (gptel-agent--repo-examined job) (gptel-agent--repo-count job))))))
      (gptel-agent--repo-dispose job)
      (when result
        (with-current-buffer (gptel-agent--repo-source job)
          (funcall (gptel-agent--repo-callback job) result))))))

(defun gptel-agent--repo-schedule (job function)
  "Run the next bounded stage FUNCTION for JOB after yielding to Emacs."
  (setf (gptel-agent--repo-timer job)
        (run-at-time 0.001 nil
                     (lambda ()
                       (if (not (gptel-agent--repo-live-p job)) (gptel-agent--repo-dispose job)
                         (condition-case err (funcall function job)
                           (error (gptel-agent--repo-finish job (concat "incomplete: " (error-message-string err))))))))))

(defun gptel-agent--repo-walk-end (job status)
  "Stop enumeration and process JOB's bounded candidates with STATUS."
  (gptel-agent--repo-stop-process job)
  (unless (equal status "complete") (setf (gptel-agent--repo-status job) status))
  (if (equal (gptel-agent--repo-name job) "Glob")
      (gptel-agent--repo-finish job status)
    (setf (gptel-agent--repo-files job)
          (mapcar #'car (sort (gptel-agent--repo-entries job) #'gptel-agent--repo-newest))
          (gptel-agent--repo-entries job) nil
          (gptel-agent--repo-stage job) 'grep)
    (gptel-agent--repo-schedule job #'gptel-agent--repo-grep-batch)))

(defun gptel-agent--repo-walk-record (job record)
  "Consume a bounded traversal JSON RECORD for JOB."
  (cl-incf (gptel-agent--repo-walk-bytes job) (string-bytes record))
  (if (> (gptel-agent--repo-walk-bytes job) (gptel-agent--repo-traversal-limit job))
      (gptel-agent--repo-walk-end job "incomplete: traversal byte limit")
    (let* ((object (json-parse-string record :object-type 'plist :array-type 'list :null-object nil))
           (entry (plist-get object :entry)))
      (if (not entry)
          (setf (gptel-agent--repo-walk-status job) (plist-get object :status)
                (gptel-agent--repo-examined job) (plist-get object :examined))
        (cl-incf (gptel-agent--repo-examined job))
        (when (> (gptel-agent--repo-examined job) (gptel-agent--repo-max-entries job))
          (error "traversal entry limit"))
        (let ((case-fold-search t))
          (with-case-table (standard-case-table)
            (when (and (or (null (gptel-agent--repo-glob job))
                           (string-match-p (gptel-agent--repo-glob job) (file-name-nondirectory (car entry))))
                       (or (equal (gptel-agent--repo-type job) "all")
                           (equal (cadr entry) (if (equal (gptel-agent--repo-type job) "directories") "directory" "file"))))
              (push entry (gptel-agent--repo-entries job))
              (when (equal (gptel-agent--repo-name job) "Glob")
                (cl-incf (gptel-agent--repo-count job))
                ;; Charge the exact rendered size before retaining more paths.
                (pcase-let ((`(,path ,kind ,link ,_) entry))
                  (cl-incf (gptel-agent--repo-used job)
                           (length (format "[%s%s] %S%s\n" kind (if link " symlink" "") path
                                           (if link (format " -> %S" link) "")))))
                (cond ((> (gptel-agent--repo-used job) (gptel-agent--repo-limit job))
                       (pop (gptel-agent--repo-entries job))
                       (cl-decf (gptel-agent--repo-count job))
                       (setf (gptel-agent--repo-used job) 0)
                       (gptel-agent--repo-walk-end job "incomplete: output limit"))
                      ((>= (gptel-agent--repo-count job) (gptel-agent--repo-max-results job))
                       (setf (gptel-agent--repo-used job) 0)
                       (gptel-agent--repo-walk-end job "incomplete: result limit")))))))))))

(defun gptel-agent--repo-grep-record (job record)
  "Render one filename-NUL-line RECORD from JOB's search process."
  (unless (equal record "--")
    (unless (string-match "\\`\\([^\0]+\\)\0\\([0-9]+\\)\\([:-]\\)\\(.*\\)\\'" record)
      (error "invalid or oversized search record"))
    (let ((text (format "%S:%s%s%s\n" (match-string 1 record) (match-string 2 record)
                        (match-string 3 record) (match-string 4 record)))
          (match (equal (match-string 3 record) ":")))
      (if (> (+ (gptel-agent--repo-used job) (length text)) (gptel-agent--repo-limit job))
          (gptel-agent--repo-finish job "incomplete: output limit")
        (push text (gptel-agent--repo-output job))
        (cl-incf (gptel-agent--repo-used job) (length text))
        (when match (cl-incf (gptel-agent--repo-count job)))
        (when (>= (gptel-agent--repo-count job) (gptel-agent--repo-max-results job))
          (gptel-agent--repo-finish job "incomplete: match limit"))))))

(defun gptel-agent--repo-filter (job process output)
  "Parse OUTPUT incrementally, bounding even an unterminated giant record.
Grep filenames end at NUL, so embedded newlines in filenames remain literal."
  (if (not (gptel-agent--repo-live-p job)) (gptel-agent--repo-dispose job)
    (condition-case err
        (let ((pos 0) (stage (gptel-agent--repo-stage job)))
          (while (and (< pos (length output)) (not (gptel-agent--repo-done job))
                      (eq stage (gptel-agent--repo-stage job))
                      (eq process (gptel-agent--repo-process job)))
            (let* ((room (- 65536 (string-bytes (gptel-agent--repo-pending job))))
                   ;; Characters can occupy four UTF-8 bytes.  Check the exact
                   ;; size too, without retaining an unbounded partial record.
                   (end (min (length output) (+ pos (max 1 (min 1024 (/ room 4))))))
                   (pending (concat (gptel-agent--repo-pending job) (substring output pos end)))
                   boundary)
              (when (> (string-bytes pending) 65536)
                (error "record exceeds 65536 bytes; narrow the search"))
              (setq pos end)
              (setf (gptel-agent--repo-pending job) pending)
              (while (and (not (gptel-agent--repo-done job))
                          (eq stage (gptel-agent--repo-stage job))
                          (setq boundary
                                (cond ((or (eq stage 'walk) (string-prefix-p "--\n" pending))
                                       (string-match "\n" pending))
                                      ((string-match "\0" pending)
                                       (string-match "\n" pending (1+ (match-beginning 0)))))))
                (let ((record (substring pending 0 boundary)))
                  (setq pending (substring pending (1+ boundary)))
                  (setf (gptel-agent--repo-pending job) pending)
                  (if (eq stage 'walk) (gptel-agent--repo-walk-record job record)
                    (gptel-agent--repo-grep-record job record)))))))
      (error (gptel-agent--repo-finish job (concat "incomplete: " (error-message-string err)))))))

(defun gptel-agent--repo-launch (job command)
  "Start COMMAND directly for JOB, with bounded stdout and stderr filters."
  (unless (and (car command) (file-executable-p (car command)))
    (error "advertised backend is unavailable; contract refreshes at the next request boundary"))
  (setf (gptel-agent--repo-pending job) "" (gptel-agent--repo-stderr job) "")
  (let ((default-directory temporary-file-directory)
        (process-environment (copy-sequence process-environment)))
    (setenv "LC_ALL" "C") (setenv "RIPGREP_CONFIG_PATH" nil) (setenv "GREP_OPTIONS" nil)
    (setf (gptel-agent--repo-stderr-process job)
          (make-pipe-process
           :name "gptel-repo-stderr" :buffer nil :noquery t :coding 'utf-8-unix
           :filter (lambda (_process output)
                     (unless (gptel-agent--repo-done job)
                       (let ((room (- 2048 (length (gptel-agent--repo-stderr job)))))
                         (when (> room 0)
                           (setf (gptel-agent--repo-stderr job)
                                 (concat (gptel-agent--repo-stderr job) (substring output 0 (min room (length output)))))))))))
    (setf (gptel-agent--repo-process job)
          (make-process
           :name "gptel-repo-search" :buffer nil :noquery t :connection-type 'pipe
           :coding 'utf-8-unix :command command :stderr (gptel-agent--repo-stderr-process job)
           :filter (apply-partially #'gptel-agent--repo-filter job)
           :sentinel
           (lambda (process _event)
             (when (and (memq (process-status process) '(exit signal))
                        (not (gptel-agent--repo-done job)))
               (if (not (gptel-agent--repo-live-p job)) (gptel-agent--repo-dispose job)
                 (condition-case err
                     (let* ((walk (eq (gptel-agent--repo-stage job) 'walk))
                            (ok (and (eq (process-status process) 'exit)
                                     (memq (process-exit-status process) (if walk '(0) '(0 1)))
                                     (string-empty-p (gptel-agent--repo-pending job)))))
                       (if (not ok)
                           (gptel-agent--repo-finish
                            job (format "incomplete: subprocess failed (exit %d): %s"
                                        (process-exit-status process) (gptel-agent--repo-stderr job)))
                         (gptel-agent--repo-stop-process job)
                         (cond (walk
                                (setf (gptel-agent--repo-used job) 0)
                                (gptel-agent--repo-walk-end job (or (gptel-agent--repo-walk-status job)
                                                                    "incomplete: missing traversal status")))
                               ((gptel-agent--repo-files job) (gptel-agent--repo-schedule job #'gptel-agent--repo-grep-batch))
                               (t (gptel-agent--repo-finish job "complete")))))
                   (error (gptel-agent--repo-finish job (concat "incomplete: " (error-message-string err))))))))))
    (unless (processp (gptel-agent--repo-process job)) (error "Could not start search backend"))
    (process-send-eof (gptel-agent--repo-process job))))

(defun gptel-agent--repo-emacs-command (job form)
  "Return a clean batch worker invocation for JOB evaluating FORM."
  (list (plist-get (gptel-agent--repo-backend job) :emacs) "--batch" "-Q"
        "-l" gptel-agent--repo-worker "--eval"
        (prin1-to-string `(let ((coding-system-for-write 'utf-8-unix)) ,form))))

(defun gptel-agent--repo-grep-batch (job)
  "Search the next bounded batch of explicit filenames for JOB."
  (let ((size 0) files (backend (gptel-agent--repo-backend job)))
    (while (and (gptel-agent--repo-files job) (< (length files) 64) (< size 32768))
      (let ((file (pop (gptel-agent--repo-files job))))
        (cl-incf size (string-bytes file)) (push file files)))
    (setq files (nreverse files))
    (gptel-agent--repo-launch
     job
     (if (eq (plist-get backend :name) 'emacs)
         (gptel-agent--repo-emacs-command
          job `(gptel-agent-search-worker-grep ,(gptel-agent--repo-regex job) ',files ,(gptel-agent--repo-context job)))
       (append (list (plist-get backend :program))
               (if (eq (plist-get backend :name) 'rg)
                   ;; Parallel rg buffers output per file; stream one file at
                   ;; a time so the parent can stop even a prolific match.
                   '("--no-config" "--no-heading" "--color=never" "--no-mmap"
                     "--threads=1" "--line-buffered" "--text" "--null" "-H" "-n")
                 '("-E" "-a" "--line-buffered" "--null" "-H" "-n"))
               (list (format "-C%d" (gptel-agent--repo-context job)) "-e" (gptel-agent--repo-regex job) "--")
               ;; With no candidates, validate the regexp against empty stdin.
               files)))))

(defun gptel-agent--repo-pattern (pattern &optional glob)
  "Validate PATTERN and return its glob regexp when GLOB is non-nil."
  (unless (and (stringp pattern) (<= 1 (string-bytes pattern) 4096)
               (not (string-match-p "[\n\0]" pattern)))
    (error "Pattern requires 1..4096 UTF-8 bytes, without literal newline or NUL"))
  (when glob
    (when (string-match-p "/\\|\\*\\*" pattern) (error "Glob requires a basename pattern, without / or **"))
    (wildcard-to-regexp pattern)))

(defun gptel-agent--repo-checked-limit (value ceiling name)
  "Validate VALUE against CEILING for argument NAME."
  (unless (and (integerp ceiling) (> ceiling 0)
               (or (null value) (and (integerp value) (<= 1 value ceiling))))
    (error "%s must be 1..%s (configured ceiling)" name ceiling))
  (or value ceiling))

(defun gptel-agent--repo-start (callback name pattern path glob context depth type max-results max-entries fsm backend)
  "Start an owned NAME search with validated arguments and captured BACKEND."
  (gptel-agent--repo-pattern pattern (equal name "Glob"))
  (when glob (gptel-agent--repo-pattern glob t))
  (unless (or (null depth) (and (integerp depth) (> depth 0))) (error "depth must be an integer >= 1"))
  (unless (and (integerp context) (<= 0 context 15)) (error "context_lines must be 0..15"))
  (unless (member type '("files" "directories" "all")) (error "type must be files, directories, or all"))
  ;; Remote handlers can block before an asynchronous process even starts.
  (when (file-remote-p (expand-file-name path)) (error "%s requires a local filesystem path" name))
  (setq path (gptel-agent-path-resolve path (if (equal name "Glob") 'directory 'any) t))
  (unless (or (file-directory-p path) (file-regular-p path)) (error "Search requires a regular file or directory"))
  (let* ((job (gptel-agent--make-repo
               :name name :source (current-buffer) :callback callback :fsm fsm
               :backend (or backend (gptel-agent--repo-select-backend)) :root path :regex pattern
               :glob (and glob (wildcard-to-regexp glob)) :context context :depth depth :type type
               :max-results (gptel-agent--repo-checked-limit max-results gptel-agent-search-max-results "max_results")
               :max-entries (gptel-agent--repo-checked-limit max-entries gptel-agent-search-max-entries "max_entries")
               :limit (max 0 gptel-agent-tool-result-max-chars)
               :traversal-limit (max 0 gptel-agent-search-traversal-max-bytes)))
         (cleanup (lambda (&rest _) (gptel-agent--repo-dispose job))))
    (push job gptel-agent--repository-searches)
    (add-hook 'kill-buffer-hook #'gptel-agent--repository-searches-cleanup nil t)
    (setf (gptel-agent--repo-cleanup job) cleanup)
    (when fsm (setf (plist-get (gptel-fsm-info fsm) :post) (cons cleanup (plist-get (gptel-fsm-info fsm) :post))))
    (setf (gptel-agent--repo-timeout job)
          (run-at-time gptel-agent-search-timeout nil
                       (lambda () (gptel-agent--repo-finish job "incomplete: timeout"))))
    (condition-case err
        (if (file-directory-p path)
            (progn
              (setf (gptel-agent--repo-stage job) 'walk)
              (gptel-agent--repo-launch
               job (if (eq (plist-get (gptel-agent--repo-backend job) :walker) 'python)
                       (list (plist-get (gptel-agent--repo-backend job) :walk-program)
                             "-I" "-u" "-c" gptel-agent--repo-python-walk
                             (json-encode (vector path depth (gptel-agent--repo-max-entries job))))
                     (gptel-agent--repo-emacs-command
                      job `(gptel-agent-search-worker-walk ,path ,depth ,(gptel-agent--repo-max-entries job))))))
          (setf (gptel-agent--repo-stage job) 'grep
                (gptel-agent--repo-examined job) 1
                (gptel-agent--repo-files job) (list path))
          (gptel-agent--repo-grep-batch job))
      (error (gptel-agent--repo-finish job (concat "incomplete: " (error-message-string err))))))
  nil)

(defun gptel-agent--glob (callback pattern &optional root depth type max-results max-entries fsm backend)
  "Asynchronously find typed basename PATTERN matches under ROOT.
DEPTH and TYPE retain Glob semantics.  MAX-RESULTS and MAX-ENTRIES lower the
configured limits.  FSM owns cancellation; BACKEND captures the advertised
contract."
  (gptel-agent--repo-start callback "Glob" pattern (or root ".") pattern 0 depth (or type "files")
                           max-results max-entries fsm backend))

(defun gptel-agent--grep (callback regex path &optional glob context depth max-results max-entries fsm backend)
  "Asynchronously search PATH with REGEX in the advertised BACKEND dialect.
GLOB filters basenames; CONTEXT is 0..15.  DEPTH, MAX-RESULTS and MAX-ENTRIES
bound traversal and global matches.  FSM owns cancellation."
  (gptel-agent--repo-start callback "Grep" regex path glob (or context 0) depth "files"
                           max-results max-entries fsm backend))

(provide 'gptel-agent-search)
;;; gptel-agent-search.el ends here
