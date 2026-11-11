;;; gptel-agent-read.el --- Bounded text and saved-result readers -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Byte-oriented UTF-8 readers.  No helper retains an unbounded input line.
;; Saved-result searching is owned by the calling buffer and request FSM.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-agent-path)

(defvar gptel-agent-read-file-size-threshold)
(defvar gptel-agent-tool-result-preview-chars)
(declare-function gptel-agent--scoped-result-file "gptel-agent-tools")

(defconst gptel-agent--read-chunk-size 65536
  "Maximum bytes in one file read, independent of line length.")

(defun gptel-agent--read-bytes (file begin end)
  "Read at most one bounded chunk of FILE between byte offsets BEGIN and END."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally
       file nil begin (min end (+ begin gptel-agent--read-chunk-size))))
    (buffer-string)))

(defun gptel-agent--utf8-end (bytes end)
  "Move END back before a partial UTF-8 character in unibyte BYTES."
  (let ((pos end))
    (while (and (> pos 0) (< pos (length bytes))
                (<= #x80 (aref bytes pos) #xbf))
      (cl-decf pos))
    pos))

(defun gptel-agent--read-range (file start end column limit)
  "Read FILE's START..END lines, from byte COLUMN, retaining at most LIMIT bytes.
Return :text, :line, :column (the next position), and :limited.
START and COLUMN are one-based.  END can be nil.  All disk reads are bounded;
skipped lines, including enormous ones, are never retained."
  (unless (and (or (null start) (and (integerp start) (> start 0)))
               (or (null end) (and (integerp end) (>= end (or start 1))))
               (or (null column) (and (integerp column) (> column 0))))
    (error "Read requires positive lines/column and end_line >= start_line"))
  (setq start (or start 1) column (or column 1) limit (max 4 limit))
  (let ((size (file-attribute-size (file-attributes file)))
        (offset 0) (line 1) (col 1) (used 0) pieces limited finished)
    (while (and (< offset size) (not finished))
      ;; Three lookahead bytes allow output to stop on a UTF-8 boundary.
      (let* ((bytes (gptel-agent--read-bytes file offset size))
             (len (length bytes)) (pos 0))
        (when (= len 0) (error "File changed during Read; retry"))
        (while (and (< pos len) (not finished))
          (let* ((newline (string-match "\n" bytes pos))
                 (stop (if newline (1+ newline) len))
                 (skip (if (= line start) (max 0 (- column col)) 0)))
            (when (and (= line start) newline (> skip (- newline pos)))
              (error "start_column is beyond the end of start_line"))
            (cond
             ((and end (> line end)) (setq finished t))
             ((or (< line start) (> skip 0))
              (setq stop (if (< line start) stop (min stop (+ pos skip))))))
            (unless finished
              (when (and (>= line start) (= skip 0))
                (when (and (= line start) (= col column) (zerop used)
                           (<= #x80 (aref bytes pos) #xbf))
                  (error "start_column splits a UTF-8 character"))
                (let* ((room (- limit used))
                       (take (min stop (+ pos room)))
                       (safe (if (< take stop)
                                 (gptel-agent--utf8-end bytes take) take)))
                  ;; A chunk can split a codepoint.  Re-read its short tail.
                  (when (and (= safe len) (< (+ offset len) size))
                    (let* ((tail (max pos (- len 3)))
                           (i (1- len)))
                      (while (and (> i tail) (<= #x80 (aref bytes i) #xbf))
                        (cl-decf i))
                      (when (>= (aref bytes i) #xc0) (setq safe i))))
                  (when (> safe pos)
                    (push (substring bytes pos safe) pieces)
                    (cl-incf used (- safe pos)))
                  (cond
                   ((< take stop) (setq limited t finished t))
                   ((< safe stop) (setq stop safe)))
                  (when finished (setq stop safe))))
              (cl-incf col (- stop pos))
              (when (and newline (= stop (1+ newline)))
                (cl-incf line) (setq col 1))
              (setq pos stop)
              (when (and end (> line end)) (setq finished t))
              ;; End this iteration at a split codepoint, without retaining it.
              (when (and (< pos len) (< (- len pos) 4) (not finished)
                         (>= (aref bytes pos) #xc0))
                (setq len pos)))))
        (cl-incf offset pos))
      (unless finished (sit-for 0)))
    (when (and (= line start) (< col column))
      (error "start_column is beyond the end of start_line"))
    (list :text (decode-coding-string (apply #'concat (nreverse pieces)) 'utf-8-unix)
          :line line :column col :limited limited)))

(defun gptel-agent--read-continuation (page tool)
  "Format PAGE's continuation notice for TOOL."
  (when (plist-get page :limited)
    (format "\n[%s incomplete: output limit; continue with start_line=%d, start_column=%d (1-based UTF-8 byte column).]"
            tool (plist-get page :line) (plist-get page :column))))

(defun gptel-agent--read-string-range (text start end column limit)
  "Read an already retained staged TEXT without copying its unbounded lines.
START, END, COLUMN and LIMIT have the same meaning as for disk ranges."
  (unless (and (or (null start) (and (integerp start) (> start 0)))
               (or (null end) (and (integerp end) (>= end (or start 1))))
               (or (null column) (and (integerp column) (> column 0))))
    (error "Read requires positive lines/column and end_line >= start_line"))
  (let ((pos 0) (line 1) (col 1) (size (length text)) newline)
    (while (and (< line (or start 1)) (< pos size))
      (setq newline (string-match "\n" text pos) pos (if newline (1+ newline) size))
      (when newline (cl-incf line)))
    (while (and (< pos size) (< col (or column 1)) (/= (aref text pos) ?\n))
      (cl-incf col (string-bytes (char-to-string (aref text pos)))) (cl-incf pos))
    (when (> col (or column 1)) (error "start_column splits a UTF-8 character"))
    (when (and (= line (or start 1)) (< col (or column 1)))
      (error "start_column is beyond the end of start_line"))
    (let* ((beg pos)
           (stop (min size (+ beg (max 4 limit))))
           (last (and end (1+ (- end line)))))
      (while (and last (> last 0) (setq newline (string-match "\n" text pos)) (< newline stop))
        (setq pos (1+ newline)) (cl-decf last)
        (when (= last 0) (setq stop pos)))
      (let* ((bytes (encode-coding-string (substring text beg stop) 'utf-8-unix))
             (take (gptel-agent--utf8-end bytes (min (length bytes) (max 4 limit))))
             (output (decode-coding-string (substring bytes 0 take) 'utf-8-unix))
             (limited (or (< take (length bytes))
                          (and (< stop size) (not (eql last 0)))))
             (i 0))
        (while (setq newline (string-match "\n" output i))
          (cl-incf line) (setq col 1 i (1+ newline)))
        (cl-incf col (string-bytes (substring output i)))
        (list :text output :limited limited :line line :column col)))))

(defun gptel-agent--read-file-lines (filename start-line end-line &optional start-column)
  "Read a bounded UTF-8 range of FILENAME, starting at START-COLUMN.
START-LINE and END-LINE are inclusive.  A missing range requests the whole
file, subject to `gptel-agent-read-file-size-threshold'."
  (setq filename (gptel-agent-path-resolve filename 'file t))
  (unless (file-readable-p filename) (error "File %s is not readable" filename))
  (when (file-directory-p filename) (error "Cannot read directory %s as file" filename))
  (when (file-symlink-p filename) (setq filename (file-truename filename)))
  (let ((limit (max 4 (* 1024 gptel-agent-read-file-size-threshold))))
    (when (and (null start-line) (null end-line)
               (> (file-attribute-size (file-attributes filename)) limit))
      (error "File is too large (> %d KB); specify a line range"
             gptel-agent-read-file-size-threshold))
    (let ((page (gptel-agent--read-range filename start-line end-line start-column limit)))
      (concat (plist-get page :text) (gptel-agent--read-continuation page "Read")))))

(defconst gptel-agent--search-record-limit 8192
  "Maximum retained external match-record bytes, including numeric prefixes.")

(defconst gptel-agent--search-line-limit 65536
  "Maximum retained bytes of a line in the Emacs regexp fallback.")

(defconst gptel-agent--result-search-description
  "Search only the assigned registered saved result. Returns bounded excerpts with exact line/UTF-8 byte columns and match byte offsets. Reports complete versus incomplete coverage and next_start_byte. Only nonempty, non-overlapping, single-line matches are reported. The 30-second deadline includes evidence extraction. An oversized external match record stops collection with incomplete coverage; narrow the regexp. ")

(defun gptel-agent--result-search-backend ()
  "Select a local executable and its native regexp contract, preferring rg."
  (let ((default-directory temporary-file-directory))
    (if-let* ((program (executable-find "rg")))
        (list :name 'rg :program program)
      (if-let* ((program (executable-find "grep")))
          (list :name 'grep :program program)
        (list :name 'emacs)))))

(defun gptel-agent--result-search-syntax (backend)
  "Describe the native regexp dialect and limits of BACKEND."
  (pcase (plist-get backend :name)
    ('rg "Selected backend: rg (ripgrep). Use native ripgrep/Rust regex syntax (Unicode enabled; no PCRE2 and no multiline mode), e.g. ERROR|WARN, error.*timeout, or \\p{Greek}+. case_sensitive=false uses ripgrep Unicode case folding.")
    ('grep "Selected backend: grep -E. Use native extended regular expressions (ERE) in the C locale, e.g. ERROR|WARN, error.*timeout, [0-9]+. Matching and case folding are byte/ASCII based; Unicode literals match their exact UTF-8 bytes.")
    ('emacs "Selected backend: bounded Emacs fallback. Use native Emacs regular expressions, e.g. ERROR\\|WARN or error.*timeout. Case folding uses Emacs's standard case table. Each complete UTF-8 line is matched separately; chunk boundaries within lines are joined. Lines exceeding 65536 bytes are skipped and coverage is reported incomplete; use ResultRead to inspect them. No cross-line matching.")))

(defun gptel-agent--result-search-tool (base fsm backend)
  "Clone BASE with BACKEND advertised in its schema and captured by its dispatcher."
  (let ((tool (gptel--copy-tool base))
        (args (copy-tree (gptel-tool-args base)))
        (syntax (gptel-agent--result-search-syntax backend)))
    (setf (gptel-tool-description tool) (concat gptel-agent--result-search-description syntax)
          (plist-get (car args) :description) (concat syntax " Pattern: 1..4096 UTF-8 bytes, no literal newline or NUL.")
          (gptel-tool-args tool) args
          (gptel-tool-function tool)
          (lambda (callback regex &optional sensitive max-results start-byte)
            (gptel-agent--grep-scoped-result callback regex sensitive max-results start-byte fsm backend)))
    tool))

(defun gptel-agent--localize-result-search (fsm)
  "Advertise and capture ResultGrep's selected backend before serializing FSM."
  (when-let* ((base (cl-find "ResultGrep" gptel-tools :key #'gptel-tool-name :test #'equal)))
    ;; Skill activation can localize tools during a tool batch.  Only WAIT may
    ;; change an already advertised contract; pending queries keep its dialect.
    (let* ((backend (or (plist-get (gptel-fsm-info fsm) :agent-result-search-backend)
                        (gptel-agent--result-search-backend)))
           (tool (gptel-agent--result-search-tool base fsm backend)))
      (setf (gptel-fsm-info fsm) (plist-put (gptel-fsm-info fsm) :agent-result-search-backend backend))
      (setq-local gptel-tools (mapcar (lambda (item) (if (eq item base) tool item)) gptel-tools)))))

(defun gptel-agent--refresh-result-search (fsm)
  "Refresh changed backend contracts immediately before FSM's next model request.
The dispatcher used by an existing tool batch is never reinterpreted."
  (let* ((info (gptel-fsm-info fsm))
         (old (plist-get info :agent-result-search-backend))
         (base (and old (cl-find "ResultGrep" (plist-get info :tools)
                                 :key #'gptel-tool-name :test #'equal))))
    (when base
      (let ((backend (gptel-agent--result-search-backend)))
        (unless (equal backend old)
          (let* ((tool (gptel-agent--result-search-tool base fsm backend))
                 (tools (mapcar (lambda (item) (if (eq item base) tool item)) (plist-get info :tools)))
                 (schemas (gptel--parse-tools (plist-get info :backend) tools)))
            (setf (plist-get info :tools) tools
                  (plist-get (plist-get info :data) :tools) schemas
                  (plist-get info :agent-result-search-backend) backend)))))))

(advice-add 'gptel--handle-wait :before #'gptel-agent--refresh-result-search)

(cl-defstruct (gptel-agent--search (:constructor gptel-agent--make-search))
  file size stamp source callback fsm cleanup process stderr-process timer timeout
  backend regex sensitive max-results limit (start-byte 0)
  (pending "") (stderr "") (candidates nil) (entries nil) (used 0) (count 0)
  (offset 0) (carry "") (line 1) (line-start 0) (next-byte 0)
  skipping (skipped-lines 0) first-skipped-byte first-skipped-line
  evidence-offset cached-line cached-start status done)

(defvar-local gptel-agent--searches nil
  "Saved-result searches owned by this conversation.")
(put 'gptel-agent--searches 'permanent-local t)

(defun gptel-agent--search-dispose (job)
  "Invalidate JOB before releasing processes, timers and ownership hooks."
  (unless (gptel-agent--search-done job)
    (setf (gptel-agent--search-done job) t)
    (dolist (timer (list (gptel-agent--search-timer job) (gptel-agent--search-timeout job)))
      (when (timerp timer) (cancel-timer timer)))
    (dolist (process (list (gptel-agent--search-process job)
                           (gptel-agent--search-stderr-process job)))
      (when (processp process)
        (set-process-filter process #'ignore)
        (set-process-sentinel process #'ignore)
        (when (process-live-p process) (delete-process process))))
    (when-let* ((fsm (gptel-agent--search-fsm job)))
      (setf (plist-get (gptel-fsm-info fsm) :post)
            (remq (gptel-agent--search-cleanup job) (plist-get (gptel-fsm-info fsm) :post))))
    (when (buffer-live-p (gptel-agent--search-source job))
      (with-current-buffer (gptel-agent--search-source job)
        (setq gptel-agent--searches (delq job gptel-agent--searches))))))

(defun gptel-agent--searches-cleanup ()
  "Silently cancel saved-result searches owned by the current buffer."
  (mapc #'gptel-agent--search-dispose (copy-sequence gptel-agent--searches)))
(put 'gptel-agent--searches-cleanup 'permanent-local-hook t)

(defun gptel-agent--search-live-p (job)
  "Whether JOB still belongs to a live tool request."
  (and (not (gptel-agent--search-done job))
       (buffer-live-p (gptel-agent--search-source job))
       (or (null (gptel-agent--search-fsm job))
           (eq (gptel-fsm-state (gptel-agent--search-fsm job)) 'TOOL))))

(defun gptel-agent--search-finish (job status)
  "Deliver JOB's bounded evidence once, with explicit coverage STATUS."
  (unless (gptel-agent--search-done job)
    (let* ((live (gptel-agent--search-live-p job))
           (file (gptel-agent--search-file job))
           (attrs (and live (file-attributes file)))
           (changed (and live (not (equal (list (file-attribute-size attrs)
                                                (file-attribute-modification-time attrs))
                                          (gptel-agent--search-stamp job)))))
           (status (cond
                    (changed "incomplete: file changed; retry from start_byte=0")
                    ((> (gptel-agent--search-skipped-lines job) 0)
                     (format "incomplete: skipped %d lines over %d bytes; first skipped line=%d byte=%d; %s"
                             (gptel-agent--search-skipped-lines job) gptel-agent--search-line-limit
                             (gptel-agent--search-first-skipped-line job)
                             (gptel-agent--search-first-skipped-byte job)
                             (if (equal status "complete") "remaining lines scanned" status)))
                    (t status)))
           (result
            (when live
              (concat
               (mapconcat #'identity (nreverse (gptel-agent--search-entries job)) "\n")
               (when (and (equal status "complete") (zerop (gptel-agent--search-count job)))
                 (format "No nonempty matches in %s from start_byte=%d."
                         file (gptel-agent--search-start-byte job)))
               (format "\n[ResultGrep %s; backend=%s; next_start_byte=%d. Columns are 1-based UTF-8 bytes.]"
                       status (gptel-agent--search-backend job)
                       (if-let* ((skipped (gptel-agent--search-first-skipped-byte job)))
                           (min skipped (gptel-agent--search-next-byte job))
                         (gptel-agent--search-next-byte job)))))))
      (gptel-agent--search-dispose job)
      (when result
        (with-current-buffer (gptel-agent--search-source job)
          (funcall (gptel-agent--search-callback job) result))))))

(defun gptel-agent--search-schedule (job function)
  "Schedule one bounded slice of FUNCTION for JOB, allowing redisplay/input."
  (when (timerp (gptel-agent--search-timer job))
    (cancel-timer (gptel-agent--search-timer job)))
  (setf (gptel-agent--search-timer job)
        (run-at-time
         0.001 nil
         (lambda ()
           (if (not (gptel-agent--search-live-p job))
               (gptel-agent--search-dispose job)
             (condition-case err (funcall function job)
               (error (gptel-agent--search-finish
                       job (concat "incomplete: " (error-message-string err))))))))))

(defun gptel-agent--search-collected (job status)
  "Stop collection and render JOB's candidates with coverage STATUS."
  (unless (or (gptel-agent--search-done job) (gptel-agent--search-status job))
    (setf (gptel-agent--search-status job) status
          (gptel-agent--search-candidates job) (nreverse (gptel-agent--search-candidates job)))
    (when-let* ((process (gptel-agent--search-process job)))
      (set-process-filter process #'ignore)
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process)))
    (gptel-agent--search-schedule job #'gptel-agent--search-evidence)))

(defun gptel-agent--search-candidate (job line offset length &optional column)
  "Retain a bounded candidate for JOB at LINE, OFFSET, of LENGTH bytes.
COLUMN, if already known, is a one-based byte column."
  (when (and (> length 0) (>= offset (gptel-agent--search-start-byte job)))
    (push (list line offset length column) (gptel-agent--search-candidates job))
    (cl-incf (gptel-agent--search-count job))
    (when (>= (gptel-agent--search-count job) (gptel-agent--search-max-results job))
      (gptel-agent--search-collected job "incomplete: match limit"))))

(defun gptel-agent--search-filter (job _process output)
  "Parse bounded line:byte-offset:match records from OUTPUT into JOB.
Never accumulate a whole subprocess line: malformed records stop at 8192 bytes."
  (when (and (not (gptel-agent--search-done job)) (not (gptel-agent--search-status job)))
    (if (not (gptel-agent--search-live-p job)) (gptel-agent--search-dispose job)
      (condition-case err
          (let ((pos 0))
            (while (and (< pos (length output)) (not (gptel-agent--search-status job)))
              (let* ((newline (string-match "\n" output pos))
                     (end (or newline (length output)))
                     (room (- gptel-agent--search-record-limit (length (gptel-agent--search-pending job)))))
                (when (> (- end pos) room) (error "match record exceeds 8192 bytes; narrow the regexp"))
                (setf (gptel-agent--search-pending job)
                      (concat (gptel-agent--search-pending job) (substring output pos end)))
                (setq pos (if newline (1+ newline) end))
                (when newline
                  (let ((record (gptel-agent--search-pending job)))
                    (setf (gptel-agent--search-pending job) "")
                    (unless (string-match "\\`\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)\\'" record)
                      (error "invalid search record"))
                    (gptel-agent--search-candidate
                     job (string-to-number (match-string 1 record))
                     (string-to-number (match-string 2 record))
                     (length (match-string 3 record))))))))
        (error (gptel-agent--search-collected
                job (concat "incomplete: " (error-message-string err))))))))

(defun gptel-agent--search-fallback-line (job bytes)
  "Evaluate JOB's native Emacs regexp on one bounded complete line BYTES."
  (let* ((text (decode-coding-string bytes 'utf-8-unix))
         (case-fold-search (not (gptel-agent--search-sensitive job)))
         (pos 0) (byte-pos 0) (previous 0))
    (with-temp-buffer
      (set-case-table (standard-case-table))
      (while (and (<= pos (length text)) (not (gptel-agent--search-status job))
                  (string-match (gptel-agent--search-regex job) text pos))
        (let ((begin (match-beginning 0)) (end (match-end 0)))
          (cl-incf byte-pos (string-bytes (substring text previous begin)))
          (gptel-agent--search-candidate
           job (gptel-agent--search-line job) (+ (gptel-agent--search-line-start job) byte-pos)
           (string-bytes (substring text begin end)) (1+ byte-pos))
          (setq previous begin pos (if (= begin end) (1+ end) end)))))))

(defun gptel-agent--search-fallback (job)
  "Scan one chunk for JOB, joining short lines and discarding oversized lines."
  (let* ((offset (gptel-agent--search-offset job))
         (size (gptel-agent--search-size job))
         (bytes (gptel-agent--read-bytes (gptel-agent--search-file job) offset size))
         (len (length bytes)) (pos 0) (eof (= (+ offset len) size)))
    (when (and (zerop len) (< offset size)) (error "file changed during scan"))
    (while (and (< pos len) (not (gptel-agent--search-status job)))
      (let* ((newline (string-match "\n" bytes pos))
             (end (or newline len)))
        (unless (gptel-agent--search-skipping job)
          (if (> (+ (length (gptel-agent--search-carry job)) (- end pos)) gptel-agent--search-line-limit)
              (setf (gptel-agent--search-skipping job) t (gptel-agent--search-carry job) "")
            (setf (gptel-agent--search-carry job)
                  (concat (gptel-agent--search-carry job) (substring bytes pos end)))))
        (when (or newline eof)
          (if (gptel-agent--search-skipping job)
              (when (>= (+ offset end) (gptel-agent--search-start-byte job))
                (cl-incf (gptel-agent--search-skipped-lines job))
                (unless (gptel-agent--search-first-skipped-byte job)
                  (setf (gptel-agent--search-first-skipped-byte job)
                        (max (gptel-agent--search-start-byte job) (gptel-agent--search-line-start job))
                        (gptel-agent--search-first-skipped-line job) (gptel-agent--search-line job))))
            (gptel-agent--search-fallback-line job (gptel-agent--search-carry job)))
          (setf (gptel-agent--search-carry job) "" (gptel-agent--search-skipping job) nil)
          (cl-incf (gptel-agent--search-line job))
          (setf (gptel-agent--search-line-start job) (+ offset end (if newline 1 0))))
        (setq pos (if newline (1+ newline) end))))
    (setf (gptel-agent--search-offset job) (+ offset len))
    (unless (gptel-agent--search-status job)
      (if eof (gptel-agent--search-collected job "complete")
        (gptel-agent--search-schedule job #'gptel-agent--search-fallback)))))

(defun gptel-agent--search-evidence (job)
  "Render one candidate, or locate its line start in one bounded backwards read."
  (if (null (gptel-agent--search-candidates job))
      (progn
        (when (equal (gptel-agent--search-status job) "complete")
          (setf (gptel-agent--search-next-byte job) (gptel-agent--search-size job)))
        (gptel-agent--search-finish job (gptel-agent--search-status job)))
    (pcase-let* ((`(,line ,offset ,length ,column) (car (gptel-agent--search-candidates job)))
                 (file (gptel-agent--search-file job))
                 (line-start
                  (cond (column (- (1+ offset) column))
                        ((eql line (gptel-agent--search-cached-line job))
                         (gptel-agent--search-cached-start job)))))
      (unless line-start
        (let* ((end (or (gptel-agent--search-evidence-offset job) offset))
               (begin (max 0 (- end gptel-agent--read-chunk-size)))
               (bytes (gptel-agent--read-bytes file begin end))
               (newline (cl-position ?\n bytes :from-end t)))
          (cond (newline (setq line-start (+ begin newline 1)))
                ((zerop begin) (setq line-start 0))
                (t (setf (gptel-agent--search-evidence-offset job) begin)))))
      (when line-start
        (let* ((begin (max line-start (- offset 160)))
               (end (min (gptel-agent--search-size job) (+ offset 400)))
               (bytes (gptel-agent--read-bytes file begin end))
               (from 0)
               (to (length bytes)))
          ;; Excerpts may start/end in UTF-8 characters; drop partial characters.
          (while (and (< from to) (<= #x80 (aref bytes from) #xbf)) (cl-incf from))
          (when (< end (gptel-agent--search-size job))
            (let ((look (gptel-agent--read-bytes file begin (min (+ end 3) (gptel-agent--search-size job)))))
              (setq to (gptel-agent--utf8-end look to))))
          (let* ((snippet (decode-coding-string (substring bytes from to) 'utf-8-unix))
                 (snippet (string-replace "\n" "\\n" (string-replace "\r" "\\r" (string-replace "\t" "\\t" snippet))))
                 (entry (format "%s:%d:%d: %s [match byte offset %d; excerpt byte column %d]"
                                file line (1+ (- offset line-start)) snippet offset
                                (1+ (- (+ begin from) line-start)))))
            (if (> (+ (gptel-agent--search-used job) (length entry) 1) (gptel-agent--search-limit job))
                (gptel-agent--search-finish job "incomplete: output limit")
              (push entry (gptel-agent--search-entries job))
              (cl-incf (gptel-agent--search-used job) (1+ (length entry)))
              (setf (gptel-agent--search-next-byte job) (+ offset length)
                    (gptel-agent--search-candidates job) (cdr (gptel-agent--search-candidates job))
                    (gptel-agent--search-evidence-offset job) nil
                    (gptel-agent--search-cached-line job) line
                    (gptel-agent--search-cached-start job) line-start)))))
      (unless (gptel-agent--search-done job)
        (gptel-agent--search-schedule job #'gptel-agent--search-evidence)))))

(defun gptel-agent--grep-scoped-result
    (callback regex &optional case-sensitive max-results start-byte fsm backend)
  "Asynchronously search the registered file with BACKEND's native REGEX.
Call CALLBACK once for a live request, or silently dispose on cancellation.
CASE-SENSITIVE defaults to nil; folding follows the advertised backend.
MAX-RESULTS is 1..100.  START-BYTE is a zero-based continuation offset.
Optional FSM owns the call.  BACKEND captures the advertised executable;
it is never replaced with another dialect while executing a query."
  (let* ((file (gptel-agent--scoped-result-file))
         (backend (or backend (gptel-agent--result-search-backend)))
         (name (plist-get backend :name))
         (program (plist-get backend :program))
         (attrs (file-attributes file))
         (size (file-attribute-size attrs)))
    (unless (and (stringp regex) (<= 1 (string-bytes regex) 4096)
                 (not (string-match-p "[\n\0]" regex)))
      (error "ResultGrep requires 1..4096 UTF-8 pattern bytes, without literal newline or NUL"))
    (unless (or (null max-results) (and (integerp max-results) (<= 1 max-results 100)))
      (error "max_results must be 1..100"))
    (unless (or (null start-byte) (and (integerp start-byte) (<= 0 start-byte size)))
      (error "start_byte must be between 0 and the file size"))
    (when (file-remote-p file) (error "ResultGrep requires a local saved result"))
    (let* ((job (gptel-agent--make-search
                 :file file :size size :stamp (list size (file-attribute-modification-time attrs))
                 :source (current-buffer) :callback callback :fsm fsm
                 :sensitive (eq case-sensitive t) :regex regex :backend name
                 :max-results (or max-results 50) :limit (max 0 gptel-agent-tool-result-preview-chars)
                 :start-byte (or start-byte 0) :next-byte (or start-byte 0)))
           (cleanup (lambda (&rest _) (gptel-agent--search-dispose job))))
      (push job gptel-agent--searches)
      (add-hook 'kill-buffer-hook #'gptel-agent--searches-cleanup nil t)
      (setf (gptel-agent--search-cleanup job) cleanup)
      (when fsm
        (setf (plist-get (gptel-fsm-info fsm) :post)
              (cons cleanup (plist-get (gptel-fsm-info fsm) :post))))
      (setf (gptel-agent--search-timeout job)
            (run-at-time 30 nil (lambda () (gptel-agent--search-finish job "incomplete: timeout"))))
      (condition-case err
          (if (eq name 'emacs)
              (progn
                ;; Validate even for an empty file, before claiming no matches.
                (string-match regex "")
                (gptel-agent--search-schedule job #'gptel-agent--search-fallback))
            (unless (and program (file-executable-p program))
              (error "advertised %s backend is unavailable; contract refreshes at the next request boundary" name))
            (let* ((default-directory temporary-file-directory)
                   (process-environment (copy-sequence process-environment))
                   (args (append
                          (if (eq name 'rg) '("--no-config" "--no-heading" "--no-filename" "--color=never"
                                              "--no-mmap" "--encoding=none" "-a" "-n" "-b" "-o")
                            '("-a" "-n" "-b" "-o" "-E" "-h"))
                          (unless (eq case-sensitive t) '("-i"))
                          (list "-e" regex "--" file))))
              (setenv "LC_ALL" "C")
              (setenv "RIPGREP_CONFIG_PATH" nil)
              (setenv "GREP_OPTIONS" nil)
              (setf (gptel-agent--search-stderr-process job)
                    (make-pipe-process
                     :name "gptel-result-search-stderr" :buffer nil :noquery t :coding 'no-conversion
                     :filter (lambda (_process output)
                               (unless (gptel-agent--search-done job)
                                 (let ((room (- 2048 (length (gptel-agent--search-stderr job)))))
                                   (when (> room 0)
                                     (setf (gptel-agent--search-stderr job)
                                           (concat (gptel-agent--search-stderr job)
                                                   (substring output 0 (min room (length output))))))))))
                    (gptel-agent--search-process job)
                    (make-process
                     :name "gptel-result-search" :buffer nil :noquery t :connection-type 'pipe
                     :coding '(no-conversion . utf-8-unix) :command (cons program args)
                     :stderr (gptel-agent--search-stderr-process job)
                     :filter (apply-partially #'gptel-agent--search-filter job)
                     :sentinel
                     (lambda (process _event)
                       (when (memq (process-status process) '(exit signal))
                         (gptel-agent--search-collected
                          job
                          (if (and (eq (process-status process) 'exit)
                                   (memq (process-exit-status process) '(0 1))
                                   (string-empty-p (gptel-agent--search-pending job)))
                              "complete"
                            (format "incomplete: search failed (exit %d): %s"
                                    (process-exit-status process)
                                    (decode-coding-string (gptel-agent--search-stderr job) 'utf-8-unix))))))))
              (unless (processp (gptel-agent--search-process job))
                (error "Could not start the advertised search backend"))))
        (error (gptel-agent--search-finish job (concat "incomplete: " (error-message-string err)))))))
  nil)

(provide 'gptel-agent-read)
;;; gptel-agent-read.el ends here
