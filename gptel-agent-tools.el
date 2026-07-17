;;; gptel-agent-tools.el --- LLM tools for gptel-agent     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds the following gptel tools.
;; System:
;; - "Bash"           : Execute a Bash command.
;;
;; Web:
;; - "WebSearch"             : Search the web for the first five results to a query.
;; - "Read"               : Fetch and read the contents of a URL.
;; - "YouTube"       : Find the description and video transcript for a youtube video.
;;
;; Filesystem:
;; - "Mkdir"  : Create a new directory.
;; - "Glob"      : Find files matching a glob pattern
;; - "Grep"      : Grep for text in file(s).
;; - "Read" : Read a specific line range from a file.
;; - "Insert"  : Insert text at a specific line number in a file.
;; - "Edit"      : Replace text in file(s) using string match or unified diff.
;; - "Write"      : Create a new file with content.

;;; Code:



(require 'gptel)
(require 'eww)
(require 'url-http)
(require 'cl-lib)

(declare-function org-escape-code-in-region "org-src")
(declare-function gptel-agent--cached-read-file "gptel-agent")
(declare-function gptel-agent--agent-plist "gptel-agent")
(declare-function gptel-agent--skills-system-message "gptel-agent")
(declare-function gptel-agent--agents-tool-message "gptel-agent")

(defvar url-http-end-of-headers)
(defvar gptel-agent--agents)
(defvar gptel-agent--skills)
(defvar gptel-agent--enabled-skills)
(defvar gptel-agent--enabled-agents)
(defconst gptel-agent--hrule
  (propertize "\n" 'face '(:inherit shadow :underline t :extend t)))

;;; Customizable variables
(defcustom gptel-agent-read-file-size-threshold 400
  "Maximum file size in KB above which the \"Read\" tool refuses to read
the entire file and requires a line range.

Default: 400 KB."
  :type 'integer
  :group 'gptel-agent)

(defcustom gptel-agent-preset nil
  "gptel preset to apply when calling sub-agents.

If you want sub-agent calls to use a different backend or (typically
smaller or cheaper) model from the main LLM in use, you can specify it
here, along with any other gptel settings.

It can specified as the name (a symbol) of a preset defined with
`gptel-make-preset', or as a plist with preset keys like :backend and
:model.  See this function for recognized keys and types.

Note that you can also specify these parameters per-agent in the agent
files, in the Markdown frontmatter or Org properties.  Agent-specific
parameters take precedence over this value."
  :type '(choice (symbol :tag "Name of preset")
                 (plist  :tag "Preset plist spec"))
  :group 'gptel-agent)

(defcustom gptel-agent-max-tool-repetitions 5
  "Max times a tool can be called with identical arguments before blocking.

When a subagent calls the same tool with the same arguments this many
times consecutively, the call is blocked with an error message asking the
LLM to change its approach.  Set to nil to disable repetition detection."
  :type '(choice (natnum :tag "Max repetitions")
                 (const :tag "Disable" nil))
  :group 'gptel-agent)

(defcustom gptel-agent-max-similar-tool-calls 4
  "Max similar read/search calls allowed in the recent tool history.

This catches non-consecutive stalls such as repeatedly reading overlapping
sections of the same file or repeatedly searching for near-identical queries.
Set to nil to disable similar-call detection."
  :type '(choice (natnum :tag "Max similar calls")
                 (const :tag "Disable" nil))
  :group 'gptel-agent)

(defcustom gptel-agent-tool-history-size 12
  "Number of recent tool calls to keep for loop detection."
  :type 'natnum
  :group 'gptel-agent)

(defcustom gptel-agent-tool-cycle-length 4
  "Length of alternating exact tool-call cycle to block.

With the default value of 4, the sequence A B A B is blocked.  Set to nil
to disable alternating-cycle detection."
  :type '(choice (natnum :tag "Cycle length")
                 (const :tag "Disable" nil))
  :group 'gptel-agent)

(defcustom gptel-agent-max-loop-violations 2
  "Number of detected similar/cyclic tool loops before stopping a sub-agent.

The first violation is returned to the model as a blocked tool result so it
can change strategy.  A consecutive violation stops the run."
  :type 'natnum
  :group 'gptel-agent)

(defcustom gptel-agent-max-web-tool-calls 12
  "Maximum web tool calls allowed in one supervised sub-agent run.

This counts calls to WebSearch, WebFetch and YouTube, not asynchronous
callbacks.  Set to nil to disable the separate web-call budget."
  :type '(choice (natnum :tag "Maximum calls")
                 (const :tag "Disable" nil))
  :group 'gptel-agent)

(defcustom gptel-agent-task-timeout 700
  "Timeout in seconds for sub-agent tasks.

If a sub-agent task does not complete within this many seconds,
it is aborted and an error message is returned to the parent.
Set to nil to disable the timeout."
  :type '(choice (natnum :tag "Timeout in seconds")
                 (const :tag "Disable" nil))
  :group 'gptel-agent)

(defcustom gptel-agent-max-request-retries 2
  "Maximum transient transport retries for one sub-agent run.

Retries reuse the request FSM and payload, so completed tool calls are not
repeated.  Authentication, malformed-request and unsupported-parameter
errors are never retried."
  :type 'natnum
  :group 'gptel-agent)

(defcustom gptel-agent-retry-delay 1.0
  "Initial delay in seconds before retrying a transient request failure.

Successive retries use exponential backoff."
  :type 'number
  :group 'gptel-agent)

(defcustom gptel-agent-max-request-rounds 30
  "Maximum model request rounds in a sub-agent run.

The initial model request is one round and every request following tool
results is another.  Transport retries do not consume additional rounds.
Set to nil to disable this limit."
  :type '(choice (natnum :tag "Maximum rounds")
                 (const :tag "Disable" nil))
  :group 'gptel-agent)

(defcustom gptel-agent-run-history-size 20
  "Number of terminal sub-agent run summaries retained for diagnostics."
  :type 'natnum
  :group 'gptel-agent)

(defcustom gptel-agent-keep-failed-run-buffers t
  "Whether to keep failed, timed-out and cancelled sub-agent buffers.

Kept buffers contain the live diagnostic transcript and remain accessible
from the Agent overlay until the user kills the buffer."
  :type 'boolean
  :group 'gptel-agent)

(defvar-local gptel-agent--current-agent nil
  "Name of the agent represented by the current buffer.")

(defvar-local gptel-agent--agent-snapshot nil
  "Request-stable snapshot of agent definitions available in this buffer.")

(defvar-local gptel-agent--registry-snapshot nil
  "Project-local snapshot of all discovered agent definitions.")

(defvar-local gptel-agent--supervised-run nil
  "The `gptel-agent--run' owned by this sub-agent buffer.")

(defvar gptel-agent--run-counter 0
  "Monotonic counter used to assign identifiers to sub-agent runs.")

(defvar gptel-agent--runs (make-hash-table :test #'equal)
  "Live sub-agent runs keyed by run identifier.")

(defvar gptel-agent--run-history nil
  "Recent terminal sub-agent run summaries, newest first.")

(cl-defstruct (gptel-agent--run
               (:constructor gptel-agent--make-run))
  "Owned state for one delegated Agent tool invocation."
  id state agent description prompt
  parent-fsm parent-buffer child-buffer overlay callback
  fsm response response-checkpoint rounds retries retry-timer timeout-timer
  parent-kill-hook terminal-reason started-at configuration
  tool-calls web-tool-calls)

;;; Tool call repetition detection
(defvar-local gptel-agent--last-tool-call-key nil
  "Last tool call key tracked for repetition detection.
The key is a cons cell (NAME . ARGS).")

(defvar-local gptel-agent--tool-call-streak 0
  "Current streak of consecutive identical tool calls.")

(defvar-local gptel-agent--tool-call-history nil
  "Recent tool-call history for loop and similarity detection.

Each entry is a plist with at least :name, :args and :signature.")

(defvar-local gptel-agent--tool-loop-violations 0
  "Number of consecutive tool loop detections in the current run.")

(defun gptel-agent--reset-tool-call-counts (&rest _)
  "Reset the tool call repetition tracker."
  (setq gptel-agent--last-tool-call-key nil
        gptel-agent--tool-call-streak 0
        gptel-agent--tool-call-history nil
        gptel-agent--tool-loop-violations 0))

(defun gptel-agent--arg (args key)
  "Return KEY from ARGS, accepting plist or alist shapes."
  (or (plist-get args key)
      (alist-get key args)
      (alist-get (intern (substring (symbol-name key) 1))
                 args nil nil #'equal)
      (alist-get (substring (symbol-name key) 1) args nil nil #'equal)))

(defun gptel-agent--normalize-text (text)
  "Normalize TEXT for comparing similar tool-call arguments."
  (when text
    (let ((case-fold-search t)
          (s (downcase (format "%s" text))))
      (setq s (replace-regexp-in-string "[^[:alnum:]_./:-]+" " " s))
      (string-trim (replace-regexp-in-string "[[:space:]]+" " " s)))))

(defun gptel-agent--normalize-path (path)
  "Normalize PATH for comparing tool-call arguments."
  (when path
    (abbreviate-file-name
     (expand-file-name (substitute-in-file-name (format "%s" path))))))

(defun gptel-agent--tokenize (text)
  "Tokenize normalized TEXT for rough similarity checks."
  (cl-remove-if
   (lambda (token) (< (length token) 2))
   (split-string (or (gptel-agent--normalize-text text) "") " " t)))

(defun gptel-agent--similar-token-sets-p (a b)
  "Return non-nil when strings A and B are obviously similar."
  (let* ((a (gptel-agent--normalize-text a))
         (b (gptel-agent--normalize-text b))
         (a-tokens (gptel-agent--tokenize a))
         (b-tokens (gptel-agent--tokenize b)))
    (cond
     ((or (string-empty-p (or a ""))
          (string-empty-p (or b "")))
      nil)
     ((or (string= a b)
          (string-search a b)
          (string-search b a))
      t)
     ((or (null a-tokens) (null b-tokens))
      nil)
     (t
      (let* ((intersection
              (cl-count-if (lambda (token) (member token b-tokens)) a-tokens))
             (shorter (min (length a-tokens) (length b-tokens))))
        (and (> shorter 0)
             (>= (/ (float intersection) shorter) 0.75)))))))

(defun gptel-agent--ranges-overlap-or-near-p (start-a end-a start-b end-b)
  "Return non-nil if two optional line ranges overlap or are very close."
  (cond
   ((or (not start-a) (not end-a) (not start-b) (not end-b))
    ;; Whole-file or partially specified reads against the same file are similar.
    t)
   (t
    (let ((gap 20))
      (and (<= start-a (+ end-b gap))
           (<= start-b (+ end-a gap)))))))

(defun gptel-agent--tool-signature (name args)
  "Return a stable signature for exact loop detection."
  (cons name args))

(defun gptel-agent--search-tool-p (name)
  "Return non-nil if NAME is a read/search style tool worth comparing loosely."
  (member name '("Read" "Grep" "Glob" "WebSearch" "WebFetch" "YouTube")))

(defun gptel-agent--similar-tool-call-p (current previous)
  "Return non-nil if CURRENT and PREVIOUS are similar read/search calls."
  (let* ((name (plist-get current :name))
         (args (plist-get current :args))
         (prev-name (plist-get previous :name))
         (prev-args (plist-get previous :args)))
    (and (equal name prev-name)
         (gptel-agent--search-tool-p name)
         (pcase name
           ("Read"
            (and (equal (gptel-agent--normalize-path
                         (gptel-agent--arg args :file_path))
                        (gptel-agent--normalize-path
                         (gptel-agent--arg prev-args :file_path)))
                 (gptel-agent--ranges-overlap-or-near-p
                  (gptel-agent--arg args :start_line)
                  (gptel-agent--arg args :end_line)
                  (gptel-agent--arg prev-args :start_line)
                  (gptel-agent--arg prev-args :end_line))))
           ("Grep"
            (and (equal (gptel-agent--normalize-path
                         (gptel-agent--arg args :path))
                        (gptel-agent--normalize-path
                         (gptel-agent--arg prev-args :path)))
                 (equal (gptel-agent--normalize-text
                         (gptel-agent--arg args :glob))
                        (gptel-agent--normalize-text
                         (gptel-agent--arg prev-args :glob)))
                 (gptel-agent--similar-token-sets-p
                  (gptel-agent--arg args :regex)
                  (gptel-agent--arg prev-args :regex))))
           ("Glob"
            (and (equal (gptel-agent--normalize-path
                         (or (gptel-agent--arg args :path) "."))
                        (gptel-agent--normalize-path
                         (or (gptel-agent--arg prev-args :path) ".")))
                 (gptel-agent--similar-token-sets-p
                  (gptel-agent--arg args :pattern)
                  (gptel-agent--arg prev-args :pattern))))
           ("WebSearch"
            (gptel-agent--similar-token-sets-p
             (gptel-agent--arg args :query)
             (gptel-agent--arg prev-args :query)))
           ((or "WebFetch" "YouTube")
            (equal (gptel-agent--normalize-text
                    (gptel-agent--arg args :url))
                   (gptel-agent--normalize-text
                    (gptel-agent--arg prev-args :url))))
           (_ nil)))))

(defun gptel-agent--alternating-cycle-p (history)
  "Return non-nil if HISTORY starts with an exact alternating cycle."
  (when (and gptel-agent-tool-cycle-length
             (>= gptel-agent-tool-cycle-length 4)
             (cl-evenp gptel-agent-tool-cycle-length)
             (>= (length history) gptel-agent-tool-cycle-length))
    (let ((recent (cl-subseq history 0 gptel-agent-tool-cycle-length))
          first second)
      (setq first (plist-get (nth 0 recent) :signature)
            second (plist-get (nth 1 recent) :signature))
      (and (not (equal first second))
           (cl-loop for idx from 0 below gptel-agent-tool-cycle-length
                    for signature = (plist-get (nth idx recent) :signature)
                    always (equal signature
                                  (if (cl-evenp idx) first second)))))))

(defun gptel-agent--similar-call-count (entry history)
  "Return number of calls similar to ENTRY in HISTORY, including ENTRY."
  (1+ (cl-count-if
       (lambda (previous)
         (gptel-agent--similar-tool-call-p entry previous))
       history)))

(defun gptel-agent--push-tool-call-history (entry)
  "Push ENTRY onto `gptel-agent--tool-call-history' and trim it."
  (push entry gptel-agent--tool-call-history)
  (when (> (length gptel-agent--tool-call-history)
           gptel-agent-tool-history-size)
    (setcdr (nthcdr (1- gptel-agent-tool-history-size)
                    gptel-agent--tool-call-history)
            nil)))

(defun gptel-agent--loop-violation (message)
  "Block with MESSAGE once, then stop after repeated loop violations."
  (cl-incf gptel-agent--tool-loop-violations)
  (if (>= gptel-agent--tool-loop-violations
          (max 1 gptel-agent-max-loop-violations))
      (list :stop t :stop-reason message)
    (list :block message)))

(defun gptel-agent--detect-repetition (tool-call-info)
  "Pre-tool-call hook that detects and blocks repeated identical tool calls.

TOOL-CALL-INFO is a plist with :name, :args, :buffer, :backend, :model."
  (let* ((name (plist-get tool-call-info :name))
         (args (plist-get tool-call-info :args))
         (key (cons name args))
         (entry (list :name name
                      :args args
                      :signature (gptel-agent--tool-signature name args)))
         (count
          (if (equal key gptel-agent--last-tool-call-key)
              (1+ gptel-agent--tool-call-streak)
            1))
         (similar-count
          (and gptel-agent-max-similar-tool-calls
               (gptel-agent--similar-call-count
                entry gptel-agent--tool-call-history)))
         (run gptel-agent--supervised-run)
         (web-tool-p (member name '("WebSearch" "WebFetch" "YouTube")))
         result)
    (setq gptel-agent--last-tool-call-key key
          gptel-agent--tool-call-streak count)
    (gptel-agent--push-tool-call-history entry)
    (when run
      (cl-incf (gptel-agent--run-tool-calls run))
      (when web-tool-p
        (cl-incf (gptel-agent--run-web-tool-calls run))))
    (setq
     result
     (cond
      ((and run web-tool-p gptel-agent-max-web-tool-calls
            (> (gptel-agent--run-web-tool-calls run)
               gptel-agent-max-web-tool-calls))
       (list :stop t
             :stop-reason
             (format "Agent stopped after exceeding the web tool budget (%d calls)."
                     gptel-agent-max-web-tool-calls)))
      ((and gptel-agent-max-tool-repetitions
            (> count gptel-agent-max-tool-repetitions))
       (list :stop t
             :stop-reason
             (format "Agent stuck: tool \"%s\" called %d times with same arguments"
                     name count)))
      ((and gptel-agent-max-tool-repetitions
            (eq count gptel-agent-max-tool-repetitions))
       (list :block
             (format "Error: You have called tool \"%s\" %d times with identical \
arguments. This is not making progress. Change your approach, use different \
parameters, or stop and report what you have so far."
                     name count)))
      ((gptel-agent--alternating-cycle-p gptel-agent--tool-call-history)
       (gptel-agent--loop-violation
        "Error: Tool calls are alternating in a repeated A-B-A-B pattern. \
This is not making progress. Change strategy, use different evidence, or stop \
and report what you have so far."))
      ((and similar-count
            (>= similar-count gptel-agent-max-similar-tool-calls))
       (gptel-agent--loop-violation
        (format "Error: You have made %d similar %s calls recently. \
Repeated reads/searches with similar arguments are not making progress. \
Use a different approach, broaden or narrow the query substantially, or stop \
and report what you have so far."
                similar-count name)))))
    (unless result
      (setq gptel-agent--tool-loop-violations 0))
    result))

;;; Tool use preview
(defun gptel-agent--confirm-overlay (from to &optional no-hide)
  "Set up tool call preview overlay FROM TO.

If NO-HIDE is non-nil, don't hide the overlay body by default."
  (let ((ov (make-overlay from to nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'gptel-agent-tool t)
    (overlay-put ov 'priority 10)
    (overlay-put ov 'keymap
                 (make-composed-keymap
                  (define-keymap
                    "n"     'gptel-agent--next-overlay
                    "p"     'gptel-agent--previous-overlay
                    "q"     'gptel--reject-tool-calls
                    "<tab>" 'gptel-agent--cycle-overlay
                    "TAB"   'gptel-agent--cycle-overlay)
                  gptel-tool-call-actions-map))
    (unless no-hide
      (gptel-agent--cycle-overlay ov))
    ov))

(defun gptel-agent--cycle-overlay (ov)
  "Cycle tool call preview overlay OV at point."
  (interactive (list (cdr (get-char-property-and-overlay
                           (point) 'gptel-agent-tool))))
  (save-excursion
    (goto-char (overlay-start ov))
    (let ((line-end (line-end-position))
          (end      (overlay-end ov)))
      (pcase-let ((`(,value . ,hide-ov)
                   (get-char-property-and-overlay line-end 'invisible)))
        (if (and hide-ov (eq value t))
            (delete-overlay hide-ov)
          (unless hide-ov (setq hide-ov (make-overlay line-end (1- end) nil t)))
          (overlay-put hide-ov 'evaporate t)
          (overlay-put hide-ov 'invisible t)
          (overlay-put hide-ov 'before-string " ▼"))))))

(defun gptel-agent--next-overlay ()
  "Jump to the next `gptel-agent' tool overlay."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'gptel-agent-tool)))
              (end (overlay-end ov)))
    (when (get-char-property end 'gptel-tool)
      (goto-char end))))

(defun gptel-agent--previous-overlay ()
  "Jump to the previous `gptel-agent' tool overlay."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (1- (point)) 'gptel-agent-tool))))
    (goto-char (overlay-start ov))))

(defsubst gptel-agent--block-bg ()
  "Return a background face suitable for displaying code."
  (cond
   ((derived-mode-p 'org-mode) 'org-block)
   ((derived-mode-p 'markdown-mode) 'markdown-code-face)
   (t `( :background ,(face-attribute 'mode-line-inactive :background)
         :extend t))))

(defun gptel-agent--fontify-block (path-or-mode start end)
  "Fontify region from START to END.

Fontification is assuming it is the contents of file PATH-OR-MODE (if it
is a string), or major-mode (if it is a symbol).  Applied font-lock-face
properties persist through refontification."
  (let ((lang-mode)                     ; (org-src-get-lang-mode lang)
        (org-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties org-buffer start end)
      (insert " ")                      ; Add space to ensure property change
      (delay-mode-hooks
        (if (symbolp path-or-mode)
            (setq lang-mode path-or-mode)
          (let ((buffer-file-name path-or-mode))
            (setq lang-mode
                  (or (cdr (assoc-string
                            (concat
                             "\\." (file-name-extension path-or-mode) "\\'")
                            auto-mode-alist))
                      (progn (set-auto-mode t) major-mode)))))
        (funcall lang-mode))
      (font-lock-ensure)
      (let ((pos (point-min)))
        (while (< pos (1- (point-max))) ; Skip the added space
          (let* ((next (next-property-change pos nil (1- (point-max))))
                 (face-prop (get-text-property pos 'face)))
            (when face-prop
              (put-text-property
               (+ start (- pos (point-min)))
               (+ start (- (or next (1- (point-max))) (point-min)))
               'font-lock-face face-prop org-buffer))
            (setq pos (or next (1- (point-max))))))))))

;;; System tools
;; "Execute Bash commands to inspect files and system state.

;; This tool provides access to a Bash shell with GNU coreutils (or equivalents)
;; available. You can use any standard Linux commands including: cd, ls, file, cat,
;; grep, awk, sed, head, tail, wc, find, sort, uniq, cut, tr, and more.

;; PURPOSE:
;; - Efficiently inspect files and system state WITHOUT consuming excessive
;; tokens. This is preferred over reading entire large files.
;; - Modify files or system state as appropriate, using cp, mv, rm, patch,
;; git subcommands (git log, commit, branch and more) and so on.

;; BEST PRACTICES:
;; - Use pipes to combine commands: 'cat file.log | grep ERROR | tail -20'
;; - For large files, use head/tail: 'head -50 file.txt' or 'tail -100 file.log'
;; - Use grep with context: 'grep -A 5 -B 5 pattern file.txt'
;; - Check file sizes first: 'wc -l file.txt' before reading
;; - Use file command to identify file types: 'file *'
;; - Combine with other tools: 'find . -name \"*.el\" | head -10'

;; EXAMPLES:
;; - List files with details: 'ls -lah /path/to/dir'
;; - Print lines 25-35 of a long file/stream: 'sed -n \"25,35p\" app.log'
;; - Find recent errors: 'grep -i error /var/log/app.log | tail -20'
;; - Check file type: 'file document.pdf'
;; - Count lines: 'wc -l *.txt'
;; - Search with context: 'grep -A 3 \"function foo\" script.sh'

;; The command will be executed in the current working directory. Output is
;; returned as a string. Long outputs should be filtered/limited using pipes."

;; - Can run commands in background with `run_in_background: true`
;; - Default timeout is 2 minutes (120000ms), max is 10 minutes

(defun gptel-agent--eval-elisp-preview-setup (arg-values _info)
  "Setup preview overlay for Elisp evaluation tool call.

ARG-VALUES is the list of arguments for the tool call."
  (let ((expr (car arg-values))
        (from (point)) (inner-from))
    (insert
     "(" (propertize "Eval" 'font-lock-face 'font-lock-keyword-face)
     ")\n")
    (setq inner-from (point))
    (insert expr)
    (gptel-agent--fontify-block 'emacs-lisp-mode inner-from (point))
    ;; (add-text-properties inner-from (point) '(line-prefix "  " wrap-prefix "  "))
    (insert "\n\n")
    (font-lock-append-text-property
     inner-from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--execute-bash-preview-setup (arg-values _info)
  "Setup preview overlay for Bash command execution tool call.

ARG-VALUES is the list of arguments for the tool call."
  (let ((command (car arg-values))
        (from (point)) (inner-from))
    (insert
     "(" (propertize "Bash" 'font-lock-face 'font-lock-keyword-face)
     " in " (propertize (abbreviate-file-name default-directory)
                        'font-lock-face 'font-lock-string-face)
     ")\n")
    (setq inner-from (point))
    (insert command)
    (gptel-agent--fontify-block 'sh-mode inner-from (point))
    (insert "\n\n")
    (font-lock-append-text-property
     inner-from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--execute-bash (callback command)
  "Execute COMMAND asynchronously in bash and call CALLBACK with output.

CALLBACK is called with the command output string when the process finishes.
COMMAND is the bash command string to execute."
  (let* ((output-buffer (generate-new-buffer " *gptel-agent-bash*"))
         (proc (make-process
                :name "gptel-agent-bash"
                :buffer output-buffer
                :command (list "bash" "-c" command)
                :connection-type 'pipe
                :file-handler t
                :sentinel
                (lambda (process _event)
                  (when (memq (process-status process) '(exit signal))
                    (let* ((exit-code (process-exit-status process))
                           (output (with-current-buffer (process-buffer process)
                                     (buffer-string))))
                      (kill-buffer (process-buffer process))
                      (funcall callback
                               (if (zerop exit-code)
                                   output
                                 (format "Command failed with exit code %d:\nSTDOUT+STDERR:\n%s"
                                         exit-code output)))))))))
    proc))

;;; Web tools

(defun gptel-agent--fetch-with-timeout (url url-cb tool-cb failed-msg &rest args)
  "Fetch URL and call URL-CB in the result buffer.

Call TOOL-CB if there is an error or a timeout.  TOOL-CB and ARGS are
passed to URL-CB.  FAILED-MSG is a fragment used for messaging.  The tool
callback is guaranteed to run at most once, including parser exceptions."
  (let* ((timeout 30) timer done proc-buffer
         (inherit-process-coding-system t)
         (finish
          (lambda (result)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (funcall tool-cb result)))))
    (condition-case err
        (setq
         proc-buffer
         (url-retrieve
          url
          (lambda (status)
            (unwind-protect
                (if-let* ((request-error (plist-get status :error)))
                    (funcall finish
                             (format "Error: %s failed with error: %S"
                                     failed-msg request-error))
                  (condition-case parse-error
                      (apply url-cb finish args)
                    (error
                     (funcall
                      finish
                      (format "Error: %s could not be parsed: %S"
                              failed-msg parse-error)))))
              (when (buffer-live-p (current-buffer))
                (kill-buffer (current-buffer)))))
          nil 'silent))
      (error
       (funcall finish
                (format "Error: %s could not start: %S" failed-msg err))))
    (when (and proc-buffer (not done))
      (setq timer
            (run-at-time
             timeout nil
             (lambda (buf)
               (unless done
                 (when (buffer-live-p buf)
                   (let ((kill-buffer-query-functions)) (kill-buffer buf)))
                 (funcall
                  finish (format "Error: %s timed out after %d seconds."
                                 failed-msg timeout))))
             proc-buffer)))
    proc-buffer))

;;;; Web search
(defun gptel-agent--shr-next-link ()
  "Jump to the next SHR link in the buffer.  Return jump position."
  (let ((current-prop (get-char-property (point) 'shr-url))
        (next-pos (point)))
    (while (and (not (eobp))
                (setq next-pos
                      (or (next-single-property-change (point) 'shr-url)
                          (point-max)))
                (let ((next-prop (get-char-property next-pos 'shr-url)))
                  (or (equal next-prop current-prop)
                      (equal next-prop nil))))
      (goto-char next-pos))
    (goto-char next-pos)))

(defvar gptel-agent--web-search-active nil)

(defun gptel-agent--web-search-eww (tool-cb query &optional count)
  "Search the web using eww's default search engine (usually DuckDuckGo).

Call TOOL-CB with the results as a string.  QUERY is the search string.
COUNT is the number of results to return (default 5)."
  ;; No more than two active searches at one time
  (setq gptel-agent--web-search-active
        (cl-delete-if-not
         (lambda (buf) (and (buffer-live-p buf)
                       (process-live-p (get-buffer-process buf))))
         gptel-agent--web-search-active))
  (if (>= (length gptel-agent--web-search-active) 2)
      (progn (message "Web search: waiting for turn")
             (run-at-time 5 nil #'gptel-agent--web-search-eww
                          tool-cb query count))
    (when-let* ((buffer
                 (gptel-agent--fetch-with-timeout
                  (concat eww-search-prefix (url-hexify-string query))
                  #'gptel-agent--web-search-eww-callback
                  tool-cb (format "Web search for \"%s\"" query) count)))
      (push buffer gptel-agent--web-search-active))))

(defun gptel-agent--web-fix-unreadable ()
  "Replace invalid characters from point to end in current buffer."
  (while (and (skip-chars-forward "\0-\x3fff7f")
              (not (eobp)))
    (display-warning
     '(gptel gptel-agent-tools)
     (format "Invalid character in buffer \"%s\"" (buffer-name)))
    (delete-char 1) (insert "?")))

(defun gptel-agent--web-search-eww-callback (cb &optional count)
  "Extract website text and run callback CB with it."
  (let* ((count (or count 5)) (results))
    (goto-char (point-min))
    (goto-char url-http-end-of-headers)
    ;; (gptel-agent--web-fix-unreadable)
    (let* ((dom (libxml-parse-html-region (point) (point-max)))
           (result-count 0))
      (eww-score-readability dom)
      ;; (erase-buffer) (buffer-disable-undo)
      (with-temp-buffer
        (shr-insert-document (eww-highest-readability dom))
        (goto-char (point-min))
        (while (and (not (eobp)) (< result-count count))
          (let ((pos (point))
                (url (get-char-property (point) 'shr-url))
                (next-pos (gptel-agent--shr-next-link)))
            (when-let* (((stringp url))
                        (idx (string-search "http" url))
                        (url-fmt (url-unhex-string (substring url idx))))
              (cl-incf result-count)
              (push (list :url url-fmt
                          :excerpt
                          (string-trim
                           (buffer-substring-no-properties pos next-pos)))
                    results))))))
    (funcall cb (prin1-to-string (nreverse results)))))

;;;; Read URLs
(defun gptel-agent--read-url (tool-cb url)
  "Fetch URL text and call TOOL-CB with it."
  (gptel-agent--fetch-with-timeout
   url
   (lambda (cb)
     (goto-char (point-min)) (forward-paragraph)
     (condition-case errdata
         (let ((dom (libxml-parse-html-region (point) (point-max))))
           (with-temp-buffer
             (eww-score-readability dom)
             (shr-insert-document (eww-highest-readability dom))
             (decode-coding-region (point-min) (point-max) 'utf-8)
             (funcall
              cb (buffer-substring-no-properties
                  (point-min) (point-max)))))
       (error (funcall cb (format "Error: Request failed with error data:\n%S"
                                  errdata)))))
   tool-cb (format "Fetch for \"%s\"" url)))

;;;; Fetch youtube transcript
(defun gptel-agent--yt-parse-captions (xml-string)
  "Parse YouTube caption XML-STRING and return DOM."
  (with-temp-buffer
    (insert xml-string)
    (set-buffer-multibyte t)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (goto-char (point-min))
    ;; Clean up the XML
    (dolist (reps '(("\n" . " ")
                    ("&amp;" . "&")
                    ("&quot;" . "\"")
                    ("&#39;" . "'")
                    ("&lt;" . "<")
                    ("&gt;" . ">")))
      (save-excursion
        (while (search-forward (car reps) nil t)
          (replace-match (cdr reps) nil t))))
    (libxml-parse-xml-region (point-min) (point-max))))

(defun gptel-agent--yt-format-captions (caption-dom &optional chunk-time)
  "Format CAPTION-DOM as paragraphs with timestamps.

CHUNK-TIME is the number of seconds per paragraph (default 30)."
  (when (and (listp caption-dom)
             (eq (car-safe caption-dom) 'transcript))
    (let ((chunk-time (or chunk-time 30))
          (result "")
          (current-para "")
          (para-start-time 0))
      (dolist (elem (cddr caption-dom)) ;; Process each text element
        (when (and (listp elem) (eq (car elem) 'text))
          (let* ((attrs (cadr elem))
                 (text (caddr elem))
                 (start (string-to-number (cdr (assoc 'start attrs))))
                 ;; Check if we've crossed into a new chunk-time boundary
                 (should-chunk (and (> (abs (- start para-start-time)) 3)
                                    (not (= (floor para-start-time chunk-time)
                                            (floor start chunk-time))))))
            (when (and should-chunk (> (length current-para) 0))
              ;; Add completed paragraph
              (setq result (concat result
                                   (format "[%d:%02d]\n%s\n\n"
                                           (floor para-start-time 60)
                                           (mod para-start-time 60)
                                           (string-trim current-para))))
              (setq current-para "")
              (setq para-start-time start))

            (when text
              (setq current-para (concat current-para " " text))))))

      ;; Add final paragraph
      (when (> (length current-para) 0)
        (setq result (concat result
                             (format "[%d:%02d]\n%s\n\n"
                                     (floor para-start-time 60)
                                     (mod para-start-time 60)
                                     (string-trim current-para)))))
      result)))

(defun gptel-agent--yt-fetch-watch-page (callback video-id)
  "Step 1: Fetch YouTube watch page for VIDEO-ID.

Call CALLBACK with error or proceeds to fetch InnerTube data."
  (url-retrieve
   (format "https://youtube.com/watch?v=%s" video-id)
   (lambda (status callback video-id)
     (if-let ((error (plist-get status :error)))
         (funcall callback (format "Error fetching page: %s" error))
       (goto-char (point-min))
       (search-forward "\n\n" nil t)
       (let* ((html (buffer-substring (point) (point-max)))
              (api-key (and (string-match
                             "\"INNERTUBE_API_KEY\":\"\\([a-zA-Z0-9_-]+\\)"
                             html)
                            (match-string 1 html))))
         (if api-key
             (gptel-agent--yt--fetch-innertube callback video-id api-key)
           (funcall callback "Error: Could not extract API key")))))
   (list callback video-id)))

(defun gptel-agent--yt--fetch-innertube (callback video-id api-key)
  "Step 2: Fetch VIDEO-ID metadata from YouTube InnerTube API.

Call CALLBACK with error or proceeds to fetch captions."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")
           ("Accept-Language" . "en-US")))
        (url-request-data
         (encode-coding-string
          (json-encode
           `((context . ((client . ((clientName . "ANDROID")
                                    (clientVersion . "20.10.38")))))
             (videoId . ,video-id)))
          'utf-8)))
    (url-retrieve
     (format "https://www.youtube.com/youtubei/v1/player?key=%s" api-key)
     (lambda (status callback)
       (if-let ((error (plist-get status :error)))
           (funcall callback (format "Error fetching metadata: %s" error))
         (goto-char (point-min))
         (search-forward "\n\n" nil t)
         (let* ((json-data (ignore-errors
                             (json-parse-buffer :object-type 'plist)))
                (video-details (plist-get json-data :videoDetails))
                (description (plist-get video-details :shortDescription))
                (caption-tracks (map-nested-elt
                                 json-data
                                 '(:captions
                                   :playerCaptionsTracklistRenderer
                                   :captionTracks))))
           (gptel-agent--yt-fetch-captions callback description caption-tracks))))
     (list callback))))

(defun gptel-agent--yt-fetch-captions (callback description caption-tracks)
  "Step 3: Find and fetch English captions for CAPTION-TRACKS.

Call CALLBACK with formatted result containing DESCRIPTION and transcript."
  (if (not caption-tracks)
      (funcall callback
               (format "# Description\n\n%s\n\n# Transcript\n\nNo transcript available."
                       (or description "No description available.")))
    (let ((en-caption
           (cl-find-if
            (lambda (track)
              (string-match-p "^en" (or (plist-get track :languageCode) "")))
            caption-tracks)))
      (if (not en-caption)
          (funcall callback
                   (format "# Description\n\n%s\n\n# Transcript\n\nNo English transcript available."
                           (or description "No description available.")))
        (let ((base-url (replace-regexp-in-string
                         "&fmt=srv3" ""
                         (plist-get en-caption :baseUrl))))
          (url-retrieve
           base-url
           (lambda (status callback description)
             (if-let ((error (plist-get status :error)))
                 (funcall callback
                          (format "# Description\n\n%s\n\n# Transcript\n\nError fetching transcript: %s"
                                  (or description "No description available.")
                                  error))
               (goto-char (point-min))
               (search-forward "\n\n" nil t)
               (let* ((xml-string (buffer-substring (point) (point-max)))
                      (caption-dom (gptel-agent--yt-parse-captions xml-string))
                      (formatted-transcript
                       (gptel-agent--yt-format-captions caption-dom 30)))
                 (funcall callback
                          (format "# Description\n\n%s\n\n# Transcript\n\n%s"
                                  (or description "No description available.")
                                  (or formatted-transcript "Error parsing transcript."))))))
           (list callback description)))))))

(defun gptel-agent--yt-read-url (callback url)
  "Fetch YouTube metadata and transcript for URL, calling CALLBACK with result.
CALLBACK is called with a markdown-formatted string containing the video
description and transcript formatted as timestamped paragraphs."
  (if-let* ((video-id
             (and (string-match
                   (rx bol (opt "http" (opt "s") "://")
                       (opt "www.") "youtu" (or ".be" "be.com") "/"
                       (opt "watch?v=")
                       (group (one-or-more (not (any "?&")))))
                   url)
                  (match-string 1 url))))
      (gptel-agent--yt-fetch-watch-page callback video-id)
    (funcall callback "Error: Invalid YouTube URL")))

;;; Code tools
;;;; Diagnostics from flymake
(declare-function flymake--project-diagnostics "flymake")
(declare-function flymake--diag-beg "flymake")
(declare-function flymake--diag-type "flymake")
(declare-function flymake--diag-text "flymake")
(declare-function flymake-diagnostic-buffer "flymake")

(defun gptel-agent--flymake-diagnostics (&optional all)
  "Collect flymake errors across all open buffers in the current project.

Errors with low severity are not collected.  With ALL, collect all
diagnostics."
  (let ((project (project-current)))
    (unless project
      (error "Not in a project.  Cannot collect flymake diagnostics"))
    (require 'flymake)
    (let ((results '()))
      (dolist (diag (flymake--project-diagnostics project))
        (let ((severity (flymake--diag-type diag)))
          (when (memq severity `(:error :warning ,@(and all '(:note))))
            (with-current-buffer (flymake-diagnostic-buffer diag)
              (let* ((beg (flymake--diag-beg diag))
                     (line-num (line-number-at-pos beg))
                     (line-text (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position))))
                (push (format "File: %s:%d\nSeverity: %s\nMessage: %s\n---\n%s"
                              (buffer-file-name)
                              line-num
                              severity
                              (flymake--diag-text diag)
                              line-text)
                      results))))))
      (string-join (nreverse results) "\n\n"))))

;;; Filesystem tools
;;;; Make directories
;;;; Writing to files
(defun gptel-agent--edit-files-preview-setup (arg-values _info)
  "Insert tool call preview for ARG-VALUES for \"Edit\" tool."
  (pcase-let ((from (point)) (files-affected) (description)
              (`(,path ,old-str ,new-str-or-diff ,diffp) arg-values))

    (if (and diffp (not (eq diffp :json-false)))
        (progn                          ;Patch
          (insert new-str-or-diff)
          (save-excursion
            (while (re-search-backward "^\\+\\+\\+ \\(.*\\)$" from t)
              (push (match-string 1) files-affected))
            (goto-char from)
            (when (looking-at "^ *```\\(diff\\|patch\\)\\s-*\n")
              (delete-region (match-beginning 0) (match-end 0))))
          (skip-chars-backward " \t\r\n")
          (when (looking-back "^ *```\\s-*\\'" (line-beginning-position))
            (delete-region (line-beginning-position) (line-end-position)))
          (setq description "Patch")
          (require 'diff-mode)
          (gptel-agent--fontify-block 'diff-mode from (point)))
      (when old-str                     ;Text replacement
        (push path files-affected)
        (setq description "ReplaceIn")
        (insert
         (propertize old-str 'font-lock-face 'diff-removed
                     'line-prefix (propertize "-" 'face 'diff-removed))
         "\n" (propertize new-str-or-diff 'font-lock-face 'diff-added
                          'line-prefix (propertize "+" 'face 'diff-added))
         "\n")))
    (insert "\n")
    (font-lock-append-text-property
     from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
    (when (derived-mode-p 'org-mode)
      (org-escape-code-in-region from (1- (point))))
    (save-excursion
      (goto-char from)
      (insert
       "(" (propertize description 'font-lock-face 'font-lock-keyword-face)
       " " (mapconcat (lambda (f) (propertize (concat "\"" f "\"")
                                         'font-lock-face 'font-lock-constant-face))
                      files-affected " ")
       ")\n"))
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--fix-patch-headers ()
  "Fix line numbers in hunks in diff at point."
  ;; Find and process each hunk header
  (while (re-search-forward "^@@ -\\([0-9]+\\),\\([0-9]+\\) +\\+\\([0-9]+\\),\\([0-9]+\\) @@" nil t)
    (let ((hunk-start (line-beginning-position))
          (orig-line (string-to-number (match-string 1)))
          (new-line (string-to-number (match-string 3)))
          (orig-count 0)
          (new-count 0))

      ;; Count lines in this hunk until we hit the next @@ or EOF
      (goto-char hunk-start)
      (forward-line 1)
      (save-match-data
        (while (and (not (eobp))
                    (not (looking-at-p "^@@")))
          (cond
           ;; Removed lines (not ---)
           ((looking-at-p "^-[^-]")
            (cl-incf orig-count))
           ;; Added lines (not +++)
           ((looking-at-p "^\\+[^+]")
            (cl-incf new-count))
           ;; Context lines (space at start)
           ((looking-at-p "^ ")
            (cl-incf orig-count)
            (cl-incf new-count)))
          (forward-line 1)))

      ;; Replace the hunk header with corrected counts
      (goto-char hunk-start)
      (delete-line)
      (insert (format "@@ -%d,%d +%d,%d @@\n"
                      orig-line orig-count new-line new-count)))))

;;;; Create a directory
(defun gptel-agent--make-directory (parent name)
  "Create a directory NAME in PARENT directory.

Creates the directory and any missing parent directories.  If the
directory already exists, this is a no-op and returns success.

PARENT is the parent directory path,NAME is the name of the new
directory to create."
  (condition-case errdata
      (progn
        (make-directory (expand-file-name name parent) t)
        (format "Directory %s created/verified in %s" name parent))
    (error "Error creating directory %s in %s:\n%S" name parent errdata)))

(defun gptel-agent--edit-files (path &optional old-str new-str-or-diff diffp)
  "Replace text in file(s) at PATH using either string matching or unified diff.

This function supports two distinct modes of operation:

1. STRING REPLACEMENT MODE (DIFFP is nil or :json-false):
   - Searches for OLD-STR in the file at PATH
   - Replaces it with NEW-STR-OR-DIFF
   - Requires OLD-STR to match exactly once (uniquely) in the file
   - Only works on single files, not directories

2. DIFF/PATCH MODE (when DIFFP is non-nil and not :json-false):
   - Applies NEW-STR-OR-DIFF as a unified diff using the `patch` command
   - Works on both single files and directories
   - OLD-STR is ignored in this mode
   - NEW-STR-OR-DIFF can contain the diff in fenced code blocks
     (=diff or =patch)
   - Uses the -N (--forward) option to ignore already-applied patches

PATH - File or directory path to modify (must be readable)
OLD-STR - (String mode only) Exact text to find and replace
NEW-STR-OR-DIFF - Replacement text (string mode) or unified diff (diff mode)
DIFFP - If non-nil (and not :json-false), use diff/patch mode

Error Conditions:
  - PATH not readable
  - (String mode) PATH is a directory
  - (String mode) OLD-STR not found in file
  - (String mode) OLD-STR matches multiple times (ambiguous)
  - (Diff mode) patch command fails (exit status non-zero)

Returns:
  Success message string describing what was changed

Signals:
  error - On any failure condition (caught and displayed by gptel)"
  (unless (file-readable-p path)
    (error "Error: File or directory %s is not readable" path))

  (unless new-str-or-diff
    (error "Required argument `new_str' missing"))

  (if (or (eq diffp :json-false) old-str)
      ;; Replacement by Text
      (progn
        (when (file-directory-p path)
          (error "Error: String replacement is intended for single files, not directories (%s)"
                 path))
        (with-temp-buffer
          (insert-file-contents path)
          (if (search-forward old-str nil t)
              (if (save-excursion (search-forward old-str nil t))
                  (error "Error: Match is not unique.\
Consider providing more context for the replacement, or a unified diff")
                ;; TODO: More robust backspace escaping
                (replace-match (string-replace  "\\" "\\\\" new-str-or-diff))
                (write-region nil nil path)
                (format "Successfully replaced %s (truncated) with %s (truncated)"
                        (truncate-string-to-width old-str 20 nil nil t)
                        (truncate-string-to-width new-str-or-diff 20 nil nil t)))
            (error "Error: Could not find old_str \"%s\" in file %s"
                   (truncate-string-to-width old-str 20) path))))
    ;; Replacement by Diff
    (let ((default-directory (file-name-directory (expand-file-name path))))
      (unless (executable-find "patch" t)
        (error "Error: Command \"patch\" not available, cannot apply diffs.\
Use string replacement instead"))
      (let* ((out-buf-name (generate-new-buffer-name "*patch-stdout*"))
             ;; (err-buf-name (generate-new-buffer-name "*patch-stderr*"))
             (target-file (expand-file-name path))
             (diff-file (make-nearby-temp-file "gptel-agent-diff-" nil ".patch"))
             (exit-status -1)           ; Initialize to a known non-zero value
             ;; (result-error "")
             (result-output ""))
        (unwind-protect
            (let ((patch-options '("--forward" "--verbose")))
              (with-temp-message
                  (format "Applying diff to: `%s` with options: %s"
                          target-file patch-options)
                (with-temp-buffer
                  (insert new-str-or-diff)
                  ;; Insert trailing newline, required by patch
                  (unless (eq (char-before (point-max)) ?\n)
                    (goto-char (point-max))
                    (insert "\n"))
                  (goto-char (point-min))
                  ;; Remove code fences, if present
                  (when (looking-at-p "^ *```diff\n")
                    (save-excursion
                      (delete-line)
                      (goto-char (point-max))
                      (forward-line -1) ;guaranteed to be at a blank newline
                      (when (looking-at-p "^ *```") (delete-line))))
                  ;; Fix line numbers in hunk headers
                  (gptel-agent--fix-patch-headers)
                  (write-region nil nil diff-file nil 'silent))

                (setq exit-status
                      (apply #'process-file "patch" (file-local-name diff-file)
                             (list out-buf-name t) ; stdout/stderr buffer name
                             nil patch-options)))

              ;; Retrieve content from buffers using their names
              (when-let* ((stdout-buf (get-buffer out-buf-name)))
                (when (buffer-live-p stdout-buf)
                  (with-current-buffer stdout-buf
                    (setq result-output (buffer-string)))))

              (if (= exit-status 0)
                  (format "Diff successfully applied to %s.
Patch command options: %s
Patch STDOUT:\n%s"
                          target-file patch-options result-output)
                ;; Signal an Elisp error, which gptel will catch and display.
                ;; The arguments to 'error' become the error message.
                (error "Error: Failed to apply diff to %s (exit status %s).
Patch command options: %s
Patch STDOUT:\n%s"
                       target-file exit-status patch-options
                       result-output)))
          (let ((stdout-buf-obj (get-buffer out-buf-name))) ;Clean up
            (when (buffer-live-p stdout-buf-obj) (kill-buffer stdout-buf-obj)))
          (when (file-exists-p diff-file) (delete-file diff-file)))))))

(defun gptel-agent--insert-in-file-preview-setup (arg-values _info)
  "Preview setup for Insert.
INFO is the tool call info plist.
ARG-VALUES is a list: (path line-number new-str)"
  (let ((from (point)) (line-offset)
        (face-bg (gptel-agent--block-bg))
        (cb (current-buffer)))
    (pcase-let ((`(,path ,line-number ,new-str) arg-values))
      (insert "("
              (propertize "insert_into_file " 'font-lock-face 'font-lock-keyword-face)
              (propertize (concat "\"" path "\"")
                          'font-lock-face 'font-lock-constant-face)
              ")\n")
      (if (file-readable-p path)
          (insert
           (with-temp-buffer       ;NEW-STR with context lines, styled as a diff
             (insert-file-contents path)
             (pcase line-number
               (-1 (goto-char (point-max)))
               (_ (forward-line line-number)))
             (save-excursion
               (forward-line -6)
               (setq line-offset (line-number-at-pos))
               (delete-region (point-min) (point))
               (dotimes (_ 12)
                 (put-text-property
                  (line-beginning-position) (line-end-position)
                  'line-prefix (propertize (format "%4d " line-offset) 'face
                                           `(:inherit ,face-bg :inherit line-number)))
                 (forward-line 1) (when (eolp) (insert " "))
                 (cl-incf line-offset)))
             (insert (propertize new-str 'font-lock-face 'diff-added
                                 'fontified t 'font-lock-multiline t
                                 'line-prefix (propertize "   + " 'face 'diff-added)))
             (save-excursion
               (forward-line 6)
               (delete-region (point) (point-max)))
             (font-lock-append-text-property
              (point-min) (point-max) 'font-lock-face face-bg)
             (when (provided-mode-derived-p
                    (buffer-local-value 'major-mode cb) 'org-mode)
               (org-escape-code-in-region (point-min) (point-max)))
             (buffer-string)) "\n")
        (insert (propertize "[File not readable]\n\n" 'font-lock-face 'error)))
      (gptel-agent--confirm-overlay from (point)))))

(defun gptel-agent--insert-in-file (path line-number new-str)
  "Insert NEW-STR at LINE-NUMBER in file at PATH.

LINE-NUMBER conventions:
- 0 inserts at the beginning of the file
- -1 inserts at the end of the file
- N > 1 inserts before line N"
  (unless (file-readable-p path)
    (error "Error: File %s is not readable" path))

  (when (file-directory-p path)
    (error "Error: Cannot insert into directory %s" path))

  (with-temp-buffer
    (insert-file-contents path)

    (pcase line-number
      (0 (goto-char (point-min)))       ; Insert at the beginning
      (-1 (goto-char (point-max)))      ; Insert at the end
      (_ (goto-char (point-min))
         (forward-line line-number)))   ; Insert before line N

    ;; Insert the new string
    (insert new-str)

    ;; Ensure there's a newline after the inserted text if not already present
    (unless (or (string-suffix-p "\n" new-str) (eobp))
      (insert "\n"))

    ;; Write the modified content back to the file
    (write-region nil nil path)

    (format "Successfully inserted text at line %d in %s" line-number path)))

(defun gptel-agent--write-file-preview-setup (arg-values _info)
  "Setup preview overlay for Write file tool call.

ARG-VALUES is the list of arguments for the tool call."
  (pcase-let ((from (point))
              (`(,path ,filename ,content) arg-values))
    (insert
     "(" (propertize "Write " 'font-lock-face 'font-lock-keyword-face)
     (propertize (prin1-to-string path) 'font-lock-face 'font-lock-constant-face) " "
     (propertize (prin1-to-string filename) 'font-lock-face 'font-lock-constant-face)
     ")\n")
    (let ((inner-from (point)))
      (insert content)
      (gptel-agent--fontify-block filename inner-from (point))
      (insert "\n\n")
      (font-lock-append-text-property
       inner-from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
      (when (derived-mode-p 'org-mode)
        (org-escape-code-in-region inner-from (1- (point)))))
    (gptel-agent--confirm-overlay from (point))))

;;;; Write content to a file
(defun gptel-agent--write-file (path filename content)
  "Write CONTENT to FILENAME in PATH.

PATH and FILENAME are expanded to create the full path.  CONTENT is
written to the file.  Returns a success message string, or signals an
error if writing fails.

PATH, FILENAME, and CONTENT must all be strings."
  (unless (and (stringp path) (stringp filename) (stringp content))
    (error "PATH, FILENAME or CONTENT is not a string, cancelling action"))
  (when-let* ((remote (file-remote-p default-directory)))
    (setq path (concat remote path)))
  (let ((full-path (expand-file-name filename path)))
    (condition-case errdata
        (with-temp-buffer
          (insert content)
          (write-file full-path)
          (format "Created file %s in %s" filename path))
      (error "Error: Could not write file %s:\n%S" path errdata))))

;;;; Find files using regexes
(defun gptel-agent--truncate-buffer (prefix &optional max-lines)
  "Truncate the current buffer if it exceeds 20000 chars.

Save the full content to a temporary file and replace the buffer
with a truncated preview when the size limit is exceeded.

PREFIX is a string identifier for the temporary file name.
MAX-LINES is the number of lines to keep, defaulting to 50."
  ;; Too large - save to temp file and return truncated info
  (when (> (buffer-size) 20000)
    (let* ((max-lines (or max-lines 50))
           (temp-file (make-nearby-temp-file
                       (format "gptel-agent-%s-%s" prefix
                               (format-time-string "%Y%m%d-%H%M%S"))
                       nil ".txt"))
           (orig-size (buffer-size))
           (orig-lines (line-number-at-pos (point-max))))
      ;; Save full content
      (write-region nil nil temp-file)
      ;; Insert truncated header
      (goto-char (point-min))
      (insert (format "%s results too large (%d chars, %d lines) \
 for context window.\nStored in: %s\n\nFirst %d lines:\n\n"
                      prefix orig-size orig-lines temp-file max-lines))
      ;; Truncate to first max-lines lines
      (forward-line max-lines)
      (delete-region (point) (point-max))
      ;; Add footer with read instruction
      (goto-char (point-max))
      (insert (format "\n\n[Use Read tool with file_path=\"%s\" to view full results]"
                      temp-file)))))

(defun gptel-agent--glob (pattern &optional path depth)
  "Find files matching PATTERN using the `tree' command.

PATTERN is a case-insensitive regex pattern to match filenames against.
PATH is the optional directory to search (defaults to current directory).
DEPTH limits recursion depth when provided (non-negative integer).

Returns a string listing matching files with full paths, sorted by
modification time.  If the output is too large (>20000 chars), it writes
the full results to a temporary file and returns a truncated version with
instructions to use `Read' for the full contents.

Raises an error if PATTERN is empty, PATH is not readable, or the
`tree' executable is not found."
  (when (string-empty-p pattern)
    (error "Error: pattern must not be empty"))
  (if path
      (unless (and (file-readable-p path) (file-directory-p path))
        (error "Error: path %s is not readable" path))
    (setq path "."))
  (unless (executable-find "tree" t)
    (error "Error: Executable `tree` not found.  This tool cannot be used"))
  (let ((full-path (expand-file-name path)))
    (with-temp-buffer
      (let* ((args (list "-l" "-f" "-i" "-I" ".git"
                         "--sort=mtime" "--ignore-case"
                         "--prune" "-P" pattern
                         (file-local-name full-path)))
             (args (if (natnump depth)
                       (nconc args (list "-L" (number-to-string depth)))
                     args))
             (exit-code (apply #'process-file "tree" nil t nil args)))
        (when (/= exit-code 0)
          (goto-char (point-min))
          (insert (format "Glob failed with exit code %d\n.STDOUT:\n\n"
                          exit-code))))
      (gptel-agent--truncate-buffer "glob")
      (buffer-string))))

;;;; Read files or directories
(defun gptel-agent--read-file-lines (filename start-line end-line)
  "Return lines START-LINE to END-LINE fom FILENAME."
  (unless (file-readable-p filename)
    (error "Error: File %s is not readable" filename))

  (when (file-directory-p filename)
    (error "Error: Cannot read directory %s as file" filename))

  (when (file-symlink-p filename)
    (setq filename (file-truename filename)))

  (if (and (not start-line) (not end-line)) ;read full file
      (if (> (file-attribute-size (file-attributes filename))
             (* gptel-agent-read-file-size-threshold 1024))
          (error "Error: File is too large (> %d KB).Please specify a line range to read"
                 gptel-agent-read-file-size-threshold)
        (with-temp-buffer
          (insert-file-contents filename)
          (buffer-string)))
    ;; TODO: Handle nil start-line OR nil end-line
    (cl-decf start-line)
    (let* ((file-size (nth 7 (file-attributes filename)))
           (chunk-size (min file-size (* gptel-agent-read-file-size-threshold 1024)))
           (byte-offset 0) (line-offset (- end-line start-line)))
      (with-temp-buffer
        ;; Go to start-line
        (while (and (> start-line 0)
                    (< byte-offset file-size))
          (insert-file-contents
           filename nil byte-offset (+ byte-offset chunk-size))
          (setq byte-offset (+ byte-offset chunk-size))
          (setq start-line (forward-line start-line))
          (when (eobp)
            (if (/= (line-beginning-position) (line-end-position))
                ;; forward-line counted 1 extra line
                (cl-incf start-line))
            (delete-region (point-min) (line-beginning-position))))

        (delete-region (point-min) (point))

        ;; Go to end-line, forward by line-offset
        (cl-block nil
          (while (> line-offset 0)
            (setq line-offset (forward-line line-offset))
            (when (and (eobp) (/= (line-beginning-position) (line-end-position)))
              ;; forward-line counted 1 extra line
              (cl-incf line-offset))
            (if (= line-offset 0)
                (delete-region (point) (point-max))
              (if (>= byte-offset file-size)
                  (cl-return)
                (insert-file-contents
                 filename nil byte-offset (+ byte-offset chunk-size))
                (setq byte-offset (+ byte-offset chunk-size))))))

        (buffer-string)))))

(defun gptel-agent--grep (regex path &optional glob context-lines)
  "Search for REGEX in file or directory at PATH using ripgrep.

REGEX is a PCRE-format regular expression to search for.
PATH can be a file or directory to search in.

Optional arguments:
GLOB restricts the search to files matching the glob pattern.
  Examples: \"*.el\", \"*.md\", \"*.rs\"
CONTEXT-LINES specifies the number of lines of context to show
  around each match (0-15 inclusive, defaults to 0).

Returns a string containing matches grouped by file, with line numbers
and optional context.  Results are limited to 1000 or fewer matches per
file.  Results are sorted by modification time."
  (unless (file-readable-p path)
    (error "Error: File or directory %s is not readable" path))
  (let* ((full-path (expand-file-name (substitute-in-file-name path)))
         ;; Explicitly set remote to save ourselves multiple file-remote-p
         ;; checks inside `executable-find'
         (remote (file-remote-p default-directory))
         (git-root (and (executable-find "git" remote)
                        (locate-dominating-file full-path ".git")))
         (grepper (cond
                   (git-root "git")
                   ((executable-find "rg" remote) "rg")
                   ((executable-find "grep" remote) "grep")
                   (t (error "Error: ripgrep/grep/git-grep not available, \
this tool cannot be used")))))
    (with-temp-buffer
      (let* ((default-directory (or git-root default-directory))
             (args
              (cond
               ((string= "git" grepper)
                (let* ((rel-path (file-relative-name full-path git-root))
                       (pathspecs
                        (list (if (and glob (file-directory-p full-path))
                                  (file-name-concat rel-path glob)
                                rel-path))))
                  (delq nil
                        (nconc
                         (list "grep"
                               "--line-number"
                               "--no-color"
                               (and (natnump context-lines)
                                    (format "-C%d" context-lines))
                               "--max-count=1000"
                               "--untracked"
                               "-P" regex
                               "--")
                         pathspecs))))
               ((string= "rg" grepper)
                (delq nil (list "--sort=modified"
                                (and (natnump context-lines)
                                     (format "--context=%d" context-lines))
                                (and glob (format "--glob=%s" glob))
                                ;; "--files-with-matches"
                                "--max-count=1000"
                                "--heading" "--line-number" "-e" regex
                                (file-local-name full-path))))
               ((string= "grep" grepper)
                (delq nil (list "--recursive"
                                (and (natnump context-lines)
                                     (format "--context=%d" context-lines))
                                (and glob (format "--include=%s" glob))
                                "--max-count=1000"
                                "--line-number" "--regexp" regex
                                (file-local-name full-path))))))
             (exit-code (apply #'process-file grepper nil '(t t) nil args)))
        (when (/= exit-code 0)
          (goto-char (point-min))
          (insert (format "Error: search failed with exit-code %d.  Tool output:\n\n" exit-code)))
        (gptel-agent--truncate-buffer "grep")
        (buffer-string)))))

;;; Todo-write tool (task tracking)
(defvar-local gptel-agent--todos nil)

(defun gptel-agent-toggle-todos ()
  "Toggle the display of the gptel agent todo list."
  (interactive)
  (pcase-let ((`(,prop-value . ,ov)
               (or (get-char-property-and-overlay (point) 'gptel-agent--todos)
                   (get-char-property-and-overlay
                    (previous-single-char-property-change
                     (point) 'gptel-agent--todos nil (point-min))
                    'gptel-agent--todos))))
    (if-let* ((fmt (overlay-get ov 'after-string)))
        (progn (overlay-put ov 'gptel-agent--todos fmt)
               (overlay-put ov 'after-string nil))
      (overlay-put ov 'after-string
                   (and (stringp prop-value) prop-value))
      (overlay-put ov 'gptel-agent--todos t))))

(defun gptel-agent--write-todo (todos)
  "Display a formatted task list in the buffer.

TODOS is a list of plists with keys :content, :activeForm, and :status.
Completed items are displayed with strikethrough and shadow face.
Exactly one item should have status \"in_progress\"."
  (setq gptel-agent--todos todos)
  ;; Update overlay
  (let* ((info (gptel-fsm-info gptel--fsm-last))
         (where-from
          (previous-single-property-change
           (plist-get info :position) 'gptel nil (point-min)))
         (where-to (plist-get info :position)))
    (unless (= where-from where-to)
      (pcase-let ((`(,_ . ,todo-ov)
                   (get-char-property-and-overlay where-from 'gptel-agent--todos)))
        (if todo-ov
            ;; Move if reusing an old overlay and the text has changed.
            (move-overlay todo-ov where-from where-to)
          (setq todo-ov (make-overlay where-from where-to nil t))
          (overlay-put todo-ov 'gptel-agent--todos t)
          (overlay-put todo-ov 'evaporate t)
          (overlay-put todo-ov 'priority -40)
          (overlay-put todo-ov 'keymap (define-keymap
                                         "<tab>" #'gptel-agent-toggle-todos
                                         "TAB"   #'gptel-agent-toggle-todos))
          (plist-put
           info :post              ; Don't use push, see note in gptel-anthropic
           (cons (lambda (&rest _)      ; Clean up header line after tasks are done
                   (when (and gptel-mode gptel-use-header-line header-line-format)
                     (setf (nth 2 header-line-format) gptel--header-line-info)))
                 (plist-get info :post))))
        (let* ((formatted-todos         ; Format the todo list
                (mapconcat
                 (lambda (todo)
                   (pcase (plist-get todo :status)
                     ("completed"
                      (concat "✓ " (propertize (plist-get todo :content)
                                               'face '(:inherit shadow :strike-through t))))
                     ("in_progress"
                      (concat "● " (propertize (plist-get todo :activeForm)
                                               'face '(:inherit bold :inherit warning))))
                     (_ (concat "○ " (plist-get todo :content)))))
                 todos "\n"))
               (in-progress
                (cl-loop for todo across todos
                         when (equal (plist-get todo :status) "in_progress")
                         return (plist-get todo :activeForm)))
               (todo-display
                (concat
                 (unless (= (char-before (overlay-end todo-ov)) 10) "\n")
                 gptel-agent--hrule
                 (propertize "Task list: [ "
                             'face '(:inherit font-lock-comment-face :inherit bold))
                 (save-excursion
                   (goto-char (1- (overlay-end todo-ov)))
                   (propertize (substitute-command-keys "\\[gptel-agent-toggle-todos]")
                               'face 'help-key-binding))
                 (propertize " to toggle display ]\n" 'face 'font-lock-comment-face)
                 formatted-todos "\n"
                 gptel-agent--hrule)))
          (overlay-put todo-ov 'after-string todo-display)
          (when (and gptel-mode gptel-use-header-line in-progress header-line-format)
            (setf (nth 2 header-line-format)
                  (concat (propertize
                           " " 'display
                           `(space :align-to (- right ,(+ 5 (length in-progress)))))
                          (propertize (concat "Task: " in-progress)
                                      'face 'font-lock-escape-face))))))))
  t)

;;; Skill tool
(defun gptel-agent--get-skill (skill &optional _args)
  "Return the details of the SKILL.

This loads the body of the corresponding SKILL.  When using this as a
tool in gptel, make sure the known skills are added to the context
window.  `gptel-agent--skills-system-message' can be used to generate
the known skills as string ready to be included to the context."
  (let ((skill-dir
         (car-safe
          (alist-get skill gptel-agent--skills nil nil #'string-equal))))
    (if (not skill-dir)
        (format "Error: skill %s not found." skill)
      (let* ((skill-dir-expanded (expand-file-name skill-dir))
             (skill-files
              (mapcar
               (lambda (full-path)
                 (cons (file-relative-name full-path skill-dir-expanded)
                       full-path))
               (directory-files-recursively skill-dir-expanded ".*")))
             (body (plist-get
                    (cdr (gptel-agent--cached-read-file
                          (expand-file-name "SKILL.md" skill-dir) t))
                    :system)))
        (if body
            (let (start)
              (with-temp-buffer
                (insert "## Skill: " skill
                        "\n- base dir: " skill-dir-expanded "\n")
                (setq start (point))
                (insert body)
                (pcase-dolist (`(,rel-path . ,full-path) skill-files)
                  (unless (string-match-p "SKILL\\.md" rel-path)
                    (goto-char start)
                    (while (search-forward-regexp (regexp-quote rel-path) nil t)
                      (replace-match full-path t t))))
                (buffer-string)))
          (format "Could not load body of skill %s" skill))))))

;;; Task tool (sub-agent)

(defun gptel-agent--task-cleanup-overlay (ov)
  "Safely delete task overlay OV if it is still live."
  (when (and ov (overlay-buffer ov))
    (delete-overlay ov)))

(defun gptel-agent--task-finish-response (partial info)
  "Return the completed task response from PARTIAL and INFO."
  (if-let* ((transformer (plist-get info :transformer)))
      (funcall transformer partial)
    partial))

(defconst gptel-agent--request-config-variables
  '(gptel-backend gptel-model gptel-stream gptel-system-prompt
    gptel-tools gptel-use-tools gptel-use-context gptel-context
    gptel--request-params gptel-temperature gptel-max-tokens
    gptel-include-reasoning gptel-cache gptel-use-curl
    gptel-confirm-tool-calls gptel-prompt-transform-functions
    gptel-pre-tool-call-functions gptel-post-tool-call-functions
    gptel-org-convert-response gptel-track-media
    gptel--num-messages-to-send)
  "gptel settings copied into and owned by a sub-agent request buffer.")

(defconst gptel-agent--terminal-run-states
  '(completed failed timed-out cancelled)
  "Terminal states of a `gptel-agent--run'.")

(defun gptel-agent--run-active-p (run)
  "Return non-nil when RUN has not reached a terminal state."
  (not (memq (gptel-agent--run-state run)
             gptel-agent--terminal-run-states)))

(defun gptel-agent--run-from-fsm (fsm)
  "Return the sub-agent run owned by FSM."
  (plist-get (gptel-fsm-info fsm) :context))

(defun gptel-agent--display-run-buffer (run &rest _)
  "Display RUN's live diagnostic buffer."
  (if (buffer-live-p (gptel-agent--run-child-buffer run))
      (pop-to-buffer (gptel-agent--run-child-buffer run))
    (user-error "This sub-agent diagnostic buffer is no longer available")))

(defun gptel-agent--run-inspect-button (run)
  "Return a clickable string that displays RUN's diagnostic buffer."
  (propertize
   (buttonize "[Inspect sub-agent]"
              (lambda (&rest _)
                (gptel-agent--display-run-buffer run)))
   'help-echo "Open the live sub-agent request and tool transcript"))

(defun gptel-agent--run-log (run format-string &rest args)
  "Append a timestamped event to RUN using FORMAT-STRING and ARGS."
  (when (buffer-live-p (gptel-agent--run-child-buffer run))
    (with-current-buffer (gptel-agent--run-child-buffer run)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (or (bobp) (bolp)) (insert "\n"))
        (insert (format "[%s] " (format-time-string "%H:%M:%S"))
                (apply #'format format-string args))
        (unless (bolp) (insert "\n"))))))

(defun gptel-agent--run-append-model-output (run text)
  "Append streamed model TEXT to RUN's diagnostic buffer."
  (when (and (stringp text)
             (buffer-live-p (gptel-agent--run-child-buffer run)))
    (with-current-buffer (gptel-agent--run-child-buffer run)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)))))

(defun gptel-agent--run-update-terminal-overlay (run result)
  "Show RUN's terminal RESULT and inspection action in its overlay."
  (when-let* ((overlay (gptel-agent--run-overlay run))
              ((overlay-buffer overlay)))
    (overlay-put
     overlay 'after-string
     (concat
      (overlay-get overlay 'msg)
      (propertize
       (format "%s after %d rounds and %d tool calls.\n"
               (capitalize (symbol-name (gptel-agent--run-state run)))
               (or (gptel-agent--run-rounds run) 0)
               (or (gptel-agent--run-tool-calls run) 0))
       'face (if (eq (gptel-agent--run-state run) 'completed)
                 'success 'error))
      (unless (eq (gptel-agent--run-state run) 'completed)
        (format "%s\n"
                (truncate-string-to-width
                 (gptel--to-string result) 500 nil nil t)))
      gptel-agent--hrule))))

(defun gptel-agent--run-cancel-timer (run accessor setter)
  "Cancel RUN timer returned by ACCESSOR and clear it with SETTER."
  (when-let* ((timer (funcall accessor run)))
    (cancel-timer timer)
    (funcall setter nil run)))

(defun gptel-agent--run-abort-transport (run)
  "Abort RUN's exact child request, if it still has an active transport."
  (when-let* ((fsm (gptel-agent--run-fsm run))
              (info (gptel-fsm-info fsm)))
    ;; Avoid a second semantic completion through gptel's abort callback.
    (plist-put info :callback #'ignore))
  (when (buffer-live-p (gptel-agent--run-child-buffer run))
    (ignore-errors (gptel-abort (gptel-agent--run-child-buffer run)))))

(defun gptel-agent--record-terminal-run (run result)
  "Retain a bounded diagnostic summary of terminal RUN and RESULT."
  (let* ((configuration (gptel-agent--run-configuration run))
         (backend (plist-get configuration :backend))
         (summary
          (list :id (gptel-agent--run-id run)
                :state (gptel-agent--run-state run)
                :agent (gptel-agent--run-agent run)
                :description (gptel-agent--run-description run)
                :terminal-reason (gptel-agent--run-terminal-reason run)
                :rounds (gptel-agent--run-rounds run)
                :tool-calls (gptel-agent--run-tool-calls run)
                :web-tool-calls (gptel-agent--run-web-tool-calls run)
                :retries (gptel-agent--run-retries run)
                :backend (and backend (gptel-backend-name backend))
                :model (plist-get configuration :model)
                :stream (plist-get configuration :stream)
                :started-at (gptel-agent--run-started-at run)
                :ended-at (float-time)
                :result (and (not (eq (gptel-agent--run-state run) 'completed))
                             (truncate-string-to-width
                              (gptel--to-string result) 1000 nil nil t)))))
    (if (zerop gptel-agent-run-history-size)
        (setq gptel-agent--run-history nil)
      (push summary gptel-agent--run-history)
      (when (> (length gptel-agent--run-history)
               gptel-agent-run-history-size)
        (setcdr (nthcdr (1- gptel-agent-run-history-size)
                        gptel-agent--run-history)
                nil)))))

(defun gptel-agent--run-finish (run state result &optional reason suppress-delivery)
  "Finish RUN exactly once in terminal STATE with RESULT.

REASON is retained for diagnostics.  When SUPPRESS-DELIVERY is non-nil, do
not send RESULT to the parent tool callback."
  (when (gptel-agent--run-active-p run)
    (setf (gptel-agent--run-state run) state
          (gptel-agent--run-terminal-reason run) reason)
    (gptel-agent--run-log
     run "TERMINAL %s: %s" state (gptel--to-string result))
    (gptel-agent--run-cancel-timer
     run #'gptel-agent--run-timeout-timer
     (lambda (value object)
       (setf (gptel-agent--run-timeout-timer object) value)))
    (gptel-agent--run-cancel-timer
     run #'gptel-agent--run-retry-timer
     (lambda (value object)
       (setf (gptel-agent--run-retry-timer object) value)))
    (when (memq state '(timed-out cancelled))
      (gptel-agent--run-abort-transport run))
    (gptel-agent--record-terminal-run run result)
    (gptel-agent--run-update-terminal-overlay run result)
    (when (buffer-live-p (gptel-agent--run-parent-buffer run))
      (with-current-buffer (gptel-agent--run-parent-buffer run)
        (when-let* ((hook (gptel-agent--run-parent-kill-hook run)))
          (remove-hook 'kill-buffer-hook hook t))))
    (remhash (gptel-agent--run-id run) gptel-agent--runs)
    (when (and (not suppress-delivery)
               (buffer-live-p (gptel-agent--run-parent-buffer run)))
      ;; Async tool callbacks can arrive from a Curl/process buffer.  Resume
      ;; the parent FSM in its own buffer so its buffer-local gptel settings
      ;; cannot be taken from an unrelated concurrent request.
      (with-current-buffer (gptel-agent--run-parent-buffer run)
        (condition-case err
            (funcall (gptel-agent--run-callback run) result)
          (error
           (message "gptel-agent: parent callback for run %s failed: %S"
                    (gptel-agent--run-id run) err)))))
    (if (and gptel-agent-keep-failed-run-buffers
             (not (eq state 'completed))
             (buffer-live-p (gptel-agent--run-child-buffer run)))
        (with-current-buffer (gptel-agent--run-child-buffer run)
          (rename-buffer
           (format "*gptel-agent-run:%s:%s*"
                   (gptel-agent--run-id run) state)
           t))
      (gptel-agent--task-cleanup-overlay (gptel-agent--run-overlay run))
      (when (buffer-live-p (gptel-agent--run-child-buffer run))
        (kill-buffer (gptel-agent--run-child-buffer run))))
    t))

(defun gptel-agent--cancel-runs-for-buffer (parent-buffer)
  "Cancel live sub-agent runs owned by PARENT-BUFFER.

Return the number cancelled.  Each distinct parent FSM is transitioned to
ABRT after its children have stopped."
  (let (runs parent-fsms)
    ;; `gptel-agent--run-finish' removes entries, so collect before iterating.
    (maphash
     (lambda (_id run)
       (when (eq (gptel-agent--run-parent-buffer run) parent-buffer)
         (push run runs)))
     gptel-agent--runs)
    (dolist (run runs)
      (cl-pushnew (gptel-agent--run-parent-fsm run) parent-fsms :test #'eq)
      (gptel-agent--run-finish
       run 'cancelled "Error: Parent request was aborted." 'parent-abort t))
    (dolist (fsm parent-fsms)
      (when (and (gptel-fsm-p fsm)
                 (not (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT))))
        (gptel--fsm-transition fsm 'ABRT)))
    (length runs)))

(defun gptel-agent--abort-with-owned-runs (original buffer)
  "Around advice for `gptel-abort' that also cancels BUFFER's children."
  (gptel-agent--cancel-runs-for-buffer buffer)
  (funcall original buffer))

(defun gptel-agent--error-text (info)
  "Return a normalized diagnostic string for failed request INFO."
  (downcase
   (mapconcat #'gptel--to-string
              (delq nil (list (plist-get info :status)
                              (plist-get info :http-status)
                              (plist-get info :error)))
              " ")))

(defun gptel-agent--transient-request-error-p (info)
  "Return non-nil when failed request INFO is safe to retry."
  (let* ((text (gptel-agent--error-text info))
         (http-raw (plist-get info :http-status))
         (http (cond ((numberp http-raw) http-raw)
                     ((stringp http-raw) (string-to-number http-raw))
                     (t 0))))
    (and
     ;; Deterministic client/configuration failures must remain terminal.
     (not (string-match-p
           (concat "auth\\|unauthori[sz]ed\\|forbidden\\|invalid api\\|"
                   "malformed\\|unsupported\\|unknown parameter\\|"
                   "invalid_request\\|bad request\\|stream must be")
           text))
     (or (memq http '(408 409 425 429))
         (>= http 500)
         (string-match-p
          (concat "curl failure\\|curl failed\\|timed? out\\|timeout\\|"
                  "temporar\\|connection reset\\|connection refused\\|"
                  "connection closed\\|network\\|dns\\|name resolution\\|"
                  "empty reply\\|recv failure\\|send failure")
          text)))))

(defun gptel-agent--retry-request (run fsm)
  "Retry RUN's current request payload through FSM after backoff."
  (let* ((retry (1+ (gptel-agent--run-retries run)))
         (delay (* gptel-agent-retry-delay (expt 2 (1- retry)))))
    ;; Streaming failures may have delivered a partial response.  Replaying
    ;; the same payload must replace those chunks, not duplicate them.
    (setf (gptel-agent--run-response run)
          (substring (gptel-agent--run-response run) 0
                     (gptel-agent--run-response-checkpoint run)))
    (setf (gptel-agent--run-retries run) retry
          (gptel-agent--run-state run) 'retry-wait
          (gptel-agent--run-retry-timer run)
          (run-at-time
           delay nil
           (lambda (owned-run owned-fsm)
             (setf (gptel-agent--run-retry-timer owned-run) nil)
             (when (and (gptel-agent--run-active-p owned-run)
                        (buffer-live-p
                         (gptel-agent--run-child-buffer owned-run)))
               (let ((info (gptel-fsm-info owned-fsm)))
                 (dolist (key '(:error :status :http-status))
                   (plist-put info key nil)))
               (with-current-buffer
                   (gptel-agent--run-child-buffer owned-run)
                 (gptel--fsm-transition owned-fsm 'WAIT))))
           run fsm))))

(defun gptel-agent--handle-task-error (fsm)
  "Retry or terminate a sub-agent FSM in ERRS state."
  (let* ((info (gptel-fsm-info fsm))
         (run (gptel-agent--run-from-fsm fsm)))
    (when (and run (gptel-agent--run-active-p run))
      (gptel-agent--run-log
       run "REQUEST ERROR status=%s error=%S"
       (or (plist-get info :status) "unknown") (plist-get info :error))
      (if (and (< (gptel-agent--run-retries run)
                  gptel-agent-max-request-retries)
               (gptel-agent--transient-request-error-p info))
          (progn
            (gptel-agent--run-log
             run "RETRY %d/%d scheduled"
             (1+ (gptel-agent--run-retries run))
             gptel-agent-max-request-retries)
            (gptel-agent--retry-request run fsm))
        (gptel-agent--run-finish
         run 'failed
         (format "Error: Sub-agent request failed. Status: %s, Error: %S"
                 (or (plist-get info :status) "unknown")
                 (plist-get info :error))
         (or (plist-get info :error) (plist-get info :status)))))))

(defun gptel-agent--handle-task-done (fsm)
  "Complete a sub-agent FSM only when it enters DONE."
  (let* ((info (gptel-fsm-info fsm))
         (run (gptel-agent--run-from-fsm fsm))
         (response (and run (gptel-agent--run-response run))))
    (when (and run (gptel-agent--run-active-p run))
      (if (string-blank-p (or response ""))
          (gptel-agent--run-finish
           run 'failed
           (format "Error: Sub-agent completed but produced no usable output. \
Status: %s" (or (plist-get info :status) "unknown"))
           'empty-response)
        (gptel-agent--run-finish
         run 'completed
         (gptel-agent--task-finish-response
          (format "%s result for task: %s\n\n%s"
                  (capitalize (gptel-agent--run-agent run))
                  (gptel-agent--run-description run)
                  response)
          info)
         'done)))))

(defun gptel-agent--handle-task-abort (fsm)
  "Terminate a sub-agent FSM that enters ABRT."
  (when-let* ((run (gptel-agent--run-from-fsm fsm))
              ((gptel-agent--run-active-p run)))
    (gptel-agent--run-finish
     run 'cancelled "Error: Sub-agent request was aborted." 'aborted)))

(defun gptel-agent--handle-task-wait (fsm)
  "Send the next request for FSM while enforcing RUN's round budget."
  (let* ((run (gptel-agent--run-from-fsm fsm))
         (retryp (and run (eq (gptel-agent--run-state run) 'retry-wait))))
    (when (and run (gptel-agent--run-active-p run))
      (if (and (not retryp)
               gptel-agent-max-request-rounds
               (>= (gptel-agent--run-rounds run)
                   gptel-agent-max-request-rounds))
          (gptel-agent--run-finish
           run 'failed
           (format "Error: Sub-agent exceeded the limit of %d model/tool rounds."
                   gptel-agent-max-request-rounds)
           'round-limit)
        (unless retryp
          (cl-incf (gptel-agent--run-rounds run))
          (setf (gptel-agent--run-response-checkpoint run)
                (length (gptel-agent--run-response run))))
        (gptel-agent--run-log
         run "%s round %d/%s"
         (if retryp "RETRYING REQUEST" "REQUEST")
         (gptel-agent--run-rounds run)
         (or gptel-agent-max-request-rounds "unlimited"))
        (unless (eq (gptel-agent--run-state run) 'requesting-after-tool)
          (setf (gptel-agent--run-state run) 'requesting))
        ;; `gptel--handle-wait' consults buffer-local transport settings.
        (with-current-buffer (gptel-agent--run-child-buffer run)
          (gptel--handle-wait fsm))))))

(defun gptel-agent--handle-task-tool (fsm)
  "Run tools for FSM and record that its supervisor is waiting on them."
  (when-let* ((run (gptel-agent--run-from-fsm fsm))
              ((gptel-agent--run-active-p run)))
    (setf (gptel-agent--run-state run) 'waiting-for-tool)
    (dolist (call (plist-get (gptel-fsm-info fsm) :tool-use))
      (gptel-agent--run-log
       run "TOOL CALL %s %S"
       (plist-get call :name) (plist-get call :args)))
    (gptel-agent--indicate-tool-call fsm)
    (gptel--handle-tool-use fsm)))

(defun gptel-agent--handle-task-tool-result (fsm)
  "Process tool results for FSM unless its owner is already terminal."
  (when-let* ((run (gptel-agent--run-from-fsm fsm))
              ((gptel-agent--run-active-p run)))
    (setf (gptel-agent--run-state run) 'requesting-after-tool)
    (pcase-dolist (`(,tool ,args ,result)
                   (plist-get (gptel-fsm-info fsm) :tool-result))
      (gptel-agent--run-log
       run "TOOL RESULT %s %S\n%s"
       (if tool (gptel-tool-name tool) "unknown") args
       (truncate-string-to-width
        (gptel--to-string result) 4000 nil nil t)))
    (gptel--handle-post-tool fsm)
    (gptel--handle-tool-result fsm)))

(defvar gptel-agent-request--handlers
  `((WAIT ,#'gptel-agent--indicate-wait
          ,#'gptel-agent--handle-task-wait)
    (TPRE ,#'gptel--handle-pre-tool ,#'gptel--fsm-transition)
    (TOOL ,#'gptel-agent--handle-task-tool)
    (TRET ,#'gptel-agent--handle-task-tool-result)
    (ERRS ,#'gptel-agent--handle-task-error)
    (DONE ,#'gptel-agent--handle-task-done)
    (ABRT ,#'gptel-agent--handle-task-abort))
  "FSM handlers for sub-agent tasks.  See `gptel-request--handlers'.")

(defun gptel-agent--task-preview-setup (arg-values _info)
  "Preview setup for Agent.
INFO is the tool call info plist.
ARG-VALUES is a list: (type description prompt)"
  (pcase-let ((from (point))
              (`(,type ,desc ,prompt) arg-values))
    (insert "("
            (propertize "Agent " 'font-lock-face 'font-lock-keyword-face)
            (propertize (prin1-to-string type)
                        'font-lock-face 'font-lock-escape-face)
            " " (propertize (prin1-to-string desc)
                            'font-lock-face
                            '(:inherit font-lock-constant-face :inherit bold))
            "\n" (propertize (prin1-to-string prompt)
                             'line-prefix "  "
                             'wrap-prefix "  "
                             'font-lock-face 'font-lock-constant-face)
            ")\n\n")
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--indicate-wait (fsm)
  "Display waiting indicator for agent task FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (run (plist-get info :context))
              (info-ov (gptel-agent--run-overlay run))
              ((overlayp info-ov))
              ((overlay-buffer info-ov))
              (count (overlay-get info-ov 'count)))
    (run-at-time
     1.5 nil
     (lambda (ov count)
       (when (and (overlay-buffer ov)
                  (eql (overlay-get ov 'count) count))
         (let* ((task-msg (overlay-get ov 'msg))
                (new-info-msg
                 (concat task-msg
                         (concat
                          (propertize "Waiting... " 'face 'warning) "\n"
                          (propertize "\n" 'face
                                      '(:inherit shadow :underline t :extend t))))))
           (overlay-put ov 'after-string new-info-msg))))
     info-ov count)))

(defun gptel-agent--indicate-tool-call (fsm)
  "Display tool call indicator for agent task FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (tool-use (plist-get info :tool-use))
              (run (plist-get info :context))
              (ov (gptel-agent--run-overlay run))
              ((overlayp ov))
              ((overlay-buffer ov)))
    ;; Update overlay with tool calls
    (let* ((task-msg (overlay-get ov 'msg))
           (info-count (overlay-get ov 'count))
           (new-info-msg))
      (setq new-info-msg
            (concat task-msg
                    (concat
                     (propertize "Calling Tools... " 'face 'mode-line-emphasis)
                     (if (= info-count 0) "\n" (format "(+%d)\n" info-count))
                     (mapconcat (lambda (call)
                                  (gptel--format-tool-call
                                   (plist-get call :name)
                                   (map-values (plist-get call :args))))
                                tool-use)
                     "\n" gptel-agent--hrule)))
      (overlay-put ov 'count (+ info-count (length tool-use)))
      (overlay-put ov 'after-string new-info-msg))))

(defun gptel-agent--task-overlay (where &optional agent-type description run)
  "Create an Agent task overlay at WHERE.

AGENT-TYPE and DESCRIPTION identify the task.  RUN supplies the inspection
button for its live diagnostic buffer."
  (let* ((bounds                  ;where to place the overlay, handle edge cases
          (save-excursion
            (goto-char where)
            (when (bobp) (insert "\n"))
            (if (and (bolp) (eolp))
                (cons (1- (point)) (point))
              (cons (line-beginning-position) (line-end-position)))))
         (ov (make-overlay (car bounds) (cdr bounds) nil t))
         (model
          (propertize (concat (gptel--model-name gptel-model))
                      'face 'font-lock-comment-face))
         (msg (concat
               (unless (eq (char-after (car bounds)) 10) "\n")
               "\n" gptel-agent--hrule
               (propertize (concat (capitalize agent-type) " Task: ")
                           'face 'font-lock-escape-face)
               (propertize (or description "(no description)") 'face 'font-lock-doc-face)
               (propertize
                " " 'display
                (if (and (display-graphic-p) (fboundp 'string-pixel-width))
                    `(space :align-to (- right (,(string-pixel-width model))))
                  `(space :align-to (- right ,(+ 5 (string-width model))))))
               model
               (when run (concat " " (gptel-agent--run-inspect-button run)))
               "\n")))
    (prog1 ov
      (overlay-put ov 'gptel-agent t)
      (overlay-put ov 'count 0)
      (overlay-put ov 'msg msg)
      (overlay-put ov 'line-prefix "")
      (overlay-put
       ov 'after-string
       (concat msg (propertize "Waiting..." 'face 'warning) "\n"
               gptel-agent--hrule)))))

(defun gptel-agent--copy-parent-request-config (parent-buffer child-buffer)
  "Copy owned gptel request settings from PARENT-BUFFER to CHILD-BUFFER."
  (dolist (symbol gptel-agent--request-config-variables)
    (when (boundp symbol)
      (let ((value (buffer-local-value symbol parent-buffer)))
        (with-current-buffer child-buffer
          (set (make-local-variable symbol)
               (if (consp value) (copy-tree value) value)))))))

(defun gptel-agent--task (main-cb agent-type description prompt
                                  &optional parent-fsm agent-snapshot)
  "Call a gptel agent to do specific compound tasks.

MAIN-CB is the main callback to return a value to the main loop.
AGENT-TYPE is the name of the agent.
DESCRIPTION is a short description of the task.
PROMPT is the detailed prompt instructing the agent on what is required.
PARENT-FSM and AGENT-SNAPSHOT are supplied by the request-local Agent tool."
  (let* ((parent-info (and (gptel-fsm-p parent-fsm)
                           (gptel-fsm-info parent-fsm)))
         (parent-buffer (and parent-info (plist-get parent-info :buffer)))
         (agent-snapshot (or agent-snapshot gptel-agent--agent-snapshot
                             (gptel-agent--snapshot-agent-definitions)))
         (agent-plist (cdr (assoc-string agent-type agent-snapshot t))))
    (cond
     ((not (buffer-live-p parent-buffer))
      (funcall main-cb "Error: Agent dispatch has no live parent request."))
     ((not agent-plist)
      (with-current-buffer parent-buffer
        (funcall main-cb
                 (format "Error: Agent %S is not available for this request."
                         agent-type))))
     (t
      (let* ((where (or (plist-get parent-info :tracking-marker)
                        (plist-get parent-info :position)))
             (parent-position
              (if (markerp where)
                  (copy-marker where)
                (with-current-buffer parent-buffer
                  (set-marker (make-marker) where))))
             (child-buffer
              (generate-new-buffer
               (format " *gptel-agent-run:%s*" agent-type)))
             (preset (and gptel-agent-preset
                          (copy-sequence
                           (if (symbolp gptel-agent-preset)
                               (gptel-get-preset gptel-agent-preset)
                             gptel-agent-preset))))
             (configuration
              (append (list :include-reasoning nil :use-tools t :context nil)
                      preset (copy-tree agent-plist)))
             (run (gptel-agent--make-run
                   :id (format "agent-%d" (cl-incf gptel-agent--run-counter))
                   :state 'created :agent agent-type :description description
                   :prompt prompt :parent-fsm parent-fsm
                   :parent-buffer parent-buffer :child-buffer child-buffer
                   :callback main-cb :response "" :response-checkpoint 0
                   :rounds 0 :retries 0 :tool-calls 0 :web-tool-calls 0
                   :started-at (float-time))))
        (condition-case err
            (progn
              (gptel-agent--copy-parent-request-config
               parent-buffer child-buffer)
              (with-current-buffer child-buffer
                (setq default-directory
                      (buffer-local-value 'default-directory parent-buffer))
                (setq-local gptel-agent--current-agent agent-type
                            gptel-agent--agent-snapshot agent-snapshot
                            gptel-agent--supervised-run run
                            gptel--preset nil)
                (gptel--apply-preset
                 configuration
                 (lambda (symbol value)
                   (set (make-local-variable symbol) value)))
                (gptel-agent--reset-tool-call-counts)
                (setf (gptel-agent--run-configuration run)
                      (list :backend gptel-backend :model gptel-model
                            :stream gptel-stream :system gptel-system-prompt
                            :tools (copy-sequence gptel-tools)
                            :request-params (copy-tree gptel--request-params)))
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert (format "Sub-agent run: %s\nAgent: %s\nTask: %s\nModel: %s\n\nPrompt:\n%s\n\n"
                                  (gptel-agent--run-id run) agent-type description
                                  (gptel--model-name gptel-model) prompt)))
                (setq buffer-read-only t))
              (with-current-buffer parent-buffer
                (gptel--update-status " Calling Agent..."
                                      'font-lock-escape-face)
                (setf (gptel-agent--run-overlay run)
                      (gptel-agent--task-overlay
                       where agent-type description run)))
              (puthash (gptel-agent--run-id run) run gptel-agent--runs)
              (let ((parent-kill-hook
                     (lambda ()
                       (gptel-agent--run-finish
                        run 'cancelled
                        "Error: Parent buffer was killed during sub-agent task."
                        'parent-killed t))))
                (setf (gptel-agent--run-parent-kill-hook run)
                      parent-kill-hook)
                (with-current-buffer parent-buffer
                  (add-hook 'kill-buffer-hook parent-kill-hook nil t)))
              (with-current-buffer child-buffer
                (add-hook
                 'kill-buffer-hook
                 (lambda ()
                   (when (gptel-agent--run-active-p run)
                     (gptel-agent--run-finish
                      run 'failed
                      "Error: Sub-agent request buffer was killed."
                      'child-buffer-killed))
                   (unless (gptel-agent--run-active-p run)
                     (gptel-agent--task-cleanup-overlay
                      (gptel-agent--run-overlay run))))
                 nil t))
              (when gptel-agent-task-timeout
                (setf (gptel-agent--run-timeout-timer run)
                      (run-at-time
                       gptel-agent-task-timeout nil
                       (lambda (owned-run)
                         (when (gptel-agent--run-active-p owned-run)
                           (gptel-agent--run-finish
                            owned-run 'timed-out
                            (format "Error: Sub-agent task %S timed out after %d seconds."
                                    (gptel-agent--run-description owned-run)
                                    gptel-agent-task-timeout)
                            'timeout)))
                       run)))
              (with-current-buffer child-buffer
                (let* ((transforms
                        (cl-remove 'gptel--transform-apply-preset
                                   gptel-prompt-transform-functions))
                       (transforms
                        (if (memq #'gptel-agent--localize-agent-tool transforms)
                            transforms
                          (append transforms
                                  (list #'gptel-agent--localize-agent-tool))))
                       (task-callback
                        (lambda (resp info)
                          (condition-case callback-error
                              (pcase resp
                                ((pred stringp)
                                 (setf (gptel-agent--run-response run)
                                       (concat
                                        (gptel-agent--run-response run) resp))
                                 (gptel-agent--run-append-model-output run resp))
                                (`(tool-call . ,calls)
                                 (unless (plist-get info :tracking-marker)
                                   (plist-put info :tracking-marker where))
                                 (gptel--display-tool-calls calls info))
                                ;; ERRS, DONE and ABRT handlers own terminal
                                ;; completion.  Other callback events are data.
                                (_ nil))
                            (error
                             (gptel-agent--run-finish
                              run 'failed
                              (format "Error: Internal callback error in task %S: %S"
                                      description callback-error)
                              callback-error)))))
                       (fsm
                        (gptel-request prompt
                          :buffer child-buffer
                          ;; Keep confirmation UI in the visible parent while
                          ;; all request configuration and tool execution stay
                          ;; isolated in CHILD-BUFFER.
                          :position parent-position
                          :stream gptel-stream
                          :system gptel-system-prompt
                          :context run
                          :fsm (gptel-make-fsm
                                :table gptel-send--transitions
                                :handlers gptel-agent-request--handlers)
                          :transforms transforms
                          :callback task-callback)))
                  (setf (gptel-agent--run-fsm run) fsm))))
          (error
           (gptel-agent--run-finish
            run 'failed
            (format "Error: Could not start sub-agent task %S: %S"
                    description err)
            err))))))))

;;; Register tool call preview functions

(pcase-dolist (`(,tool-name . ,setup-fn)
               `(("Write"     ,#'gptel-agent--write-file-preview-setup)
                 ("Eval"     ,#'gptel-agent--eval-elisp-preview-setup)
                 ("Bash"   ,#'gptel-agent--execute-bash-preview-setup)
                 ("Edit"     ,#'gptel-agent--edit-files-preview-setup)
                 ("Insert" ,#'gptel-agent--insert-in-file-preview-setup)
                 ("Agent"     ,#'gptel-agent--task-preview-setup)))
  (setf (alist-get tool-name gptel--tool-preview-alist
                   nil nil #'equal)
        setup-fn))

;;; Ask tools (user interaction)

(defun gptel-agent--ask-overlay-at-point ()
  "Return the ask overlay at point, if any."
  (seq-find (lambda (ov) (overlay-get ov 'gptel-ask))
            (overlays-at (point))))

(defun gptel-agent--ask-make-keymap (choices)
  "Generate keymap for CHOICES interaction with number keys."
  (let ((map (make-sparse-keymap))
        (count (min 9 (length choices))))
    (dotimes (i count)
      (let ((idx i))
        (define-key map (kbd (format "%d" (1+ i)))
          (lambda () (interactive) (gptel-agent--ask-select-choice idx)))
        (define-key map (kbd (format "<kp-%d>" (1+ i)))
          (lambda () (interactive) (gptel-agent--ask-select-choice idx)))))
    (define-key map (kbd "RET") 'gptel-agent--ask-confirm-choice)
    (define-key map (kbd "<return>") 'gptel-agent--ask-confirm-choice)
    (define-key map (kbd "TAB") 'gptel-agent--ask-cycle-choice)
    (define-key map (kbd "<tab>") 'gptel-agent--ask-cycle-choice)
    (define-key map (kbd "n") 'gptel-agent--ask-next-choice)
    (define-key map (kbd "p") 'gptel-agent--ask-prev-choice)
    (define-key map (kbd "C-c C-k") 'gptel-agent--ask-cancel)
    map))

(defun gptel-agent--ask-draw-ui (question choices selection)
  "Return UI string for QUESTION and CHOICES with SELECTION highlighted."
  (let* ((width (min (window-body-width) 80))
         (wrap-width (max 10 (- width 4)))
         (header (propertize (format " 🤖 AGENT ASKS: %s"
                                     (string-fill question wrap-width))
                             'font-lock-face 'font-lock-keyword-face))
         (choice-strs
          (cl-loop for choice in choices
                   for idx from 0
                   collect
                   (let* ((selected (= idx selection))
                          (val (or (plist-get choice :value) "Unknown"))
                          (desc (plist-get choice :description))
                          (reco (plist-get choice :recommended))
                          (mark (if selected " ● " " ○ "))
                          (face (if selected '(:inherit highlight :weight bold) 'default)))
                     (concat
                      (propertize mark 'font-lock-face face)
                                  (propertize (format "[%d] %s%s" (1+ idx) val
                                                      (if (and reco (not (eq reco :json-false)))
                                                          " [RECOMMENDED]" "")) 'font-lock-face face)
                      (when (and desc (not (equal desc "")))
                        (concat "\n    "
                                (propertize (string-fill desc wrap-width)
                                            'font-lock-face 'font-lock-comment-face)))))))
         (footer (propertize "\n [RET] Confirm [n/p] Down/Up [1-9] Select  [C-c C-k] Cancel"
                             'font-lock-face '(:inherit shadow :height 0.8)))
         gptel-agent--hrule
         (content (concat "\n" header "\n\n" (mapconcat #'identity choice-strs "\n") footer "\n")))
    content))

(defun gptel-agent--ask-update-overlay (ov)
  "Redraw overlay OV based on its current properties."
  (let* ((question (overlay-get ov 'gptel-ask--question))
         (choices (overlay-get ov 'gptel-ask--choices))
         (selection (overlay-get ov 'gptel-ask--selection))
         (new-text (gptel-agent--ask-draw-ui question choices selection))
         (inhibit-read-only t)
         (beg (overlay-start ov))
         (end (overlay-end ov)))
    (save-excursion
      (goto-char beg)
      (delete-region beg end)
      (insert new-text)
      (move-overlay ov beg (point)))))

(defun gptel-agent--ask-select-choice (n)
  "Select choice N and update display."
  (interactive)
  (when-let ((ov (gptel-agent--ask-overlay-at-point))
             (choices (overlay-get ov 'gptel-ask--choices)))
    (when (< n (length choices))
      (overlay-put ov 'gptel-ask--selection n)
      (gptel-agent--ask-update-overlay ov)
      (let ((val (or (plist-get (nth n choices) :value) "Option")))
        (message "Selected [%d]: %s" (1+ n) val)))))

(defun gptel-agent--ask-cycle-choice (&optional prev)
  "Cycle to next or PREV choice."
  (interactive)
  (when-let* ((ov (gptel-agent--ask-overlay-at-point))
              (len (length (overlay-get ov 'gptel-ask--choices)))
              (curr (overlay-get ov 'gptel-ask--selection)))
    (let ((next (mod (+ curr (if prev -1 1)) len)))
      (overlay-put ov 'gptel-ask--selection next)
      (gptel-agent--ask-update-overlay ov))))

(defun gptel-agent--ask-next-choice () (interactive) (gptel-agent--ask-cycle-choice))
(defun gptel-agent--ask-prev-choice () (interactive) (gptel-agent--ask-cycle-choice t))

(defun gptel-agent--ask-teardown (ov)
  "Remove ask UI overlay OV completely."
  (when (overlayp ov)
    (let ((inhibit-read-only t)
          (beg (overlay-start ov))
          (end (overlay-end ov)))
      (when (and beg end)
        (delete-region beg end)))
    (delete-overlay ov)))

(defun gptel-agent--ask-confirm-choice ()
  "Confirm selection and call callback."
  (interactive)
  (when-let* ((ov (gptel-agent--ask-overlay-at-point))
              (callback (overlay-get ov 'gptel-ask--callback))
              (choices (overlay-get ov 'gptel-ask--choices))
              (sel-idx (overlay-get ov 'gptel-ask--selection))
              (choice (nth sel-idx choices))
              (val (plist-get choice :value)))
    ;; Treat the appended custom option (always the last in CHOICES)
    ;; as the free-text sentinel, instead of relying on VAL being \"Custom\".
    (if (= sel-idx (1- (length choices)))
        (let (custom-response)
          (unwind-protect
              (setq custom-response (read-string "Enter your custom response: "))
            (gptel-agent--ask-teardown ov))
          (funcall callback custom-response))
      (gptel-agent--ask-teardown ov)
      (funcall callback val))))

(defun gptel-agent--ask-cancel ()
  "Cancel ask interaction."
  (interactive)
  (when-let ((ov (gptel-agent--ask-overlay-at-point)))
    (when-let ((cb (overlay-get ov 'gptel-ask--callback)))
      (funcall cb "User cancelled interaction."))
    (gptel-agent--ask-teardown ov)))

(defun gptel-agent--ask-question (callback question choices)
  "Ask user QUESTION with CHOICES, calling CALLBACK with result.

Always appends a custom option allowing the user to provide their own response."
  (let* ((choices-list (append choices nil))
         ;; Always add a custom option at the end
         (choices-with-custom
          (append choices-list
                  (list (list :value "Custom"
                              :description "Provide your own custom response"
                              :recommended :json-false))))
         (ui-text (gptel-agent--ask-draw-ui question choices-with-custom 0))
         (inhibit-read-only t))
    (goto-char (point-max))
    (let ((start-pos (point)))
      (unless (bolp) (insert "\n"))
      (insert ui-text)
      (insert "\n")
      (let ((ov (make-overlay start-pos (point))))
        (overlay-put ov 'gptel-ask t)
        (overlay-put ov 'gptel-ask--question question)
        (overlay-put ov 'gptel-ask--choices choices-with-custom)
        (overlay-put ov 'gptel-ask--selection 0)
        (overlay-put ov 'gptel-ask--callback callback)
        (overlay-put ov 'keymap (gptel-agent--ask-make-keymap choices-with-custom))
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'priority 1000)
        (goto-char (overlay-start ov))
        (recenter)))))

(defun gptel-agent--ask-multiple (callback questions)
  "Ask user multiple QUESTIONS sequentially, calling CALLBACK with results."
  (let* ((qs (append questions nil))
         (results (make-hash-table :test 'equal))
         (question-order nil)
         (total (length qs)))
    (cl-labels ((ask-next (idx)
                  (if (>= idx total)
                      ;; Format all Q&A pairs in order
                      (let ((formatted-output
                             (mapconcat
                              (lambda (q-and-idx)
                                (let* ((q (car q-and-idx))
                                       (qnum (cdr q-and-idx)))
                                  (format "Q%d: %s\nR%d: %s"
                                          qnum q
                                          qnum (gethash q results))))
                              (nreverse question-order)
                              "\n\n")))
                        (funcall callback formatted-output))
                    (let* ((item (nth idx qs))
                           (q (plist-get item :question))
                           (c (plist-get item :choices)))
                      (push (cons q (1+ idx)) question-order)
                      (gptel-agent--ask-question
                       (lambda (answer)
                         (puthash q answer results)
                         (ask-next (1+ idx)))
                       q (append c nil))))))
      (ask-next 0))))

;;; All tool declarations

(gptel-make-tool
 :name "Bash"
 :function #'gptel-agent--execute-bash
 :description "Execute Bash commands.

This tool provides access to a Bash shell with GNU coreutils (or equivalents) available.
Use this to inspect system state, run builds, tests or other development or system administration tasks.

Do NOT use this for file operations, finding, reading or editing files.
Use the provided file tools instead: `Read`, `Write`, `Edit`, \
`Glob`, `Grep`

- Quote file paths with spaces using double quotes.
- Chain dependent commands with && (or ; if failures are OK)
- Use absolute paths instead of cd when possible
- For parallel commands, make multiple `Bash` calls in one message
- Run tests, check your work or otherwise close the loop to verify changes you make.

EXAMPLES:
- List files with details: 'ls -lah /path/to/dir'
- Find recent errors: 'grep -i error /var/log/app.log | tail -20'
- Check file type: 'file document.pdf'
- Count lines: 'wc -l *.txt'

The command will be executed in the current working directory.  Output is
returned as a string.  Long outputs should be filtered/limited using pipes."
 :args '(( :name "command"
           :type string
           :description "The Bash command to execute.  \
Can include pipes and standard shell operators.
Example: 'ls -la | head -20' or 'grep -i error app.log | tail -50'"))
 :category "gptel-agent"
 :confirm t
 :include t
 :async t)

(gptel-make-tool
 :name "Eval"
 :function
 (lambda (expression)
   (let ((standard-output (generate-new-buffer " *gptel-agent-eval-elisp*"))
         (result nil) (output nil))
     (unwind-protect
         (condition-case err
             (progn
               (setq result (eval (read expression) t))
               (when (> (buffer-size standard-output) 0)
                 (setq output (with-current-buffer standard-output (buffer-string))))
               (concat
                (format "Result:\n%S" result)
                (and output (format "\n\nSTDOUT:\n%s" output))))
           ((error user-error)
            (concat
             (format "Error: eval failed with error %S: %S"
                     (car err) (cdr err))
             (and output (format "\n\nSTDOUT:\n%s" output)))))
       (kill-buffer standard-output))))
 :description "Evaluate Elisp EXPRESSION and return result and any printed output.

EXPRESSION can be anything to evaluate.  It can be a function call, a
variable, a quasi-quoted expression.  The only requirement is that only
the first sexp will be read and evaluated, so if you need to evaluate
multiple expressions, make one call per expression.  Do not combine
expressions using progn etc.  Just go expression by expression and try
to make standalone single expressions.

Instead of saying \"I can't calculate that\" etc, use this tool to
evaluate the result.

The return value is formated to a string using %S, so a string will be
returned as an escaped embedded string and literal forms will be
compatible with `read' where possible.  Some forms have no printed
representation that can be read and will be represented with
#<hash-notation> instead.

Output from `print`, `prin1`, and `princ` is captured and returned as STDOUT.
Use `print` for diagnostic output, not `message` (which goes to *Messages* buffer
and is not captured).

You can use this to quickly change a user setting, check a variable, or
demonstrate something to the user."
 :args '(( :name "expression"
           :type string
           :description "A single elisp sexp to evaluate."))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "WebSearch"
 :function 'gptel-agent--web-search-eww
 :description "Search the web for the first five results to a query.  The query can be an arbitrary string.  Returns the top five results from the search engine as a list of plists.  Each object has the keys `:url` and `:excerpt` for the corresponding search result.

This tool uses the Emacs web browser (eww) with its default search engine (typically DuckDuckGo) to perform searches. No API key is required.

If required, consider using the url as the input to the `Read` tool to get the contents of the url.  Note that this might not work as the `Read` tool does not handle javascript-enabled pages."
 :args '((:name "query"
                :type string
                :description "The natural language search query, can be multiple words.")
         (:name "count"
                :type integer
                :description "Number of results to return (default 5)"
                :optional t))
 :include t
 :async t
 :category "gptel-agent")

(gptel-make-tool
 :function #'gptel-agent--read-url
 :name "WebFetch"
 :description "Fetch and read the contents of a URL.

- Returns the text of the URL (not HTML) formatted for reading.
- Request times out after 30 seconds."
 :args '(( :name "url"
           :type "string"
           :description "The URL to read"))
 :async t
 :include t
 :category "gptel-agent")

(gptel-make-tool
 :name "YouTube"
 :function #'gptel-agent--yt-read-url
 :description "Find the description and video transcript for a youtube video.  Returns a markdown formatted string containing two sections:

\"description\": The video description added by the uploader
\"transcript\": The video transcript in SRT format"
 :args '((:name "url"
                :description "The youtube video URL, for example \"https://www.youtube.com/watch?v=H2qJRnV8ZGA\""
                :type "string"))
 :category "gptel-agent"
 :async t
 :include t)

(gptel-make-tool
 :name "Diagnostics"
 :description "Collect all code diagnostics with severity high/medium \
across all open buffers in the current project.

With optional argument `all`, collect notes and low-severity diagnostics
too."
 :function #'gptel-agent--flymake-diagnostics
 :args (list '( :name "all"
                :type boolean
                :description
                "Whether low-severity diagnostics (notes) should also be collected."
                :optional t))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "Mkdir"
 :description "Create a new directory with the given name in the specified parent directory"
 :function #'gptel-agent--make-directory
 :args (list '( :name "parent"
                :type "string"
                :description "The parent directory where the new directory should be created, e.g. /tmp")
             '( :name "name"
                :type "string"
                :description "The name of the new directory to create, e.g. testdir"))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "Edit"
 :description
 "Replace text in one or more files.

To edit a single file, provide the file `path`.

For the replacement, there are two methods:
- Short replacements: Provide both `old_str` and `new_str`, in which case `old_str` \
needs to exactly match one unique section of the original file, including any whitespace.  \
Make sure to include enough context that the match is not ambiguous.  \
The entire original string will be replaced with `new str`.
- Long or involved replacements: set the `diff` parameter to true and provide a unified diff \
in `new_str`. `old_str` can be ignored.

To edit multiple files,
- provide the directory path,
- set the `diff` parameter to true
- and provide a unified diff in `new_str`.

Diff instructions:

- The diff must be provided within fenced code blocks (=diff or =patch) and be in unified format.
- The LLM should generate the diff such that the file paths within the diff \
  (e.g., '--- a/filename' '+++ b/filename') are appropriate for the 'path'.

To simply insert text at some line, use the \"Insert\" instead."
 :function #'gptel-agent--edit-files
 :args '(( :name "path"
           :description "File path or directory to edit"
           :type string)
         ( :name "old_str"
           :description "Original string to replace.  If providing a unified diff, this should be false"
           :type string
           :optional t)
         ( :name "new_str"
           :description "Replacement string OR unified diff text"
           :type string)
         ( :name "diff"
           :description "Whether the replacement is a string or a diff.  `true` for a diff, `false` otherwise."
           :type boolean))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "Insert"
 :description "Insert `new_str` after `line_number` in file at `path`.

Use this tool for purely additive actions: adding text to a file at a \
specific location with no changes to the surrounding context."
 :function #'gptel-agent--insert-in-file
 :args '(( :name "path"
           :description "Path of file to edit."
           :type string)
         ( :name "line_number"
           :description "The line number at which to insert `new_str`, with
- 0 to insert at the beginning, and
- -1 to insert at the end."
           :type integer)
         ( :name "new_str"
           :description "String to insert at `line_number`."
           :type string))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "Write"
 :description "Create a new file with the specified content.
Overwrites an existing file, so use with care!
Consider using the more granular tools \"Insert\" or \"Edit\" first."
 :function #'gptel-agent--write-file
 :args (list '( :name "path"
                :type "string"
                :description "The directory where to create the file, \".\" is the current directory.")
             '( :name "filename"
                :type "string"
                :description "The name of the file to create.")
             '( :name "content"
                :type "string"
                :description "The content to write to the file"))
 :category "gptel-agent"
 :confirm t)

(gptel-make-tool
 :name "Glob"
 :description "Recursively find files matching a provided glob pattern.

- Supports glob patterns like \"*.md\" or \"*test*.py\".
  The glob applies to the basename of the file (with extension).
- Does not support double wildcard \"**/*\".
- Returns matching file paths at all depths sorted by modification time.
  Limit the depth of the search by providing the `depth` argument.
- When you are doing an open ended search that may require multiple rounds
  of globbing and grepping, use the \"task\" tool instead
- You can call multiple tools in a single response.  It is always better to
  speculatively perform multiple searches in parallel if they are potentially useful."
 :function #'gptel-agent--glob
 :args '(( :name "pattern"
           :type string
           :description "Glob pattern to match, for example \"*.el\". Must not be empty.
Use \"*\" to list all files in a directory.")
         ( :name "path"
           :type string
           :description "Directory to search in.  Supports relative paths and defaults to \".\""
           :optional t)
         ( :name "depth"
           :description "Limit directory depth of search, 1 or higher. Defaults to no limit."
           :type integer
           :optional t))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "Read"
 :description (format "Read textual files' contents between specified line numbers `start_line` and `end_line`,
with both ends included. Use only on human readable textual files

Consider using the \"Grep\" tool to find the right range to read first.

Reads the whole file if the line range is not provided.

Files over %d KB in size can only be read by specifying a line range."
                      gptel-agent-read-file-size-threshold)
 :function #'gptel-agent--read-file-lines
 :args '(( :name "file_path"
           :type string
           :description "The path to the file to be read.")
         ( :name "start_line"
           :type integer
           :description "The line to start reading from, defaults to the start of the file"
           :optional t)
         ( :name "end_line"
           :type integer
           :description "The line up to which to read, defaults to the end of the file."
           :optional t))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "Grep"
 :description "Search for text in file(s) at `path`.

Use this tool to find relevant parts of files to read.

Returns a list of matches prefixed by the line number, and grouped by file.  Can search an individual file (if providing a file path) or a directory.  Consider using this tool to find the right line range for the \"Read\" tool.

When searching directories, optionally restrict the types of files in the search with a `glob`.  Can request context lines around each match using the `context_lines` parameters."
 :function #'gptel-agent--grep
 :args '(( :name "regex"
           :description "Regular expression to search for in file contents."
           :type string)
         ( :name "path"
           :description "File or directory to search in."
           :type string)
         ( :name "glob"
           :description "Optional glob to restrict file types to search for.
Only required when path is a directory.
Examples: *.md, *.rs"
           :type string
           :optional t)
         ( :name "context_lines"
           :description "Number of lines of context to retrieve around each match (0-15 inclusive).
Optional, defaults to 0."
           :optional t
           :type integer
           :maximum 15))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "TodoWrite"
 :description "Create and manage a structured task list for your current session.  \
Helps track progress and organize complex tasks. Use proactively for multi-step work.

Only one todo can be `in_progress` at a time."
 :function #'gptel-agent--write-todo
 :args
 '(( :name "todos"
     :type array
     :items
     ( :type object
       :properties
       (:content
        ( :type string :minLength 1
          :description "Imperative form describing what needs to be done (e.g., 'Run tests')")
        :status
        ( :type string
          :enum ["pending" "in_progress" "completed"]
          :description "Task status: pending, in_progress, or completed (exactly one)")
        :activeForm
        ( :type string :minLength 1
          :description "Present continuous form shown during execution (e.g., 'Running tests')")))))
 :category "gptel-agent"
 :include nil)

(defconst gptel-agent--skill-tool-base-desc
  "Load a skill into the current conversation.

Each skill provides guidance on how to execute a specific task.
You can invoke a skill with optional args, the args are for your future reference only.

When to use:
- When a skill is relevant, you must invoke this tool IMMEDIATELY
- This is a BLOCKING REQUIREMENT: invoke the relevant Skill tool before generating any other response about the task
- Only use skills listed in your prompt
- Do not invoke a skill that is already loaded.

How to use:
- Invoke with the skill name and optional args.  The args are for your reference only
- Examples:
    - `skill: \"pdf\"` - invoke the pdf skill
    - `skill: \"commit\", args: \"-m 'Fix bug'\"` - invoke with arguments
    - `skill: \"review-pr\", args: \"123\"` - invoke with arguments"
  "Base description for the Skill tool, without the available skills list.")

(defun gptel-agent--update-skill-tool ()
  "Update the Skill tool's description and enum with currently enabled skills."
  (let* ((tool (gptel-get-tool "Skill"))
         (enabled (or gptel-agent--enabled-skills
                      (mapcar #'car gptel-agent--skills)))
         (skills-msg (gptel-agent--skills-system-message gptel-agent--skills)))
    (setf (gptel-tool-description tool)
          (concat gptel-agent--skill-tool-base-desc "\n\n" skills-msg))
    (setf (plist-get (car (gptel-tool-args tool)) :enum)
          (vconcat enabled))))

(gptel-make-tool
 :name "Skill"
 :description gptel-agent--skill-tool-base-desc
 :function #'gptel-agent--get-skill
 :args '(( :name "skill"
           :type string
           :description "Name of the skill, chosen from the list of available skills")
         ( :name "args"
           :type string
           :optional t
           :description "Args relevant to the skill, for your future reference"))
 :category "gptel-agent"
 :include t)

(defconst gptel-agent--agent-tool-base-desc
  "Launch a specialized agent to handle complex, multi-step tasks autonomously.

Agents run independently and return results in one message.  \
Use for open-ended searches, complex research, exploration tasks, \
or when a task matches an available agent's description.  \
You can launch multiple agents in parallel for independent tasks.  \
Treat agent results as evidence reports: inspect their evidence, assumptions, \
verification gaps and confidence before relying on them."
  "Base description for the Agent tool, without the available agents list.")

(defun gptel-agent--full-agent-definitions ()
  "Return full definitions for the currently discovered agent registry."
  (mapcar
   (lambda (entry)
     (cons (car entry)
           (copy-tree
            (or (gptel-agent--agent-plist (car entry))
                (cdr entry)))))
   gptel-agent--agents))

(defun gptel-agent--snapshot-agent-definitions ()
  "Return full, request-stable definitions for currently allowed agents."
  (let* ((inherited gptel-agent--agent-snapshot)
         (source (or inherited
                     gptel-agent--registry-snapshot
                     (gptel-agent--full-agent-definitions)))
         ;; An inherited snapshot was already filtered by the parent request.
         (enabled (and (not inherited) gptel-agent--enabled-agents))
         (current gptel-agent--current-agent))
    (cl-loop for (name . plist) in source
             unless (or (member name '("gptel-agent" "gptel-plan" "ask"))
                        (and current (string-equal name current))
                        (and enabled (not (member name enabled))))
             collect (cons name (copy-tree plist)))))

(defun gptel-agent--agent-snapshot-message (snapshot)
  "Format SNAPSHOT for an invocation-specific Agent tool description."
  (concat
   "Available sub-agents for this request.  Use them when appropriate."
   "\n<available_agents>\n"
   (mapconcat
    (lambda (agent)
      (format "  <agent>\n    <name>%s</name>\n    <description>%s</description>\n  </agent>"
              (car agent) (or (plist-get (cdr agent) :description) "")))
    snapshot "\n")
   "\n</available_agents>"))

(defun gptel-agent--localize-agent-tool (fsm)
  "Replace Agent in the current prompt buffer with an FSM-owned clone.

The clone captures FSM and a snapshot of the allowed agent registry.  Its
schema and dispatcher therefore cannot change when another project updates
the global registry while this request is in flight."
  (when-let* ((base-tool
               (cl-find "Agent" gptel-tools
                        :key #'gptel-tool-name :test #'string=)))
    (let* ((source-buffer (plist-get (gptel-fsm-info fsm) :buffer))
           ;; gptel copies only its own buffer-local variables into the prompt
           ;; transform buffer.  Read our project/agent identity from the
           ;; explicit request buffer recorded in FSM instead.
           (snapshot
            (if (buffer-live-p source-buffer)
                (with-current-buffer source-buffer
                  (gptel-agent--snapshot-agent-definitions))
              (gptel-agent--snapshot-agent-definitions)))
           (tool (gptel--copy-tool base-tool))
           (args (copy-tree (gptel-tool-args base-tool))))
      (setf (gptel-tool-description tool)
            (concat gptel-agent--agent-tool-base-desc "\n\n"
                    (gptel-agent--agent-snapshot-message snapshot))
            (gptel-tool-args tool) args
            (plist-get (car args) :enum) (vconcat (mapcar #'car snapshot))
            (gptel-tool-function tool)
            (lambda (main-cb agent-type description prompt)
              (gptel-agent--task main-cb agent-type description prompt
                                 fsm snapshot)))
      (setq-local
       gptel-tools
       (mapcar (lambda (candidate)
                 (if (eq candidate base-tool) tool candidate))
               gptel-tools)))))

(defun gptel-agent--update-agent-tool ()
  "Retain compatibility; Agent schemas are now built per request.

The registered tool is deliberately immutable.  See
`gptel-agent--localize-agent-tool'."
  (gptel-get-tool "Agent"))

(gptel-make-tool
 :name "Agent"
 :description gptel-agent--agent-tool-base-desc
 :function #'gptel-agent--task
 :args '(( :name "subagent_type"
           :type string
           :description "The type of specialized agent to use for this task")
         ( :name "description"
           :type string
           :description "A short (3-5 word) description of the task")
         ( :name "prompt"
           :type "string"
           :description "The detailed task for the agent to perform autonomously.  \
Should include exactly what information the agent should return."))
 :category "gptel-agent"
 :async t
 :confirm t
 :include t)

(gptel-make-tool
 :name "AskUserQuestion"
 :function #'gptel-agent--ask-multiple
 :description "Ask the user one or more questions sequentially.

Each question in QUESTIONS should have `question' and `choices' keys.
CHOICES must contain objects with a `value' key. An optional `description'
key provides additional context for each choice. An optional `recommended' key
tells the user which choice is recommended in this context.

A \"Custom\" option is always automatically appended to each question's
choices, allowing the user to provide their own free-text response if
none of the predefined options are suitable."
 :args '(( :name "questions"
           :type array
           :items (:type object
                   :properties (:question (:type string)
                                :choices (:type array
                                          :items (:type object
                                                  :properties (:value (:type string)
                                                                      :description (:type string)
                                                                      :recommended (:type boolean))
                                                  :required ["value"]))))))
 :category "gptel-agent"
 :async t
 :include t)

;;; Register repetition detection hook
(add-hook 'gptel-pre-tool-call-functions #'gptel-agent--detect-repetition)
(add-hook 'gptel-post-response-functions #'gptel-agent--reset-tool-call-counts)
(add-hook 'gptel-prompt-transform-functions
          #'gptel-agent--localize-agent-tool t)
(advice-add 'gptel-abort :around #'gptel-agent--abort-with-owned-runs)

(provide 'gptel-agent-tools)
;;; gptel-agent-tools.el ends here

;; Local Variables:
;; elisp-flymake-byte-compile-load-path: ("~/.local/share/git/elpaca/repos/gptel/" "~/.local/share/git/elpaca/repos/transient/lisp" "~/.local/share/git/elpaca/repos/compat/")
;; End:
