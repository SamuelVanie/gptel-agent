;;; gptel-agent-policy-request.el --- Request-local tool policies -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; gptel's programmatic FSM may skip pre-tool hooks.  Its interactive FSM
;; permits hook rewrites and lets a confirmation edit the tool as well as
;; its arguments.  Keep the named arguments and owner through all three paths.

;;; Code:

(require 'gptel)
(require 'gptel-agent-policy)

(defvar gptel-agent-policy--active-check nil
  "During an admitted tool or hook, check a plist of argument overrides.
The optional second argument requires a positive constraint on that field.")
(defvar gptel-agent-policy--active-tool nil
  "The request-local tool whose arguments are being checked.")
(defvar-local gptel-agent-policy--prompt-directory nil
  "Owner's working directory at the time a temporary prompt was created.")

(defun gptel-agent-policy--copy-prompt (original source start end thunk)
  "Capture SOURCE's policy before ORIGINAL creates a prompt for THUNK."
  (let* ((ceiling (gptel-agent-policy-buffer-ceiling source))
         (selector (buffer-local-value 'gptel-agent-tool-policy source))
         (snapshot (gptel-agent-policy--freeze
                    ceiling (buffer-local-value 'gptel-agent-policy--inherited-snapshot source)
                    (and selector (car ceiling))))
         (directory (buffer-local-value 'default-directory source)))
    (funcall original source start end
             (lambda ()
               (setq-local gptel-agent-policy--prompt-directory directory
                           gptel-agent-tool-policy selector
                           gptel-agent-policy--inherited ceiling
                           gptel-agent-policy--inherited-snapshot snapshot)
               (funcall thunk)))))

(defun gptel-agent-policy--preset (&rest _)
  "Accumulate every policy selected in a temporary prompt."
  (when (and gptel-agent-tool-policy gptel-agent-policy--prompt-directory)
    (let* ((default-directory gptel-agent-policy--prompt-directory)
           (ceiling (gptel-agent-policy-buffer-ceiling)))
      (gptel-agent-policy-inherit
       ceiling (gptel-agent-policy--freeze ceiling gptel-agent-policy--inherited-snapshot
                                          (car ceiling))))))

(defun gptel-agent-policy-request-capture (fsm)
  "Capture FSM's ceiling from its construction buffer or resumed conversation.
A resumed worker reconstructs its serialized prompt directly.  Preserve the
saved invocation's limits alongside the new parent and child settings."
  (let* ((info (gptel-fsm-info fsm))
         (owner (plist-get info :buffer))
         (data (plist-get info :data))
         (prompt (and (bufferp data) (buffer-live-p data) data)))
    (when (buffer-live-p owner)
      (with-current-buffer (or prompt owner)
        (let* ((default-directory (or gptel-agent-policy--prompt-directory
                                      (buffer-local-value 'default-directory owner)))
               (ceiling (delete-dups
                         (append (plist-get info :agent-tool-policy-ceiling)
                                 ;; A parent may queue steering while an asynchronous
                                 ;; child prompt transform is still preparing this FSM.
                                 (when prompt
                                   (if gptel-agent-policy--prompt-directory
                                       (buffer-local-value 'gptel-agent-policy--inherited owner)
                                     (gptel-agent-policy-buffer-ceiling owner)))
                                 (gptel-agent-policy-buffer-ceiling)))))
          (plist-put info :agent-tool-policy-ceiling ceiling)
          (when ceiling
            (plist-put info :agent-tool-policy-snapshot
                       (gptel-agent-policy--freeze
                        ceiling (cl-remove-duplicates
                                 (append (plist-get info :agent-tool-policy-snapshot)
                                         gptel-agent-policy--inherited-snapshot
                                         (when prompt
                                           (buffer-local-value 'gptel-agent-policy--inherited-snapshot
                                                               owner)))
                                 :test #'eq)))))))))

(defun gptel-agent-policy-request-ceiling (fsm)
  "Return FSM's captured policy names and their originating workspaces."
  (when fsm (plist-get (gptel-fsm-info fsm) :agent-tool-policy-ceiling)))

(defun gptel-agent-policy--dispatch-ceiling (fsm)
  "Capture even a host-constructed FSM that enters tool dispatch directly."
  (unless (plist-member (gptel-fsm-info fsm) :agent-tool-policy-ceiling)
    (gptel-agent-policy-request-capture fsm))
  (gptel-agent-policy-request-ceiling fsm))

(defun gptel-agent-policy-request-covers-p (child parent)
  "Whether CHILD enforces every workspace and policy version required by PARENT.
An asynchronously transforming CHILD has a prompt buffer but may not have
reached final request construction yet.  Capture its existing limits first."
  (let ((required (gptel-agent-policy-request-ceiling parent)))
    (or (null required)
        (when child
          (let* ((info (gptel-fsm-info child))
                 (prompt (plist-get info :data)))
            (when (and (bufferp prompt) (buffer-live-p prompt))
              (gptel-agent-policy-request-capture child))
            (and (cl-subsetp required (gptel-agent-policy-request-ceiling child) :test #'equal)
                 (cl-every
                  (lambda (definition)
                    (cl-member definition (plist-get info :agent-tool-policy-snapshot)
                               :test #'gptel-agent-policy--same-definition-p))
                  (plist-get (gptel-fsm-info parent) :agent-tool-policy-snapshot))))))))

(defun gptel-agent-policy-request-trust-tool (fsm tool)
  "Admit the precise TOOL instance supplied by trusted workflow code to FSM."
  (let* ((info (gptel-fsm-info fsm))
         (trusted (plist-get info :agent-tool-policy-trusted)))
    (plist-put info :agent-tool-policy-trusted (cl-adjoin tool trusted :test #'eq)))
  tool)

(defun gptel-agent-policy--tool (fsm name)
  "Look up NAME among FSM's effective tools, including any dynamic additions."
  (cl-find name (plist-get (gptel-fsm-info fsm) :tools)
           :key #'gptel-tool-name :test #'equal))

(defun gptel-agent-policy--check (fsm tool args &optional required-field)
  "Check FSM's actual TOOL with named ARGS and optional REQUIRED-FIELD."
  (let* ((info (gptel-fsm-info fsm))
         (ceiling (gptel-agent-policy-request-ceiling fsm))
         (result-explorer
          (assq gptel-agent-policy--result-explorer-name ceiling))
         (gptel-agent-policy--snapshot (plist-get info :agent-tool-policy-snapshot)))
    (and tool (buffer-live-p (plist-get info :buffer))
         ;; A same-named third-party tool is not a scoped result reader.
         ;; Trusted workflow tools must also respect this mandatory ceiling
         ;; and every narrower user policy attached to the result explorer.
         (or (not result-explorer)
             (and (equal (gptel-tool-category tool) "gptel-agent-result-explorer")
                  (member (gptel-tool-name tool) '("ResultRead" "ResultGrep"))))
         (or (and (not result-explorer) (null required-field)
                  (memq tool (plist-get info :agent-tool-policy-trusted)))
             (gptel-agent-policy--allows-p
              ceiling tool args
              (buffer-local-value 'default-directory (plist-get info :buffer))
              required-field)))))

(defun gptel-agent-policy-request-allows-p (fsm name args &optional required-field)
  "Check FSM's effective tool NAME with ARGS, optionally requiring REQUIRED-FIELD."
  (gptel-agent-policy--check fsm (gptel-agent-policy--tool fsm name) args required-field))

(defun gptel-agent-policy--denial (name)
  "Explain a per-call policy denial for NAME."
  (format "Error: Tool policy denied %s with the supplied arguments." name))

(defun gptel-agent-policy--in-context (fsm tool args function)
  "Run FUNCTION in FSM's owning buffer with the checked TOOL and named ARGS."
  (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
    (let ((gptel-agent-policy--active-tool tool)
          (gptel-agent-policy--active-check
           (lambda (overrides &optional required-field)
             (let ((effective (copy-sequence args)))
               (while overrides
                 (setq effective (plist-put effective (pop overrides) (pop overrides))))
               (and (eq tool (gptel-agent-policy--tool fsm (gptel-tool-name tool)))
                    (gptel-agent-policy--check fsm tool effective required-field))))))
      (funcall function))))

(defun gptel-agent-policy--scan (fsm)
  "Finish denied calls before instruction discovery or other preparing hooks."
  (let ((info (gptel-fsm-info fsm)))
    (dolist (call (plist-get info :tool-use))
      (let* ((name (plist-get call :name))
             (tool (gptel-agent-policy--tool fsm name)))
        (unless (or (plist-get call :result)
                    ;; gptel also uses a synthetic response for structured output.
                    (and (null tool) (equal name gptel--ersatz-json-tool))
                    (gptel-agent-policy--check fsm tool (plist-get call :args)))
          (plist-put call :error t)
          (gptel--process-tool-call fsm tool call (gptel-agent-policy--denial name)))))))

(defun gptel-agent-policy--hook (fsm hook input)
  "Apply HOOK to admitted INPUT in FSM and check any rewrite for later hooks."
  (let* ((name (plist-get input :name))
         (args (plist-get input :args))
         (tool (gptel-agent-policy--tool fsm name)))
    ;; Upstream's pre-hook loop snapshots the batch.  It will still visit
    ;; a previously blocked rewrite when it runs the next hook.
    (when (gptel-agent-policy--check fsm tool args)
      (let* ((reply (gptel-agent-policy--in-context
                     fsm tool args (lambda () (funcall hook input))))
             (next-name (or (plist-get reply :name) name))
             ;; gptel only merges the reply when :name or :args is non-nil.
             (next-args (if (and (or (plist-get reply :name) (plist-get reply :args))
                                 (plist-member reply :args))
                            (plist-get reply :args)
                          args)))
        (if (gptel-agent-policy-request-allows-p fsm next-name next-args)
            reply
          (list :name next-name :args next-args
                :block (gptel-agent-policy--denial next-name)))))))

(defun gptel-agent-policy--pre-tool (original fsm)
  "Guard FSM before ORIGINAL and between successive user hooks."
  (let (gptel-agent-policy--active-tool gptel-agent-policy--active-check)
    (if (not (gptel-agent-policy--dispatch-ceiling fsm)) (funcall original fsm)
      (gptel-agent-policy--scan fsm)
      (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
        (let* ((hooks (cl-loop for hook in gptel-pre-tool-call-functions
                               if (eq hook t) append (default-value 'gptel-pre-tool-call-functions)
                               else collect hook))
               (gptel-pre-tool-call-functions
                (mapcar (lambda (hook) (apply-partially #'gptel-agent-policy--hook fsm hook)) hooks)))
          (funcall original fsm))))))

(cl-defstruct (gptel-agent-policy--pending (:constructor gptel-agent-policy--make-pending))
  "A protected confirmation associated with gptel's per-call result callback."
  fsm call tool approved exported started)

(defvar gptel-agent-policy--pending (make-hash-table :test #'eq :weakness 'key)
  "Map a protected pending callback to the request that issued it.")

(defun gptel-agent-policy--guard-confirmation (state selected args)
  "Protect execution of SELECTED with named ARGS for confirmation STATE."
  (let* ((fsm (gptel-agent-policy--pending-fsm state))
         (call (gptel-agent-policy--pending-call state))
         (original (gptel-agent-policy--pending-tool state))
         ;; Outside approval can decorate the request tool after dispatch.
         (approved (gptel-agent-policy--pending-approved state))
         (old (or (eq selected (gptel-agent-policy--pending-exported state))
                  (eq selected original)))
         (delegate (if old approved selected))
         (tool (if old original selected))
         (name (gptel-tool-name tool))
         (proxy (gptel--copy-tool delegate))
         (named (copy-tree args t)))
    (setf (gptel-tool-function proxy)
          (lambda (&rest supplied)
            (let (gptel-agent-edit--preflight-changes gptel-agent-edit--preflight-disk-hooks
                  (callback (and (gptel-tool-async delegate) (pop supplied))))
              (if (and (eq (gptel-fsm-state fsm) 'TOOL)
                       (memq call (plist-get (gptel-fsm-info fsm) :tool-use))
                       (not (plist-get call :result))
                       (not (gptel-agent-policy--pending-started state))
                       (eq tool (gptel-agent-policy--tool fsm name))
                       (equal supplied (gptel--map-tool-args tool named))
                       (gptel-agent-policy--check fsm tool named)
                       ;; An edit may introduce a new outside operation after
                       ;; the original preparation.  Request a fresh approval.
                       (or (and old (not (eq approved original)))
                           (condition-case nil
                               (not (and (fboundp 'gptel-agent-access--outside-p)
                                         (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
                                           (gptel-agent-access--outside-p name named fsm))))
                             (error nil))))
                  (gptel-agent-policy--in-context
                   fsm tool named
                   (lambda ()
                     (setf (gptel-agent-policy--pending-started state) t)
                     (apply (gptel-tool-function delegate)
                            (if callback (cons callback supplied) supplied))))
                (plist-put call :error t)
                (let ((denial (gptel-agent-policy--denial name)))
                  (if callback (funcall callback denial) denial))))))
    proxy))

(defun gptel-agent-policy--export (pending)
  "Protect the final PENDING tuple, including a possible outside approval."
  (when-let* ((state (gethash (nth 2 pending) gptel-agent-policy--pending)))
    (setf (gptel-agent-policy--pending-approved state) (car pending))
    (let ((tool (gptel-agent-policy--guard-confirmation
                 state (gptel-agent-policy--pending-tool state) (cadr pending))))
      (setf (gptel-agent-policy--pending-exported state) tool
            (car pending) tool)))
  pending)

(defun gptel-agent-policy-request-confirmation-tool (tool args callback)
  "Guard TOOL and named ARGS for a pending CALLBACK if its request is protected.
Use this before adding a known dispatcher's own wrapper to an edited tool."
  (if-let* ((state (gethash callback gptel-agent-policy--pending)))
      (gptel-agent-policy--guard-confirmation state tool args)
    tool))

(defun gptel-agent-policy--accept (arguments)
  "Guard freshly edited confirmation ARGUMENTS before native dispatch."
  (let ((pending (car arguments)))
    (if (not (cl-some (lambda (p) (gethash (nth 2 p) gptel-agent-policy--pending)) pending))
        arguments
      (cons
       (mapcar
        (lambda (p)
          (cons (gptel-agent-policy-request-confirmation-tool (car p) (cadr p) (nth 2 p))
                (cdr p)))
        pending)
       (cdr arguments)))))

(defun gptel-agent-policy--tool-use (original fsm)
  "Scan before ORIGINAL prepares FSM and capture finalized pending tuples."
  (let (gptel-agent-policy--active-tool gptel-agent-policy--active-check)
    (if (not (gptel-agent-policy--dispatch-ceiling fsm)) (funcall original fsm)
      (gptel-agent-policy--scan fsm)
      (let* ((info (gptel-fsm-info fsm))
             (callback (plist-get info :callback)))
        (unwind-protect
            (progn
              (setf (plist-get info :callback)
                    (lambda (response state &rest extra)
                      (when (eq (car-safe response) 'tool-call)
                        (mapc #'gptel-agent-policy--export (cdr response)))
                      (apply callback response state extra)))
              (funcall original fsm))
          (setf (plist-get info :callback) callback))))))

(defun gptel-agent-policy--confirm-p (tool call args)
  "Use gptel's confirmation precedence for TOOL, CALL, and positional ARGS."
  (if-let* ((override (plist-member call :confirm))) (cadr override)
    (and gptel-confirm-tool-calls
         (or (eq gptel-confirm-tool-calls t)
             (let ((confirm (gptel-tool-confirm tool)))
               (if (functionp confirm) (apply confirm args) confirm))))))

(defun gptel-agent-policy--invoke (fsm tool args done)
  "Invoke an admitted TOOL with ARGS in its owner and finish via DONE."
  (let ((values (gptel--map-tool-args tool args)))
    (if (gptel-tool-async tool)
        (gptel-agent-policy--in-context
         fsm tool args (lambda () (apply (gptel-tool-function tool) done values)))
      (funcall done
               (condition-case err
                   (gptel-agent-policy--in-context
                    fsm tool args (lambda () (apply (gptel-tool-function tool) values)))
                 (error (mapconcat #'gptel--to-string err " ")))))))

(defun gptel-agent-policy--dispatch-call (fsm call)
  "Process one real CALL or return its pending confirmation tuple."
  (let* ((name (plist-get call :name))
         (tool (gptel-agent-policy--tool fsm name))
         (args (plist-get call :args))
         (done (apply-partially #'gptel--process-tool-call fsm tool call))
         (admitted (gptel-agent-policy--check fsm tool args))
         (confirm (and admitted
                       (gptel-agent-policy--confirm-p tool call (gptel--map-tool-args tool args)))))
    ;; Functional confirmation predicates run arbitrary trusted Lisp.  Re-read
    ;; the proposal and registry before queuing or executing the operation.
    (setq args (plist-get call :args))
    (cond
     ((not (and admitted (equal name (plist-get call :name))
                (eq tool (gptel-agent-policy--tool fsm name))
                (gptel-agent-policy--check fsm tool args)))
      (plist-put call :error t)
      (funcall done (gptel-agent-policy--denial name)) nil)
     (confirm
      (puthash done (gptel-agent-policy--make-pending :fsm fsm :call call :tool tool)
               gptel-agent-policy--pending)
      (list tool args done))
     (t (gptel-agent-policy--invoke fsm tool args done) nil))))

(defun gptel-agent-policy--dispatch (original fsm)
  "Dispatch protected FSM calls with their owning buffer and named arguments."
  (if (not (gptel-agent-policy-request-ceiling fsm)) (funcall original fsm)
    (let* ((info (gptel-fsm-info fsm))
           (calls (cl-remove-if (lambda (call) (plist-get call :result))
                                (plist-get info :tool-use)))
           pending)
      (with-current-buffer (plist-get info :buffer)
        (dolist (call calls)
          (let ((name (plist-get call :name)))
            (if (and (equal name gptel--ersatz-json-tool) (not (gptel-agent-policy--tool fsm name)))
                ;; This is structured model output, not a registered function.
                (progn
                  (funcall (plist-get info :callback) (gptel--json-encode (plist-get call :args)) info)
                  (setf (plist-get info :tool-use) (delq call (plist-get info :tool-use)))
                  (when (plist-get info :stream) (funcall (plist-get info :callback) t info)))
              (when-let* ((confirmation (gptel-agent-policy--dispatch-call fsm call)))
                (push confirmation pending)))))
        (when pending
          (plist-put info :tool-pending t)
          (funcall (plist-get info :callback) (cons 'tool-call pending) info))))))

(advice-add 'gptel--with-buffer-copy-internal :around #'gptel-agent-policy--copy-prompt)
(advice-add 'gptel--apply-preset :after #'gptel-agent-policy--preset)
(advice-add 'gptel--realize-query :before #'gptel-agent-policy-request-capture '((depth . -90)))
(advice-add 'gptel--handle-pre-tool :around #'gptel-agent-policy--pre-tool '((depth . -90)))
(advice-add 'gptel--handle-tool-use :around #'gptel-agent-policy--tool-use '((depth . -90)))
(advice-add 'gptel--handle-tool-use :around #'gptel-agent-policy--dispatch '((depth . 90)))
(advice-add 'gptel--accept-tool-calls :filter-args #'gptel-agent-policy--accept)

(provide 'gptel-agent-policy-request)
;;; gptel-agent-policy-request.el ends here
