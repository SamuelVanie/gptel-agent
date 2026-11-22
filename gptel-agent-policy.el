;;; gptel-agent-policy.el --- Named argument policies for gptel tools -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Policies are trusted configuration.  Compile their value predicates when
;; defined, then match the named JSON arguments before gptel maps them onto
;; positional Lisp arguments.  Each inherited policy keeps its own workspace.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'rx)
(require 'subr-x)
(require 'url-parse)

(defvar-local gptel-agent-tool-policy nil
  "Named tool policy for this conversation, or nil for ordinary dispatch.
Select it in a gptel preset with :agent-tool-policy.  See
`gptel-agent-define-tool-policy' for the rule syntax.")

(defvar-local gptel-agent-policy--inherited nil
  "Policy names and their original workspaces inherited by this conversation.")

(defvar gptel-agent-policy--definitions (make-hash-table :test #'eq))
(defconst gptel-agent-policy--result-explorer-name 'gptel-agent--result-explorer
  "Reserved policy identifying a result explorer's mandatory tool ceiling.")
(defvar gptel-agent-policy--result-explorer-rules)
(defvar-local gptel-agent-policy--inherited-snapshot nil
  "Compiled definitions retained by a live descendant.  Never serialized.")
(defvar gptel-agent-policy--snapshot nil
  "Alist of compiled policy definitions captured for the executing request.")
(defconst gptel-agent-policy--missing (make-symbol "absent argument"))
(defvar gptel-agent-policy--workspace)
(defvar gptel-agent-policy--argument-directory)
(defvar gptel-agent-policy--matching-deny nil
  "Treat an ambiguously decoded JSON value conservatively for a denial.")

(defun gptel-agent-policy-buffer-ceiling (&optional buffer)
  "Return the policy names and workspace roots applying in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((ceiling (copy-tree gptel-agent-policy--inherited)))
      (when gptel-agent-tool-policy
        (let ((root default-directory))
          (unless (file-remote-p root)
            (let (file-name-handler-alist)
              (setq root (file-truename root))))
          (let ((selected (cons gptel-agent-tool-policy root)))
            (setq ceiling (cons selected (delete selected ceiling))))))
      ceiling)))

(defun gptel-agent-policy--same-definition-p (left right)
  "Whether LEFT and RIGHT capture the same named rule version and workspace."
  (and (equal (car left) (car right)) (eq (cdr left) (cdr right))))

(defun gptel-agent-policy--freeze (ceiling &optional prior selected)
  "Capture definitions in CEILING, retaining those frozen in PRIOR.
SELECTED is an explicitly selected (NAME . WORKSPACE) entry.  Apply
its current definition even if a parent captured an earlier version.
An entry with a nil definition permanently denies that request's calls."
  (let ((snapshot (copy-sequence prior)))
    (dolist (entry ceiling snapshot)
      (when (or (equal entry selected) (not (assoc entry snapshot)))
        (cl-pushnew (cons entry
                          (if (eq (car entry) gptel-agent-policy--result-explorer-name)
                              gptel-agent-policy--result-explorer-rules
                            (gethash (car entry) gptel-agent-policy--definitions)))
                    snapshot :test #'gptel-agent-policy--same-definition-p)))))

(defun gptel-agent-policy-inherit (ceiling &optional snapshot)
  "Retain CEILING for subsequent requests in this buffer.
For a live descendant also retain its parent's compiled SNAPSHOT."
  (unless (and (proper-list-p ceiling)
               (cl-every (lambda (entry)
                           (and (consp entry) (car entry) (symbolp (car entry))
                                (stringp (cdr entry)) (file-name-absolute-p (cdr entry))))
                         ceiling))
    (error "Invalid inherited tool policy"))
  (dolist (entry ceiling)
    (cl-pushnew (copy-tree entry) gptel-agent-policy--inherited :test #'equal))
  (dolist (definition snapshot)
    (cl-pushnew definition gptel-agent-policy--inherited-snapshot
                :test #'gptel-agent-policy--same-definition-p)))

(defun gptel-agent-policy-protect-result-explorer ()
  "Retain the mandatory result-explorer ceiling in the current buffer.
Keep any separately selected user policy, which can further narrow access.
Call after applying child presets and their hooks."
  (let* ((gptel-agent-tool-policy gptel-agent-policy--result-explorer-name)
         (ceiling (list (car (gptel-agent-policy-buffer-ceiling)))))
    (gptel-agent-policy-inherit ceiling (gptel-agent-policy--freeze ceiling))))

(defun gptel-agent-policy--plist-p (value)
  "Whether VALUE is a plist with unique keyword keys."
  (and (proper-list-p value)
       (let (keys)
         (cl-loop for (key . rest) on value by #'cddr
                  always (and rest (keywordp key) (not (memq key keys))
                              (push key keys))))))

(defun gptel-agent-policy--path (value directory)
  "Return the canonical local path of VALUE relative to DIRECTORY."
  (when (and (stringp value) (not (string-empty-p value))
             (not (file-remote-p value)) (not (file-remote-p directory)))
    (let ((default-directory (file-name-as-directory directory))
          file-name-handler-alist)
      (directory-file-name (file-truename value)))))

(defun gptel-agent-policy--within-p (path base)
  "Whether canonical PATH is at or below canonical BASE on this filesystem."
  (or (equal path base) (string-prefix-p (file-name-as-directory base) path)
      ;; The filesystem can identify two spellings of an existing directory
      ;; (case, Unicode normalization, mount aliases).  Walk parents even for
      ;; proposed new files whose own attributes do not yet exist.
      (let ((base-attrs (file-attributes base)))
        (if base-attrs
            (let ((base-id (file-attribute-file-identifier base-attrs)))
              (cl-loop for node = path then parent
                       for attrs = (file-attributes node)
                       for parent = (directory-file-name (file-name-directory node))
                       thereis (and attrs (equal (file-attribute-file-identifier attrs) base-id))
                       until (equal node parent)))
          ;; A deny also covers case variants of a prospective directory.
          (and gptel-agent-policy--matching-deny
               (or (string-equal-ignore-case path base)
                   (string-prefix-p (file-name-as-directory base) path t))
               (let ((parent base))
                 (while (not (file-exists-p parent))
                   (setq parent (directory-file-name (file-name-directory parent))))
                 (file-name-case-insensitive-p parent)))))))

(defun gptel-agent-policy--under-p (value roots)
  "Whether VALUE is a local path at or below a member of ROOTS."
  (when-let* ((path (gptel-agent-policy--path value gptel-agent-policy--argument-directory))
              ((not (file-remote-p path))))
    (let ((paths (list path))
          (bases (delq nil
                       (mapcar (lambda (root)
                                 (gptel-agent-policy--path
                                  (if (eq root 'workspace) "." root)
                                  gptel-agent-policy--workspace))
                               roots))))
      ;; Some tools expand ".." before opening a path.  The OS follows
      ;; symlinks as it traverses components.  Cover both possible targets.
      (when (member ".." (split-string value "/" t))
        (let ((expanded (expand-file-name value gptel-agent-policy--argument-directory)))
          (cl-pushnew (gptel-agent-policy--path expanded gptel-agent-policy--argument-directory)
                      paths :test #'equal)))
      (funcall (if gptel-agent-policy--matching-deny #'cl-some #'cl-every)
               (lambda (target)
                 (and target (not (file-remote-p target))
                      (cl-some (lambda (base)
                                 (and (not (file-remote-p base))
                                      (gptel-agent-policy--within-p target base)))
                               bases)))
               paths))))

(defun gptel-agent-policy--origin (value &optional configuration)
  "Parse VALUE into (SCHEME HOST PORT), or nil for a malformed network URL.
When CONFIGURATION is non-nil, require a bare origin."
  (when (and (stringp value) (not (string-match-p "[[:space:][:cntrl:]\\\\]" value))
             ;; Check the authority before calling the permissive URL parser.
             (let ((case-fold-search nil))
               (string-match-p
                (concat "\\`[a-zA-Z][a-zA-Z0-9+.-]*://"
                        "\\(?:[a-zA-Z0-9._~!$&'()*+,;=:%-]+@\\)?"
                        "\\(?:[A-Za-z0-9._-]+\\|\\[[a-fA-F0-9:.]+\\]\\)"
                        "\\(?::[0-9]+\\)?\\(?:[/#?]\\|\\'\\)") value)))
    (let* ((url (url-generic-parse-url value))
           (port (url-port url)))
      (when (and (<= 1 port 65535)
                 (or (not configuration)
                     (and (not (url-user url)) (not (url-password url))
                          (not (url-target url)) (member (url-filename url) '("" "/")))))
        (list (downcase (url-type url)) (downcase (url-host url)) port)))))

(defun gptel-agent-policy--compile-fields (fields)
  "Compile named FIELDS, keeping their patterns for request schema checks."
  (unless (gptel-agent-policy--plist-p fields) (error "Invalid tool policy fields: %S" fields))
  (cl-loop for (key pattern) on fields by #'cddr
           collect (list key pattern (gptel-agent-policy--compile-value pattern))))

(defun gptel-agent-policy--match-fields (fields args)
  "Test the compiled FIELDS against the named values in ARGS."
  (cl-every (lambda (field)
              (funcall (nth 2 field)
                       (if-let* ((tail (plist-member args (car field))))
                           (cadr tail) gptel-agent-policy--missing)))
            fields))

(defun gptel-agent-policy--declared-p (pattern schema)
  "Check nested field names in PATTERN wherever SCHEMA declares them."
  (pcase pattern
    ((pred vectorp)
     (cl-every (lambda (item)
                 (gptel-agent-policy--declared-p item (plist-get schema :items))) pattern))
    ((and (pred consp) (guard (keywordp (car pattern))))
     (gptel-agent-policy--declared-p (cons 'object pattern) schema))
    (`(object . ,fields)
     (let ((properties (plist-get schema :properties))
           (extra (plist-get schema :additionalProperties)))
       (cl-loop for (key subpattern) on fields by #'cddr
                for property = (plist-member properties key)
                always (and (or property (plist-get schema :patternProperties)
                                (not (memq extra '(:json-false false))))
                            (gptel-agent-policy--declared-p
                             subpattern (or (cadr property) (and (listp extra) extra)))))))
    (`(every ,subpattern)
     (gptel-agent-policy--declared-p subpattern (plist-get schema :items)))
    (`(optional ,subpattern) (gptel-agent-policy--declared-p subpattern schema))
    (`(one-of . ,choices)
     (cl-every (lambda (choice) (gptel-agent-policy--declared-p choice schema)) choices))
    (_ t)))

(defun gptel-agent-policy--compile-literal (datum)
  "Compile an exact JSON literal DATUM, including nested arrays and objects."
  (cond
   ((vectorp datum)
    (let ((elements (mapcar #'gptel-agent-policy--compile-literal datum)))
      (lambda (value)
        (and (vectorp value) (= (length value) (length elements))
             (cl-every #'funcall elements value)))))
   ((consp datum)
    (unless (gptel-agent-policy--plist-p datum) (error "Invalid JSON literal: %S" datum))
    (let ((fields (cl-loop for (key literal) on datum by #'cddr
                           collect (list key literal (gptel-agent-policy--compile-literal literal)))))
      (lambda (value)
        (and (gptel-agent-policy--plist-p value) (= (length value) (* 2 (length fields)))
             (gptel-agent-policy--match-fields fields value)))))
   (t (gptel-agent-policy--compile-value datum))))

(defun gptel-agent-policy--compile-value (pattern)
  "Compile a trusted value PATTERN into a predicate."
  (pcase pattern
    ((or 't 'true) (lambda (value) (eq value t)))
    ((or 'false :json-false) (lambda (value) (eq value :json-false)))
    ((or 'null :null)
     (lambda (value)
       (or (eq value :null) (and gptel-agent-policy--matching-deny (null value)))))
    ((or (pred stringp) (pred numberp))
     (lambda (value) (equal pattern value)))
    ((or (pred vectorp) (and (pred consp) (guard (keywordp (car pattern)))))
     (gptel-agent-policy--compile-literal pattern))
    (`(rx . ,forms)
     (let ((regexp (rx-to-string `(seq string-start ,@forms string-end) t)))
       (lambda (value)
         (and (stringp value)
              (let ((case-fold-search nil)) (string-match-p regexp value))))))
    (`(one-of ,first . ,rest)
     (let ((choices (mapcar #'gptel-agent-policy--compile-value (cons first rest))))
       (lambda (value) (cl-some (lambda (choice) (funcall choice value)) choices))))
    (`(object . ,fields)
     (let ((fields (gptel-agent-policy--compile-fields fields)))
       ;; Some gptel parsers represent both {} and null as nil.
       (lambda (value)
         (and (or (consp value) (and gptel-agent-policy--matching-deny (null value)))
              (gptel-agent-policy--plist-p value)
              (gptel-agent-policy--match-fields fields value)))))
    (`(every ,element)
     (let ((test (gptel-agent-policy--compile-value element)))
       (lambda (value) (and (vectorp value) (not (recordp value))
                            (cl-every test value)))))
    (`(optional ,element)
     (let ((test (gptel-agent-policy--compile-value element)))
       (lambda (value) (or (eq value gptel-agent-policy--missing) (funcall test value)))))
    (`(under ,first . ,rest)
     (let ((roots (cons first rest)))
       (unless (cl-every (lambda (root)
                           (or (eq root 'workspace)
                               (and (stringp root) (not (string-empty-p root))
                                    (not (file-remote-p root)))))
                         roots)
         (error "Invalid local roots: %S" roots))
       (lambda (value) (gptel-agent-policy--under-p value roots))))
    (`(url-origin ,first . ,rest)
     (let ((origins (mapcar (lambda (url)
                              (or (gptel-agent-policy--origin url t)
                                  (error "Invalid URL origin: %S" url)))
                            (cons first rest))))
       (lambda (value) (member (gptel-agent-policy--origin value) origins))))
    (_ (error "Invalid tool policy value pattern: %S" pattern))))

(defun gptel-agent-policy--compile-rules (rules)
  "Group compiled RULES by tool name without consulting the tool registry."
  (unless (proper-list-p rules) (error "Expected a list of tool policy rules"))
  (let (compiled)
    (dolist (rule rules)
      (let ((name (plist-get rule :tool)))
        (unless (and (gptel-agent-policy--plist-p rule) (stringp name)
                     (not (string-empty-p name))
                     (cl-loop for (key _) on rule by #'cddr always (memq key '(:tool :args))))
          (error "Invalid tool policy rule: %S" rule))
        (push (gptel-agent-policy--compile-fields (plist-get rule :args))
              (alist-get name compiled nil nil #'equal))))
    compiled))

(defconst gptel-agent-policy--result-explorer-rules
  (cons (gptel-agent-policy--compile-rules
         '((:tool "ResultRead") (:tool "ResultGrep")))
        nil)
  "Canonical result-explorer rules, independent of user policy definitions.")

;;;###autoload
(cl-defun gptel-agent-define-tool-policy (name &key allow deny)
  "Define NAME as a reusable policy with ALLOW and DENY rules.
A rule has the form (:tool NAME :args (:KEY PATTERN ...)).  Only
mentioned keys are constrained.  Patterns can be JSON literals (strings,
numbers, true, false, null, vectors or keyword plists), (rx ...),
(one-of PATTERN ...),
(object :KEY PATTERN ...), (every PATTERN), (optional PATTERN),
(under ROOT ...) or (url-origin ORIGIN ...).  A ROOT can be the symbol
workspace or a local directory string.  Any matching denial takes
precedence.  Every protected call requires an explicit matching allow."
  (unless (and name (symbolp name) (not (keywordp name)))
    (error "Expected a symbol naming the tool policy"))
  (when (eq name gptel-agent-policy--result-explorer-name)
    (error "The result-explorer tool policy is reserved"))
  (let ((rules (cons (gptel-agent-policy--compile-rules allow)
                     (gptel-agent-policy--compile-rules deny))))
    ;; Install the request integration even when only this autoloaded public
    ;; entry point is used with an ordinary third-party gptel preset.
    (require 'gptel-agent-policy-request)
    (puthash name rules gptel-agent-policy--definitions))
  name)

(defun gptel-agent-policy--match-policy (rules name args schema required-field)
  "Match RULES against NAME and ARGS with the request-local SCHEMA.
Return `constrained' for an allow mentioning present REQUIRED-FIELD,
`allowed' for another matching allow, and nil for a denial."
  (let ((allows (cdr (assoc name (car rules))))
        (denies (cdr (assoc name (cdr rules)))))
    (when (and allows
               ;; An unknown constraint, including an optional one, must
               ;; never silently open this tool after a schema change.
               (cl-every (lambda (fields)
                           (cl-every (lambda (field)
                                       (when-let* ((spec (assq (car field) schema)))
                                         (gptel-agent-policy--declared-p (cadr field) (cdr spec))))
                                     fields))
                         (append denies allows))
               (not (let ((gptel-agent-policy--matching-deny t))
                      (cl-some (lambda (fields)
                                 (gptel-agent-policy--match-fields fields args)) denies))))
      (let (result)
        (dolist (fields allows result)
          (when (gptel-agent-policy--match-fields fields args)
            (cond ((and required-field (assq required-field fields)
                        (plist-member args required-field))
                   (setq result 'constrained))
                  ((null result) (setq result 'allowed)))))))))

(defun gptel-agent-policy--allows-p (ceiling tool args workspace &optional required-field)
  "Check request-local TOOL and named ARGS against CEILING in WORKSPACE.
If REQUIRED-FIELD is supplied, also require an explicitly matched allow
constraint on this present field in at least one policy."
  (if (null ceiling) (not required-field)
    (condition-case nil
        (let ((gptel-agent-policy--argument-directory workspace)
              (definitions (or gptel-agent-policy--snapshot
                               (gptel-agent-policy--freeze ceiling)))
              (name (gptel-tool-name tool))
              (schema (mapcar (lambda (arg) (cons (intern (concat ":" (plist-get arg :name))) arg))
                              (gptel-tool-args tool)))
              constrained)
          (and (gptel-agent-policy--plist-p args)
               (cl-loop for entry in ceiling
                        always
                        (let ((gptel-agent-policy--workspace (cdr entry))
                              (versions (cl-remove-if-not (lambda (def) (equal entry (car def)))
                                                          definitions)))
                          (and versions
                               (cl-loop for (_ . rules) in versions
                                        for result = (gptel-agent-policy--match-policy
                                                      rules name args schema required-field)
                                        when (eq result 'constrained) do (setq constrained t)
                                        always result))))
               (or (not required-field) constrained)))
      (error nil))))

(provide 'gptel-agent-policy)
;;; gptel-agent-policy.el ends here
