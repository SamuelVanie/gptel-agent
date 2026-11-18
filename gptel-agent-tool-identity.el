;;; gptel-agent-tool-identity.el --- Identify package-owned tools -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'gptel-request)

(defun gptel-agent--builtin-tool-name (tool)
  "Return TOOL's exposed name when it belongs to a gptel-agent category.
Request-local copies retain their category.  Other packages may register
the same exposed name with an entirely different argument schema."
  (when (and tool (member (gptel-tool-category tool)
                          '("gptel-agent" "gptel-agent-result-explorer")))
    (gptel-tool-name tool)))

(defun gptel-agent--find-builtin-tool (name tools)
  "Return NAME's effective tool in TOOLS when it belongs to gptel-agent.
Use gptel's first matching name even when an ambiguous preset contains
another tool with the same name later in the list."
  (let ((tool (cl-find name tools :key #'gptel-tool-name :test #'equal)))
    (and (gptel-agent--builtin-tool-name tool) tool)))

(defun gptel-agent--registered-builtin-tool (name)
  "Return the registered gptel-agent tool named NAME."
  (cl-loop for (_ . entries) in gptel--known-tools
           for tool = (cdr (assoc name entries))
           when (gptel-agent--builtin-tool-name tool) return tool
           finally (error "No registered gptel-agent tool named %s" name)))

(provide 'gptel-agent-tool-identity)
;;; gptel-agent-tool-identity.el ends here
