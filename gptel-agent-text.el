;;; gptel-agent-text.el --- Unicode text for tool results -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun gptel-agent--json-text (text)
  "Return TEXT as Unicode suitable for a JSON string.
Recover UTF-8 from unibyte strings and runs of Emacs raw-byte characters.
Represent undecodable bytes and other non-Unicode characters with U+FFFD."
  (unless (multibyte-string-p text)
    (setq text (decode-coding-string text 'utf-8-unix t)))
  (let ((non-scalar "[^\0-\uD7FF\uE000-\U0010FFFF]"))
    (if (not (string-match-p non-scalar text)) text
      ;; Raw bytes can coexist with decoded text, for example when a tool
      ;; combines a filename with output from a binary process.
      (setq text (replace-regexp-in-string
                  "[\x3FFF80-\x3FFFFF]+"
                  (lambda (raw)
                    (decode-coding-string (string-to-unibyte raw) 'utf-8-unix))
                  text t t))
      ;; Emacs can decode extended UTF-8 to characters above U+10FFFF.
      (replace-regexp-in-string non-scalar "\uFFFD" text t t))))

(provide 'gptel-agent-text)
;;; gptel-agent-text.el ends here
