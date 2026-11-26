;;; gptel-agent-instructions-workspace-test.el --- Chat and source scopes -*- lexical-binding: t; -*-

(require 'gptel-agent-instructions-test)
(require 'gptel-agent-review-aside-test)

(ert-deftest gptel-agent-instructions-workspace-chat-vs-source-file ()
  (gptel-agent-instructions-test--with-project
    (dolist (chat '(t nil))
      (let ((path (expand-file-name (if chat "transcript.org" "library.el") back)))
        (gptel-agent-instructions-test--write path "Explain this change")
        (with-current-buffer (find-file-noselect path)
          (unwind-protect
              (progn
                (when chat (gptel-mode 1))
                (gptel-agent--copy-parent-request-config parent (current-buffer))
                (setq-local default-directory front
                            gptel-use-tools nil
                            gptel-prompt-transform-functions '(gptel-agent-instructions--transform))
                (goto-char (point-max))
                (gptel-send)
                (let ((payload (prin1-to-string (car requests))))
                  (should (string-match-p "ROOT-INSTRUCTION" payload))
                  (should (string-match-p (if chat "FRONT-INSTRUCTION" "BACK-INSTRUCTION") payload))
                  (should-not (string-match-p (if chat "BACK-INSTRUCTION" "FRONT-INSTRUCTION") payload))))
            (kill-buffer)))))))

(ert-deftest gptel-agent-instructions-workspace-aside-with-nested-proposal ()
  (gptel-aside-test--with-chat
    (gptel-review-test--submit "Write" '(:file_path "nested/new.txt" :content "proposal"))
    (gptel-agent-instructions-test--write (expand-file-name "AGENTS.md") "ROOT-INSTRUCTION")
    (gptel-agent-instructions-test--write (expand-file-name "nested/AGENTS.md") "PROPOSAL-INSTRUCTION")
    (gptel-agent-instructions-test--write (expand-file-name "docs/AGENTS.md") "DOCS-INSTRUCTION")
    (gptel-agent-instructions-test--write (expand-file-name "docs/notes.txt") "ATTACHED-NOTES")
    (setq-local gptel-agent-use-project-instructions t
                gptel-use-context 'user
                gptel-context (list (list (expand-file-name "docs/notes.txt"))))
    (setq aside (gptel-agent-review-aside
                 gptel-agent-review--queue (gptel-review-test--file 2) "Explain this proposal"))
    (let ((data (gptel-aside-test--data (car requests))))
      (dolist (text '("ROOT-INSTRUCTION" "PROPOSAL-INSTRUCTION" "DOCS-INSTRUCTION" "ATTACHED-NOTES"))
        (should (string-match-p text data))))))

(provide 'gptel-agent-instructions-workspace-test)
;;; gptel-agent-instructions-workspace-test.el ends here
