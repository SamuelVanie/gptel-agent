;;; gptel-agent-hook-discovery-test.el --- Agent hook discovery -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent)
(require 'gptel-transient)

(defvar gptel-agent-hook-discovery-test--events nil)

(defun gptel-agent-hook-discovery-test--hook (id)
  "Record evaluation of ID's expression and return a hook recording its call."
  (push (list id 'evaluated) gptel-agent-hook-discovery-test--events)
  (lambda () (push (list id gptel-system-prompt) gptel-agent-hook-discovery-test--events)))

(ert-deftest gptel-agent-hook-discovery-applies-only-selected-preset ()
  (let* ((root (make-temp-file "gptel-agent-hooks-" t))
         (md (expand-file-name "gptel-plan.md" root))
         (org (expand-file-name "ask.org" root))
         (gptel-agent-dirs (list root)) (gptel-agent-skill-dirs nil)
         gptel-agent--agents gptel-agent--agent-files gptel-agent--file-cache
         gptel-agent--registered-agents gptel-agent--additional-subagents
         gptel--known-presets gptel-agent-hook-discovery-test--events)
    (unwind-protect
        (with-temp-buffer
          (setq default-directory (file-name-as-directory root))
          (with-temp-file md
            (insert "---\npre: \"(gptel-agent-hook-discovery-test--hook 'plan)\"\n---\nPlan"))
          (with-temp-file org
            (insert ":PROPERTIES:\n:POST: (gptel-agent-hook-discovery-test--hook 'ask)"
                    "\n:END:\nAsk"))
          ;; Exercise cold reads, the metadata/full cache filled by refresh,
          ;; registration of project presets and subsequent display lookups.
          (dotimes (_ 2)
            (dolist (file (list md org))
              (should (gptel-agent-read-file file nil t))
              (should (plist-get (cdr (gptel-agent-read-file file)) :system)))
            (gptel-agent-update)
            (dolist (preset '(gptel-plan ask))
              (should (assq preset gptel--known-presets))
              (should (plist-get (gptel-get-preset preset) :system))
              (gptel--preset-mismatch-p preset))
            (should-not gptel-agent-hook-discovery-test--events))
          (setq-local gptel-system-prompt "Initial")
          (let ((setter (lambda (sym val) (set (make-local-variable sym) val))))
            (gptel-preset 'gptel-plan setter)
            (should (equal gptel-agent-hook-discovery-test--events
                           '((plan "Initial") (plan evaluated))))
            ;; An edited hook invalidates the cache without running at refresh.
            (with-temp-file org
              (insert ":PROPERTIES:\n:POST: (gptel-agent-hook-discovery-test--hook 'revised)"
                      "\n:END:\nRevised"))
            (set-file-times org (time-add (current-time) 2))
            (setq gptel-agent-hook-discovery-test--events nil)
            (gptel-agent-update)
            (should-not gptel-agent-hook-discovery-test--events)
            (gptel-preset 'ask setter)
            (should (equal gptel-agent-hook-discovery-test--events
                           '((revised "Revised") (revised evaluated))))
            ;; Reapplying the same cached definition calls the resolved hook.
            (setq gptel-agent-hook-discovery-test--events nil)
            (gptel-agent-update)
            (gptel-preset 'ask setter)
            (should (equal gptel-agent-hook-discovery-test--events '((revised "Revised"))))))
      (delete-directory root t))))

(provide 'gptel-agent-hook-discovery-test)
;;; gptel-agent-hook-discovery-test.el ends here
