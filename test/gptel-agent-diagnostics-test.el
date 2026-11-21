;;; gptel-agent-diagnostics-test.el --- Flymake tool results -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'flymake)
(require 'gptel-agent)

(ert-deftest gptel-agent-diagnostics-reports-location-without-moving-or-widening-user ()
  (with-temp-buffer
    (insert "unrelated first line\nbroken expression\nlast visible line\n")
    (setq buffer-file-name "/tmp/gptel-diagnostic.el")
    (let ((diagnostic (flymake-make-diagnostic (current-buffer) 22 39 :error "Invalid expression")))
      (goto-char 40)
      (narrow-to-region 40 (point-max))
      (let ((position (point)) (minimum (point-min)) (maximum (point-max)))
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) '(transient . "/tmp/")))
                  ((symbol-function 'flymake--project-diagnostics) (lambda (_) (list diagnostic))))
          (should (equal (gptel-agent--flymake-diagnostics)
                         "File: /tmp/gptel-diagnostic.el:2\nSeverity: :error\nMessage: Invalid expression\n---\nbroken expression")))
        (should (= position (point)))
        (should (= minimum (point-min)))
        (should (= maximum (point-max)))))))

(ert-deftest gptel-agent-diagnostics-keeps-warning-and-optional-note-filter ()
  (with-temp-buffer
    (insert "warning expression\nnote expression\n")
    (setq buffer-file-name "/tmp/gptel-diagnostic-filter.el")
    (let ((diagnostics (list (flymake-make-diagnostic (current-buffer) 1 19 :warning "Warning detail")
                             (flymake-make-diagnostic (current-buffer) 20 35 :note "Note detail"))))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) '(transient . "/tmp/")))
                ((symbol-function 'flymake--project-diagnostics) (lambda (_) diagnostics)))
        (let ((ordinary (gptel-agent--flymake-diagnostics))
              (all (gptel-agent--flymake-diagnostics t)))
          (should (string-match-p "Warning detail\n---\nwarning expression" ordinary))
          (should-not (string-match-p "Note detail" ordinary))
          (should (string-match-p ":2\nSeverity: :note\nMessage: Note detail\n---\nnote expression" all)))))))

(provide 'gptel-agent-diagnostics-test)
;;; gptel-agent-diagnostics-test.el ends here
