;;; gptel-agent-encoding-test.el --- Lossless file edits -*- lexical-binding: t; -*-

(require 'gptel-agent-edit-test)

(defun gptel-encoding-test--bytes (path)
  "Return PATH's literal bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(ert-deftest gptel-edit-unencodable-write-preserves-file-and-history ()
  (gptel-edit-test--with-files nil
    (let* ((original ";; -*- coding: iso-latin-1-unix -*-\ncafé\n")
           (coding-system-for-write 'iso-latin-1-unix))
      (with-temp-file "one" (insert original))
      (set-file-modes "one" #o640)
      (let ((bytes (gptel-encoding-test--bytes "one"))
            (buffer (find-file-noselect "one"))
            (err (should-error (gptel-agent--write-file "one" (concat original "😀\n")))))
        (should (string-match-p "Cannot encode" (error-message-string err)))
        (should (string-match-p "iso-latin-1" (error-message-string err)))
        (should (string-match-p "one" (error-message-string err)))
        (should (equal (gptel-encoding-test--bytes "one") bytes))
        (should (= (file-modes "one") #o640))
        (should (equal (with-current-buffer buffer (buffer-string)) original))
        (should-not (file-exists-p gptel-agent-history-directory))))))

(ert-deftest gptel-edit-unencodable-batch-writes-nothing ()
  (gptel-edit-test--with-files '(("first" . "unchanged\n") ("obsolete" . "keep\n"))
    (let ((coding-system-for-write 'iso-latin-1-dos))
      (with-temp-file "last"
        (insert ";; -*- coding: iso-latin-1-dos -*-\ncafé\n")))
    (let ((before (mapcar (lambda (path) (cons path (gptel-encoding-test--bytes path)))
                          '("first" "obsolete" "last"))))
      (should-error
       (gptel-agent--edit-files
        "batch" nil "." nil nil
        [(:operation "replace" :file_path "first" :old_str "unchanged" :new_str "changed")
         (:operation "delete" :file_path "obsolete")
         (:operation "write" :file_path "nested/new" :content "new\n")
         (:operation "insert" :file_path "last" :line_number -1 :new_str "😀\n")]))
      (dolist (file before)
        (should (equal (gptel-encoding-test--bytes (car file)) (cdr file)))))
    (should-not (file-exists-p "nested"))
    (should-not (file-exists-p gptel-agent-history-directory))))

(ert-deftest gptel-edit-encodable-text-preserves-coding-and-restores ()
  (dolist (coding '(iso-latin-1-dos utf-8-dos))
    (gptel-edit-test--with-files nil
      (let* ((header (format ";; -*- coding: %s -*-\n" coding))
             (original (concat header "café\n"))
             (replacement (concat header (if (eq coding 'utf-8-dos) "été 😀\n" "été\n")))
             (coding-system-for-write coding))
        (with-temp-file "one" (insert original))
        (let ((bytes (gptel-encoding-test--bytes "one")))
          (gptel-agent--write-file "one" replacement)
          (should (equal (gptel-encoding-test--bytes "one")
                         (encode-coding-string replacement coding)))
          (gptel-agent-history-restore (car (gptel-agent-history--entries)))
          (should (equal (gptel-encoding-test--bytes "one") bytes)))))))

(ert-deftest gptel-edit-auto-encoding-allows-unicode-in-ascii-and-new-files ()
  (gptel-edit-test--with-files '(("ascii" . "old\n"))
    (gptel-agent--edit-files
     "batch" nil "." nil nil
     [(:operation "write" :file_path "ascii" :content "café 😀\n")
      (:operation "write" :file_path "new" :content "雪\n")])
    (should (equal (gptel-edit-test--read "ascii") "café 😀\n"))
    (should (equal (gptel-edit-test--read "new") "雪\n"))
    (gptel-agent-history-restore (car (gptel-agent-history--entries)))
    (should (equal (gptel-encoding-test--bytes "ascii") "old\n"))
    (should-not (file-exists-p "new"))))

(provide 'gptel-agent-encoding-test)
;;; gptel-agent-encoding-test.el ends here
