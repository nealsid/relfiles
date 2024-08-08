(defmacro with-project-and-directory (project-dir-name-binding &rest forms)
  "Macro to wrap a test case body that creates bindings for the
unique project directory for the test case."
  `(let* ((,project-dir-name-binding (concat (make-temp-file "relfiles-test-" t) "/"))
     (progn
       ,@forms))))

(defmacro with-current-buffer-close (buffer-or-name &rest body)
  `(progn
     (with-current-buffer ,buffer-or-name ,@body)
     (kill-buffer ,buffer-or-name)))

(ert-deftest related-files-same-directory ()
  "Opens a project with one file"
  (with-project-and-directory
   project-dir
   (let ((development-file (concat project-dir "main.cpp"))
         (test-file (concat project-dir "main_test.cpp")))
     (with-current-buffer-close
      (find-file-noselect development-file)
      (make-empty-file development-file)
      (make-empty-file test-file)
      (relfiles-visit-related-files (buffer-file-name))
      (should (eq (current-buffer) (get-file-buffer test-file)))))))
