(defmacro with-test-directory (dir-name-binding &rest forms)
  "Macro to wrap a test case body that creates bindings for the
unique project directory for the test case."
  `(let* ((,dir-name-binding (concat (make-temp-file "relfiles-test-" t) "/")))
     ,@forms))

(defmacro with-current-buffer-close (buffer-or-name &rest body)
  `(progn
     (with-current-buffer ,buffer-or-name ,@body)
     (kill-buffer ,buffer-or-name)))

(ert-deftest related-files-no-related-files ()
  "Test related files when related files don't exist"
  (with-test-directory
   project-dir
   (let* ((query-about-changed-file nil) ;; We touch the files after
                                         ;; initially visiting them,
                                         ;; so make sure Emacs doesn't
                                         ;; ask about reverting the
                                         ;; buffer when visiting them
                                         ;; again later on
          (development-file (concat project-dir "main.cpp"))
          (development-file-2 (concat project-dir "main_1.cpp"))
          (dev-file-buffer (find-file-noselect development-file))
          (dev-file-2-buffer (find-file-noselect development-file-2)))
     (make-empty-file development-file)
     (make-empty-file development-file-2)

     (with-current-buffer-close dev-file-buffer
      (relfiles-visit-related-files-for-fn (buffer-file-name))
      (should (eq (current-buffer) dev-file-buffer))
      (kill-buffer dev-file-2-buffer)))))

(ert-deftest related-files-same-directory-c++ ()
  "Test related files in the same directory in C++ mode"
  (with-test-directory
   project-dir
   (let* ((query-about-changed-file nil) ;; We touch the files after
                                         ;; initially visiting them,
                                         ;; so make sure Emacs doesn't
                                         ;; ask about reverting the
                                         ;; buffer when visiting them
                                         ;; again later on
          (header-file (concat project-dir "main.h"))
          (development-file (concat project-dir "main.cpp"))
          (test-file (concat project-dir "main_test.cpp"))
          (header-file-buffer (find-file-noselect header-file))
          (test-file-buffer (find-file-noselect test-file))
          (dev-file-buffer (find-file-noselect development-file)))
     (make-empty-file header-file)
     (make-empty-file development-file)
     (make-empty-file test-file)

     (with-current-buffer-close dev-file-buffer
      (relfiles-visit-related-files-for-fn (buffer-file-name))
      (should (eq (current-buffer) header-file-buffer))
      (relfiles-visit-related-files-for-fn (buffer-file-name))
      (should (eq (current-buffer) test-file-buffer))
      (relfiles-visit-related-files-for-fn (buffer-file-name))
      (should (eq (current-buffer) dev-file-buffer))
      (kill-buffer header-file-buffer)
      (kill-buffer test-file-buffer)))))

(ert-deftest related-files-java ()
  "Test related files in Java mode"
  (with-test-directory
   project-dir
   (let* ((query-about-changed-file nil) ;; We touch the files after
                                         ;; initially visiting them,
                                         ;; so make sure Emacs doesn't
                                         ;; ask about reverting the
                                         ;; buffer when visiting them
                                         ;; again later on
          (development-file (concat project-dir "src/main/java/com/package/Main.java"))
          (test-file (concat project-dir "src/test/java/com/package/MainTest.java"))
          (test-file-buffer (find-file-noselect test-file t))
          (dev-file-buffer (find-file-noselect development-file t)))
     (make-empty-file development-file t)
     (make-empty-file test-file t)

     (with-current-buffer-close dev-file-buffer
      (relfiles-visit-related-files-for-fn (buffer-file-name))
      (should (eq (current-buffer) test-file-buffer))
      (relfiles-visit-related-files-for-fn (buffer-file-name))
      (should (eq (current-buffer) dev-file-buffer))
      (kill-buffer test-file-buffer)))))

(ert-deftest related-files-c++-per-directory-config ()
         "Test related files with a configuration specified by Emacs
directory local variables"
         (with-test-directory
          project-dir
          (let* ((dirlocals-file (concat project-dir ".dir-locals.el"))
                 (directory-local-config "((nil . ((relfiles-suffixes-alist .
                                ((java-mode . (\"Test\" \"Model\"))
                                 (c++-mode . (\"_tests\"))
                                 (c-mode . (\"_tests\"))
                                 (emacs-lisp-mode . (\"-test\")))))))"))
            ;; Create the directory local variables file
            (with-current-buffer-close (find-file-noselect dirlocals-file)
              (insert directory-local-config)
              (save-buffer))

            ;; Open files in our test project, with
            ;; safe-local-variable-directories set so that Emacs marks
            ;; the directory local variables and values as safe.
            (let* ((safe-local-variable-directories `(,project-dir))
                   (query-about-changed-file nil)
                   (header-file (concat project-dir "main.h"))
                   (development-file (concat project-dir "main.cpp"))
                   (test-file (concat project-dir "main_test.cpp"))
                   (tests-file (concat project-dir "main_tests.cpp"))
                   (header-file-buffer (find-file-noselect header-file))
                   (dev-file-buffer (find-file-noselect development-file))
                   (test-file-buffer (find-file-noselect test-file))
                   (tests-file-buffer (find-file-noselect tests-file)))
              (make-empty-file header-file)
              (make-empty-file development-file)
              (make-empty-file test-file)
              (make-empty-file tests-file)

              (with-current-buffer-close dev-file-buffer
               (relfiles-visit-related-files-for-fn (buffer-file-name))
               (should (eq (current-buffer) header-file-buffer))
               (relfiles-visit-related-files-for-fn (buffer-file-name))
               (should (eq (current-buffer) tests-file-buffer))
               (relfiles-visit-related-files-for-fn (buffer-file-name))
               (should (eq (current-buffer) dev-file-buffer))
               (kill-buffer test-file-buffer)
               (kill-buffer tests-file-buffer)
               (kill-buffer header-file-buffer))))))
