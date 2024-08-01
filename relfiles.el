(setq relfiles-suffixes-alist
      '((java-mode . ("test"))
        (c++-mode . ("_test"))))

(setq relfiles-extensions-alist
      '((java-mode . ("java"))
        (c++-mode . ("cc" "cpp" "cxx" "c" "h" "hpp" "hxx"))))

(setq relfiles-search-directories-alist
      '((java-mode . (relfiles-parallel-java-tree))
        (c++-mode . ("tests"))))

(defun relfiles-parallel-java-tree (x)
  (cond ((string-match "/java/" x) (file-name-directory (string-replace "/java/" "/javatests/" x)))
        ((string-match "/javatests/" x) (file-name-directory (string-replace "/java/" "/javatests/" x)))
        (x)))

(defun relfiles-compute-directories-for-filename (filename)
  (with-current-buffer (find-file filename)
    (let ((file-path (file-name-directory filename))
          (relfiles-directories (alist-get major-mode relfiles-search-directories-alist)))
      (mapcar (lambda (pathname-or-function)
                (if (functionp pathname-or-function)
                    (apply pathname-or-function (list filename))
                  (concat file-path pathname-or-function))) relfiles-directories))))

(defun relfiles-for-filename (filename)
  (with-current-buffer (find-file filename)
    (let ((relfiles-exts ((alist-get major-mode relfiles-extensions-alist)))
          (relfiles-suffixes ((alist-get major-mode relfiles-suffixes-alist)))
          (filename-base (file-name-base filename))
          (filename-directory (file-name-directory filename))
          )

      )
    )
  )
(defun relfiles (fn)
  )
