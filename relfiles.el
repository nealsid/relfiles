(setq relfiles-suffixes-alist
      '((java-mode . ("test" "model"))
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

(defun relfiles-compute-file-name-base (filename)
  (let* ((fn (file-name-base filename))
         (suffixes (alist-get major-mode relfiles-suffixes-alist))
         (removed-suffixes
          (cl-remove-if-not 'identity
                            ;; For each suffix, match it against the
                            ;; end of the filename base.  If it's a
                            ;; match, return a new string which
                            ;; replaces the suffix with an empty
                            ;; string.  If it's not a match, return
                            ;; nil.  Nil entries are removed (see
                            ;; above remove-if-not) and the remaining
                            ;; entry is considered the filename base,
                            ;; or, if all entries are nil, meaning no
                            ;; suffixes matched, take the filename as
                            ;; passed in the argument as the base
                            ;; filename.
                            (mapcar (lambda (suffix)
                                      (let ((suffix-regexp (concat suffix "$")))
                                        (if (string-match suffix-regexp fn)
                                            (replace-regexp-in-string suffix-regexp "" fn)
                                          nil))) suffixes))))
    (cond ((not removed-suffixes) fn)
          ((nth 0 removed-suffixes)))
    ))


(defun relfiles-compute-search-directories-for-filename (filename)
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
