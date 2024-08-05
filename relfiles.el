(defvar-local relfiles-suffixes-alist
          '((java-mode . ("test" "model"))
            (c++-mode . ("_test"))))

(defvar-local relfiles-extensions-alist
      '((java-mode . ("java"))
        (c++-mode . ("cc" "cpp" "cxx" "c" "h" "hpp" "hxx"))))

(defvar-local relfiles-search-directories-alist
      '((java-mode . (relfiles-parallel-java-tree))
        ;; Have to include trailing / to be consistent with
        ;; default-directory variable.
        (c++-mode . ("tests/" "../include/" "../../../../../../usr/jjjinclude/" "../src/"))))

(defun relfiles-parallel-java-tree (x)
  (cond ((string-match "/java/" x) (file-name-directory (string-replace "/java/" "/javatests/" x)))
        ((string-match "/javatests/" x) (file-name-directory (string-replace "/javatests/" "/java/" x)))
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
                                          nil)))
                                    suffixes))))
    (cond ((not removed-suffixes) fn)
          ((nth 0 removed-suffixes)))
    ))

(defun relfiles-compute-search-directories-for-filename (filename)
  (with-current-buffer (find-file-noselect filename)
    (let ((file-path (file-name-directory filename))
          (relfiles-directories (alist-get major-mode relfiles-search-directories-alist)))
      (append (mapcar (lambda (pathname-or-function)
                        (if (functionp pathname-or-function)
                            (apply pathname-or-function (list filename))
                          (concat file-path pathname-or-function)))
                      relfiles-directories)
              (list default-directory)))))

;; It's easiest to read the comments for this function from the
;; innermost mapcar outward, i.e., scanning up instead of down
(defun relfiles-for-filename (filename)
  (with-current-buffer (find-file-noselect filename)
    (let ((search-directories (relfiles-compute-search-directories-for-filename filename))
          (filename-base (relfiles-compute-file-name-base filename))
          (suffixes (alist-get major-mode relfiles-suffixes-alist))
          (extensions (alist-get major-mode relfiles-extensions-alist)))
      ;; 3. For each pathname in 2, generate a pathname for each extension configured for this major-mode and remove ones that do not exist.
      (cl-remove-if-not 'file-attributes
                        (delete-dups
                         (mapcar 'expand-file-name
                                 (flatten-tree (mapcar (lambda (filename-no-extension)
                                                         (mapcar (lambda (extension)
                                                                   (concat filename-no-extension "." extension))
                                                                 extensions))
                                                       ;; 2. For each pathname in 1, construct an entry per suffix (plus the empty string to
                                                       ;; find files that don't have a suffix).  Flatten the result since the lambda for the
                                                       ;; outer mapcar is calling mapcar, which results in a list for each element.
                                                       (flatten-tree (mapcar (lambda (directory-and-base)
                                                                               (mapcar (lambda (suffix)
                                                                                         (concat directory-and-base suffix))
                                                                                       (append suffixes '(""))))
                                                  ;;; 1. For each search directory, construct a pathname that contains the filename base,
                                                                             ;; filtering out directories that don't exist.
                                                                             (cl-remove-if-not 'identity (mapcar (lambda (directory)
                                                                                                                   (if (not (eq (file-attributes directory) nil))
                                                                                                                       (concat directory filename-base)
                                                                                                                     nil))
                                                                                                                 search-directories))))))))))))

(defun relfiles-visit-related-files (fn)
  (let* ((related-files (relfiles-for-filename fn))
         (fn-index (cl-position fn related-files :test 'string-equal-ignore-case)))
    (message "%s" related-files)
    (find-file (nth (% (+ 1 fn-index) (length related-files)) related-files))))
