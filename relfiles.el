(defvar-local relfiles-suffixes-alist
          '((java-mode . ("Test" "Model"))
            (c++-mode . ("_test"))
            (c-mode . ("_test"))))

(defvar-local relfiles-extensions-alist
      '((java-mode . ("java"))
        (c++-mode . ("cc" "cpp" "cxx" "c" "h" "hpp" "hxx"))
        (c-mode . ("cc" "cpp" "cxx" "c" "h" "hpp" "hxx"))))

(defvar-local relfiles-search-directories-alist
      '((java-mode . (relfiles-parallel-java-tree))
        ;; Have to include trailing / to be consistent with
        ;; default-directory variable.
        (c++-mode . ("../test/" "../include/" "../src/"))))

(defmacro log-and-return (x)
  `(progn
     (let ((return-value ,x))
       (message "nealsid: %s" return-value)
       return-value)))

(defun relfiles-parallel-java-tree (x)
  (cond ((string-match "/src/main/" x) (file-name-directory (string-replace "/src/main/" "/src/test/" x)))
        ((string-match "/src/test/" x) (file-name-directory (string-replace "/src/test/" "/src/main/" x)))
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

(defun relfiles-for-filename (filename)
  (with-current-buffer (find-file-noselect filename)
    (let ((search-directories (relfiles-compute-search-directories-for-filename filename))
          (filename-base (relfiles-compute-file-name-base filename))
          (suffixes (alist-get major-mode relfiles-suffixes-alist))
          (extensions (alist-get major-mode relfiles-extensions-alist)))
      ;; 6. Remove nonexistent files
      (cl-remove-if-not 'file-attributes
                        ;; 5. Delete duplicates from the list after canonicalization.
                        (delete-dups
                         ;; 4. Expand the file name (i.e. canonicalizing filenames to remove .., for instance)
                         (mapcar 'expand-file-name
                                 ;; 3. Append extensions to each list item from 2
                                 (log-and-return
                                  (append-extensions
                                   ;; 2. Remove any nil entries (signifying directories that don't exist) and then append suffixes to each item from 1.
                                   (log-and-return
                                    (append-suffixes (cl-remove-if-not 'identity
                                                                       ;; 1. Append the filename base to the search directories
                                                                       (log-and-return (append-filename-base-to-directories search-directories filename-base)))
                                                     (append suffixes '(""))))
                                   extensions))))))))

(defun append-extensions (filenames-with-suffixes extensions)
  (flatten-tree (mapcar (lambda (filename-no-extension)
                          (mapcar (lambda (extension)
                                    (concat filename-no-extension "." extension))
                                                                 extensions))
                        filenames-with-suffixes)))

(defun append-suffixes (directories-and-base suffixes)
  (flatten-tree
   (mapcar (lambda (directory-and-base)
             (mapcar (lambda (suffix)
                       (concat directory-and-base suffix))
                     suffixes))
           directories-and-base)))

(defun append-filename-base-to-directories (directories filename-base)
  (mapcar (lambda (directory)
            (if (not (eq (file-attributes directory) nil))
                (concat directory filename-base)
              nil))
          directories))

(defun relfiles-visit-related-files (fn)
  (interactive)
  (let* ((related-files (relfiles-for-filename fn))
         (fn-index (cl-position fn related-files :test 'string-equal-ignore-case)))
    (message "nealsid: %s" related-files)
    (message "nealsid: %s" fn-index)
    (find-file (nth (% (+ 1 fn-index) (length related-files)) related-files))))
