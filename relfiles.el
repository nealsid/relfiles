(setq relfiles-basename-suffixes
      '("test" "controller" "model" "view"))

(setq relfiles-xform-alist
      '("%project_root%/%reldir%/%basename%.el"
        "%project_root%/%reldir%/%basename~(_test$)%.el"))

(defun apply-xform-to-fn (fn xform)

  )
(defun transform-fn-by-xforms (fn)
  (let      ((fn-dir (file-name-directory fn))
        (fn-filename (file-name-nondirectory fn))
       (fn-extension (file-name-extension fn)))
    (mapcar)
    ))

(defun relfiles (fn)
  )
