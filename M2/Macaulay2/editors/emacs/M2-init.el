;; Setup M2.el for autoloading

(autoload 'M2             "M2" "Run Macaulay2 in an emacs buffer" t)
(autoload 'M2-mode        "M2" "Macaulay2 editing mode" t)
(autoload 'm2-mode        "M2" "Macaulay2 editing mode, name in lower case" t)
(autoload 'm2-comint-mode "M2" "Macaulay2 command interpreter mode, name in lower case" t)
(add-to-list 'auto-mode-alist '("\\.m2\\'" . M2-mode))

;; Uncomment these lines to enable syntax highlighting for the interpreter language
;(autoload 'D-mode "D-mode" "Editing mode for the interpreter language" t)
;(add-to-list 'auto-mode-alist '("\\.dd?\\'" . D-mode))
