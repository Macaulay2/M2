(setq load-path 
      (append
       '( "/capybara/share/emacs/site-lisp/" )
       load-path
       ))
(global-set-key [ f12 ] 'M2)
(load "M2-init.el" t)
