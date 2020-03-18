;; .emacs

; TODO: fix setupEmacs() to use this: (add-to-list 'load-path "...")

;; Macaulay2 start
(load "~/.emacs-Macaulay2" t)

(custom-set-variables
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "\
-- Welcome to Macaulay2!\n\
-- This buffer is for scratchwork, and for Macaulay2 evaluations with \\[M2-send-to-program].\n\
-- You can save this buffer into a file with \\[save-buffer], or open a file with \\[find-file].\n\
-- Use \\[other-window] to cycle through visible buffers or \\[switch-to-buffer] to twitch to other buffers.\n\
-- To exit, close Emacs with \\[save-buffers-kill-terminal].

-* Here are some sample kcommands:\n\
  R = ZZ/101[a,b,c,d]\n\
  I = ideal(a^2-b*c, a^3-b^3, a^4-b*d^3, a^5-c^2*d^3)\n\
  J = ideal groebnerBasis I;\n\
  netList J_*\n\
\n\
  -- Some examples of rings\n\
  A = ZZ/32003[a..g]\n\
  B = QQ[x_1..x_6]\n\
  C = ZZ/101[vars(0..12)]\n\
*-\n\n"))

(setq M2-demo-buffer "*scratch*")
(setq initial-major-mode 'M2-mode)
;; Macaulay2 end

;; start on eshell, M2, and scratch
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (eshell)
	    (call-interactively 'M2)
	    (goto-line 8 "*scratch*")))

;; set decent font size
(custom-set-faces
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 143 :width normal)))))

;; Macaulay 2 start
(load "~/.emacs-Macaulay2" t)
;; Macaulay 2 end
