;; .emacs

(custom-set-variables
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "\
-- Welcome to Macaulay2!
-- This buffer is for scratchwork, and for Macaulay2 evaluations with \\[M2-send-to-program].
-- You can save this buffer into a file with \\[save-buffer], or open a file with \\[find-file].
-- Use \\[other-window] to cycle through visible buffers or \\[switch-to-buffer] to switch to other buffers.
-- To exit, close Emacs with \\[save-buffers-kill-terminal].

-- Here are some sample commands:
R = ZZ/101[a,b,c,d]
I = ideal(a^2-b*c, a^3-b^3, a^4-b*d^3, a^5-c^2*d^3)
J = ideal groebnerBasis I;
netList J_*

-- Some examples of rings
A = ZZ/32003[a..g]
B = QQ[x_1..x_6]
C = ZZ/101[vars(0..12)]\n\n"))

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
