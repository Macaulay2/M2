;;; M2.el -- run Macaulay 2 as a command interpreter in an Emacs buffer

;; Macaulay 2 makes no attempt to wrap long output lines, so we provide
;; functions which make horizontal scrolling easier.

(require 'comint)
(defvar M2-demo-buffer "ann.m2"
  "The buffer from which lines are obtained by M2-send-to-program when the
cursor is at the end of the buffer.  Set it with M2-set-demo-buffer."
  )
(defvar M2-map (copy-keymap comint-mode-map) 
  "local key map for Macaulay 2 command interpreter buffers")
(define-key M2-map [ f2 ] 'M2-position-point)

(defun M2-comint-run (program &rest args)
  "Run PROGRAM in a comint buffer with ARGS and switch to it."
  (let ((name (file-name-nondirectory program)))
    (switch-to-buffer (apply 'make-comint (append (list name program nil) args)))
    (run-hooks (intern-soft (concat "comint-" name "-hook")))))

(defun M2()
  "Run Macaulay 2 in a buffer."
  (interactive)
  (M2-comint-run "M2" "-tty")
  )

(defvar M2-usual-jog 30 
  "Usual distance scrolled by M2-jog-left and M2-jog-right")
(defvar M2-comint-prompt-regexp
  (concat "^"
	  "[ \t]*"
	  "\\("
	  "[io][0-9]+ [:=] "
	  "\\)"
	  "?"
	  )
  "Regular expression used to recognize the Macaulay 2 prompt."
  )
(defun M2-left-hand-column () (window-hscroll))
(defun M2-right-hand-column () (+ (window-hscroll) (window-width) -1))
(defun M2-on-screen ()
  (and 
   (< (M2-left-hand-column) (current-column)) 
   (< (current-column) (M2-right-hand-column))))
(defun M2-position-point (pos)
  "Scroll display horizontally so point ends up at center of screen, or
  at column position given by prefix argument."
  (interactive "P")
  (if (listp pos) (setq pos (car pos)))
  (if (not pos) 
      (setq pos (/ (window-width) 2))
    (if (< pos 0) (setq pos (+ pos (window-width)))))
  (set-window-hscroll (selected-window) (+ 1 (- (current-column) pos))))
(defun M2-jog-right (arg)
  "Move point right and scroll display so it remains visible.  Optional
  prefix argument tells how far to move."
  (interactive "P")
  (if (listp arg) (setq arg (car arg)))
  (goto-char 
   (if arg
       (+ (point) arg)
     (min (save-excursion (end-of-line) (point)) (+ (point) M2-usual-jog))))
  (if (not (M2-on-screen)) (M2-position-point -2)))
(defun M2-jog-left (arg)
  "Move point left and scroll display so it remains visible.  Optional
  prefix argument tells how far to move."
  (interactive "P")
  (if (listp arg) (setq arg (car arg)))
  (goto-char 
   (if arg
       (- (point) arg)
     (max (save-excursion (beginning-of-line) (point)) (- (point) M2-usual-jog))))
  (if (not (M2-on-screen)) (M2-position-point 1)))
(defun M2-visible-horizontally ()
  (save-excursion
    (and
     (<= 0 (- (current-column) (window-hscroll (selected-window))))
     (< (- (current-column) (window-hscroll (selected-window))) 
	(window-width (selected-window))))))
(defun M2-toggle-truncate-lines ()
  "Toggle the value of truncate-lines, the variable which determines whether 
  long lines are truncated or wrapped on the screen."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (if truncate-lines 
      (if (not (M2-visible-horizontally))
	  (set-window-hscroll 
	   (selected-window)
	   (- (current-column) (/ (window-width) 2))))
    (set-window-hscroll (selected-window) 0))
  (update-screen))

(defun update-screen ()
    (set-window-start (selected-window) (window-start (selected-window)))
    )

(defun M2-dynamic-complete-symbol()
  "Dynamic completion function for Macaulay 2 symbols."
  (interactive)
  (let ((word (comint-word "a-zA-Z")))
    (if word (comint-dynamic-simple-complete word M2-symbols))))

(add-hook 'comint-M2-hook 
	  (function 
	   (lambda ()
	     (setq truncate-lines t)
	     (setq comint-input-autoexpand nil)
	     (setq case-fold-search nil)
	     (setq comint-prompt-regexp M2-comint-prompt-regexp)
	     (use-local-map M2-map)
	     (local-set-key "\t" 'comint-dynamic-complete)
	     (local-set-key "\^C." 'M2-position-point)
	     (local-set-key [ f3 ] 'M2-jog-left)
	     (local-set-key "\^C<" 'M2-jog-left)
	     (local-set-key [ f4 ] 'M2-jog-right)
	     (local-set-key "\^C>" 'M2-jog-right)
	     (local-set-key [ f5 ] 'M2-toggle-truncate-lines)
	     (local-set-key "\^C?" 'M2-toggle-truncate-lines)
	     (local-set-key [ f6 ] 'scroll-left)
	     (local-set-key "\^Cl" 'scroll-left)
	     (local-set-key [ f7 ] 'scroll-right)
	     (local-set-key "\^Cr" 'scroll-right)
	     (local-set-key [ f8 ] 'switch-to-completions)
	     (local-set-key "\^Cc" 'switch-to-completions)
	     (setq comint-dynamic-complete-functions 
		   '(
		     M2-dynamic-complete-symbol
		     comint-dynamic-complete-filename)))))

(defvar M2-symbols '()
  "A list of the symbols available in Macaulay 2, for use with
  dynamic completion."
  )
(load "M2-symbols" t t) 

(defun M2-to-end-of-prompt()
     "Move to end of prompt matching M2-comint-prompt-regexp on this line."
     (interactive)
     (beginning-of-line)
     (let ((case-fold-search nil))
       (if (looking-at M2-comint-prompt-regexp) 
	   (goto-char (match-end 0))
	 (back-to-indentation))))

(defun M2-match-next-bracketed-input()
  "Move forward to the next region bracketed by <<< and >>>, marking
it with the point and the mark.  After marking the region, the code
can be executed with \\[M2-send-to-program]."
  (interactive)
  (goto-char
   (prog1
       (re-search-forward "<<<")
     (re-search-forward ">>>")
     (set-mark (match-beginning 0)))))
(global-set-key [ f10 ] 'M2-match-next-bracketed-input)

(defun M2-match-previous-bracketed-input()
  "Move backward to the previous region bracketed by <<< and >>>, marking
it with the point and the mark.  After marking the region, the code
can be executed with \\[M2-send-to-program]."
  (interactive)
  (goto-char
   (progn
     (re-search-backward ">>>")
     (set-mark (match-beginning 0))
     (re-search-backward "<<<")
     (match-end 0))))
(global-set-key [ M-f10 ] 'M2-match-previous-bracketed-input)

(defun M2-send-to-program() 
     "Send the current line except for a possible prompt, or the region,
     if the mark is active, to Macaulay 2 in its buffer, making its
     window visible.  Afterwards, in the case where the mark is not
     active, move the cursor to the next line.  Alternatively, if the
     point is at a prompt at the end of the buffer *M2*, get the next line
     of input from demo buffer set by M2-set-demo-buffer, or if it's
     at the end of the buffer *M2* with a line of input already there,
     submit it."
     (interactive)
     (or (get-buffer-window "*M2*" 'visible)
	 (pop-to-buffer (prog1 (current-buffer) (pop-to-buffer "*M2*"))))
     (select-window
      (prog1
	  (selected-window)
	  (let* ((send-it t)
		 (cmd (if (and
			  (equal (point) (point-max))
			  (equal (current-buffer) (save-excursion (set-buffer "*M2*"))))
			 (if (equal (point) 
				    (save-excursion (M2-to-end-of-prompt) (point)))
			     (let* ((s (current-buffer))
				    (db (set-buffer M2-demo-buffer))
				    (bol (progn (beginning-of-line) (point)))
				    (eol (progn (end-of-line) (point)))
				    (eob (point-max))
				    (cmd (if (equal bol eob)
					     (concat "-- end of buffer "
						     (if (stringp M2-demo-buffer)
							 M2-demo-buffer
						       (buffer-name M2-demo-buffer)))
					   (buffer-substring bol eol))))
			       (end-of-line)
			       (forward-line)
			       (set-window-point
				(get-buffer-window (current-buffer) 'visible)
				(point))
			       (set-buffer s)
			       (setq send-it nil)
			       cmd)
			   "")
		       (if mark-active
			   (buffer-substring (point) (mark))
			 (buffer-substring
			  (save-excursion (M2-to-end-of-prompt) (point))
			  (save-excursion (end-of-line) (point)))))))
	    (progn
	      (select-window (get-buffer-window (set-buffer "*M2*") 'visible))
	      (goto-char (point-max))
	      (insert cmd)
	      (goto-char (point-max))
	      (set-window-point (get-buffer-window "*M2*" 'visible) (point))
	      (if send-it (comint-send-input))
	      ; (setq deactivate-mark t)
	      ))))
     (setq deactivate-mark nil)
     (if (and (not mark-active) 
	      (not (and
		    (equal (point) (point-max))
		    (equal (current-buffer) (save-excursion (set-buffer "*M2*")))
		    )))
	 (progn
	   (end-of-line)
	   (if (= 1 (forward-line 1))
	       (progn
		 (end-of-line)
		 (insert "\n")))
	   (M2-to-end-of-prompt))))
(defun M2-set-demo-buffer()
  "Set the variable M2-demo-buffer to the current buffer, so that later,
M2-send-to-prorgram can obtain lines from this buffer."
  (interactive)
  (setq M2-demo-buffer (current-buffer)))

(defun M2-switch-to-demo-buffer()
  "Switch to the buffer given by the variable M2-demo-buffer."
  (interactive)
  (switch-to-buffer M2-demo-buffer))

(defun M2-demo()
  "Sets up a new frame with a big font for a Macaulay 2 demo."
  (interactive)
  (let* ((f (prog1
	      (select-frame 
	       (new-frame
		'(
		  (height . 24) 
		  (width . 67)
		  (width . 80)
		  (visiblity . t)
		  (minibuffer . t) (name . "DEMO"))))
	      (toggle-scroll-bar 0)
	      ; (set-default-font "12x24")
	      (set-default-font "-adobe-courier-bold-r-normal--24-240-75-75-m-150-iso8859-1")
	      )
	    )
	 (width (frame-pixel-width))
	 (height (frame-pixel-height))
	 )
    (modify-frame-parameters f '((left - 0) (top - 0)))
    (M2)
    (setq comint-scroll-show-maximum-output t)
    ))

(global-set-key [ f11 ] 'M2-send-to-program)
(global-set-key [ SunF36 ] 'M2-send-to-program)
(global-set-key [ C-f11 ] 'M2-switch-to-demo-buffer)
(global-set-key [ C-SunF36 ] 'M2-switch-to-demo-buffer)
(global-set-key [ M-f11 ] 'M2-set-demo-buffer)
(global-set-key [ M-SunF36 ] 'M2-set-demo-buffer)
(global-set-key [ M-f12 ] 'M2-demo)
(global-set-key [ M-SunF37 ] 'M2-demo)
(global-set-key [ kp-enter ] 'M2-send-to-program)
(global-set-key "\^Cs" 'M2-send-to-program)

