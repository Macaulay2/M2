;;; M2.el 
;;;    -1- run Macaulay 2 as a command interpreter in an Emacs buffer
;;;    -2- provide a major mode used for editing Macaulay 2 source files

;; Macaulay 2 makes no attempt to wrap long output lines, so we provide
;; functions which make horizontal scrolling easier.

(require 'font-lock)
(require 'comint)
(require 'M2-symbols) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M2 command interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq process-coding-system-alist (cons '("M2" . raw-text) process-coding-system-alist))

(defun m2-mode() 
  "Macaulay 2 editing mode, name in lower case"
  (M2-mode))
(defun m2-comint-mode() 
  "Macaulay 2 command interpreter mode, name in lower case"
  (M2-comint-mode))

(define-derived-mode M2-mode fundamental-mode "Macaulay 2"
  "Major mode for editing Macaulay 2 source code.

\\{M2-mode-map}"
  ;; (kill-all-local-variables)
  ;; (set-buffer-modified-p (buffer-modified-p))
  (M2-common)
  )

(define-derived-mode M2-comint-mode comint-mode "Macaulay 2 Interaction"
  "Major mode for interacting with a Macaulay 2 process.

\\{M2-comint-mode-map}"
  (M2-common)
  (setq comint-prompt-regexp M2-comint-prompt-regexp)
  (setq comint-use-prompt-regexp-instead-of-fields t)
  (setq comint-dynamic-complete-functions '( M2-dynamic-complete-symbol comint-dynamic-complete-filename))
  )

(defun M2-common()
  "Set up features common to both Macaulay 2 major modes."
  (set (make-local-variable 'comint-use-prompt-regexp-instead-of-fields) nil) ; we might want to start using "input fields", too!
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 60)
  (set (make-local-variable 'comment-start-skip) "-- *")
  (set (make-local-variable 'comint-input-autoexpand) nil)
  (set (make-local-variable 'transient-mark-mode) t)
  (setq font-lock-defaults '( M2-mode-font-lock-keywords ))
  (setq truncate-lines t)
  (setq case-fold-search nil)
  )

;; key bindings

(define-key M2-mode-map "\177" 'backward-delete-char-untabify)
(define-key M2-mode-map "\^M" 'M2-newline-and-indent)
(define-key M2-mode-map "\t" 'M2-electric-tab)
;; (define-key M2-mode-map "}" 'M2-electric-right-brace)
(define-key M2-mode-map ";" 'M2-electric-semi)
;; (define-key M2-mode-map "\^Cd" 'M2-find-documentation)

(define-key M2-comint-mode-map "\t" 'comint-dynamic-complete)
(define-key M2-comint-mode-map [ f2 ] 'M2-position-point)
(define-key M2-comint-mode-map [ (control C) ?. ] 'M2-position-point)
(define-key M2-comint-mode-map [ f3 ] 'M2-jog-left)
(define-key M2-comint-mode-map [ (control C) < ] 'M2-jog-left)
(define-key M2-comint-mode-map [ f4 ] 'M2-jog-right)
(define-key M2-comint-mode-map [ (control C) > ] 'M2-jog-right)
(define-key M2-comint-mode-map [ f5 ] 'M2-toggle-truncate-lines)
(define-key M2-comint-mode-map [ (control C) ? ] 'M2-toggle-truncate-lines)
(define-key M2-comint-mode-map [ f6 ] 'scroll-left)
(define-key M2-comint-mode-map [ (control C) l ] 'scroll-left)
(define-key M2-comint-mode-map [ f7 ] 'scroll-right)
(define-key M2-comint-mode-map [ (control C) r ] 'scroll-right)
(define-key M2-comint-mode-map [ f8 ] 'switch-to-completions)
(define-key M2-comint-mode-map [ (control C) c ] 'switch-to-completions)
;; (define-key M2-comint-mode-map [ (control C) d ] 'M2-find-documentation)

(mapcar
 (function 
  (lambda (mode-map)
    (define-key mode-map [ f12 ] 'M2)
    (define-key mode-map [ (meta f12) ] 'M2-demo)
    (define-key mode-map [ (control f11) ] 'M2-switch-to-demo-buffer)
    (define-key mode-map [ f11 ] 'M2-send-to-program)
    (define-key mode-map [ (meta f11) ] 'M2-set-demo-buffer)
    (define-key mode-map "\^C\t" 'M2-dynamic-complete-symbol)
    (define-key mode-map [ f10 ] 'M2-match-next-bracketed-input)
    (define-key mode-map [ (meta f10) ] 'M2-match-previous-bracketed-input)))
 (list M2-mode-map M2-comint-mode-map))

;; syntax

(mapcar
 (function
  (lambda (syntax-table)
    (modify-syntax-entry ?\\ "\\" syntax-table)
    (modify-syntax-entry ?-  ". 12" syntax-table)
    (modify-syntax-entry ?*  "." syntax-table)
    (modify-syntax-entry ?_  "." syntax-table)
    (modify-syntax-entry ?+  "." syntax-table)
    (modify-syntax-entry ?=  "." syntax-table)
    (modify-syntax-entry ?%  "." syntax-table)
    (modify-syntax-entry ?<  "." syntax-table)
    (modify-syntax-entry ?>  "." syntax-table)
    (modify-syntax-entry ?'  "w" syntax-table)
    (modify-syntax-entry ?&  "." syntax-table)
    (modify-syntax-entry ?|  "." syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    (modify-syntax-entry ?\^m ">" syntax-table)))
 (list M2-mode-syntax-table M2-comint-mode-syntax-table))

;;

(defvar M2-exe "M2" "*The default Macaulay2 executable name.")
(defvar M2-command (concat M2-exe " --no-readline --print-width " (number-to-string (- (window-width) 1)) " ") "*The default Macaulay2 command line.")
(defvar M2-history (list M2-command) "The history of recent Macaulay2 command lines.")
(defvar M2-send-to-buffer "*M2*" "*The default buffer that \\[M2-send-to-program] sends input to.")
(make-variable-buffer-local 'M2-send-to-buffer)

(defun M2-add-width-option (command)
  (concat
   (replace-regexp-in-string " +--print-width [0-9]+\\| +$" "" command)
   " --print-width "
   (number-to-string (- (window-width) 1))
   " "))

(defun M2 (command)
  "Run Macaulay 2 in a buffer.  With a prefix argument, the command line 
that runs Macaulay 2 can be edited in the minibuffer.  The command line
will always have the appropriate option for the width of the current window
added to it."
  (interactive
   (list
    (cond 
     (current-prefix-arg
      (read-from-minibuffer
	   "M2 command line: "
	   (M2-add-width-option (if M2-history (car M2-history) M2-command))
	   nil
	   nil
	   (if M2-history '(M2-history . 1) 'M2-history)))
     (M2-history (M2-add-width-option (car M2-history)))
     (t (M2-add-width-option M2-command)))))
  (setq foobar current-prefix-arg)
  (switch-to-buffer 
   (make-comint "M2" ; specifying "M2" here means the buffer will be named *M2*
		"/bin/sh" ; name of program
		nil			; starting input file
		"-c" (concat "echo; set -x; " command)		; arguments to the program
		))
  (M2-comint-mode))

(defun Macaulay2 () "Run Macaulay 2 in a buffer, non-interactive." (M2 M2-command))
(defvar M2-usual-jog 30 "Usual distance scrolled by M2-jog-left and M2-jog-right")
(defvar M2-comint-prompt-regexp
  (concat "^"
	  "\\("
	  "[ \t]*"
	  "[io]*[1-9][0-9]* [:=] "
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

(defun M2-send-to-program() 
     "Send the current line except for a possible prompt, or the region, if the
mark is active, to Macaulay 2 in its buffer, making its window visible.
Afterwards, in the case where the mark is not active, move the cursor to
the next line.  Alternatively, if the point is at a prompt or a blank line
at the end of the buffer *M2*, get the next line of input from demo buffer
set by M2-set-demo-buffer, or if it's at the end of the buffer *M2* with a
line of input already there, submit it."
     (interactive)
     (or (get-buffer-window M2-send-to-buffer 'visible)
	 (pop-to-buffer (prog1 (current-buffer) (pop-to-buffer M2-send-to-buffer))))
     (select-window
      (prog1
	  (selected-window)
	  (let* ((send-it t)
		 (cmd (if (and
			  (equal (point) (point-max))
			  (equal (current-buffer) (save-excursion (set-buffer M2-send-to-buffer))))
			 (if (equal (point) 
				    (save-excursion
				      (M2-to-end-of-prompt)
				      (if (looking-at "[ \t]+") (goto-char (match-end 0)))
				      (point)))
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
		       (if (and (boundp 'mark-active) mark-active)
			   (buffer-substring (point) (mark))
			 (buffer-substring
			  (save-excursion (M2-to-end-of-prompt) (point))
			  (save-excursion (end-of-line) (point)))))))
	    (progn
	      (select-window (get-buffer-window (set-buffer M2-send-to-buffer) 'visible))
	      (goto-char (point-max))
	      (insert cmd)
	      (goto-char (point-max))
	      (set-window-point (get-buffer-window M2-send-to-buffer 'visible) (point))
	      (if send-it (comint-send-input))
	      ; (setq deactivate-mark t)
	      ))))
     (setq deactivate-mark nil)
     (if (and (not (and (boundp 'mark-active) mark-active)) 
	      (not (and
		    (equal (point) (point-max))
		    (equal (current-buffer) (save-excursion (set-buffer M2-send-to-buffer)))
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
		'((height . 24)
		  (width . 65)
		  (menu-bar-lines . 0)
		  (visiblity . t)
		  ; (minibuffer . nil)
		  ;; (reverse . t)
		  (modeline . nil);; doesn't work
		  (name . "DEMO"))))
	      (toggle-scroll-bar 0)
	      (set-frame-font ; use (w32-select-font) to get good font names under windows
	       (cond ((eq window-system 'w32) "-*-Lucida Console-bold-r-*-*-19-142-*-*-c-*-*-ansi-")
		     ((eq window-system 'x) "-adobe-courier-bold-r-normal--24-240-75-75-m-150-iso8859-1")
		     (t "12x24")))))
	 (width (frame-pixel-width))
	 (height (frame-pixel-height))
	 )
    (modify-frame-parameters f '((left + 0) (top + 0)))
    ; (M2)
    (make-variable-buffer-local 'comint-scroll-show-maximum-output)
    (save-excursion 
      (set-buffer "*M2*")
      (setq comint-scroll-show-maximum-output t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M2-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (boundp 'font-lock-constant-face))
    (setq font-lock-constant-face font-lock-function-name-face))

(defconst M2-indent-level 5 "*Indentation increment in Macaulay 2 mode")

(defun parse-line ()
     (save-excursion
       (let (eol)
	 (end-of-line)
	 (setq eol (point))
	 (beginning-of-line)
	 (parse-partial-sexp (point) eol))))

(defun paren-change ()
     (car (parse-line)))

(defun M2-electric-semi ()
     (interactive)
     (insert ?;)
     (and (eolp) (next-line-blank) (= 0 (paren-change))
	 (M2-newline-and-indent))
     )

(defun next-line-indent-amount ()
     (+ (current-indentation) (* (paren-change) M2-indent-level)))

(defun this-line-indent-amount ()
     (save-excursion
	  (beginning-of-line)
	  (if (bobp)
	      0
	      (previous-line 1)
	      (next-line-indent-amount))))

(defun in-front ()
     (save-excursion (skip-chars-backward " \t") (bolp)))

(defun blank-line ()
     (save-excursion (beginning-of-line) (skip-chars-forward " \t") (eolp)))
     		   
(defun next-line-blank()
     (save-excursion
	  (end-of-line)
	  (or (eobp)
	      (progn (forward-char) (blank-line))
	      )))

(defun M2-newline-and-indent ()
     "Start a new line and indent it properly for Macaulay 2 code."
     (interactive)
     (newline)
     (indent-to (this-line-indent-amount))
     )

(defun M2-electric-right-brace()
     (interactive)
     (self-insert-command 1)
     (and (eolp) (next-line-blank) (< (paren-change) 0) (M2-newline-and-indent))
     )

(defun M2-electric-tab ()
     (interactive)
     (if (or (not (in-front)) (blank-line))
	 (indent-to (+ (current-column) M2-indent-level))
	 (let ((i (this-line-indent-amount))
	       (j (current-indentation)))
	      (if (not (= i j))
		  (progn
		       (if (< i j)
			    (delete-region (progn (beginning-of-line) (point))
					   (progn (back-to-indentation) (point)))
			    (back-to-indentation))
		       (indent-to i))))))

(defvar M2-demo-buffer 
  (save-excursion 
    (set-buffer (get-buffer-create "*M2-demo-buffer*"))
    (M2-mode)
    (current-buffer))
  "The buffer from which lines are obtained by M2-send-to-program when the
cursor is at the end of the buffer.  Set it with M2-set-demo-buffer." )

; enable syntax highlighting:
(add-hook 'M2-comint-mode-hook 'turn-on-font-lock)
(add-hook 'M2-mode-hook 'turn-on-font-lock) 

(provide 'M2)

; Local Variables:
; compile-command: "make -C $M2BUILDDIR/Macaulay2/emacs "
; End:
