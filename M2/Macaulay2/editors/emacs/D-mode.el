;; Put this file in your path and add these lines to your .emacs file
;; to enable syntax highlighting for the interpreter language
;(autoload 'D-mode "D-mode" "Editing mode for the interpreter language" t)
;(add-to-list 'auto-mode-alist '("\\.dd?\\'" . D-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dd?\\'" . D-mode))

(defvar D-mode-hook nil
  "*Hook evaluated when first loading D-mode.")

(defvar D-symbols '()
  "A list of the symbols available in D-mode, for use with dynamic completion.")


(if (fboundp 'font-lock-add-keywords)
    (progn
      (defvar D-mode-font-lock-keywords
	'(("\\<\\(do\\|new\\|len\\|until\\|while\\|is\\|or\\|when\\|foreach\\|function\\|for\\)\\>"
	   . font-lock-keyword-face)
	  ("\\<\\(at\\|in\\|by\\|from\\|to\\|if\\|op\\|package\\|signature\\|use\\)\\>"
	   . font-lock-keyword-face)
	  ("\\<\\(export\\|import\\|then\\|else\\|break\\|provide\\|return\\)\\>"
	   . font-lock-keyword-face)
	  )
	)
      (font-lock-add-keywords 'D-mode D-mode-font-lock-keywords)
      ))

(defun D-dynamic-complete-symbol()
  "Dynamic completion function for D-mode symbols."
  (interactive)
  (let ((word (comint-word "a-zA-Z")))
    (if word (comint-dynamic-simple-complete word D-symbols))))

(defun d-mode() (D-mode))		;setting file variables lowers the case

;;;###autoload
(defun D-mode()
  "Major mode used for editing contents of a D source file.

\\{D-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map D-mode-map)
  (setq major-mode 'D-mode)
  (setq mode-name "D")
  (setq local-abbrev-table D-mode-abbrev-table)
  (set-syntax-table D-mode-syntax-table)
  (set-buffer-modified-p (buffer-modified-p))
  (make-local-variable 'comment-start)
  (setq comment-start "-- ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 60)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "-- *")
  (local-set-key "\^C\t" 'D-dynamic-complete-symbol)
  (if (fboundp 'turn-on-font-lock)
      (turn-on-font-lock))
  (run-hooks 'D-mode-hook)
  )

(defvar D-mode-abbrev-table nil
  "Abbrev table in use in D-mode buffers.")
(define-abbrev-table 'D-mode-abbrev-table ())
(defvar D-mode-map nil "Keymap containing D-mode commands.")
(if D-mode-map
    nil
  (setq D-mode-map (make-sparse-keymap))
  (define-key D-mode-map "\177" 'backward-delete-char-untabify)
  (define-key D-mode-map "\^M" 'D-newline-and-indent)
  (define-key D-mode-map "\t" 'D-electric-tab)
  (define-key D-mode-map "}" 'D-electric-right-brace)
  (define-key D-mode-map ";" 'D-electric-semi)
  )

(defvar D-mode-syntax-table nil
  "Syntax table in use in D-mode buffers.")
(setq D-mode-syntax-table (copy-syntax-table))

(modify-syntax-entry ?\\ "\\"   D-mode-syntax-table)
(modify-syntax-entry ?-  ". 12" D-mode-syntax-table)
(modify-syntax-entry ?*  "."    D-mode-syntax-table)
(modify-syntax-entry ?+  "."    D-mode-syntax-table)
(modify-syntax-entry ?=  "."    D-mode-syntax-table)
(modify-syntax-entry ?%  "."    D-mode-syntax-table)
(modify-syntax-entry ?<  "."    D-mode-syntax-table)
(modify-syntax-entry ?>  "."    D-mode-syntax-table)
(modify-syntax-entry ?'  "w"    D-mode-syntax-table)
(modify-syntax-entry ?&  "."    D-mode-syntax-table)
(modify-syntax-entry ?|  "."    D-mode-syntax-table)
(modify-syntax-entry ?\n ">"    D-mode-syntax-table)
(modify-syntax-entry ?\^m ">"   D-mode-syntax-table)

(defconst D-indent-level 5 "*Indentation increment in D-mode")

(defun parse-line ()
     (save-excursion
	  (let (eol)
	       (end-of-line)
	       (setq eol (point))
	       (beginning-of-line)
	       (parse-partial-sexp (point) eol))))

(defun paren-change ()
     (car (parse-line)))

(defun D-electric-semi ()
     (interactive)
     (insert ?;)
     (and (eolp) (next-line-blank) (= 0 (paren-change))
	 (D-newline-and-indent))
     )

(defun next-line-indent-amount ()
     (+ (current-indentation) (* (paren-change) D-indent-level)))

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

(defun D-newline-and-indent ()
     "Start a new line and indent it properly for D code."
     (interactive)
     (newline)
     (indent-to (this-line-indent-amount))
     )

(defun D-electric-right-brace()
     (interactive)
     (self-insert-command 1)
     (and (eolp) (next-line-blank) (< (paren-change) 0) (D-newline-and-indent))
     )

(defun D-electric-tab ()
     (interactive)
     (if (or (not (in-front)) (blank-line))
	 (indent-to (+ (current-column) D-indent-level))
	 (let ((i (this-line-indent-amount))
	       (j (current-indentation)))
	      (if (not (= i j))
		  (progn
		       (if (< i j)
			    (delete-region (progn (beginning-of-line) (point))
					   (progn (back-to-indentation) (point)))
			    (back-to-indentation))
		       (indent-to i)
		       ) ) ) ) )

(provide 'D-mode)
