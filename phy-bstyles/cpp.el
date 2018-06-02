;; A C preprocessor written for GnuEmacs for use in generating BibTeX style
;; files from a master file.  In addition it removes whole-line comments (with
;; "%" in the first column) and collapses multiple blank lines to a single
;; blank line.

;;  Charles Karney
;;  Plasma Physics Laboratory	Phone:	 +1 609 243 2607
;;  Princeton University	E-mail:	 Karney@Princeton.EDU
;;  PO Box 451
;;  Princeton, NJ 08543-0451

;; RESTRICTIONS:

;; Recognizes only a subset of C precessor symbols:
;;     #include, #ifdef, #ifndef, #define, #undef, #if, #else, #endif.

;; Macro values can only be numbers.

;; Expressions are not allowed, except for !MACRO.  Thus
;;     "#if FOO" and "#if !FOO" work OK
;;     "#if (FOO | BAR)" and "#if FOO == 4" do not work

;; Value substitution is done in a separate pass through the file at the end.
;; This means that the value substituted for a particular macro is the last
;; one defined.  Thus
;;     #define FOO 4
;;     FOO
;;     #define FOO 10
;;     FOO
;; produces
;;     10
;;     10

(defvar cpp-macros nil "currently defined macros")
(defvar cpp-values nil "values for currently defined macros")

(defun cpp (init)
  "Run C preprocessor on current buffer.  Argument is single macro to
get defined before processing begins.
Recognizes a subset of C precessor symbols:
  #include, #ifdef, #ifndef, #define, #if, #else, #endif.
Also strips out any comments starting with % in the first column."
  (interactive "sRun cpp defining: ")
  (goto-char (point-min))
  (setq init (upcase init))
  (let (verb)
      (setq cpp-macros nil cpp-values nil)
      (cond ((> (length init) 0)
	     (cpp-define init 1)
	     (insert (concat "%% #define " init
			     " 1\t\t% " (current-time-string) "\n"))))
      (insert (concat "%% #include \"" (buffer-file-name) "\"\n"))
      (while (re-search-forward "^#" nil t)
	(save-excursion (beginning-of-line) (insert "%% "))
	(setq verb (cpp-next-word))
	(cond ((equal verb "include") (cpp-include (cpp-next-word)))
	      ((equal verb "define")
	       (cpp-define (cpp-next-word) (cpp-eval (cpp-next-word))))
	      ((equal verb "undef")
	       (cpp-undef (cpp-next-word)))
	      ((equal verb "ifdef")
	       (cond ((zerop (cpp-ifdef (cpp-next-word))) (cpp-skip))))
	      ((equal verb "ifndef")
	       (cond ((not (zerop (cpp-ifdef (cpp-next-word)))) (cpp-skip))))
	      ((equal verb "if")
	       (cond ((zerop (cpp-eval (cpp-next-word))) (cpp-skip))))
	      ((equal verb "else")
	       (cpp-skip))
	      ((equal verb "endif"))
	      (t (error "Unknown preprocessor directive: %s" verb)))))
    (goto-char (point-min))
    (forward-line 2)
    (while (re-search-forward "^%" nil t)
      (beginning-of-line)
      (delete-region (point) (progn (forward-line 1) (point))))
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n" nil t)
      (backward-char 3)
      (delete-char 1))
    (let ((macros cpp-macros) (values cpp-values) (case-fold-search nil))
      (while macros
	(goto-char (point-min))
	(forward-line 2)
	(while (re-search-forward (concat "\\b" (car macros) "\\b") nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (int-to-string (car values))))
	(setq macros (cdr macros) values (cdr values)))))

(defun cpp-include (file)
  "Include a file"
  (forward-line 1)
  (insert-file (substring file 1 -1)))

(defun cpp-eval (macro)
  "Returns the value of a macro"
  (cond ((equal (substring macro 0 1) "!")
	 (if (eq (cpp-eval (substring macro 1 nil)) 0)
	     1
	   0))
	((or (equal (substring macro 0 1) "0")
	     (not (eq (string-to-int macro) 0)))
	 (string-to-int macro))
	(t (let ((macros cpp-macros) (values cpp-values))
	     (while (not (or (null macros)
			     (equal (car macros) macro)))
	       (setq macros (cdr macros)
		     values (cdr values)))
	     (if (null macros)
		 (error "Undefined macro %s",macro)
	       (car values))))))
	
(defun cpp-define (macro value)
  "Make a definition"
  (let ((macros cpp-macros) (values cpp-values))
    (while (not (or (null macros)
		    (equal (car macros) macro)))
      (setq macros (cdr macros)
	    values (cdr values)))
    (if (null macros) (setq cpp-macros (cons macro cpp-macros)
			    cpp-values (cons value cpp-values))
      (rplaca values value))))

(defun cpp-undef (macro)
  "Remove a definition"
  (let ((macros cpp-macros) (values cpp-values) macrosa valuesa)
    (cond ((null macros))
	  ((equal (car macros) macro)
	   (setq cpp-macros (cdr macros)
		 cpp-values (cdr values)))
	  (t (setq macrosa (cdr macros) valuesa (cdr values))
	     (while (not (or (null macrosa)
			     (equal (car macrosa) macro)))
	       (setq macros macrosa
		     values valuesa
		     macrosa (cdr macrosa)
		     valuesa (cdr valuesa)))
	     (cond ((null macros))
		   (t (rplacd macros (cdr macrosa))
		      (rplacd values (cdr valuesa))))))))

(defun cpp-ifdef (macro)
  "Returns 1 if macro is defined, 0 otherwise"
  (let ((macros cpp-macros) (values cpp-values))
    (while (not (or (null macros)
		    (equal (car macros) macro)))
      (setq macros (cdr macros)
	    values (cdr values)))
    (if (null macros) 0 1)))

(defun cpp-next-word ()
  "Return next blank-delimited word in buffer"
  (skip-chars-forward " \t")
  (buffer-substring (point)
		    (progn (re-search-forward " \\|\t\\|$")
			   (match-beginning 0))))

(defun cpp-skip ()
  "Skips to next endif or else"
  (forward-line 1)
  (delete-region
   (point)
   (let ((count 1))
     (while (> count 0)
       (re-search-forward "^#[ \t]*\\(if\\|else\\|endif\\)")
       (goto-char (match-beginning 1))
       (cond ((looking-at "if") (setq count (1+ count)))
	     ((looking-at "else") (if (eq count 1) (setq count 0)))
	     ((looking-at "endif") (setq count (1- count)))))
     (beginning-of-line)
     (point)))
  (insert "%% "))

(defun cpp-file (name)
  "Run C preprocessor on physics.btx.  Argument is single macro to
get defined before processing begins and this is used in the filename
that the results get written to."
  (interactive "sRun cpp-file defining: ")
  (let ((indir "tex$root:[bibtex]")
	(outdir "tex$root:[latex]"))
    (find-file (concat indir "physics.btx"))
    (cpp name)
    (write-file (concat outdir name ".bst"))))

(defun cpp-everything ()
  "Runs cpp on physics.btx to produce all standard styles"
  (interactive)
  (let ((macros (append
		 (list "aip" "pf" "nf" "nflet" "iaea" "cpc" "rmp"
		       "report" "ppcf" "jcp")
;		 (list "apalike")
;		 (list "plain" "unsrt" "alpha" "abbrv")
		 )))
					; The standard styles are
					; "plain" "unsrt" "abbrv" "alpha"
					; A semi-standard style is "apalike"
    (while macros
      (cpp-file (car macros))
      (kill-buffer (buffer-name))
      (setq macros (cdr macros)))))
