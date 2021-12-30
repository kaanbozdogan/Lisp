(defun is-val (token)
	(if (or (equal token "") (and (equal "0" (subseq token 0 1)) (> (length token) 1)))
		(return-from is-val nil))
		
	(loop for i from 0 to (- (length token) 1) do
		(if (not (digit-char-p (coerce (subseq token i (+ i 1)) 'character)))
			(return-from is-val nil)
		)
	)
	t
)


(defun is-id (token)	
	(if (or (equal token "") (not (alpha-char-p (coerce (subseq token 0 1) 'character))))
		(return-from is-id nil)
	)
	(loop for i from 0 to (- (length token) 1) do
		(if (not (or (alpha-char-p (coerce (subseq token i (+ i 1)) 'character)) (digit-char-p (coerce (subseq token i (+ i 1)) 'character))))
			(return-from is-id nil)
		)
	)
	t
)


(defun is-comment (token)
	(if (<= 2 (length token))
		(if (and (equal #\; (char token 0)) (equal #\; (char token 1)))
			t
			nil
		)
	)
)


(defun token-creator (token)
	(cond
		((equal token "") nil)
		((equal token " ") nil)
		((equal token "	") nil)
		((equal token "and") (list "KW_AND"))
		((equal token "or") (list "KW_OR"))
		((equal token "not") (list "KW_NOT"))
		((equal token "equal") (list "KW_EQUAL"))
		((equal token "less") (list "KW_LESS"))
		((equal token "nil") (list "KW_NIL"))
		((equal token "list") (list "KW_LIST"))
		((equal token "append") (list "KW_APPEND"))
		((equal token "concat") (list "KW_CONCAT"))
		((equal token "set") (list "KW_SET"))
		((equal token "deffun") (list "KW_DEFFUN")) 
		((equal token "for") (list "KW_FOR"))
		((equal token "if") (list "KW_IF"))
		((equal token "exit") (list "KW_EXIT"))
		((equal token "load") (list "KW_LOAD"))
		((equal token "disp") (list "KW_DISP"))
		((equal token "true") (list "KW_TRUE"))
		((equal token "false") (list "KW_FALSE"))
		((equal token "+") (list "OP_PLUS"))
		((equal token "-") (list "OP_MINUS"))
		((equal token "/") (list "OP_DIV"))
		((equal token "*") (list "OP_MULT"))
		((equal token "(") (list "OP_OP"))
		((equal token ")") (list "OP_CP"))
		((equal token "**") (list "OP_DBLMULT"))
		((equal token "\"") (list "OP_OC"))
		((equal token "\"") (list "OP_CC"))
		((equal token ",") (list "OP_COMMA"))
		((is-id token) (list "IDENTIFIER"))
		((is-val token) (list "VALUE"))
		(t (list (concatenate 'string "ERROR " token " can not be tokenized")))
    )
)


(defun seperator (ls chr)
	(let ((str (car ls)))
		(if (not (null str))
			(let ((i (search chr str)))
				(if (null i)
					(append (list str) (seperator (cdr ls) chr))
					(append (list (subseq str 0 i)) (list chr) (seperator (list (subseq str (+ 1 i) (length str))) chr) (seperator (cdr ls) chr))
				)	
			)
		)
	)
)


(defun list-creator (str)
	(let ((ls (seperator (list str) " ")))
		(setf ls (seperator ls "	"))
		(setf ls (seperator ls "("))
		(setf ls (seperator ls ")"))
		ls
	)
)


(defun lexer (lst)
    (if (not (null lst))
        (progn
	    	(setq lst (remove "" lst :test #'equal))
	    	(let ((token (token-creator (car lst))))	
	    		(append token (lexer (cdr lst)))
	    	)
		)
	)
)


(defvar out-lst (list))


(defun read-file (filename)
	(let ((in (open filename :if-does-not-exist nil)))
		(when in (loop for line = (read-line in nil)
			while line do (append out-lst (terminal-repl line)))
		)
	)
)


(defun terminal-repl (line)
	(if (and (>= (length line) 2)(is-comment line))
		(list "COMMENT")
		(if (not(equal "" line))
			(lexer (list-creator (string-downcase line)))
		)
	)
)


(defun gpp-lexical-anlyzer (&optional filename) 
	(if (not(null filename))
		(read-file filename)				 	 
		(loop (let ((line (read-line)))  
			(if (not (equal line ""))
				(if (is-comment line)
					(setq out-lst (append out-lst (list "COMMENT")))
				    (let ((temp-lst (lexer (list-creator (string-downcase line)))))
				      	(if (not (equal temp-lst nil))
					  		(setq out-lst (append out-lst temp-lst))
					  	)
				    ) 
				)
				(return)
			)
		))
	)
	(output-to-file "parsed_lisp.txt")
)

(defun output-to-file (filename)
	(with-open-file (str filename
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
	  	(loop for token in out-lst do
	  		(format str "~d ~%" token)
	  	)
	)
)


(gpp-lexical-anlyzer (car *args*))
