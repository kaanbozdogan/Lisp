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

(defun is-bool (token)
    (if (or (equal token "true") (equal token "false"))
        t
        nil
    )
)

(defun is-comment (token)
	(if (<= 2 (length token))
		(if (and (equal #\; (char token 0)) (equal #\; (char token 1)))
			t
			nil
		)
	)
)

(defun get-value-of-id (id)
    (get-value-of-id-helper *ids* id)
)

(defun get-value-of-id-helper (ls id)
    (if (equal (car ls) nil)
        nil
        (if (equal (caar ls) id)
            (cdar ls)
            (get-value-of-id-helper (cdr ls) id)
        )
    )
)

(defun reg-calc (op i j)
    (cond
        ((equal op "+")
            (+ i j))
         ((equal op "-")
            (- i j))
         ((equal op "*")
            (* i j))
         ((equal op "/")
            (/ i j))
        (t
            nil)
    )
)

(defun bool-calc (op i j)        
    (cond
        ((equal op "and")
            (and i j))
        ((equal op "or")
            (or i j))
        ((equal op "not")
            (not i))
        ((equal op "equal")
            (equal i j))
    )
)

(defun conv-to-bool (str)
    (if (equal str "true")
        t
        nil
    )
)


(defvar *res* 0)
(defvar *cond* t)
(defvar *ids* nil)
(defvar *out-lst* (list))


(defun addto-ids (id val)
    (setq *ids* (append *ids* (list (cons id val))))
)

(defun err (str)
    (setq *cond* nil)
    (setq *res* str)
)


(defun start (ls)
    (setq *cond* t)
    (setq *res* 0)

    (if (equal (car ls) "(")
        (progn
            ;;check for the next element
            (cond
                ;;expi
                ((or (equal (cadr ls) "+") (equal (cadr ls) "-") (equal (cadr ls) "*") (equal (cadr ls) "/"))
                    (expi ls)
                )
                ;;expb
                ((or (equal (cadr ls) "and") (equal (cadr ls) "or") (equal (cadr ls) "equal") (equal (cadr ls) "not"))
                    (expb ls)
                )
                ;;explisti
                ((or (equal (cadr ls) "append") (equal (cadr ls) "concat"))
                    (explisti ls)
                )
                ;;other
                ((equal (cadr ls) "list")
                    (explist ls)
                )
                ((or (equal (cadr ls) "set") (equal (cadr ls) "defvar"))
                    (expset ls)
                )
                ((equal (cadr ls) "if")
                    (expif ls)
                )
                ;;element not recognised
                (t
                    (setq *cond* nil) 
                    (setq *res* "Expression not recognized")
                )
            )
        )
        (if (equal (car ls) "'")
            (if (equal (cadr ls) "(")
                (vals ls)
                (err "Expression starts with char '. So the next char must be ( for this expression to be a LISTVALUE")
            )
            (err "Expression must start with OP_OP or ' char")
        )
    )

    (if *cond*
        (setq *out-lst* (list "Syntax OK. Result:"))
        (setq *out-lst* (list "Syntax Error."))
    )
    (setq *out-lst* (append *out-lst* (list *res*)))
)

(defun expi (ls)
    (if (equal *cond* nil) (return-from expi 0))
    ;;get first element
    (let ((n (car ls)) (remlist (cddr ls)) (op (cadr ls)) (i 0) (sublist1 nil) (sublist2 nil))
        (cond
            ;;non-terminal
            ((equal n "(")
                (if (not (equal (get-last-from-list remlist) ")"))
                    (progn (err "Expression must end with parantheses.") (return-from expi 0) )
                )
                (setq remlist (rmv-last-from-list remlist))
                (if (or (equal op "+") (equal op "-") (equal op "*") (equal op "/"))
                    ;;get parameters
                    (progn 
                        ;;recursion for 1st param
                        (if (equal (car remlist) "(")
                            ;;non-terminal
                            (progn
                                (setq i (index-of-matching-parentheses remlist 0))
                                (if (< i 0)
                                    ;;no matching parantheses
                                    (progn (err "Unclosed parantheses.") (return-from expi 0) )
                                    ;;match found
                                    (setq sublist1 (expi (get-sublist remlist 0 i)))
                                )
                            )
                            ;;terminal
                            (setq sublist1 (expi (list (car remlist))))
                        )
                        ;;recursion for 2nd param
                        (setq remlist (get-sublist remlist (+ i 1) 999999)) ;;remove 1st param from list
                        ;;not enough parameters
                        (if (equal 0 (list-length remlist))
                            (progn (err "Not enough paramters for EXPI expression.") (return-from expi 0) )
                        )
                        (if (equal (car remlist) "(") ;;check for non-terminal
                            ;;non-terminal
                            (progn
                                (setq i (index-of-matching-parentheses remlist 0))
                                (if (< i 0)
                                    ;;no matching paranteses
                                    (progn (err "Unclosed parantheses.") (return-from expi 0) )
                                    ;;match found
                                    (setq sublist2 (expi (get-sublist remlist 0 i)))
                                )
                            )
                            ;;terminal
                            (progn 
                                (setq sublist2 (expi (list (car remlist))))
                                (setq i 0)
                            )
                        )
                        (setq remlist (get-sublist remlist (+ i 1) 999999))
                        ;;too much parameter
                        (if (< 0 (list-length remlist))
                            (if (equal (car remlist) ")")
                                (progn (err "Excess parantheses.") (return-from expi 0)
                                )
                                (progn (err "Too much paramters for EXPI expression.") (return-from expi 0) )
                            )
                        )

                        ;;if everything goes well
                        (if (not *cond*) (return-from expi 0))
                        (setq *res* (reg-calc op sublist1 sublist2))
                        (return-from expi *res*)
                    )
                    ;;wrong operator
                    (progn (err "Unidentified operator for EXPI.") (return-from expi 0) )
                )
            )
            ;;terminal
            ((is-val n)
                (parse-integer n))
            ((is-id n)
                (let ((id-val (get-value-of-id n)))
                    (if (equal id-val nil)
                        (progn (err "Undefined id.") (return-from expi 0) )
                        (parse-integer id-val)
                    )
                )
            )
            ;;invalid CFG
            (t
                (err "CFG violation. Expression not recognised") (return-from expi 0)
            )
        )
    )
)

(defun expb (ls)
    (if (equal *cond* nil) (return-from expb 0))
    ;;get first element
    (let ((n (car ls)) (remlist (cddr ls)) (op (cadr ls)) (i 0) (sublist1 nil) (sublist2 nil))
        (cond
            ;;non-terminal
            ((equal n "(")
                (if (not (equal (get-last-from-list remlist) ")"))
                    (progn (err "Expression must end with parantheses.") (return-from expb 0) )
                )
                (setq remlist (rmv-last-from-list remlist))
                (if (or (equal op "and") (equal op "or") (equal op "equal") (equal op "not"))
                    ;;get parameters
                    (progn 
                        ;;recursion for 1st param
                        (if (equal (car remlist) "(")
                            ;;non-terminal
                            (progn
                                (setq i (index-of-matching-parentheses remlist 0))
                                (if (< i 0)
                                    ;;no matching parantheses
                                    (progn (err "Unclosed parantheses.") (return-from expb 0) )
                                    ;;match found
                                    (setq sublist1 (expb (get-sublist remlist 0 i)))
                                )
                            )
                            ;;terminal
                            (setq sublist1 (expb (list (car remlist))))
                        )
                        ;;no recursion if operator is "not"
                        (if (not (equal op "not"))
                            (progn
                                ;;recursion for 2nd param
                                (setq remlist (get-sublist remlist (+ i 1) 999999)) ;;remove 1st param from list
                                ;;not enough parameters
                                (if (equal 0 (list-length remlist))
                                    (progn (err "Not enough paramters for EXPB expression.") (return-from expb 0) )
                                )
                                (if (equal (car remlist) "(") ;;check for non-terminal
                                    ;;non-terminal
                                    (progn
                                        (setq i (index-of-matching-parentheses remlist 0))
                                        (if (< i 0)
                                            ;;no matching paranteses
                                            (progn (err "Unclosed parantheses.") (return-from expb 0) )
                                            ;;match found
                                            (setq sublist2 (expb (get-sublist remlist 0 i)))
                                        )
                                    )
                                    ;;terminal
                                    (progn 
                                        (setq sublist2 (expb (list (car remlist))))
                                        (setq i 0)
                                    )
                                )
                            )
                        )
                        (setq remlist (get-sublist remlist (+ i 1) 999999))
                        ;;too much parameter
                        (if (< 0 (list-length remlist))
                            (if (equal (car remlist) ")")
                                (progn (err "Excess parantheses.") (return-from expb 0))
                                (progn (err "Too much paramters for EXPB expression.") (print remlist) (return-from expb 0) )
                            )
                        )
                        ;;if everything goes well
                        (if (not *cond*) (return-from expb 0))
                        (setq *res* (bool-calc op sublist1 sublist2))
                        (return-from expb *res*)
                    )
                    ;;wrong operator
                    (progn (err "Unidentified operator for EXPB.") (return-from expb 0) )
                )
            )
            ;;terminal
            ((is-bool n)
                (conv-to-bool n))
            ;;invalid CFG
            (t
                (err "CFG violation. Wrong evaluation of EXPB.") (print n) (return-from expb 0)
            )
        )
    )
)

(defun explisti (ls)
    (if (equal *cond* nil) (return-from explisti 0))
    ;;get first element
    (let ((n (car ls)) (remlist (cddr ls)) (op (cadr ls)) (i 0) (sublist1 nil) (sublist2 nil))
        (cond
            ;;non-terminal
            ((equal n "(")
                (if (not (equal (get-last-from-list remlist) ")"))
                    (progn (err "Expression must end with parantheses.") (return-from explisti 0) )
                )
                (setq remlist (rmv-last-from-list remlist))
                (if (or (equal op "concat") (equal op "append"))
                    ;;get parameters
                    (progn
                        ;;recursion for 1st param
                        (if (equal (car remlist) "(")
                            ;;non-terminal
                            (progn
                                (setq i (index-of-matching-parentheses remlist 0))
                                (if (< i 0)
                                    ;;no matching parantheses
                                    (progn (err "Unclosed parantheses.") (return-from explisti 0) )
                                    ;;match found
                                    (if (equal op "concat")
                                        (setq sublist1 (explisti (get-sublist remlist 0 i)))
                                        (setq sublist1 (list (expi (get-sublist remlist 0 i))))
                                    )
                                )
                            )
                            ;;terminal
                            (if (equal op "append")
                                ;;expi
                                (setq sublist1 (list (expi (list (car remlist)))))
                                ;;explisti
                                (progn
                                    (setq i (+ 1 (index-of-matching-parentheses (cdr remlist) 0)))
                                    (if (< i 0)
                                        ;;no matching parantheses
                                        (progn (err "Unclosed parantheses.") (return-from explisti 0) )
                                        ;;match found
                                        (progn
                                            (setq sublist1 (get-sublist remlist 0 i))
                                            (setq sublist1 (explisti sublist1))
                                        )
                                    )
                                )
                            )
                        )
                        ;;recursion for 2nd param
                        (setq remlist (get-sublist remlist (+ i 1) 999999)) ;;remove 1st param from list
                        ;;not enough parameters
                        (if (equal 0 (list-length remlist))
                            (progn (err "Not enough paramters for EXPLISTI expression.") (return-from explisti 0) )
                        )
                        (if (equal (car remlist) "(") ;;check for non-terminal
                            ;;non-terminal
                            (progn
                                (setq i (index-of-matching-parentheses remlist 0))
                                (if (< i 0)
                                    ;;no matching paranteses
                                    (progn (err "Unclosed parantheses.") (return-from explisti 0) )
                                    ;;match found
                                    (setq sublist2 (explisti (get-sublist remlist 0 i)))
                                )
                            )
                            ;;terminal
                            (progn
                                (setq i (+ 1 (index-of-matching-parentheses (cdr remlist) 0)))
                                (if (< i 0)
                                    ;;no matching parantheses
                                    (progn (err "Unclosed parantheses.") (return-from explisti 0) )
                                    ;;match found
                                    (progn
                                        (setq sublist2 (get-sublist remlist 0 i))
                                        (setq sublist2 (explisti sublist2))
                                    )
                                )
                            )
                        )
                        (setq remlist (get-sublist remlist (+ i 1) 999999))
                        ;;too much parameter
                        (if (< 0 (list-length remlist))
                            (if (equal (car remlist) ")")
                                (progn (err "Excess parantheses.") (return-from explisti 0) )
                                (progn (err "Too much paramters for EXPLISTI expression.") (return-from explisti 0) )
                            )
                        )
                        ;;if everything goes well
                        (if (not *cond*) (return-from explisti 0))
                        (setq *res* (append sublist1 sublist2))
                        (return-from explisti *res*)
                    )
                    ;;wrong operator
                    (progn (err "Unidentified operator for EXPLISTI.") (return-from explisti 0) )
                )
            )
            ;;terminal
            (t
                (setq retval (listvalue ls))
                (if (equal *cond* nil)
                    ;;invalid CFG
                    (progn (err "CFG violation. Expression not recognised") (return-from explisti 0))
                    (return-from explisti retval)
                )
            )
        )
    )    
)

(defun expset (ls)
    (if (equal *cond* nil) (return-from expset 0))
    (let ((n (car ls)) (remlist (cdddr ls)) (op (cadr ls)) (i 0) (var-val nil) (var-id nil))
        (if (not (equal (get-last-from-list remlist) ")"))
            (progn (err "Expression must end with parantheses.") (return-from expset 0)) )
        (setq remlist (rmv-last-from-list remlist))
        (if (or (equal op "set") (equal op "defvar"))
            ;;get parameters
            (progn 
                ;;get id and check validity
                (setq var-id (caddr ls))
                (if (is-id var-id)
                    (progn
                        ;;recursion for param
                        (if (equal (car remlist) "(")
                            ;;non-terminal
                            (progn
                                (setq i (index-of-matching-parentheses remlist 0))
                                (if (< i 0)
                                    ;;no matching parantheses
                                    (progn (err "Unclosed parantheses.") (return-from expset 0) )
                                    ;;match found
                                    (setq var-val (expi (get-sublist remlist 0 i)))
                                )
                            )
                            ;;terminal
                            (setq var-val (expi (list (car remlist))))
                        )
                        ;;assign the value and return
                        (addto-ids var-id (write-to-string var-val))
                        (return-from expset var-val)
                    )
                    (progn (err "Id name is not valid.") (return-from expset 0))
                )
            )
            ;;wrong operator
            (progn (err "Unidentified operator for EXP.") (return-from expset 0) )
        )
    )
)

(defun listvalue (ls)
    (if (equal *cond* nil) (return-from listvalue 0))
    (let ((remlist (cddr ls)))
        ;;remove paranteses frpm the list
        (if (not (equal (get-last-from-list remlist) ")"))
            (progn (err "Expression must end with parantheses.") (return-from listvalue 0)) )
        (setq remlist (rmv-last-from-list remlist))
        ;;check for the VALUES CFG
        (setq remlist (vals remlist))
        ;;check if there is any problem
        (if (equal *cond* nil)
            (return-from listvalue 0)
            (return-from listvalue remlist)
        )
    )

)

(defun explist (ls)
    (if (equal *cond* nil) (return-from expi 0))
    (let ((remlist (cddr ls)))
        ;;remove paranteses frpm the list
        (if (not (equal (get-last-from-list remlist) ")"))
            (progn (err "Expression must end with parantheses.") (return-from expi 0)) )
        (setq remlist (rmv-last-from-list remlist))
        ;;check for the VALUES CFG
        (setq remlist (vals remlist))
        ;;check if there is any problem
        (if (equal *cond* nil)
            (return-from expi 0)
            (if (equal remlist nil) (list))
        )
    )
)

(defun vals (ls)
    (if (equal (car ls) nil)
        (list)
        (let ((curr (car ls)))
            (if (is-val curr)
                (append (list (parse-integer curr)) (vals (cdr ls)))
                (progn (err "CFG violation. Expression not recognised.") (return-from vals nil))
            )
        )
    )
)

(defun expif (ls)
    (if (equal *cond* nil) (return-from expif 0))
    (let ((remlist (cddr ls)) (op (cadr ls)) (i 0) (cont nil) (sublist1 nil) (sublist2 nil))
        (if (not (equal (get-last-from-list remlist) ")"))
            (progn (err "Expression must end with parantheses.") (return-from expif 0)) )
        (setq remlist (rmv-last-from-list remlist))
        ;;expb
        (if (equal (car remlist) "(")
            ;;non-terminal
            (progn
                (setq i (index-of-matching-parentheses remlist 0))
                (if (< i 0)
                    (progn (err "Unclosed parantheses.") (return-from expif 0) )
                    (setq cont (expb (get-sublist remlist 0 i)))
                )
            )
            ;;terminal
            (setq cont (expb (list (car remlist))))
        )
        (setq remlist (get-sublist remlist (+ i 1) 999999)) ;;remove expb part from list
        (setq i 0)
        ;;recursion for 1st param
        (if (equal (car remlist) "'")
            ;;non-terminal
            (progn
                (setq i (+ 1 (index-of-matching-parentheses (cdr remlist) 0)))
                (if (< i 0)
                    (progn (err "Unclosed parantheses.") (return-from expif 0) )
                    (setq sublist1 (explisti (get-sublist remlist 0 i)))
                )
            )
            ;;terminal
            (setq sublist1 (explisti (list (car remlist))))
        )
        (setq remlist (get-sublist remlist (+ i 1) 999999)) ;;remove 1st parameter from list
        ;;recursion for 2nd param
        (if (equal (car remlist) "'")
            ;;non-terminal
            (progn
                (setq i (+ 1 (index-of-matching-parentheses (cdr remlist) 0)))
                (if (< i 0)
                    (progn (err "Unclosed parantheses.") (return-from expif 0) )
                    (setq sublist2 (explisti (get-sublist remlist 0 i)))
                )
            )
            ;;terminal
            (setq sublist2 (explisti (list (car remlist))))
        )
        ;;calc result
        (if (not *cond*) (return-from expif 0))                       
        (if cont
            (setq *res* sublist1)
            (setq *res* sublist2)
        )
        (return-from expif *res*)
    )
)


(defun index-of-matching-parentheses (ls n)
    (setq e (car ls))
    (if (equal e "(")
        (+ 1 (index-of-matching-parentheses (cdr ls) (+ n 1)))
        (if (equal e ")")
            (if (< n 2)
                0
                (+ 1 (index-of-matching-parentheses (cdr ls) (- n 1)))
            )
            (if (equal (cdr ls) nil)
                -999999
                (+ 1 (index-of-matching-parentheses (cdr ls) n))
            )
        )
    )
)

(defun get-sublist (ls i j)
    (if (and (< 0 j) (not (equal (cdr ls) nil)))
        (if (equal 0 i)
            (append (list (car ls)) (get-sublist (cdr ls) 0 (- j 1)))
            (get-sublist (cdr ls) (- i 1) (- j 1))
        )
        (if (< 0 i)
            nil
            (list (car ls))
        )
    )
)

(defun rmv-last-from-list (ls)
    (if (not (equal (cdr ls) nil))
        (append (list (car ls)) (rmv-last-from-list (cdr ls)))
        nil
    )
)

(defun get-last-from-list (ls)
    (if (equal (cdr ls) nil)
        (car ls)
        (get-last-from-list (cdr ls))
    )
)


(defun token-creator (token)
	(if (or (equal token "") (equal token " ") (equal token "	"))
		nil
		(list token)
    )
)
;seperates the ls with given chr
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
;creates a list of seperated elements in a line
(defun list-creator (str)
	(let ((ls (seperator (list str) " ")))
		(setf ls (seperator ls "	"))
		(setf ls (seperator ls "("))
		(setf ls (seperator ls ")"))
        (setf ls (seperator ls "'"))
		ls
	)
)
;tokenize an element
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


;tokenize the line
(defun terminal-repl (line)
	(if (and (>= (length line) 2) (is-comment line))
		(list "COMMENT")
		(if (not(equal "" line))
			(lexer (list-creator (string-downcase line)))			
		)
	)
)

(defvar *file-lst* (list))

(defun read-file (filename)
	(let ((in (open filename :if-does-not-exist nil)))
		(when in (loop for line = (read-line in nil)
			while line do (progn
                (setq *out-lst* (list))
                (setq *file-lst* (append *file-lst* (list line)))
                (setq *out-lst* (append *out-lst* (terminal-repl line)))
                (start *out-lst*)
                (setq *file-lst* (append *file-lst* *out-lst*))
            ))
		)
	)
)

(defun output-to-file (filename)
	(with-open-file (str filename
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
	  	(loop for token in *file-lst* do
	  		(format str "~d~%" token)
	  	)
        (format t "~%Interpretation printed to file parsed_lisp.txt~%~%")
	)
)


(defun gppinterpreter (&optional filename) 
	(if (not(null filename))
		(progn
			(read-file filename)
            (output-to-file "parsed_lisp.txt")
		)
		;;line loop			 	 
		(loop (let ((line (read-line)))  
			(if (and (not (equal line "")) (not (equal line "(exit)")))
				(if (is-comment line)
					;;comment
					(setq *out-lst* (append *out-lst* (list "COMMENT")))
					;;analyze line
					;;(setq ls (read-from-string line))
				    (let ((temp-lst (lexer (list-creator (string-downcase line)))))
				      	(if (not (equal temp-lst nil))
                            (progn
                                (setq *out-lst* (list))
                                (setq *out-lst* (append *out-lst* temp-lst))
                                (start *out-lst*)
                                (loop for token in *out-lst* do (format t "~d " token))
                                (format t "~%")
                            )
					  	)
				    ) 
				)
				(return)
			))
		)
	)
)


(gppinterpreter (car *args*))

