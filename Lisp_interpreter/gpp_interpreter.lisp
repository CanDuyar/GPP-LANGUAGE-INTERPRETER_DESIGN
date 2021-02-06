
;; CAN DUYAR - 171044075

(setq paranthese_control 0) ;;paranthese_control detects OP_OC or OP_CP
(setq Operator (list "+" "-" "/" "**" "*" "(" ")" "\"" "\"" "," "'"))
(setq KeyWord (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(setq KW (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(setq OP (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_DBLMULT" "OP_MULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA" "CONTROL" ))
(setq Possible (list "(" ")" "\""))
(setq Space (list "\n" "\t" " "))
(setq value (list))
(setq Comment ";")
(setq tokens (list))
(setq representation (list))
(setq tokenize (list))

;;[a-zA-Z_][a-zA-Z0-9_]*  return IDENTIFIER;

;;[0]|[1-9][0-9]*	return VALUE;


;;GPP INTERPRETER IT CONTINUOUS WITH FILE OR WITHOUT FILE	

(defun gppinterpreter (&optional (filename -1))
	(if (equal filename -1)
		(let ((line) (check))
			(loop
			   (setq line (read-line))
			   (setq check (detectToken line))
			   (terpri)(terpri)
			   (when (= check -1) (return))
			)
		)
		(let ((in (open filename :if-does-not-exist nil)))
   			(when in
      			(loop for line = (read-line in nil)
      
      			while line do (progn (detectToken line) (terpri)))
      			(close in)
   			)
		)
	)
)

;; this function detects the tokens.
(defun detectToken (line)
	(let ((words) (res 0) (tempword) (check 0))
		(setq tokenize (list))
		(setq tokens (list))
		(setq line (string-trim '(#\Space #\Tab #\Newline) line))
		(setq words (divideStr line))
		(loop for word in words
			do
			(progn
				(setq tempword (string-trim '(#\Space #\Tab #\Newline) word))
				(setq res (ifMatch tempword))
				(if (or (equal res 2) (equal res -1)) (return res))
			)
		)
		(if (equal res -1)
			(write "SYNTAX_ERROR Expression not recognized")
			(progn
				(if (equal res 2) ()
					(progn
						(setq check (implementTokens))
						(if (equal check nil) (setq check (explisti)))
						(if (equal check nil) (write "SYNTAX_ERROR Expression not recognized"))
					)
				)
			)
		)
		res	)
)

;; Detects that splited word matches with a token or not.
(defun ifMatch (word)
	(let ((len (length word)) (subword) (j 0) (res) (temp) (check 0) (id 0))
		(loop for i from 1 to len
			do
			(progn
				(if (= check 1) (setq check 0) )
				(setq subword (string-downcase (subseq word j i)))
				(if (= check 0) ;;operator control
					(progn
						(setq res (stringDetection subword Operator))
						(if (not (equal res nil))
							(progn
								(if (equal res 4) ;;checking ** with using single-*
									(if (and (< i len) (string= (subseq word i (+ i 1)) "*")) (progn (setq i (+ i 1)) (setq res 3)))
								)
								;; paranthese_control detects OP_OC or OP_CP
								(if (equal res 7) (progn (setq res (+ res (mod paranthese_control 2))) (setq paranthese_control (+ paranthese_control 1))))
								;;other tokens can come after them.
								(if (or (equal res 5) (equal res 6) (equal res 7) (equal res 9) (equal res 10))
									(progn (setq tokens (append tokens (list subword))) (setq tokenize (append tokenize (list (nth res OP)))) (setq j i) (setq check 1))
									(if (>= i len)
										(progn (setq tokens (append tokens (list subword))) (setq tokenize (append tokenize (list (nth res OP)))) (setq check 1))
										(progn
										 	(setq temp (subseq word i (+ i 1)))
										 	(if (equal (stringDetection temp Possible) nil)
										 		(progn (setq check -1))
										 		(progn (setq tokens (append tokens (list subword))) (setq tokenize (append tokenize (list (nth res OP)))) (setq j i) (setq check 1))
										 	)
										)
		                             )	
								)
							)	
						)
					)	
				)

				;; Detects that splited word matches with a token or not.
				(if (= check 0)
					(progn
						(setq res (stringDetection subword KeyWord))
						(if (not (equal res nil))
							(if (>= i len)
								(progn (setq tokens (append tokens (list subword))) (setq tokenize (append tokenize (list (nth res KW)))) (setq check 1))
								(progn
								 	(setq temp (subseq word i (+ i 1)))
								 	(if (and (equal (stringDetection temp Possible) nil))
								 		(if (equal (IdDetection(concatenate 'string subword temp)) nil) 
								 			(progn (setq check -1))
								 		)
								 		(progn (setq tokens (append tokens (list subword))) (setq tokenize (append tokenize (list (nth res KW)))) (setq j i) (setq check 1))
								 	)
								)
							)
						)
					)	
				)

				;;this part detectes the given token is value or not.
				(if (= check 0)
					(progn
						(setq res (valueDetection subword))
						(if (not (equal res nil))
							(progn
								(loop
									(setq temp (string-downcase (subseq word j i)))
									(setq i (+ i 1))
									(when (or (equal (valueDetection temp) nil) (> i len)) (return))
								)
								(setq i (- i 1))
								(if (equal (valueDetection temp) nil) (setq i (- i 1)))								
								(if (>= i len)
									(progn (setq tokens (append tokens (list subword))) (setq tokenize (append tokenize (list "VALUE"))) (setq check 1))
									(progn
									 	(setq temp (subseq word i (+ i 1)))
									 	(if (equal (stringDetection temp Possible) nil)
									 		(progn (setq check -1))
									 		(progn (setq tokens (append tokens (list subword))) (setq tokenize (append tokenize (list "VALUE"))) (setq j i) (setq check 1))
									 	)
									)
								)
							)	
						)
					)
				)

				;; Detects that token is comment or not.
				(if (and (= check 0) (string= subword Comment))
						(if (and (< i len) (string= (subseq word i (+ i 1)) Comment))
							(progn (setq tokens (append tokens (list "COMMENT"))) (setq tokenize (append tokenize (list "COMMENT"))) (setq j i) (setq check 2))
						)
				)

				;; Detects that token is identifier or not.
				(if (= check 0)
					(progn
						(setq res (IdDetection subword))
						(if (equal res t)
							(if (= i len)
								(progn (setq tokens (append tokens (list subword))) (setq tokenize (append tokenize (list "IDENTIFIER")))  (setq check 1))
								(progn
									(setq temp (string-downcase (subseq word j (+ i 1))))
									(setq id (IdDetection temp))
									(if (equal res id)
										()
										(progn
										 	(setq temp (subseq word i (+ i 1)))
										 	(if (equal (stringDetection temp Possible) nil)
										 		(progn (setq check -1))
										 		(progn (setq tokens (append tokens (list subword))) (setq tokenize (append tokenize (list "IDENTIFIER"))) (setq j i) (setq check 1))
										 	)
										)
									)
								)
							)
							(progn (setq check -1))
						)
					)	
				)
				(if (= check 2) (return check))

			)
		)
		check			
	)
)

;;it divides the str according the given separator 
(defun divideStr (string &optional (separator " "))
  (stringOperationHelper string separator))

(defun stringOperationHelper (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(stringOperationHelper (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))



;; this function detects the strings
(defun stringDetection (word complist &optional (i 0))
	(if (null complist)
		nil
		(if (string= word (car complist))
			i
			(stringDetection word (cdr complist) (+ i 1))
		)
	)
)

;; this function detects the IDs
(defun IdDetection (word)
	(let ((len (- (length word) 1)) (chr) (res t))

		(loop for i from 0 to len
			do
			(progn
				(setq chr (char word i))
				(if (= i 0)
					(if (or (alpha-char-p chr) (char= chr #\_) (char= chr #\.) (char= chr #\+)) (setq res t) (setq res nil))
					(if (or (alpha-char-p chr) (digit-char-p chr) (char= chr #\_) (char= chr #\.) (char= chr #\+)) () (setq res nil))
				)
				(if (equal res nil) (return res))
			)
		)
		res
	)
)

;; this function detects the values	
(defun valueDetection (word)
	(let ((chr) (res t))
		(if (equal (every #'digit-char-p word) nil)
			(setq res nil)
			(progn
				(if (= (length word) 1)
					(setq res t)
					(progn
						(setq chr (char word 0))
						(if (equal (digit-char-p chr) 0) (setq res nil) (setq res t))
					)
				)		
			)
		)
		res	
	)
)


(defun implementTokens (&optional(tk tokens) (tl tokenize)(control2 0))
	(let ((len (list-length tk)) (res 0) (control 0) (num 0) (num2 0) (val3 0) (temp 2) (keep2) (val4) (kw) (keep) (temp4))
		
		(if (and (string= (nth 0 tl) "OP_OP") (string= (nth (- len 1) tl) "OP_CP" ))
			(progn
				(setq kw (nth 1 tl))
				(if (or (string= kw "OP_PLUS") (string= kw "OP_MINUS") (string= kw "OP_MULT") (string= kw "OP_DIV") (string= kw "OP_DBLMULT"))
					(progn
						(setq control 1)
						(if (or (string= (nth 2 tl) "VALUE") (string= (nth 2 tl) "IDENTIFIER"))
							(progn
								(if (string= (nth 2 tl) "VALUE") (progn (setq num (parse-integer (nth 2 tk))) (setq temp 3)))
								(if (string= (nth 2 tl) "IDENTIFIER") (progn (setq num (targetID (nth 2 tk))) (setq temp 3)))
							)
							(progn
								(setq temp4 (expTarget (subseq tk 2)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (and (not (equal temp nil)) (< temp len)) (setq num (implementTokens (subseq tk 2 temp) (subseq tl 2 temp) 1)) (setq res nil))
								(if (equal num nil) (setq res nil))
							)
						)
						(if (equal temp nil) (setq res nil) (setq keep2 (+ temp 2)))
						(if (not (equal res nil))
							(if (or (string= (nth temp tl) "VALUE") (string= (nth temp tl) "IDENTIFIER"))
								(progn
									(if (string= (nth temp tl) "VALUE") (progn (setq num2 (parse-integer (nth temp tk)))))
									(if (string= (nth temp tl) "IDENTIFIER") (progn (setq num2 (targetID (nth temp tk)))))
								)
								(progn
									(setq temp4 (expTarget (subseq tk temp)))
									(if (equal temp4 nil) (progn (setq res nil) (setq keep2 nil)) (setq keep2 (+ temp temp4)))
									(if (and (not (equal keep2 nil)) (< keep2 len)) (progn (setq num2 (implementTokens (subseq tk temp keep2) (subseq tl temp keep2) 1)) (setq keep2 (+ keep2 1))) (setq res nil))
									(if (equal num2 nil) (setq res nil))
								)
							)
						)
						(if (and (not (equal res nil)) (equal keep2 len) (not (equal num nil)) (not (equal num2 nil)))
							(progn
								(if (string= kw "OP_PLUS") (setq res (+ num num2)))
								(if (string= kw "OP_MINUS") (setq res (- num num2)))
								(if (string= kw "OP_MULT") (setq res (* num num2)))
								(if (string= kw "OP_DIV") (setq res (/ num num2)))
								(if (string= kw "OP_DBLMULT") (setq res (expt num num2)))
							)
							(setq res nil)
						)

					)
				)
				(if (string= kw "IDENTIFIER")
					(progn
						(setq control 1)
						(setq num (explisti (subseq tk 2 (- len 1)) (subseq tl 2 (- len 1)) 1))
						(if (equal num nil) (setq num (implementTokens (subseq tk 2 (- len 1)) (subseq tl 2 (- len 1)) 1)))
						(if (equal num nil) (setq res nil) (setq res (nth 1 tk)))
						(if (equal control2 1) (setq res 0))
					)
				)
				(if (and (string= kw "KW_SET") (string= (nth 2 tl) "IDENTIFIER"))
					(progn
						(setq control 1)
						(setq num (explisti (subseq tk 3 (- len 1)) (subseq tl 3 (- len 1)) 1))
						(if (equal num nil) (setq num (implementTokens (subseq tk 3 (- len 1)) (subseq tl 3 (- len 1)) 1)))
						(if (equal num nil)
							(setq res nil)
							(progn
								(setq num2 (position (nth 2 tk) representation :test #'string=))
								(if (equal num2 nil)
									(progn
										(setq representation (append representation (list (nth 2 tk))))
										(setq value (append value (list num)))
									)
									(setf (elt value num2) num)
								)
								(setq res num)
							)
						)
					)
				)

				(if (string= kw "KW_IF")
					(progn
						(setq control 1)
						(setq keep (nth 2 tl))
						(if (or (string= keep "VALUE") (string= keep "KW_TRUE") (string= keep "KW_FALSE") (string= keep "IDENTIFIER"))
							(progn (setq num (expb (list (nth 2 tk))(list (nth 2 tl)) 1)) (setq temp 3))
							(progn
								(setq temp4 (expTarget (subseq tk 2)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (not (equal temp nil)) (setq num (expb (subseq tk 2 temp) (subseq tl 2 temp) 1)) (setq res nil))
							)
						)
						(if (equal temp nil) (setq res nil) (setq keep2 (+ temp 2)))
						(if (not (equal res nil))
							(if (or (string= (nth temp tl) "VALUE") (string= (nth temp tl) "IDENTIFIER"))
								(progn
									(if (string= (nth temp tl) "VALUE") (progn (setq num2 (parse-integer (nth temp tk)))))
									(if (string= (nth temp tl) "IDENTIFIER") (progn (setq num2 (targetID (nth temp tk)))))
								)
								(progn
									(setq temp4 (expTarget (subseq tk temp)))
									(if (equal temp4 nil) (progn (setq res nil) (setq keep2 nil)) (setq keep2 (+ temp temp4)))
									(if (not (equal keep2 nil)) (progn (setq num2 (implementTokens (subseq tk temp keep2) (subseq tl temp keep2) 1)) (setq keep2 (+ keep2 1))) (setq res nil))
									(if (equal num2 nil) (setq res nil))
								)
							)
						)
						(if (and (not (equal keep2 nil)) (< keep2 len))
							(progn (setq val3 (implementTokens (subseq tk (- keep2 1) (- len 1)) (subseq tl (- keep2 1) (- len 1)) 1))
							(if (equal val3 nil) (setq res nil) (setq keep2 len)))
							(setq res nil)
						)
						(if (and (not (equal res nil)) (= keep2 len) (not (equal num2 nil)) (not (equal val3 nil)))
							(progn
								(if (equal num -2) (setq num nil))
								(if (equal val3 nil)
									(if num (setq res num2))
									(if num (setq res num2) (setq res val3))
								)
							)
							(setq res nil)
						)
					)
				)

				(if (and (string= kw "KW_IF") (equal res nil))
					(progn
						(setq control 1)
						(setq res 0)
						(setq temp 2)
						(setq keep (nth 2 tl))
						(if (or (string= keep "VALUE") (string= keep "KW_TRUE") (string= keep "KW_FALSE") (string= keep "IDENTIFIER"))
							(progn (setq num (expb (list (nth 2 tk))(list (nth 2 tl)) 1)) (setq temp 3))
							(progn
								(setq temp4 (expTarget (subseq tk 2)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (not (equal temp nil)) (setq num (expb (subseq tk 2 temp) (subseq tl 2 temp) 1)) (setq res nil))
							)
						)
						(if (equal temp nil) (setq res nil) (setq keep2 (+ temp 2)))
						(if (not (equal res nil))
							(if (string= (nth (- temp 1) tl) "OP_OP")
								(progn
									(setq temp4 (expTarget (subseq tk (- temp 1))))
									(if (equal temp4 nil) (progn (setq res nil) (setq keep2 nil)) (setq keep2 (+ temp temp4)))
									(if (and (not (equal keep2 nil)) (< keep2 len)) (progn (setq num2 (explisti (subseq tk temp keep2) (subseq tl temp keep2) 1)) (setq keep2 (+ keep2 1))) (setq res nil))
								)
								(setq res nil)
							)
						)
						(if (not (equal keep2 len))
							(progn (setq val3 (explisti (subseq tk (- keep2 1) (- len 1)) (subseq tl (- keep2 1) (- len 1)) 1))
								(if (equal val3 nil) (setq res nil) (setq keep2 len)))
						)
						(if (and (not (equal res nil)) (= keep2 len) (not (equal num nil)) (not (equal num2 nil)))
							(progn
								(if (equal num -2) (setq num nil))
								(if (equal val3 nil)
									(if num (setq res num2))
									(if num (setq res num2) (setq res val3))
								)
							)
							(setq res nil)
						)
					)

				)
				(if (and (string= kw "KW_FOR") (string= (nth 2 tl) "OP_OP") (string= (nth 3 tl) "IDENTIFIER"))
					(progn
						(setq control 1)
						(setq num (implementTokens (list (nth 3 tk))(list (nth 3 tl)) 1))
						(if (equal num nil) (setq res nil))
						(setq temp 4)
						(if (not (equal num nil))
							(if (or (string= (nth temp tl) "VALUE") (string= (nth temp tl) "IDENTIFIER"))
								(progn
									(if (string= (nth temp tl) "VALUE") (progn (setq num2 (parse-integer (nth temp tk))) (Setq keep2 5)))
									(if (string= (nth temp tl) "IDENTIFIER") (progn (setq num2 (targetID (nth temp tk))) (Setq keep2 5)))
								)
								(progn
									(setq temp4 (expTarget (subseq tk temp)))
									(if (equal temp4 nil) (progn (setq res nil) (setq keep2 nil)) (setq keep2 (+ temp temp4)))
									(if (and (not (equal keep2 nil)) (< keep2 len)) (setq num2 (implementTokens (subseq tk temp keep2) (subseq tl temp keep2) 1)) (setq res nil))
									(if (equal num2 nil) (setq res nil))
								)
							)
						)
						(if (not (equal res nil))
							(if (or (string= (nth keep2 tl) "VALUE") (string= (nth keep2 tl) "IDENTIFIER"))
								(progn
									(if (string= (nth keep2 tl) "VALUE") (progn (setq val3 (parse-integer (nth keep2 tk))) (Setq keep 6)))
									(if (string= (nth keep2 tl) "IDENTIFIER") (progn (setq val3 (targetID (nth keep2 tk))) (Setq keep 6)))
								)
								(progn
									(setq temp4 (expTarget (subseq tk keep2)))
									(if (equal temp4 nil) (progn (setq res nil) (setq keep nil)) (setq keep (+ keep2 temp4)))
									(if (and (not (equal keep nil)) (< keep2 len)) (setq val3 (implementTokens (subseq tk keep2 keep) (subseq tl keep2 keep) 1)) (setq res nil))
									(if (equal val3 nil) (setq res nil))
								)
							)
						)
						(if (equal (nth keep tl) "OP_CP") (setq keep (+ keep 1)) (setq res nil))
						(if (not (equal res nil))
							(if (string= (nth keep tl) "OP_OP")
								(progn
									(setq temp4 (expTarget (subseq tk keep)))
									(if (equal temp4 nil) (progn (setq res nil) (setq temp4 nil)) (setq temp4 (+ keep temp4)))
									(if (and (not (equal temp4 nil)) (< temp4 len)) (progn (setq val4 (explisti (subseq tk keep temp4) (subseq tl keep temp4) 1)) (setq temp4 (+ temp4 1))) (setq res nil))
								)
								(setq res nil)
							)
						)
						(if (and (not (equal res nil)) (= temp4 len) (not (equal num2 nil)) (not (equal val3 nil)) (not (equal val4 nil)))
							(setq res val4)
							(setq res nil)
						)
					)
				)

				(if (and (string= kw "KW_LOAD") (string= (nth 2 tl) "OP_OC") (string= (nth 3 tl) "IDENTIFIER"))
					(progn
						(setq control 1)
						(setq temp (nth 3 tk))
						(setq keep2 (open temp :if-does-not-exist nil))
						(if (equal keep2 nil) (write NIL)(write T)) (terpri)
						(setq res temp)
					)
				)

				(if (and (string= kw "KW_DEFFUN") (string= (nth 2 tl) "IDENTIFIER"))
					(progn
						(setq control 1)
						(setq temp 3)
						(setq representation (append representation (list (nth 2 tk))))
						(setq value (append value (list 0)))
						(if (string= (nth 3 tl) "IDENTIFIER")
							(progn (setq num 0) (setq temp 4))
							(progn
								(setq temp4 (expTarget (subseq tk 3)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (and (not (equal temp nil)) (< temp len)) (setq num (KEEP_LIST (subseq tk 3 temp) (subseq tl 3 temp))) (setq res nil))
								(if (equal num nil) (setq res nil))
							)
						)
						(if (not (equal res nil))
							(if (or (string= (nth temp tl) "VALUE") (string= (nth temp tl) "IDENTIFIER"))
								(progn
									(if (string= (nth temp tl) "VALUE") (progn (setq num2 (parse-integer (nth temp tk)))))
									(if (string= (nth temp tl) "IDENTIFIER") (progn (setq num2 (targetID (nth temp tk)))))
								)
								(progn
									(setq temp4 (expTarget (subseq tk temp)))
									(if (equal temp4 nil) (progn (setq res nil) (setq keep2 nil)) (setq keep2 (+ temp temp4)))
									(if (and (not (equal keep2 nil)) (< keep2 len)) (progn (setq num2 (implementTokens (subseq tk temp keep2) (subseq tl temp keep2) 1)) (setq keep2 (+ keep2 1))) (setq res nil))
									(if (equal num2 nil) (setq res nil))
								)
							)
						)
						(if (and (not (equal res nil)) (= keep2 len) (not (equal num nil)) (not (equal num2 nil)))
							(setq res (nth 2 tk))
							(setq res nil)
						)
					)
				)

				(if (and (equal res nil) (string= kw "KW_DEFFUN") (string= (nth 2 tl) "IDENTIFIER"))
					(progn
						(setq control 1)
						(setq temp 3)
						(if (string= (nth 3 tl) "IDENTIFIER")
							(progn (setq num 0) (setq temp 4))
							(progn
								(setq temp4 (expTarget (subseq tk 3)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (and (not (equal temp nil)) (< temp len)) (setq num (KEEP_LIST (subseq tk 3 temp) (subseq tl 3 temp))) (setq res nil))
								(if (equal num nil) (setq res nil))
							)
						)
						(if (string= (nth temp tl) "OP_OP")
								(progn
									(setq temp4 (expTarget (subseq tk temp)))
									(if (equal temp4 nil) (progn (setq res nil) (setq keep2 nil)) (setq keep2 (+ temp temp4)))
									(if (and (not (equal keep2 nil)) (< keep2 len)) (progn (setq num2 (explisti (subseq tk temp keep2) (subseq tl temp keep2) 1)) (setq keep2 (+ keep2 1))) (setq res nil))
								)
								(setq res nil)
						)
						(if (and (not (equal res nil)) (= keep2 len) (not (equal num nil)) (not (equal num2 nil)))
							(setq res (nth 2 tk))
							(setq res nil)
						)
					)
				)

				(if (and (string= kw "KW_EXIT") (equal len 3))
					(progn (setq control 1)(terpri) (exit))
				)



				(if (equal control 0)
					(progn
						(setq res (expb tk tl 0))
					)
				)

			)
			(progn
				(if (string= (nth 0 tokens) "COMMENT")
					(setq control2 1)
					(progn
						(setq keep (nth 0 tl))
						(if (equal len 1)
							(progn
								(if (string= keep "VALUE") (setq num (parse-integer (nth 0 tk))) (setq temp 3))
								(if (string= keep "IDENTIFIER") (setq num (targetID (nth 0 tk))) (setq temp 3))
								(setq res num)
							)
							(setq res nil)
						)
					)
				)
			)
		)
		(if (and (not (equal res nil)) (not (equal res -798)) (= control2 0)) (write res))
		res
	)
)

(defun explisti (&optional(tk tokens) (tl tokenize)(control2 0))
	(let ((len (list-length tk)) (res 0) (control 0) (num 0) (num2 0) (temp 2) (keep2) (temp4) (kw))
		
		(if (and (string= (nth 0 tl) "OP_OP") (string= (nth (- len 1) tl) "OP_CP" ))
			(progn
				(setq kw (nth 1 tl))
				(if (and (or (string= kw "KW_APPEND") (string= kw "KW_CONCAT")) (or (string= (nth 2 tl) "CONTROL") (string= (nth 3 tl) "KW_LIST")))
					(progn
						(setq control 1)
						(setq temp4 (expTarget (subseq tk 2)))
						(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
						(if (equal temp nil) (setq res nil) (setq keep2 (+ temp 2)))
						(if (and (not (equal temp nil)) (< temp len)) (setq num (explisti (subseq tk 2 temp) (subseq tl 2 temp) 1)) (setq res nil))
						(if (and (not (equal temp nil)) (< temp len))
							(progn
								(setq temp4 (expTarget (subseq tk temp)))
								(if (equal temp4 nil) (progn (setq res nil) (setq keep2 nil)) (setq keep2 (+ temp temp4)))
							)
							(setq keep2 nil))
						(if (and (not (equal keep2 nil)) (< keep2 len)) (progn (setq num2 (explisti (subseq tk temp keep2) (subseq tl temp keep2) 1)) (setq keep2 (+ keep2 1))) (setq res nil))
						(if (and (not (equal res nil)) (= keep2 len) (not (equal num nil)) (not (equal num2 nil)))
							(progn
								(setq res (list))
								(setq res (append res num))
								(setq res (append res num2))
							)
							(setq res nil)
						)

					)
				)
				(if (and (equal res nil) (string= kw "KW_APPEND"))
					(progn
						(setq control 1)
						(if (or (string= (nth 2 tl) "VALUE") (string= (nth 2 tl) "IDENTIFIER"))
							(progn
								(if (string= (nth 2 tl) "VALUE") (progn (setq num (parse-integer (nth 2 tk))) (Setq temp 3)))
								(if (string= (nth 2 tl) "IDENTIFIER") (progn (setq num (targetID (nth 2 tk))) (Setq temp 3)))
							)
							(if (string= (nth 2 tl) "IDENTIFIER")
								(progn (setq num (list (targetID (nth 2 tk)))) (setq temp 3))
								(progn
									(setq temp4 (expTarget (subseq tk 2)))
									(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
									(if (not (equal temp nil)) (setq num (implementTokens (subseq tk 2 temp) (subseq tl 2 temp) 1)) (setq res nil))
									(if (equal num nil) (setq res nil))
								)
							)
						)
						(if (equal temp nil) (setq res nil) (setq keep2 (+ temp 2)))

						(if (and (not (equal temp nil)) (< temp len))
							(progn
								(setq temp4 (expTarget (subseq tk temp)))
								(if (equal temp4 nil) (progn (setq res nil) (setq keep2 nil)) (setq keep2 (+ temp temp4)))
							)
							(setq keep2 nil))
						(if (and (not (equal keep2 nil)) (< keep2 len)) (progn (setq num2 (explisti (subseq tk temp keep2) (subseq tl temp keep2) 1)) (setq keep2 (+ keep2 1))) (setq res nil))
							
						(if (and (not (equal res nil)) (= keep2 len) (not (equal num nil)) (not (equal num2 nil)))
							(progn
								(setq res (list))
								(setq res (append res num))
								(setq res (append res num2))
							)
							(setq res nil)
						)
					)
				)

				(if (string= (nth 1 tl) "KW_LIST")
					(progn
						(setq control 1)
						(setf (elt tk 1) "(")
						(setf (elt tl 1) "OP_OP")
						(setq num (change (subseq tk 1) (subseq tl 1)))
						(if (equal num nil) (setq res nil) (setq res num))
					)
				)
				(if (equal control 0) (setq res nil))	

			)
			(progn
				(if (string= (nth 0 tl) "CONTROL") (setq res (change (subseq tk 1) (subseq tl 1))) (setq res nil))
				(setq res nil)
			)
		)
		(if (and (not (equal res nil)) (= control2 0)) (write res))
		res
	)
)


(defun expb (&optional(tk tokens) (tl tokenize)(control2 0))
	(let ((len (list-length tk)) (res 0) (control 0) (num 0) (num2 0) (temp 2) (keep2) (kw) (keep)(temp4) (control3 0))
		
		(if (and (string= (nth 0 tl) "OP_OP") (string= (nth (- len 1) tl) "OP_CP" ))
			(progn
				(setq kw (nth 1 tl))
				(if (or (string= kw "KW_AND") (string= kw "KW_OR") (string= kw "KW_EQUAL") (string= kw "KW_LESS"))
					(progn
						(setq control 1)
						(setq keep (nth 2 tl))
						(if (or (string= keep "VALUE") (string= keep "KW_TRUE") (string= keep "KW_FALSE") (string= keep "IDENTIFIER"))
							(progn
								(if (string= keep "VALUE") (setq num (parse-integer (nth 2 tk))) (setq temp 3))
								(if (string= keep "KW_TRUE") (setq num t) (setq temp 3))
								(if (string= keep "KW_FALSE") (setq num -2) (setq temp 3))
								(if (string= keep "IDENTIFIER") (setq num (targetID (nth 2 tk))) (setq temp 3))	
							)
							(progn
								(setq temp4 (expTarget (subseq tk 2)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (and (not (equal temp nil)) (< temp len)) (progn (setq num (expb (subseq tk 2 temp) (subseq tl 2 temp) 1)) (setq keep2 (+ temp 2))) (setq res nil))
							)
						)
						(if (equal temp nil) (setq res nil) (setq keep (nth temp tl)))
						(if (not (equal res nil))
							(if (or (string= keep "VALUE") (string= keep "KW_TRUE") (string= keep "KW_FALSE") (string= keep "IDENTIFIER"))
								(progn
									(if (string= keep "VALUE") (setq num2 (parse-integer (nth temp tk))))
									(if (string= keep "KW_TRUE") (setq num2 t))
									(if (string= keep "KW_FALSE") (setq num2 -2))
									(if (string= keep "IDENTIFIER") (setq num2 (targetID (nth temp tk))))
									(setq keep2 5)	
								)
								(progn
									(setq temp4 (expTarget (subseq tk temp)))
									(if (equal temp4 nil) (progn (setq res nil) (setq keep2 nil)) (setq keep2 (+ temp temp4)))
									(if (and (not (equal keep2 nil)) (< keep2 len)) (progn (setq num2 (expb (subseq tk temp keep2) (subseq tl temp keep2) 1)) (setq keep2 (+ keep2 1))) (setq res nil))
								)
							)
						)
						(if (and (not (equal res nil)) (equal keep2 len) (not (equal num nil)) (not (equal num2 nil)))
							(progn
								(if (equal num -2) (setq num nil))
								(if (equal num2 -2) (setq num2 nil))
								(if (string= kw "KW_AND") (setq res (and num num2)))
								(if (string= kw "KW_OR") (setq res (or num num2)))
								(if (string= kw "KW_EQUAL") (setq res (equal num num2)))
								(if (string= kw "KW_LESS") (setq res (< num num2)))
								(if (= control2 0) (setq control3 2)
									(progn (if (equal res nil) (setq res -2))))
							)
							(setq res nil)
						)

					)
				)
				(if (string= kw "KW_NOT")
					(progn
						(setq control 1)
						(setq keep (nth 2 tl))
						(if (or (string= keep "VALUE") (string= keep "KW_TRUE") (string= keep "KW_FALSE") (string= keep "IDENTIFIER"))
							(progn
								(if (string= keep "VALUE") (setq num (parse-integer (nth 2 tk))) (setq temp 3))
								(if (string= keep "KW_TRUE") (setq num t) (setq temp 3))
								(if (string= keep "KW_FALSE") (setq num nil) (setq temp 3))
								(if (string= keep "IDENTIFIER") (setq num (targetID (nth 2 tk))) (setq temp 3))
								(setq res (not num))
								(if (= control2 0) (setq control3 2)
									(progn (if (equal res nil) (setq res -2))))
							)
							(progn
								(setq temp4 (expTarget (subseq tk 2)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (and (not (equal temp nil)) (< temp len)) (progn (setq num (expb (subseq tk 2 temp) (subseq tl 2 temp) 1)) (setq temp (+ temp 1))) (setq res nil))
								(if (and (not (equal res nil)) (equal temp len) (not (equal num nil)))
									(progn
										(setq res (not num))
									)
									(setq res nil)
								)
							)
						)
					)
				)

				(if (equal control 0) (setq res nil))

			)
			(progn
				(setq keep (nth 0 tl))
				(if (equal len 1)
					(progn
						(if (string= keep "VALUE") (setq num (parse-integer (nth 0 tk))) (setq temp 3))
						(if (string= keep "KW_TRUE") (setq num t) (setq temp 3))
						(if (string= keep "KW_FALSE") (setq num -2) (setq temp 3))
						(if (string= keep "IDENTIFIER") (setq num (targetID (nth 0 tk))) (setq temp 3))
						(setq res num)
					)
					(setq res nil)
				)
			)
		)
		(if (or (and (not (equal res nil)) (= control2 0)) (= control3 2)) (write res))
		(if (and (= control2 0) (= control3 2)) (setq res -798))
		res
 	)
)

(defun change (tk tl)
	(let ((len (list-length tk)) (kw) (res 1) (num) (llist (list)))
		(if (and (string= (nth 0 tl) "OP_OP") (string= (nth (- len 1) tl) "OP_CP" ) (> len 2))
			(progn
				(loop for i from 1 to (- len 2)
				 do(progn
				 		(setq kw (nth i tl))
				 		(if (string= kw "VALUE")
				 			(setq num (list (parse-integer (nth i tk))))
				 			(if (string= kw "IDENTIFIER")  (setq num (targetID (nth i tk))) (setq res nil))
				 		)
				 		(if (not (equal res nil)) (setq llist (append llist num)))
				 	)
				)
				(if (not (equal res nil)) (setq res llist))
			)
			(setq res nil)
		)
		res
	)
)

(defun targetID (exp)
	(let((res 0))
		(setq res (position exp representation :test #'string=))
		(if (equal res nil)
			(progn (format t "variable ~S has no value." exp) (terpri) (exit))
			(setq res (nth res value))
		)
		res
	)
)

(defun expTarget (exp)
	(let ((counter 0) (str) (j 0) (res nil))
		(if (string= (nth 0 exp) "(")
			(progn
				(loop for i in exp
					do (progn
						(setq str (string-downcase i))
						(if (string= str "(") (setq counter (+ counter 1)))
						(if (string= str ")") (setq counter (- counter 1)))
						(setq j (+ j 1))
						(if (= counter 0) (return j))
					)
				)
				(setq res j)
			)
		)
		res
	)
)


(defun KEEP_LIST (tk tl)
	(let ((len (list-length tl)) (kw) (res 1))
		(if (and (string= (nth 0 tl) "OP_OP") (string= (nth (- len 1) tl) "OP_CP" ) (> len 2))
			(progn
				(loop for i from 1 to (- len 2)
				 do(progn
				 		(setq kw (nth i tl))
				 		(if (string= kw "IDENTIFIER")  (progn (setq representation (append representation (list (nth i tk)))) (setq value (append value (list 0)))) (setq res nil))
				 	)
				)
			)
			(setq res nil)
		)
		res
	)
)


(defun GppLanguage()
    ;if terminal argument is null
  (if (null *args*) 
        (gppinterpreter)
		(gppinterpreter (car *args*)) ;else condition
    )

)


(GppLanguage)