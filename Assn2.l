; James Finlay 1263258
; CMPUT 325 - LEC B1
; Assignment # 2

;Main function to run interpreter.
;Reference the class notes for man.
;E: Expression
;P: Program
(defun interp (E P)
	(interp2 E P nil)
)
;Recursive function for interpreter.
;
;NOTE - Unquote should be allowed for this assignment. I had a really sleek
;implementation originally that cut the code down in half, but for some reason
;unquote isn't in the function list.
;
;E: Expression
;P: Program
;vars: Scoped variables
(defun interp2 (E P vars)
	(cond
		((not (null (find E vars))) (cadr (find E vars)))
		((atom E) E)
		(T (let* ((f   (car E))
				  (arg (cdr E)) 
				  (P2 (find f P)) )
			(cond

				((not (atom f)) E)

				;Primitives and specific
				((eq f 'first) (caar (l_interp arg P vars)))
				((eq f 'rest) (cdar (l_interp arg P vars)))
				((eq f 'number) (numberp (car (l_interp arg P vars))))
				((eq f 'if) (c_if (car arg) (cadr arg) (caddr arg) P vars))
				((eq f 'and) (c_and arg P vars))
				((eq f 'or) (c_or arg P vars))
				((primitive f) (apply f (l_interp arg P vars)))

				;User defined
				((not (null P2)) 
					(interp2 
						(car (funcP P2)) P 
						(append (mapvars (argsP (cdr P2)) (l_interp arg P vars)) vars)
					)
				)
				(T E)
			)
		))
	)
)
;Checks whether given value is part of primitive whitelist
;V: value to check
(defun primitive (V)
	(contains V '(null atom cons + - * > < = not equal eq))
)
;Function to see whether value is part of list
;V: value to search for
;L: list to search
(defun contains (V L)
	(cond
		((null L) nil)
		((eq V (car L)) T)
		(T (contains V (cdr L)))
	)
)
;Function to map interpreter to values of a list
;V: list
;P: program
;vars: scoped variables
(defun l_interp (V P vars)
	(mapcar #'(lambda (x) (interp2 x P vars)) V)
)
;Custom 'if' to ensure evaluated in order
;a: conditional
;b: method if true
;c: method if false
;P: program
;vars: scoped variables
(defun c_if (a b c P vars)
	(if (interp2 a P vars) (interp2 b P vars) (interp2 c P vars))
)
;Custom 'or' to ensure evaluated in order
;args: list
;P: program
;vars: scoped variables
(defun c_or (args P vars)
	(cond
		((null args) nil)
		((interp2 (car args) P vars) T) 
		(T (c_or (cdr args) P vars))
	)
)
;Customer 'and' to ensure evaluated in order
;args: list
;P: program
;vars: scoped variables
(defun c_and (args P vars)
	(cond
		((null args) nil)
		((null (interp2 (car args) P vars)) nil)
		((null (cdr args)) T)
		(T (c_and (cdr args) P vars))
	)
)
;Extract args from program definition
;P: program
(defun argsP (P)
	(if (eq '= (car P)) () (cons (car P) (argsP (cdr P))))
)
;Extract function from program definition
;P: program
(defun funcP (P)
	(if (eq '= (car P)) (cdr P) (funcP (cdr P)))
)
;Find key/value pair from 2d list
;key: key to search for
;L: key/value pair, or nil
(defun find (key L)
	(cond
		((null L) nil)
		((eq key (caar L)) (car L))
		(T (find key (cdr L)))
	)
)
;Map values to variables
;vars: list of variables
;vals: list of values
(defun mapvars (vars vals)
	(mapcar #'(lambda (x y) (list x y)) vars vals)
)
