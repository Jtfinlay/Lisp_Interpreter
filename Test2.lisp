(load "Assn2.l")

(defun testAll ()
	;Tests A2

	;PRIMITIVES (P) = 5 points total
	;Basic = 2.5 total
	(print (if (eq (interp '(+ 10 5) nil) '15) 'P1-OK 'P1-error))
	(print (if (eq (interp '(- 12 8) nil) '4) 'P2-OK 'P2-error))
	(print (if (eq (interp '(* 5 9) nil) '45) 'P3-OK 'P3-error))
	(print (if (not (interp '(> 2 3) nil)) 'P4-OK 'P4-error))
	(print (if (interp '(< 1 131) nil) 'P5-OK 'P5-error))
	(print (if (interp '(= 88 88) nil) 'P6-OK 'P6-error))
	(print (if (not(interp '(and nil T) nil)) 'P7-OK 'P7-error))
	(print (if (interp '(or T nil) nil) 'P8-OK 'P8-error))
	(print (if (not(interp '(not T) nil)) 'P9-OK 'P9-error))
	(print (if (interp '(number 354) nil) 'P10-OK 'P10-error))
	(print (if (interp '(equal (3 4 1) (3 4 1)) nil) 'P11-OK 'P11-error))
	(print (if (eq (interp '(if nil 2 3) nil) '3) 'P12-OK 'P12-error))
	(print (if (interp '(null ()) nil) 'P13-OK 'P13-error))
	(print (if (not(interp '(atom (3)) nil)) 'P14-OK 'P14-error))
	(print (if (interp '(eq x x) nil) 'P15-OK 'P15-error))
	(print (if (eq (interp '(first (8 5 16)) nil) '8) 'P16-OK 'P16-error))
	(print (if (equal (interp '(rest (8 5 16)) nil) '(5 16)) 'P17-OK 'P17-error))
	(print (if (equal (interp '(cons 6 3) nil) (cons 6 3)) 'P18-OK 'P18-error))

	;More complex = 2.5 total
	(print (if (eq (interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil) '12) 'P19-OK 'P19-error))
	(print (if (interp '(and (> (+ 3 2) (- 4 2)) (or (< 3 (* 2 2))) (not (= 3 2))) nil) 'P20-OK 'P20-error))
	(print (if (not (interp '(or (= 5 (- 4 2)) (and (not (> 2 2)) (< 3 2))) nil)) 'P21-OK 'P21-error))
	(print (if (equal (interp '(if (not (null (first (a c e)))) (if (number (first (a c e))) (first (a c e)) (cons (a c e) d)) (rest (a c e))) nil) (cons '(a c e) 'd)) 'P22-OK 'P22-error))

	;USER-DEFINED (U) = 10 points total
	;Basic = 4 total
	(print (if (eq (interp '(greater 3 5) '((greater x y = (if (> x y) x (if (< x y) y nil))))) '5) 'U1-OK 'U1-error))
	(print (if (eq (interp '(square 4) '((square x = (* x x)))) '16) 'U2-OK 'U2-error))
	(print (if (eq (interp '(simpleinterest 4 2 5) '((simpleinterest x y z = (* x (* y z))))) '40) 'U3-OK 'U3-error))
	(print (if (interp '(xor T nil) '((xor x y = (if (equal x y) nil t)))) 'U4-OK 'U4-error))
	(print (if (eq (interp '(cadr (5 1 2 7)) '((cadr x = (first (rest x))))) '1) 'U5-OK 'U5-error))
	;More complex = 6 total
	(print (if (eq (interp '(last (s u p)) '((last x = (if (null (rest x)) (first x) (last (rest x)))))) 'p) 'U6-OK 'U6-error))
	(print (if (equal (interp '(push (1 2 3) 4) '((push x y = (if (null x) (cons y nil) (cons (first x) (push (rest x) y)))))) '(1 2 3 4)) 'U7-OK 'U7-error))
	(print (if (equal (interp '(pop (1 2 3)) '((pop x = (if (atom (rest (rest x))) (cons (first x) nil) (cons (first x)(pop (rest x))))))) '(1 2)) 'U8-OK 'U8-error))
	(print (if (eq (interp '(power 4 2) '((power x y = (if (= y 1) x (power (* x x) (- y 1)))))) '16) 'U9-OK 'U9-error))
	(print (if (eq (interp '(factorial 4) '((factorial x = (if (= x 1) 1 (* x (factorial (- x 1))))))) '24) 'U10-OK 'U10-error))
	(print (if (eq (interp '(divide 24 4) '((divide x y = (div x y 0)) (div x y z = (if (> (* y z) x) (- z 1) (div x y (+ z 1)))))) '6) 'U11-OK 'U11-error))

)
