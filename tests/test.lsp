(princ "LISP Test Suite\n")

(defun assert (exp)
  (or (eval exp)
      (princ "assertion failed[ " (+ (lineno) 1) " ]: " exp ":=(" (eval exp) ")\n")))

(defun asserteq (exp1 exp2)
  (or (eq (eval exp1) (eval exp2))
      (princ "assertion failed[ " (+ (lineno) 1) " ]: " exp1 ":=(" (eval exp1) ") == "
	     exp2 ":=(" (eval exp2) ")\n")))

(defun assertneq (exp1 exp2)
  (or (not (eq (eval exp1) (eval exp2)))
      (princ "assertion failed[ " (+ (lineno) 1) " ]: " exp1 ":=(" (eval exp1) ") != "
	     exp2 ":=(" (eval exp2) ")\n")))

(defun assertequal (exp1 exp2)
  (or (equal (eval exp1) (eval exp2))
      (princ "assertion failed[ " (+ (lineno) 1) " ]: " exp1 ":=(" (eval exp1) ") == "
	     exp2 ":=(" (eval exp2) ")\n")))

(defun assertnequal (exp1 exp2)
  (or (not (equal (eval exp1) (eval exp2)))
      (princ "assertion failed[ " (+ (lineno) 1) " ]: " exp1 ":=(" (eval exp1) ") != "
	     exp2 ":=(" (eval exp2) ")\n")))

(defun assertn (exp)
  (assert (list 'not exp)))


(assert t)
(assertn nil)
(asserteq '() nil)

;; eq
(asserteq ''foo ''foo)
(assertneq ''foo ''bar)

;; equal
(assertequal ''() ''())
(assertequal ''() 'nil)
(assertequal 'nil ''())
(assertequal ''foo ''foo)
(assertnequal ''foo ''bar)
(assertequal '(list 'foo) '(list 'foo))
(assertnequal '(list 'foo) '(list 'bar))
(assertequal ''('foo . 'bar) ''('foo . 'bar))
(assertnequal ''('foo . 'bar) ''('foo . 'baz))

;; `car', `cdr' test
(assertn '(car '()))
(assertn '(cdr '()))
(asserteq '(car '(foo bar baz)) ''foo)
(asserteq '(car (cdr '(foo bar baz))) ''bar)
(asserteq '(car (cdr '(foo bar baz))) ''bar)
(asserteq '(car (cdr (cdr '(foo bar baz)))) ''baz)
(asserteq '(car (cdr (cdr (cdr '(foo bar baz))))) nil)

;; `atom' test
(assert '(atom t))
(assert '(atom nil))
(assert '(atom '()))
(assert '(atom 123))
(assert '(atom 123.456))
(assert '(atom "ABC"))
(assertn '(atom '(1 . 2)))
(assertn '(atom '(foo)))
(assertn '(atom '(foo bar baz)))

;; `cons', `list' test
(asserteq '(car (cons 'foo nil)) ''foo)
(asserteq '(cdr (cons 'foo nil)) nil)
(asserteq '(car (cdr (cons 'foo nil))) nil)
(asserteq '(car (cdr (cons 'foo (cons 'bar)))) ''bar)
(asserteq '(car (cons 'foo (cons 'bar))) ''foo)
(asserteq '(cdr (cdr (cons 'foo (cons 'bar)))) nil)
(asserteq '(car (list 'foo)) ''foo)
(asserteq '(cdr (list 'foo)) nil)
(asserteq '(car (cdr (list 'foo))) nil)
(asserteq '(car (cdr (list 'nil 'bar))) ''bar)
(asserteq '(car (list 'foo 'bar)) ''foo)
(asserteq '(cdr (cdr (list 'foo 'bar))) nil)

;; consp test
(assertn '(consp nil) )
(assertn '(consp 'some-symbol) )
(assertn '(consp 3) )
(assertn '(consp "moo") )
(assert '(consp (cons 1 2)) )
(assert '(consp '(1 . 2)) )
(assert '(consp '(1 2 3 4)) )
(assert '(consp (list 1 2 3 4)) )

;; listp test
(assert '(listp nil) )
(assertn '(listp 'some-symbol) )
(assertn '(listp 3) )
(assertn '(listp "moo") )
(assert '(listp (cons 1 2)) )
(assert '(listp '(1 . 2)) )
(assert '(listp '(1 2 3 4)) )
(assert '(listp (list 1 2 3 4)) )

;; `and', `or', `not' test
(assert '(and t))
(assertn '(and n))
(assert '(and t t))
(assertn '(and nil nil))
(assertn '(and t nil))
(assertn '(and nil t))
(assert '(or t))
(assertn '(or n))
(assert '(or t t))
(assertn '(or nil nil))
(assert '(or t nil))
(assert '(or nil t))
(assertn '(not t))
(assert '(not nil))
(assertn '(not 'foo))

;; `if' test
(assertn '(if t nil t t))
(assert '(if nil nil t t))

(assertn '(while nil t))

;; `cond' test
(assertn '(cond (nil t) (t nil)))
(assertn '(cond (nil t) (nil t)))
(assert '(cond (nil nil) (t t)))
(assert '(cond (nil) (t)))
(assertn '(cond (nil) (nil)))
(asserteq '(cond (nil 'foo) (nil 'bar)) nil)
(asserteq '(cond (nil 'foo) (t 'bar)) ''bar)
(asserteq '(cond (t 'foo) (t 'bar)) ''foo)
(asserteq '(cond (nil 'foo) (t 'bar 'baz)) ''baz)

;; `progn' test
(assertn '(progn t nil nil))
(assert '(progn t nil nil t))
(asserteq '(progn t nil nil 'foo) ''foo)
(asserteq '(progn t nil nil 'foo 'bar) ''bar)
;; FIXME: (assert '(prog1 t nil nil))
;; FIXME: (assertn '(prog2 t nil t t))

;; Recursion test
(defun last* (l)
  (cond ((eq (cdr l) nil) (car l))
	(t (last* (cdr l)))))

(asserteq '(last* '(foo bar baz)) ''baz)
(asserteq '(last* '(foo bar)) ''bar)
(asserteq '(last* '(foo)) ''foo)

;; `cond*' test
(or (eq cond* nil)
    (progn (assertn '(cond* '((nil t) (t nil))))
	   (assertn '(cond* '((nil t) (nil t))))
	   (assert '(cond* '((nil nil) (t t))))
	   (assert '(cond* '((nil) (t))))
	   (assertn '(cond* '((nil) (nil))))
	   (asserteq '(cond* '((nil 'foo) (nil 'bar))) nil)
	   (asserteq '(cond* '((nil 'foo) (t 'bar))) ''bar)
	   (asserteq '(cond* '((t 'foo) (t 'bar))) ''foo)
	   (asserteq '(cond* '((nil 'foo) (t 'bar 'baz))) ''baz)))

;; `progn*' test
(or (eq progn* nil)
    (progn (assertn '(progn* '(t nil nil)))
	   (assert '(progn* '(t nil nil t)))
	   (asserteq '(progn* '(t nil nil 'foo)) ''foo)
	   (asserteq '(progn* '(t nil nil 'foo 'bar)) ''bar)))

;; listp
(assertn '(listp t))
(assert '(listp nil))
(assert '(listp '()))
(assert '(listp '(foo bar)))
(assert '(listp '(foo . bar)))
(assert '(listp '(list foo bar)))


;; =
(assert '(= 1 1))
(assert '(= 1 1.0))
(assert '(= 1.0 1))
(assert '(= 1.0 1.0))
(assert '(= 1 1 1.0))

(assertn '(= 1 2))
(assertn '(= 1 2.0))
(assertn '(= 1.0 2))
(assertn '(= 1.0 2.0))
(assertn '(= 1 2 2.0))

;; /=
(assertn '(/= 1 1))
(assertn '(/= 1 1.0))
(assertn '(/= 1.0 1))
(assertn '(/= 1.0 1.0))
(assertn '(/= 1 1 1.0))

(assert '(/= 1 2))
(assert '(/= 1 2.0))
(assert '(/= 1.0 2))
(assert '(/= 1.0 2.0))
(assert '(/= 1 2 2.0))


;; *
(assert '(= (* 1) 1))
(assert '(= (* 1 2) 2))
(assert '(= (* 2 2) 4))
(assert '(= (* 2 3) 6))
(assert '(= (* -2 2) -4))

;; /
(assert '(= (/ 1) 1))
(assert '(= (/ 1.0 2.0) 0.5))
(assert '(= (/ 2.0 2.0) 1.0))
(assert '(= (/ 4.0 2.0) 2.0))
(assert '(= (/ -2.0 2.0) -1.0))


(princ "Done\n")
