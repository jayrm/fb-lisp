;; list.lsp

;; lisp-syntax: (car <list>)
;; lisp-syntax: (car <cons>)
;; lisp-syntax: (cdr <list>)
;; lisp-syntax: (cdr <cons>)
;; lisp-syntax: (cons <expr1> expr2)
;; lisp-syntax: (list expr...)

;; lisp-syntax: (append <expr>...)
;; lisp-syntax: (length <list>)
;; lisp-syntax: (nth <index> <list>)
;; lisp-syntax: (elt <list> <index>)
;; lisp-syntax: (last <list>)


;; --------------------------
;; car/cdr
;; --------------------------

(assertn '(car '()))
(assertn '(cdr '()))
(asserteq '(car '(foo bar baz)) ''foo)
(asserteq '(car (cdr '(foo bar baz))) ''bar)
(asserteq '(car (cdr '(foo bar baz))) ''bar)
(asserteq '(car (cdr (cdr '(foo bar baz)))) ''baz)
(asserteq '(car (cdr (cdr (cdr '(foo bar baz))))) nil)
 
(assertequal '(cdr '(foo bar baz)) ''(bar baz))
(assertequal '(cdr (cdr '(foo bar baz))) ''(baz))


;; --------------------------
;; cons
;; --------------------------

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


;; --------------------------
;; list
;; --------------------------

(assertequal '(list) 'nil )
(assertequal '(list) nil )
(assertequal '(list) ''() )

(assertequal '(list 1 2 3) '(cons 1 (cons 2 (cons 3))) )
(assertequal '(list 1 2 3) ''(1 2 3) )
(assertequal '(list 'a 'b) ''(a b) )
(assertequal '(list 1 'a) ''(1 a) )
(assertequal '(car (list 1 2 3)) '1 )
(assertequal '(cdr (list 1 2 3)) ''(2 3) )
(assertequal '(list 1) '(cons 1 nil) )
(assertequal '(list 1) '(cons 1 nil) )
(assertequal '(list 1 'a) '(cons 1 (cons 'a nil)) )
(assertequal '(list 1 'a 3) '(cons 1 (cons 'a (cons 3 nil))) )
(assertequal '(list 1 'a 3) ''(1 . (a . (3 . nil))) )
(assertequal ''(1 2 3) '(list 1 2 3) )

;; NOT-SUPPORTED
;; - (list 'a #c(1 2) "moo") => (A #C(1 2) "moo")


;; --------------------------
;; append
;; --------------------------

(assertequal '(append) nil )
(assertequal '(append '(1 2 3)) ''(1 2 3) )
(assertequal '(append '(1 2 3) '(4 5 6)) ''(1 2 3 4 5 6) )
(assertequal '(append '(1 2 3) '(4 5 6) '(7 8 9)) ''(1 2 3 4 5 6 7 8 9) )

;; NOT-SUPPORTED
;; - (let ((x '(tail list))) (eq x (cddr (append '(front list) x)))) => T


;; --------------------------
;; length
;; --------------------------

(assertequal '(length '(a . (b . nil))) '2 )

(assertequal '(length (list 'a 'b 'c)) '3 )
(assertequal '(length nil) '0 )
(assertequal '(length (cons "moo" nil)) '1 )
(assertequal '(length (cons "moo" (cons "boo" nil))) '2 )

;; NOT-SUPPORTED
;; - (assertequal '(length '#1=(a . (b . #1#))) 'nil )


;; --------------------------
;; nth
;; --------------------------

(assertequal '(nth 0 '(foo bar baz)) ''foo )
(assertequal '(nth 1 '(foo bar baz)) ''bar )
(assertequal '(nth 3 '(foo bar baz)) ''nil )
(assertequal '(setq x (list 0 1 2 3)) ''(0 1 2 3) )

;; NOT-SUPPORTED
;; - (setf) does not update 'x
;; - (assertequal '(setf (nth 2 x) "two") '"two" )
;; - (assertequal 'x ''(0 1 'two 3) )


;; --------------------------
;; elt
;; --------------------------

;; NOT-SUPPORTED

;; --------------------------
;; last
;; --------------------------

(assertequal '(last nil) '() )
(assertequal '(last nil) 'nil )
(assertequal '(last '()) '() )
(assertequal '(last '()) 'nil )

(assertequal '(last '(1 2 3)) ''(3) )
(assertequal '(last '(a . b) ) ''(a . b) )
(assertequal '(last '(list 1 2 3) ) ''(3) )

;; NOT-SUPPORTED
;; - (assertequal '(last '(1 2 3) 0) 'nil )
;; - (assertequal '(last '(1 2 3) 1) '(3) )
;; - (assertequal '(last '(1 2 3) 2) '(2 3) )
;; - (assertequal '(last '(1 2 3) 3) '(1 2 3) )
;; - (assertequal '(last '(1 2 3) 4) '(1 2 3) )
;; - (assertequal '(last '(a . b) 0) 'B )
;; - (assertequal '(last '(a . b) 1) '(A . B) )
;; - (assertequal '(last '(a . b) 2) '(A . B) )
