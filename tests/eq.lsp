;; eq.lsp


(assertn (eq 'a 'b) )   ;; is false.
(assert  (eq 'a 'a) )   ;;  is true. 
(assert? (eq 3 3) )     ;;  might be true or false, depending on the implementation. 
(assertn (eq 3 3.0) )   ;;  is false. 
(assert? (eq 3.0 3.0) ) ;;  might be true or false, depending on the implementation. 
;; (assert? (eq #c(3 -4) #c(3 -4)) ) ;;    might be true or false, depending on the implementation. 
;; (assertn (eq #c(3 -4.0) #c(3 -4)) ) ;;  is false. 
(assertn (eq (cons 'a 'b) (cons 'a 'c)) ) ;;  is false. 
(assertn (eq (cons 'a 'b) (cons 'a 'b)) ) ;;  is false. 
(assertn (eq '(a . b) '(a . b)) ) ;;  might be true or false. 
(assert  (progn (setq x (cons 'a 'b)) (eq x x)) ) ;;  is true. 
(assert  (progn (setq x '(a . b)) (eq x x)) ) ;;  is true. 
;; (assert? (eq #\A #\A) ) ;;  might be true or false, depending on the implementation. 
(assert (eq "Foo" "Foo") ) ;;  might be true or false, depending on the implementation. 
(assertn (eq "Foo" (copy-seq "Foo")) ) ;;  is false. 
(assertn (eq "FOO" "foo") ) ;;  is false.
