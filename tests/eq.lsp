;; eq.lsp

;; = 
;; compares only numbers, regardless of type. (see mathcomp.lsp)

;; (eq <expr1> <expr2>)
;; - compares symbols. 
;; - Two objects are eq iff they are actually the same object in memory. 
;; - Don't use it for numbers and characters.

;; (eql <expr1> <expr2>)
;; - compares symbols similarly to eq, numbers (type sensitive) 
;;   and characters (case sensitive)
;; - is true if the objects are eq or they are both numbers or characters of the same type 
;;   (floating point, character, integer, etc.) and are equal in value

;; (equal <expr1> <expr2>)
;; - compares more general objects. 
;; - Two objects are equal iff they are eql, strings of eql characters, 
;;   bit vectors of the same contents, or lists of equal objects. 
;; - For anything else, eq is used.

;; (equalp <expr1> <expr2>)
;; - is like equal, just more advanced. 
;; - Comparison of numbers is type insensitive. 
;; - Comparison of chars and strings is case insensitive. 
;; - Lists, hashes, arrays and structures are equalp if their members are equalp. 
;; - For anything else, eq is used.


;; --------------------------
;; eq
;; --------------------------

(assert  (eq t t) )
(assert  (eq nil nil) )
(assertn (eq t nil) )
(assertn (eq nil t) )

(assertn (eq nil 3) )
(assertn (eq t 3) )

(assertn (eq nil 3.0) )
(assertn (eq t 3.0) )

(assertn (eq nil 'x) )
(assertn (eq t 'x) )

(assertn (eq nil "foo") )
(assertn (eq t "foo") )

(assertn (eq nil (cons 'a 'b)) )
(assertn (eq t (cons 'a 'b)) )

(assertn (eq 'a 'b) ) ;; is false.
(assert  (eq 'a 'a) ) ;; is true.

(assertn (eq 3 3.0) ) ;; is false. 
(assertn (eq 3.0 3) ) ;; is false. 

(assert  (eq 3 3) )     ;; might be true or false, depending on the implementation.
(assert  (eq 3.0 3.0) ) ;; might be true or false, depending on the implementation.

(assertn (eq (cons 'a 'b) (cons 'a 'c)) ) ;; is false.
(assertn (eq (cons 'a 'b) (cons 'a 'b)) ) ;; is false.
(assertn (eq '(a . b) '(a . b)) )         ;; might be true or false, depending on the implementation.

(assert  (progn (setq x (cons 'a 'b)) (eq x x)) ) ;;  is true.
(assert  (progn (setq x '(a . b)) (eq x x)) )     ;;  is true.

(assert (eq "Foo" "Foo") )  ;;  might be true or false, depending on the implementation.
(assertn (eq "FOO" "foo") ) ;;  is false.

;; NOT-SUPPORTED
;; (assert  (eq #c(3 -4) #c(3 -4)) )   ;;    might be true or false, depending on the implementation.
;; (assertn (eq #c(3 -4.0) #c(3 -4)) ) ;;  is false. 
;; (assert  (eq #\A #\A) ) ;;  might be true or false, depending on the implementation.
;; (assertn (eq "Foo" (copy-seq "Foo")) ) ;;  is false.


;; --------------------------
;; eql
;; --------------------------

(assert  (eql t t) )
(assert  (eql nil nil) )
(assertn (eql t nil) )
(assertn (eql nil t) )

(assertn (eql nil 3) )
(assertn (eql t 3) )

(assertn (eql nil 3.0) )
(assertn (eql t 3.0) )

(assertn (eql nil 'foo) )
(assertn (eql t 'foo) )

(assertn (eql nil "foo") )
(assertn (eql t "foo") )

(assertn (eql 'a 'b) ) ;; is false. 
(assert  (eql 'a 'a) ) ;; is true. 

(assertn (eql 3 3.0) ) ;; is false. 
(assertn (eql 3.0 3) ) ;; is false. 

(assert  (eql 3 3) )     ;; is true. 
(assert  (eql 3.0 3.0) ) ;; is true. 

(assertn (eql (cons 'a 'b) (cons 'a 'c)) ) ;; is false. 
(assertn (eql (cons 'a 'b) (cons 'a 'b)) ) ;; is false. 
(assertn (eql '(a . b) '(a . b)) ) ;; might be true or false, depending on the implementation.

(assert  (progn (setq x (cons 'a 'b)) (eql x x)) ) ;; is true. 
(assert  (progn (setq x '(a . b)) (eql x x)) )     ;; is true. 

(assert  (eql "Foo" "Foo") ) ;; might be true or false, depending on the implementation.
(assertn (eql "FOO" "foo") ) ;; is false.

;; NOT-SUPPORTED
;; (assert  (eql #c(3 -4) #c(3 -4)) )   ;; is true. 
;; (assertn (eql #c(3 -4.0) #c(3 -4)) ) ;; is false. 
;; (assert  (eql #\A #\A) is true. 
;; (assertn (eql "Foo" (copy-seq "Foo")) is false. 


;; --------------------------
;; equal
;; --------------------------

(assert  (equal t t) )
(assert  (equal nil nil) )
(assertn (equal t nil) )
(assertn (equal nil t) )

(assertn (equal nil 3) )
(assertn (equal t 3) )

(assertn (equal nil 3.0) )
(assertn (equal t 3.0) )

(assertn (equal nil 'foo) )
(assertn (equal t 'foo) )

(assertn (equal nil "foo") )
(assertn (equal t "foo") )

(assertn (equal 'a 'b) ) ;; is false. 
(assert  (equal 'a 'a) ) ;;  is true. 

(assertn (equal 3 3.0) ) ;;  is false. 
(assertn (equal 3.0 3) ) ;;  is false. 

(assert  (equal 3 3) )     ;;  is true. 
(assert  (equal 3.0 3.0) ) ;;  is true. 

(assertn (equal (cons 'a 'b) (cons 'a 'c)) ) ;;  is false. 
(assert  (equal (cons 'a 'b) (cons 'a 'b)) ) ;;  is true. 
(assert  (equal '(a . b) '(a . b)) )         ;;  is true. 

(assert  (progn (setq x (cons 'a 'b)) (equal x x)) ) ;;  is true. 
(assert  (progn (setq x '(a . b)) (equal x x)) )     ;;  is true. 

(assert  (equal "Foo" "Foo") ) ;;  is true. 
(assertn (equal "FOO" "foo") ) ;;  is false.

;; NOT-SUPPORTED
;; (assertn (equal #c(3 -4) #c(3 -4)) ) ;;  is true. 
;; (assertn (equal #c(3 -4.0) #c(3 -4)) ) ;;  is false. 
;; (assertn (equal #\A #\A) ) ;;  is true. 
;; (assertn (equal "Foo" (copy-seq "Foo")) ) ;;  is true. 


;; --------------------------
;; equalp
;; --------------------------

(assert  (equalp t t) )
(assert  (equalp nil nil) )
(assertn (equalp t nil) )
(assertn (equalp nil t) )

(assertn (equalp nil 3) )
(assertn (equalp t 3) )

(assertn (equalp nil 3.0) )
(assertn (equalp t 3.0) )

(assertn (equalp nil 'foo) )
(assertn (equalp t 'foo) )

(assertn (equalp nil "foo") )
(assertn (equalp t "foo") )

(assertn (equalp 'a 'b) ) ;; is false. 
(assert  (equalp 'a 'a) ) ;; is true. 

(assert  (equalp 3 3.0) )   ;; is true. 
(assert  (equalp 3.0 3) )   ;; is true. 

(assert  (equalp 3 3) )     ;; is true. 
(assert  (equalp 3.0 3.0) ) ;; is true. 

(assertn (equalp (cons 'a 'b) (cons 'a 'c)) ) ;; is false. 
(assert  (equalp (cons 'a 'b) (cons 'a 'b)) ) ;; is true. 
(assert  (equalp '(a . b) '(a . b)) ) ;; is true. 

(assert  (progn (setq x (cons 'a 'b)) (equalp x x)) ) ;; is true. 
(assert  (progn (setq x '(a . b)) (equalp x x)) ) ;; is true. 

(assert  (equalp "Foo" "Foo") ) ;; is true. 
(assert  (equalp "FOO" "foo") ) ;; is true.

;; NOT-SUPPORTED

;; (assert (equalp #c(3 -4) #c(3 -4)) ) ;; is true. 
;; (assert (equalp #c(3 -4.0) #c(3 -4)) ) ;; is true. 
;; (assert (equalp #\A #\A) ) ;; is true. 
;; (assert (equalp "Foo" (copy-seq "Foo")) ) ;; is true. 
