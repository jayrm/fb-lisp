;; types.lsp

;; lisp-syntax: (atom <expr>)
;; lisp-syntax: (consp <expr>)
;; lisp-syntax: (listp <expr>)
;; lisp-syntax: (integerp <expr>)
;; lisp-syntax: (numberp <expr>)
;; lisp-syntax: (zerop <expr>)
;; lisp-syntax: (stringp <expr>)


;; --------------------------
;; atom
;; --------------------------

;; ATOM function returns true if the argument is not a cons cell, 
;; otherwise it returns false

(assert  '(atom nil) )            ;; => T
(assert  '(atom 'some-symbol)  )  ;; => T
(assert  '(atom 3) )              ;; => T
(assert  '(atom "moo") )          ;; => T
(assertn '(atom (cons 1 2)) )     ;; => NIL
(assertn '(atom '(1 . 2)) )       ;; => NIL
(assertn '(atom '(1 2 3 4)) )     ;; => NIL
(assertn '(atom (list 1 2 3 4)) ) ;; => NIL
 
(assert  '(atom t))
(assert  '(atom nil))
(assert  '(atom '()))
(assert  '(atom 123))
(assert  '(atom 123.456))
(assert  '(atom "ABC"))
(assertn '(atom '(1 . 2)))
(assertn '(atom '(foo)))
(assertn '(atom '(foo bar baz)))


;; --------------------------
;; consp
;; --------------------------

(assertn '(consp nil) )
(assertn '(consp 'some-symbol) )
(assertn '(consp 0) )
(assertn '(consp 0.0) )
(assertn '(consp 3) )
(assertn '(consp 3.0) )
(assertn '(consp "") )
(assertn '(consp "moo") )
(assert  '(consp (cons 1 2)) )
(assert  '(consp '(1 . 2)) )
(assert  '(consp '(1 2 3 4)) )
(assert  '(consp (list 1 2 3 4)) )
(assertn '(consp t))
(assertn '(consp '()))
(assert  '(consp '(foo bar)))
(assert  '(consp '(foo . bar)))
(assert  '(consp '(list foo bar)))


;; --------------------------
;; listp
;; --------------------------

(assert  '(listp nil) )
(assertn '(listp 'some-symbol) )
(assertn '(listp 0) )
(assertn '(listp 0.0) )
(assertn '(listp 3) )
(assertn '(listp 3.0) )
(assertn '(listp "") )
(assertn '(listp "moo") )
(assert  '(listp (cons 1 2)) )
(assert  '(listp '(1 . 2)) )
(assert  '(listp '(1 2 3 4)) )
(assert  '(listp (list 1 2 3 4)) )
(assertn '(listp t))
(assert  '(listp '()))
(assert  '(listp '(foo bar)))
(assert  '(listp '(foo . bar)))
(assert  '(listp '(list foo bar)))

;; --------------------------
;; integerp
;; --------------------------

(assertn '(integerp nil) )
(assertn '(integerp 'some-symbol) )
(assert  '(integerp 0) )
(assertn '(integerp 0.0) )
(assert  '(integerp 3) )
(assertn '(integerp 3.0) )
(assertn '(integerp "") )
(assertn '(integerp "moo") )
(assertn '(integerp (cons 1 2)) )
(assertn '(integerp '(1 . 2)) )
(assertn '(integerp '(1 2 3 4)) )
(assertn '(integerp (list 1 2 3 4)) )
(assertn '(integerp t))
(assertn '(integerp '()))
(assertn '(integerp '(foo bar)))
(assertn '(integerp '(foo . bar)))
(assertn '(integerp '(list foo bar)))

;; --------------------------
;; numberp
;; --------------------------

(assertn '(numberp nil) )
(assertn '(numberp 'some-symbol) )
(assert  '(numberp 0) )
(assert  '(numberp 0.0) )
(assert  '(numberp 3) )
(assert  '(numberp 3.0) )
(assertn '(numberp "") )
(assertn '(numberp "moo") )
(assertn '(numberp (cons 1 2)) )
(assertn '(numberp '(1 . 2)) )
(assertn '(numberp '(1 2 3 4)) )
(assertn '(numberp (list 1 2 3 4)) )
(assertn '(numberp t))
(assertn '(numberp '()))
(assertn '(numberp '(foo bar)))
(assertn '(numberp '(foo . bar)))
(assertn '(numberp '(list foo bar)))

;; --------------------------
;; zerop
;; --------------------------

(assertn '(zerop nil) )
(assertn '(zerop 'some-symbol) )
(assert  '(zerop 0) )
(assert  '(zerop 0.0) )
(assertn '(zerop 3) )
(assertn '(zerop 3.0) )
(assertn '(zerop "") )
(assertn '(zerop "moo") )
(assertn '(zerop (cons 1 2)) )
(assertn '(zerop '(1 . 2)) )
(assertn '(zerop '(1 2 3 4)) )
(assertn '(zerop (list 1 2 3 4)) )
(assertn '(zerop t))
(assertn '(zerop '()))
(assertn '(zerop '(foo bar)))
(assertn '(zerop '(foo . bar)))
(assertn '(zerop '(list foo bar)))

;; --------------------------
;; stringp
;; --------------------------

(assertn '(stringp nil) )
(assertn '(stringp 'some-symbol) )
(assertn '(stringp 0) )
(assertn '(stringp 0.0) )
(assertn '(stringp 3) )
(assertn '(stringp 3.0) )
(assert  '(stringp "") )
(assert  '(stringp "moo") )
(assertn '(stringp (cons 1 2)) )
(assertn '(stringp '(1 . 2)) )
(assertn '(stringp '(1 2 3 4)) )
(assertn '(stringp (list 1 2 3 4)) )
(assertn '(stringp t))
(assertn '(stringp '()))
(assertn '(stringp '(foo bar)))
(assertn '(stringp '(foo . bar)))
(assertn '(stringp '(list foo bar)))
