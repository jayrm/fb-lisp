;; branch.lsp

;; lisp-syntax: (cond (<expr1> expr2)...)
;; lisp-syntax: (if <expr> <then-expr> else-expr...)
;; lisp-syntax: (while <expr> expr...)
;; lisp-syntax: (unless <expr> else-expr...)
;; lisp-syntax: (when <expr> then-expr...)


;; --------------------------
;; cond
;; --------------------------

(assertn  '(cond (nil t) (t nil)))
(assertn  '(cond (nil t) (nil t)))
(assert   '(cond (nil nil) (t t)))
(assert   '(cond (nil) (t)))
(assertn  '(cond (nil) (nil)))
(asserteq '(cond (nil 'foo) (nil 'bar)) nil)
(asserteq '(cond (nil 'foo) (t 'bar)) ''bar)
(asserteq '(cond (t 'foo) (t 'bar)) ''foo)
(asserteq '(cond (nil 'foo) (t 'bar 'baz)) ''baz)


;; --------------------------
;; if
;; --------------------------

(assertequal '(if nil t   ) 'nil )
(assertequal '(if nil nil ) 'nil )
(assertequal '(if nil t   ) 'nil )
(assertequal '(if nil nil ) 'nil )

(assertequal '(if t   t   ) 't   )
(assertequal '(if t   nil ) 'nil )
(assertequal '(if t   t   ) 't   )
(assertequal '(if t   nil ) 'nil )

(assertequal '(if 0   t   ) 't   )
(assertequal '(if 0   nil ) 'nil )
(assertequal '(if 0   t   ) 't   )
(assertequal '(if 0   nil ) 'nil )

(assertequal '(if 1   t   ) 't   )
(assertequal '(if 1   nil ) 'nil )
(assertequal '(if 1   t   ) 't   )
(assertequal '(if 1   nil ) 'nil )

(assertequal '(if nil 1 2 ) '2   )
(assertequal '(if nil 1 2 ) '2   )
(assertequal '(if nil 1 2 ) '2   )
(assertequal '(if nil 1 2 ) '2   )

(assertequal '(if t   1 2 ) '1   )
(assertequal '(if t   1 2 ) '1   )
(assertequal '(if t   1 2 ) '1   )
(assertequal '(if t   1 2 ) '1   )

(assertequal '(if 0   1 2 ) '1   )
(assertequal '(if 0   1 2 ) '1   )
(assertequal '(if 0   1 2 ) '1   )
(assertequal '(if 0   1 2 ) '1   )

(assertequal '(if 1   1 2 ) '1   )
(assertequal '(if 1   1 2 ) '1   )
(assertequal '(if 1   1 2 ) '1   )
(assertequal '(if 1   1 2 ) '1   )


;; --------------------------
;; while
;; --------------------------

(assertn '(while nil t))


;; --------------------------
;; unless
;; --------------------------

(assertequal '(unless nil nil) 'nil )
(assertequal '(unless nil t  ) 't   )
(assertequal '(unless t   nil) 'nil )
(assertequal '(unless t   t  ) 'nil )

(assertequal '(unless nil 'a 'b ) ''b )


;; --------------------------
;; when
;; --------------------------

(assertequal '(when nil nil) 'nil )
(assertequal '(when nil t  ) 'nil )
(assertequal '(when t   nil) 'nil )
(assertequal '(when t   t  ) 't   )

(assertequal '(when t 'a 'b ) ''b )
