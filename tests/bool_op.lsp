;; bool_op.lsp

;; lisp-syntax: (and <expr>...)
;; lisp-syntax: (or <expr>...)
;; lisp-syntax: (not <expr>)
;; lisp-syntax: (null <expr>)


;; --------------------------
;; and
;; --------------------------

(assert  '(and t  ))
(assertn '(and nil))

(assert  '(and t   t  ))
(assertn '(and t   nil))
(assertn '(and nil t  ))
(assertn '(and nil nil))

(assert  '(and t   t   t  ))
(assertn '(and t   t   nil))
(assertn '(and t   nil t  ))
(assertn '(and t   nil nil))
(assertn '(and nil t   t  ))
(assertn '(and nil t   nil))
(assertn '(and nil nil t  ))
(assertn '(and nil nil nil))


;; --------------------------
;; or
;; --------------------------

(assert  '(or t  ))
(assertn '(or nil))

(assert  '(or t   t  ))
(assert  '(or t   nil))
(assert  '(or nil t  ))
(assertn '(or nil nil))

(assert  '(or t   t   t  ))
(assert  '(or t   t   nil))
(assert  '(or t   nil t  ))
(assert  '(or t   nil nil))
(assert  '(or nil t   t  ))
(assert  '(or nil t   nil))
(assert  '(or nil nil t  ))
(assertn '(or nil nil nil))


;; --------------------------
;; not
;; --------------------------

(assertn '(not t))
(assert  '(not nil))
(assertn '(not 'foo))
(assertn '(not 1))
(assert '(not '()))
(assertn '(not '(list)))
(assertn '(not '(list 1)))
(assertn '(not '(list 2)))


;; --------------------------
;; null
;; --------------------------

(assertn '(null t))
(assert  '(null nil))
(assertn '(null 'foo))
(assertn '(null 1))
(assert '(null '()))
(assertn '(null '(list)))
(assertn '(null '(list 1)))
(assertn '(null '(list 2)))

