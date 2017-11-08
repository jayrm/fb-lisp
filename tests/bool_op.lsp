;; bool_op.lsp

;; (and <expr>...)
;; (or <expr>...)
;; (not <expr>...)


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
