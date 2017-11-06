;; bool_op.lsp

;; (and <expr>...)
;; (or <expr>...)
;; (not <expr>...)


;; --------------------------
;; and
;; --------------------------

(assert  '(and t))
(assertn '(and n))
(assert  '(and t t))
(assertn '(and nil nil))
(assertn '(and t nil))
(assertn '(and nil t))


;; --------------------------
;; or
;; --------------------------

(assert  '(or t))
(assertn '(or n))
(assert  '(or t t))
(assertn '(or nil nil))
(assert  '(or t nil))
(assert  '(or nil t))


;; --------------------------
;; not
;; --------------------------

(assertn '(not t))
(assert  '(not nil))
(assertn '(not 'foo))
