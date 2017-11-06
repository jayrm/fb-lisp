;; mathcomp.lsp

;; (= <atom> <atom>...)
;; (/= <atom> <atom>...)
;; (< <atom> <atom>...)
;; (> <atom> <atom>...)
;; (<= <atom> <atom>...)
;; (>= <atom> <atom>...)


;; --------------------------
;; =
;; --------------------------

(assert  '(= 0 ) )
(assert  '(= 1 ) )
(assert  '(= 0.0 ) )
(assert  '(= 1.0 ) )

(assert  '(= 1   1   ) )
(assertn '(= 1   2   ) )
(assertn '(= 2   1   ) )
(assert  '(= 2   2   ) )

(assert  '(= 1.0 1.0 ) )
(assertn '(= 1.0 2.0 ) )
(assertn '(= 2.0 1.0 ) )
(assert  '(= 2.0 2.0 ) )

(assert  '(= 1   1.0 ) )
(assertn '(= 1   2.0 ) )
(assertn '(= 2   1.0 ) )
(assert  '(= 2   2.0 ) )

(assert  '(= 1.0 1   ) )
(assertn '(= 1.0 2   ) )
(assertn '(= 2.0 1   ) )
(assert  '(= 2.0 2   ) )

(assert  '(= 1 1 1) )
(assertn '(= 1 1 2) )
(assertn '(= 1 1 3) )
(assertn '(= 1 2 1) )
(assertn '(= 1 2 2) )
(assertn '(= 1 2 3) )
(assertn '(= 1 3 1) )
(assertn '(= 1 3 2) )
(assertn '(= 1 3 3) )

(assertn '(= 2 1 1) )
(assertn '(= 2 1 2) )
(assertn '(= 2 1 3) )
(assertn '(= 2 2 1) )
(assert  '(= 2 2 2) )
(assertn '(= 2 2 3) )
(assertn '(= 2 3 1) )
(assertn '(= 2 3 2) )
(assertn '(= 2 3 3) )

(assertn '(= 3 1 1) )
(assertn '(= 3 1 2) )
(assertn '(= 3 1 3) )
(assertn '(= 3 2 1) )
(assertn '(= 3 2 2) )
(assertn '(= 3 2 3) )
(assertn '(= 3 3 1) )
(assertn '(= 3 3 2) )
(assert  '(= 3 3 3) )


;; --------------------------
;; /=
;; --------------------------

(assert  '(/= 0 ) )
(assert  '(/= 1 ) )
(assert  '(/= 0.0 ) )
(assert  '(/= 1.0 ) )

(assertn '(/= 1   1   ) )
(assert  '(/= 1   2   ) )
(assert  '(/= 2   1   ) )
(assertn '(/= 2   2   ) )

(assertn '(/= 1.0 1.0 ) )
(assert  '(/= 1.0 2.0 ) )
(assert  '(/= 2.0 1.0 ) )
(assertn '(/= 2.0 2.0 ) )

(assertn '(/= 1   1.0 ) )
(assert  '(/= 1   2.0 ) )
(assert  '(/= 2   1.0 ) )
(assertn '(/= 2   2.0 ) )

(assertn '(/= 1.0 1   ) )
(assert  '(/= 1.0 2   ) )
(assert  '(/= 2.0 1   ) )
(assertn '(/= 2.0 2   ) )

(assertn '(/= 1 1 1) )
(assertn '(/= 1 1 (+ 1 1) ) )
(assertn '(/= 1 1 3) )
(assertn '(/= 1 2 1) )
(assertn '(/= 1 2 2) )
(assert  '(/= 1 2 3) )
(assertn '(/= 1 3 1) )
(assert  '(/= 1 3 2) )
(assertn '(/= 1 3 3) )

(assertn '(/= 2 1 1) )
(assertn '(/= 2 1 2) )
(assert  '(/= 2 1 3) )
(assertn '(/= 2 2 1) )
(assertn '(/= 2 2 2) )
(assertn '(/= 2 2 3) )
(assert  '(/= 2 3 1) )
(assertn '(/= 2 3 2) )
(assertn '(/= 2 3 3) )

(assertn '(/= 3 1 1) )
(assert  '(/= 3 1 2) )
(assertn '(/= 3 1 3) )
(assert  '(/= 3 2 1) )
(assertn '(/= 3 2 2) )
(assertn '(/= 3 2 3) )
(assertn '(/= 3 3 1) )
(assertn '(/= 3 3 2) )
(assertn '(/= 3 3 3) )


;; --------------------------
;; <
;; --------------------------

(assertn '(< 1   1   ) )
(assert  '(< 1   2   ) )
(assertn '(< 2   1   ) )
(assertn '(< 2   2   ) )

(assertn '(< 1.0 1.0 ) )
(assert  '(< 1.0 2.0 ) )
(assertn '(< 2.0 1.0 ) )
(assertn '(< 2.0 2.0 ) )

(assertn '(< 1   1.0 ) )
(assert  '(< 1   2.0 ) )
(assertn '(< 2   1.0 ) )
(assertn '(< 2   2.0 ) )

(assertn '(< 1.0 1   ) )
(assert  '(< 1.0 2   ) )
(assertn '(< 2.0 1   ) )
(assertn '(< 2.0 2   ) )

(assertn '(< 1 1 1) )
(assertn '(< 1 1 2) )
(assertn '(< 1 1 3) )
(assertn '(< 1 2 1) )
(assertn '(< 1 2 2) )
(assert  '(< 1 2 3) )
(assertn '(< 1 3 1) )
(assertn '(< 1 3 2) )
(assertn '(< 1 3 3) )

(assertn '(< 2 1 1) )
(assertn '(< 2 1 2) )
(assertn '(< 2 1 3) )
(assertn '(< 2 2 1) )
(assertn '(< 2 2 2) )
(assertn '(< 2 2 3) )
(assertn '(< 2 3 1) )
(assertn '(< 2 3 2) )
(assertn '(< 2 3 3) )

(assertn '(< 3 1 1) )
(assertn '(< 3 1 2) )
(assertn '(< 3 1 3) )
(assertn '(< 3 2 1) )
(assertn '(< 3 2 2) )
(assertn '(< 3 2 3) )
(assertn '(< 3 3 1) )
(assertn '(< 3 3 2) )
(assertn '(< 3 3 3) )


;; --------------------------
;; >
;; --------------------------

(assertn '(> 1   1   ) )
(assertn '(> 1   2   ) )
(assert  '(> 2   1   ) )
(assertn '(> 2   2   ) )

(assertn '(> 1.0 1.0 ) )
(assertn '(> 1.0 2.0 ) )
(assert  '(> 2.0 1.0 ) )
(assertn '(> 2.0 2.0 ) )

(assertn '(> 1   1.0 ) )
(assertn '(> 1   2.0 ) )
(assert  '(> 2   1.0 ) )
(assertn '(> 2   2.0 ) )

(assertn '(> 1.0 1   ) )
(assertn '(> 1.0 2   ) )
(assert  '(> 2.0 1   ) )
(assertn '(> 2.0 2   ) )

(assertn '(> 1 1 1) )
(assertn '(> 1 1 2) )
(assertn '(> 1 1 3) )
(assertn '(> 1 2 1) )
(assertn '(> 1 2 2) )
(assertn '(> 1 2 3) )
(assertn '(> 1 3 1) )
(assertn '(> 1 3 2) )
(assertn '(> 1 3 3) )

(assertn '(> 2 1 1) )
(assertn '(> 2 1 2) )
(assertn '(> 2 1 3) )
(assertn '(> 2 2 1) )
(assertn '(> 2 2 2) )
(assertn '(> 2 2 3) )
(assertn '(> 2 3 1) )
(assertn '(> 2 3 2) )
(assertn '(> 2 3 3) )

(assertn '(> 3 1 1) )
(assertn '(> 3 1 2) )
(assertn '(> 3 1 3) )
(assert  '(> 3 2 1) )
(assertn '(> 3 2 2) )
(assertn '(> 3 2 3) )
(assertn '(> 3 3 1) )
(assertn '(> 3 3 2) )
(assertn '(> 3 3 3) )


;; --------------------------
;; <=
;; --------------------------

(assert  '(<= 1   1   ) )
(assert  '(<= 1   2   ) )
(assertn '(<= 2   1   ) )
(assert  '(<= 2   2   ) )

(assert  '(<= 1.0 1.0 ) )
(assert  '(<= 1.0 2.0 ) )
(assertn '(<= 2.0 1.0 ) )
(assert  '(<= 2.0 2.0 ) )

(assert  '(<= 1   1.0 ) )
(assert  '(<= 1   2.0 ) )
(assertn '(<= 2   1.0 ) )
(assert  '(<= 2   2.0 ) )

(assert  '(<= 1.0 1   ) )
(assert  '(<= 1.0 2   ) )
(assertn '(<= 2.0 1   ) )
(assert  '(<= 2.0 2   ) )

(assert  '(<= 1 1 1) )
(assert  '(<= 1 1 2) )
(assert  '(<= 1 1 3) )
(assertn '(<= 1 2 1) )
(assert  '(<= 1 2 2) )
(assert  '(<= 1 2 3) )
(assertn '(<= 1 3 1) )
(assertn '(<= 1 3 2) )
(assert  '(<= 1 3 3) )

(assertn '(<= 2 1 1) )
(assertn '(<= 2 1 2) )
(assertn '(<= 2 1 3) )
(assertn '(<= 2 2 1) )
(assert  '(<= 2 2 2) )
(assert  '(<= 2 2 3) )
(assertn '(<= 2 3 1) )
(assertn '(<= 2 3 2) )
(assert  '(<= 2 3 3) )

(assertn '(<= 3 1 1) )
(assertn '(<= 3 1 2) )
(assertn '(<= 3 1 3) )
(assertn '(<= 3 2 1) )
(assertn '(<= 3 2 2) )
(assertn '(<= 3 2 3) )
(assertn '(<= 3 3 1) )
(assertn '(<= 3 3 2) )
(assert  '(<= 3 3 3) )


;; --------------------------
;; >=
;; --------------------------

(assert  '(>= 1   1   ) )
(assertn '(>= 1   2   ) )
(assert  '(>= 2   1   ) )
(assert  '(>= 2   2   ) )

(assert  '(>= 1.0 1.0 ) )
(assertn '(>= 1.0 2.0 ) )
(assert  '(>= 2.0 1.0 ) )
(assert  '(>= 2.0 2.0 ) )

(assert  '(>= 1   1.0 ) )
(assertn '(>= 1   2.0 ) )
(assert  '(>= 2   1.0 ) )
(assert  '(>= 2   2.0 ) )

(assert  '(>= 1.0 1   ) )
(assertn '(>= 1.0 2   ) )
(assert  '(>= 2.0 1   ) )
(assert  '(>= 2.0 2   ) )

(assert  '(>= 1 1 1) )
(assertn '(>= 1 1 2) )
(assertn '(>= 1 1 3) )
(assertn '(>= 1 2 1) )
(assertn '(>= 1 2 2) )
(assertn '(>= 1 2 3) )
(assertn '(>= 1 3 1) )
(assertn '(>= 1 3 2) )
(assertn '(>= 1 3 3) )

(assert  '(>= 2 1 1) )
(assertn '(>= 2 1 2) )
(assertn '(>= 2 1 3) )
(assert  '(>= 2 2 1) )
(assert  '(>= 2 2 2) )
(assertn '(>= 2 2 3) )
(assertn '(>= 2 3 1) )
(assertn '(>= 2 3 2) )
(assertn '(>= 2 3 3) )

(assert  '(>= 3 1 1) )
(assertn '(>= 3 1 2) )
(assertn '(>= 3 1 3) )
(assert  '(>= 3 2 1) )
(assert  '(>= 3 2 2) )
(assertn '(>= 3 2 3) )
(assert  '(>= 3 3 1) )
(assert  '(>= 3 3 2) )
(assert  '(>= 3 3 3) )
