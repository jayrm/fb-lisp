;; mathops.lsp

;; (+ <number> <number>...)
;; (- <number> <number>...)
;; (* <number> <number>...)
;; (/ <number> <number>...)
;; (1+ <number>)
;; (1- <number>)

;; --------------------------
;; *
;; --------------------------

(assert '(= (* 1) 1))
(assert '(= (* 1 2) 2))
(assert '(= (* 2 2) 4))
(assert '(= (* 2 3) 6))
(assert '(= (* -2 2) -4))

(assert '(= (* 1.0) 1.0))
(assert '(= (* 1.0 2.0) 2.0))
(assert '(= (* 2.0 2.0) 4))
(assert '(= (* 2.0 3.0) 6.0))
(assert '(= (* -2.0 2.0) -4.0))

(assert '(= (* 1.0) 1.0))
(assert '(= (* 1.0 2) 2.0))
(assert '(= (* 2.0 2) 4))
(assert '(= (* 2.0 3) 6.0))
(assert '(= (* -2.0 2) -4.0))


;; --------------------------
;; /
;; --------------------------

(assert '(= (/ 1) 1))
(assert '(= (/ 1.0 2.0) 0.5))
(assert '(= (/ 2.0 2.0) 1.0))
(assert '(= (/ 4.0 2.0) 2.0))
(assert '(= (/ -2.0 2.0) -1.0))
