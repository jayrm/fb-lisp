;; mathops.lsp

;; (+ <number> <number>...)
;; (- <number> <number>...)
;; (* <number> <number>...)
;; (/ <number> <number>...)
;; (abs <number>)
;; (1+ <number>)
;; (1- <number>)

;; --------------------------
;; assertequal/assertnequal
;; --------------------------

(assertequal '1 '1)
(assertnequal '1 '1.0)
(assertnequal '1.0 '1)
(assertequal '1.0 '1.0)

;; --------------------------
;; +
;; --------------------------

;; result is integer only if all args are integer
;; otherwise, result is real.

(assertequal '(+  1        )  '1   )
(assertequal '(+  1    2   )  '3   )
(assertequal '(+  2    2   )  '4   )
(assertequal '(+  2    3   )  '5   )
(assertequal '(+ -2    2   )  '0   )

(assertequal '(+  1        )  '1   )
(assertequal '(+  1    2.0 )  '3.0 )
(assertequal '(+  2    2.0 )  '4.0 )
(assertequal '(+  2    3.0 )  '5.0 )
(assertequal '(+ -2    2.0 )  '0.0 )

(assertequal '(+  1.0      )  '1.0 )
(assertequal '(+  1.0  2   )  '3.0 )
(assertequal '(+  2.0  2   )  '4.0 )
(assertequal '(+  2.0  3   )  '5.0 )
(assertequal '(+ -2.0  2   )  '0.0 )

(assertequal '(+  1.0      )  '1.0 )
(assertequal '(+  1.0  2.0 )  '3.0 )
(assertequal '(+  2.0  2.0 )  '4.0 )
(assertequal '(+  2.0  3.0 )  '5.0 )
(assertequal '(+ -2.0  2.0 )  '0.0 )

(assertequal '(+  2    2   )  '4   )
(assertequal '(+  2    2.0 )  '4.0 )
(assertequal '(+  2.0  2   )  '4.0 )
(assertequal '(+  2.0  2.0 )  '4.0 )


;; --------------------------
;; -
;; --------------------------

;; result is integer only if all args are integer
;; otherwise, result is real.

(assertequal '(-  1        ) '-1  )
(assertequal '(-  1    2   ) '-1  )
(assertequal '(-  2    2   )  '0   )
(assertequal '(-  2    3   ) '-1  )
(assertequal '(- -2    2   ) '-4  )

(assertequal '(-  1        ) '-1   )
(assertequal '(-  1    2.0 ) '-1.0 )
(assertequal '(-  2    2.0 )  '0.0 )
(assertequal '(-  2    3.0 ) '-1.0 )
(assertequal '(- -2    2.0 ) '-4.0 )

(assertequal '(-  1.0      ) '-1.0 )
(assertequal '(-  1.0  2   ) '-1.0 )
(assertequal '(-  2.0  2   )  '0.0 )
(assertequal '(-  2.0  3   ) '-1.0 )
(assertequal '(- -2.0  2   ) '-4.0 )

(assertequal '(-  1.0      ) '-1.0 )
(assertequal '(-  1.0  2.0 ) '-1.0 )
(assertequal '(-  2.0  2.0 )  '0.0 )
(assertequal '(-  2.0  3.0 ) '-1.0 )
(assertequal '(- -2.0  2.0 ) '-4.0 )

(assertequal '(-  2    2   )  '0   )
(assertequal '(-  2    2.0 )  '0.0 )
(assertequal '(-  2.0  2   )  '0.0 )
(assertequal '(-  2.0  2.0 )  '0.0 )


;; --------------------------
;; *
;; --------------------------

;; result is integer only if all args are integer
;; otherwise, result is real.

(assertequal '(*  1        )  '1   )
(assertequal '(*  1    2   )  '2   )
(assertequal '(*  2    2   )  '4   )
(assertequal '(*  2    3   )  '6   )
(assertequal '(* -2    2   ) '-4   )

(assertequal '(*  1        )  '1   )
(assertequal '(*  1    2.0 )  '2.0 )
(assertequal '(*  2    2.0 )  '4.0 )
(assertequal '(*  2    3.0 )  '6.0 )
(assertequal '(* -2    2.0 ) '-4.0 )

(assertequal '(*  1.0      )  '1.0 )
(assertequal '(*  1.0  2   )  '2.0 )
(assertequal '(*  2.0  2   )  '4.0 )
(assertequal '(*  2.0  3   )  '6.0 )
(assertequal '(* -2.0  2   ) '-4.0 )

(assertequal '(*  1.0      )  '1.0 )
(assertequal '(*  1.0  2.0 )  '2.0 )
(assertequal '(*  2.0  2.0 )  '4.0 )
(assertequal '(*  2.0  3.0 )  '6.0 )
(assertequal '(* -2.0  2.0 ) '-4.0 )

(assertequal '(*  2    2   )  '4   )
(assertequal '(*  2    2.0 )  '4.0 )
(assertequal '(*  2.0  2   )  '4.0 )
(assertequal '(*  2.0  2.0 )  '4.0 )


;; --------------------------
;; /
;; --------------------------

;; result is always float unless there is only
;; a single integer arg

(assertequal '(/  1        )  '1   )
(assertequal '(/  1    2   )  '0.5 )
(assertequal '(/  2    2   )  '1.0  )
(assertequal '(/  4    2   )  '2.0 )
(assertequal '(/ -2    2   ) '-1.0 )

(assertequal '(/  1        )  '1   )
(assertequal '(/  1    2.0 )  '0.5 )
(assertequal '(/  2    2.0 )  '1.0 )
(assertequal '(/  4    2.0 )  '2.0 )
(assertequal '(/ -2    2.0 ) '-1.0 )

(assertequal '(/  1.0      )  '1.0 )
(assertequal '(/  1.0  2   )  '0.5 )
(assertequal '(/  2.0  2   )  '1.0 )
(assertequal '(/  4.0  2   )  '2.0 )
(assertequal '(/ -2.0  2   ) '-1.0 )

(assertequal '(/  1.0      )  '1.0 )
(assertequal '(/  1.0  2.0 )  '0.5 )
(assertequal '(/  2.0  2.0 )  '1.0 )
(assertequal '(/  4.0  2.0 )  '2.0 )
(assertequal '(/ -2.0  2.0 ) '-1.0 )

(assertequal '(/  2    2   )  '1.0 )
(assertequal '(/  2    2.0 )  '1.0 )
(assertequal '(/  2.0  2   )  '1.0 )
(assertequal '(/  2.0  2.0 )  '1.0 )


;; --------------------------
;; abs
;; --------------------------

;; result is always float

(assertequal '(abs -2) '2.0 )
(assertequal '(abs -1) '1.0 )
(assertequal '(abs  0) '0.0 )
(assertequal '(abs  1) '1.0 )
(assertequal '(abs  2) '2.0 )

(assertequal '(abs -2.0) '2.0 )
(assertequal '(abs -1.0) '1.0 )
(assertequal '(abs  0.0) '0.0 )
(assertequal '(abs  1.0) '1.0 )
(assertequal '(abs  2.0) '2.0 )


;; --------------------------
;; 1+
;; --------------------------

;; result is same type as argument

(assertequal '(1+ -2) '-1 )
(assertequal '(1+ -1) '0  )
(assertequal '(1+  0) '1  )
(assertequal '(1+  1) '2  )
(assertequal '(1+  2) '3  )

(assertequal '(1+ -2.0) '-1.0 )
(assertequal '(1+ -1.0) '0.0 )
(assertequal '(1+  0.0) '1.0 )
(assertequal '(1+  1.0) '2.0 )
(assertequal '(1+  2.0) '3.0 )


;; --------------------------
;; 1+
;; --------------------------

;; result is same type as argument

(assertequal '(1- -2) '-3 )
(assertequal '(1- -1) '-2  )
(assertequal '(1-  0) '-1  )
(assertequal '(1-  1) '0  )
(assertequal '(1-  2) '1  )

(assertequal '(1- -2.0) '-3.0 )
(assertequal '(1- -1.0) '-2.0 )
(assertequal '(1-  0.0) '-1.0 )
(assertequal '(1-  1.0) '0.0 )
(assertequal '(1-  2.0) '1.0 )
