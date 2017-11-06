;; assert.lsp

(defun assert (exp)
  (or (eval exp)
      (princ (lexer-file) "(" (lexer-lineno) "): assertion failed: " exp ":=(" (eval exp) ")\n")))

(defun assertn (exp)
  (assert (list 'not exp)))

(defun asserteq (exp1 exp2)
  (or (eq (eval exp1) (eval exp2))
      (princ (lexer-file) "(" (lexer-lineno) "): assertion failed: " exp1 ":=(" (eval exp1) ") == "
	     exp2 ":=(" (eval exp2) ")\n")))

(defun assertneq (exp1 exp2)
  (or (not (eq (eval exp1) (eval exp2)))
      (princ (lexer-file) "(" (lexer-lineno) "): assertion failed: " exp1 ":=(" (eval exp1) ") != "
	     exp2 ":=(" (eval exp2) ")\n")))

(defun assertequal (exp1 exp2)
  (or (equal (eval exp1) (eval exp2))
      (princ (lexer-file) "(" (lexer-lineno) "): assertion failed: " exp1 ":=(" (eval exp1) ") == "
	     exp2 ":=(" (eval exp2) ")\n")))

(defun assertnequal (exp1 exp2)
  (or (not (equal (eval exp1) (eval exp2)))
      (princ (lexer-file) "(" (lexer-lineno) "): assertion failed: " exp1 ":=(" (eval exp1) ") != "
	     exp2 ":=(" (eval exp2) ")\n")))

;; --------------------------
;; assert
;; --------------------------

(assert t)
(assertn nil)
(asserteq '() nil)


;; --------------------------
;; eq
;; --------------------------

(asserteq ''foo ''foo)
(assertneq ''foo ''bar)


;; --------------------------
;; equal
;; --------------------------

(assertequal ''() ''())
(assertequal ''() 'nil)
(assertequal 'nil ''())
(assertequal ''foo ''foo)
(assertnequal ''foo ''bar)
(assertequal '(list 'foo) '(list 'foo))
(assertnequal '(list 'foo) '(list 'bar))
(assertequal ''('foo . 'bar) ''('foo . 'bar))
(assertnequal ''('foo . 'bar) ''('foo . 'baz))
