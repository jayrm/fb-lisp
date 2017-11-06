;; progn.lsp

;; (progn <expr>... )


;; --------------------------
;; progn
;; --------------------------

(assertn '(progn t nil nil))
(assert  '(progn t nil nil t))
(asserteq '(progn t nil nil 'foo) ''foo)
(asserteq '(progn t nil nil 'foo 'bar) ''bar)


;; --------------------------
;; Recursion test
;; --------------------------

(defun last* (l)
  (cond ((eq (cdr l) nil) (car l))
	(t (last* (cdr l)))))

(asserteq '(last* '(foo bar baz)) ''baz)
(asserteq '(last* '(foo bar)) ''bar)
(asserteq '(last* '(foo)) ''foo)
