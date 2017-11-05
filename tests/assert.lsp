;; assert.lsp

(defun assert (exp)
  (or (eval exp)
      (princ "assertion failed: " exp " (:= " (eval exp) ")\n")))

(defun assertn (exp)
  (assert (list 'not exp)))

(defun asserteq (exp1 exp2)
  (or (eq (eval exp1) (eval exp2))
      (princ "assertion failed: " exp1 " (:= " (eval exp1) ") == "
	     exp2 " (:= " (eval exp2) ")\n")))


;; test assert functions

(assert t)
(assertn nil)
(asserteq '() nil)
