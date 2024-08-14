(export defun f)

(macro defun (name args def)
  (define name (lambda args def)))

(defun f (x) (+ 1 x))
