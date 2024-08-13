(macro m-cadr (x)
  (car (cdr x)))

(defun f-cadr (x)
  (car (cdr x)))

(define test '(1 2 3))

(writeln "these should return the same value")
(write "cadr function: ")
(writeln (f-cadr test))

(write "cadr macro: ")
(writeln (m-cadr test))
