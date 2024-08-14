(macro mydefun (name params def)
  (define name (lambda params def)))

(mydefun cadr (x) (car (cdr x)))

(writeln "this should print 2:")
(writeln (cadr '(1 2 3)))
