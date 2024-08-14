(define fib
  (lambda (n)
    (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

(macro fib-d (n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(writeln "these should both be 6765:")
(writeln (fib 20))
(writeln (fib-d 20))
