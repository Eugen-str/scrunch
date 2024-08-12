(define map
  (lambda (f lst)
    (if (eq? '() lst)
      '()
      (cons (f (car lst)) (map f (cdr lst))))))

(define num-names (list '(1 "one") '(2 "two") '(3 "three") '(4 "four") '(5 "five") '(6 "six") '(7 "seven") '(8 "eight") '(9 "nine")))

(define get-num-name-helper
  (lambda (n names)
    (if (eq? (car (car names)) n)
      (car (cdr (car names)))
      (get-num-name-helper n (cdr names)))))

(define get-num-name (lambda (x) (get-num-name-helper x num-names)))

(define test (list 1 2 3 4))

(define print-list
  (lambda (lst) (map display lst)))

(write "nums: ")
(writeln test)

(write "names: ")
(writeln (map get-num-name test))
