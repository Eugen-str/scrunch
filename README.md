# scrunch

Another Lisp language.

Even though the language gets its name from Scheme, it is not a implementation of Scheme, but an original language.

## Usage

Start the REPL with `cargo run`

To execute a file, use `cargo run <filename>`, for example you can try `cargo run tests/fib.lisp`.

### Examples

```lisp
scrunch> (+ 1 2)
3
scrunch> (* 6 (+ 4 3) (/ 36 6))
252
scrunch> (eq? '(1 2) '(#\a #t))
#f
scrunch> (define add3 (lambda (x) (+ x 3)))
nil
scrunch> (add3 5)
8
scrunch> (add3 5 3)
ERROR: Wrong number of arguments: expected 1, but got 2
scrunch> (define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
nil
scrunch> (fib 20)
6765
scrunch> ((lambda (x) (+ x 3)) 5)
8
```
