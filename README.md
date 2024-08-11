# scrunch

Another Lisp language.

Even though the language gets its name from Scheme, it is not a implementation of Scheme, but an original language.

## Usage

Start the REPL with `cargo run`

*Loading files is not yet implemented.*

### Examples

```lisp
scrunch> (+ 1 2)
3
scrunch> (* 6 (+ 4 3) (/ 36 6))
252
scrunch> (eq? '(1 2) '(#\a #t))
#f
```
