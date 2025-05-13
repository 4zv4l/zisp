# zisps

A basic Lisp interpreter in Zig

Can interprete code like this:

```lisp
;; functions
(define (greet name)
  (define str (format #f "Hi ~A !" name))
  (print str))
(greet "Simon")
;; (print name) this would error since name isnt defined in this scope

;; booleans
(if true "true" "false")
(cond
  [(eq "str" "abc") "false"] ;; can use both () and []
  [(eq "str" "str") "true"]) ;; to help readability

;; lambda (they save the env they were defined in)
(define (make-counter)
  (define counter 0)
  (lambda () (set! counter (+ counter 1))))
(define my-counter (make-counter))
(my-counter) (my-counter) (my-counter) (my-counter)
(format #t "my-counter: ~A~%" (my-counter))

;; car/cdr/cons
(format #t "car: ~A, cdr: ~A, cons: ~A~%"
    (car (quote (1 2 3))) ;; 1
    (cdr `(1 ,(+ 1 1) 3)) ;; (2 3)
    (cons '(1 2) 'a))     ;; ((1 2) a)

;; runtime macro ?
(define (with-defined-name code)
  (define name "Simon")
  (code))
(with-defined-name
  '(progn (print name) (print "yayyy")))

;; fib
(define (fib n)
  (define (fib-t n a b)
    (if (eq n 0) a (fib-t (- n 1) b (+ a b))))
  (fib-t n 0 1))
(format #t "fib(~A) is ~A~%" 15 (fib 15))
```

## Setup

To compile simply run `zig build-exe -O ReleaseFast zisp.zig`.

If you do not give a file as argument to the interpreter, it will start a `repl`.
