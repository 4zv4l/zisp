;; functions
(define (greet name)
  (define str (format #f "Hi ~A !" name))
  (print str))
(greet "Simon")
;; (print name) this would error since name isnt defined in this scope

;; booleans
(if true (format #t "rand: ~A~%" (rand)) "false")
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

;; fib
(define (fib n)
  (define (fib-t n a b)
    (if (eq n 0) a (fib-t (- n 1) b (+ a b))))
  (fib-t n 0 1))
(format #t "fib(~A) is ~A~%" 15 (fib 15))

;; macro
(define (inner-fn arg)
  (format #t "~A~%" arg)
  (format #t "~A~%" arg)
  (format #t "~A~%" arg))
(defmacro (inner-macro arg)
  `(format #t "~A~%" ,arg)
  `(format #t "~A~%" ,arg)
  `(format #t "~A~%" ,arg))

(print "--- function ---")
(inner-fn (rand 5)) ; same (rand) for each (format)
(print "--- macro ---")
(inner-macro (rand 2 10)) ; exec (rand) for each (format)

(print "--- recursive macro ---")
(defmacro (m1 x) (if true `(m2 (m2 (m2 ,x)))))
(defmacro (m2 y) `(+ ,y 1))
(define a 5)
(format #t "=> ~A~%" (m1 (m1 a)))
(format #t "=> ~A~%" (macroexpand-1 '(m2 3)))
(format #t "=> ~A~%" (macroexpand-1 '(m1 3)))
(format #t "=> ~A~%" (macroexpand '(m1 3)))
(format #t "=> ~R~%" (m1 3))
