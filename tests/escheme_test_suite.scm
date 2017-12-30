;;; -*- Mode: Lisp -*-

;;
;; (load "escheme_test_suite.scm")
;;
;; escheme regression test suite
;;
;;   exercise all core functionality
;;

(if #f
    (begin
     (load "./tests/escheme_test_suite.scm")

     (repeat dotest 1)
     (repeat dotest 100)

     (perform-timed-run 1)
     (perform-timed-run 10)
     (perform-timed-run 50)
     (perform-timed-run 100)
     (perform-timed-run 500)
     (perform-timed-run 1000)
     (perform-timed-run 2000)
     (perform-timed-run 4000)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; *RUN* these tests... 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(define count 0)
(define failures 0)

(define (run-the-test)

  (set! count 0)
  (set! failures 0)

  (test-structured-ops)  
  (test-equality)
  (test-property-lists)  
  (test-arithmetic-functions)
  (test-logical-functions)
  (test-environment)
  (test-predicates)
  (test-strings)
  (test-conversions)
  (test-chars)
  (test-promises)
  (test-lets)
  (test-ports)

  (if (zero? failures)
      (if #f (displayln "All Tests Passed"))
    (begin
     (display failures)
     (displayln " failures")))

  ;; end
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (displayln text)
  (display text)
  (newline))

(define (display-done)
  (newline)
  (newline)
  (displayln "==========================================")
  (displayln "==========================================")
  (displayln "                 DONE!                    ")
  (displayln "==========================================")
  (displayln "==========================================")
  (newline)
  (newline))


(define (assert x)
  (if (not x)
      (begin
       (display "assertion failed")
       (newline)
       (set! failures (1+ failures))
       ))
  x)

(define (test-structured-ops)  
  
  (assert (equal? (cons 1 2) '(1 . 2)))
  (assert (equal? (car (cons 1 2)) 1))
  (assert (equal? (cdr (cons 1 2)) 2))
  
  (assert (equal? (list) '() ))
  (assert (equal? (list 1) '(1) ))
  (assert (equal? (list 1 2) '(1 2) ))
  (assert (equal? (list*) '() ))
  (assert (equal? (list* 1) 1 ))
  (assert (equal? (list* 1 2) '(1 . 2) ))
  (assert (equal? (list* 1 2 3) '(1 2 . 3) ))
  
  (assert (equal? (length (list)) 0))
  (assert (equal? (length (list 1)) 1))
  
  (assert (equal? (set-car! '(1 . 2) 10) '(10 . 2)))
  (assert (equal? (set-cdr! '(1 . 2) 10) '(1 . 10)))
  
  (assert (equal? (vector) #()))
  (assert (equal? (vector 1) #(1)))
  (assert (equal? (vector 1 2) #(1 2)))
  (assert (equal? (make-vector 0) #()))
  (assert (equal? (make-vector 1) #(()) ))
  (assert (equal? (make-vector 2) #(() ()) ))
  
  (set! v (vector 1 2))
  (assert (equal? (vector-ref v 0) 1))
  (assert (equal? (vector-ref v 1) 2))

  (vector-set! v 0 10)
  (assert (equal? (vector-ref v 0) 10))
  (assert (equal? v #(10 2)) )

  (vector-set! v 1 20)
  (assert (equal? v #(10 20)) )
  (assert (equal? (vector-length #()) 0))
  (assert (equal? (vector-length #(1)) 1))
  
  (assert (equal? (vector->list (list->vector '())) '()))
  (assert (equal? (vector->list (list->vector '(1))) '(1)))
  (assert (equal? (vector->list (list->vector '(1 2))) '(1 2)))
  
  (assert (equal? (string->list (list->string '())) '()))
  (assert (equal? (string->list (list->string '(#\a))) '(#\a)))
  (assert (equal? (string->list (list->string '(#\a #\b))) '(#\a #\b)))
)


(define (test-equality)
  
  (set! s1 "abc")
  (set! s2 "abc")
  (set! n1 10)
  (set! n2 10)
  (set! f1 10.0)
  (set! f2 10.0)
  
  (assert (eq? s1 s1))
  (assert (not (eq? s1 s2)))
  (assert (eqv? s1 s2))
  (assert (equal? s1 s2))
  
  (assert (not (eq? n1 n2)))
  (assert (eqv? n1 n2))
  (assert (equal? n1 n2))
  
  
  (assert (equal? (string->symbol (symbol->string 'a)) 'a))
  (assert (symbol? (gensym)))
)


(define (test-property-lists)  
  
  (set-symbol-plist! 'x ())
  (set-symbol-plist! 'foo ())
  (set-symbol-plist! 'bar ())
  
  (assert (begin (set! x 10) (equal? (symbol-value 'x) 10)))
  (assert (begin (set-symbol-value! 'x 20) (equal? (symbol-value 'x) 20)))
  
  (assert (equal? (get 'foo 'x) ()))
  (assert (begin (put 'foo 'x 10) (equal? (get 'foo 'x) 10)))
  (assert (equal? (symbol-plist 'bar) '()) )
  (assert (begin (put 'bar 'x 10) (equal? (symbol-plist 'bar) (list 'x 10))))
  (assert (begin (put 'bar 'y 20) (equal? (symbol-plist 'bar) (list 'y 20 'x 10)))))


(define (test-arithmetic-functions)
  
  ;;
  ;; Math/arithmetic functions
  ;;
  
  (assert (= 0 0))
  (assert (< 0 1))
  (assert (< -2 -1))
  (assert (<= 0 0))
  (assert (<= -2 -2))
  (assert (<= 0 1))
  (assert (<= -2 -1))
  (assert (> 1 0))
  (assert (> -1 -2))
  (assert (>= 0 0))
  (assert (>= -2 -2))
  (assert (>= 1 0))
  (assert (>= -1 -2))
  
  (assert (= (+ 1 2) 3))
  (assert (= (+ 1 -2) -1))
  (assert (= (- 1 2) -1))
  (assert (= (- 1 -2) 3))
  (assert (= (* 1 2) 2))
  (assert (= (* 1 -2) -2))
  (assert (= (/ 1 2) 0.5))
  (assert (= (/ 1 -2) -0.5))
  
  (assert (= (truncate 1.5) 1))
  (assert (= (floor -1.5) -2.0))
  (assert (= (ceiling -1.5) -1.0))
  (assert (= (round 1.4) 1.0))
  (assert (= (round 1.5) 2.0))
  (assert (= (round -1.4) -1.0))
  (assert (= (round -1.5) -2.0))
  
  ;;(assert (= (inc 1) (+ 1 1)))
  ;;(assert (= (dec 1) (- 1 1)))
  (assert (= ( 1+ 1) (+ 1 1)))
  (assert (= (-1+ 1) (- 1 1)))
  ;;(assert (= (inc 1.0) (+ 1.0 1)))
  ;;(assert (= (dec 1.0) (- 1.0 1)))
  (assert (= ( 1+ 1.0) (+ 1.0 1)))
  (assert (= (-1+ 1.0) (- 1.0 1)))
  
  (assert (= (abs 1) 1))
  (assert (= (abs -1) 1))
  (assert (= (abs 1.0) 1))
  (assert (= (abs -1.0) 1))
  
  (assert (= (quotient 10 4) 2))
  (assert (= (quotient -10 4) -2))
  (assert (= (quotient 10 -4) -2))
  (assert (= (remainder 10 4) 2))
  (assert (= (remainder -10 4) -2))
  (assert (= (remainder 10 -4) 2))
  
  (assert (= (min 1 2 3) 1))
  (assert (= (max 1 2 3) 3))

)


(define (test-logical-functions)
  
  (assert (= (logand 1 3) 1))
  (assert (= (logior 1 3) 3))
  (assert (= (logxor 1 3) 2))
  
  ;; 32-bit integers
  ;; ~#xFFFFFFFE --> -2 (or for 64-bit 4294967294)
  
  ;; 64-bit integers
  ;; ~#xFFFFFFFFFFFFFFFE --> -2
  
  (assert (= (lognot 1) -2))
  (assert (= (lognot 1) #xFFFFFFFFFFFFFFFE))
  (assert (= (lognot -2) 1))
  (assert (= (lognot #xFFFFFFFFFFFFFFFE) 1))
  
  (assert (= (shift-right 4 1) 2))
  (assert (= (shift-left 4 1) 8))
  (assert (= (shift-right-arithmetic 2 1) 1))
  (assert (= (shift-right-arithmetic -2 1) -1))
  (assert (= (shift-right-arithmetic #xFFFFFFFFFFFFFFFE 1) #xFFFFFFFFFFFFFFFF))
)


(define (test-environment)

  (set! foo (lambda (a b) (the-environment)))
  
  (assert (equal? (environment-bindings (foo 1 2)) '((a . 1) (b . 2))) )
  
  (set! foo (lambda (a b) 
	      (let ((bar (lambda (n)
			   (list a b n))))
		bar)))
  
  (assert (equal? (environment-bindings (procedure-environment (foo 1 2))) 
		   '((a . 1) (b . 2))) )
  
  (set! foo2 (lambda (a b) 
	       (let ((bar (lambda (n)
			    (list a b n))))
		 (the-environment))))
  
  (set! e (foo2 10 20))
  (set! x (environment-bindings e))
  ;;(print x)
  
  (assert (equal? (access a (foo2 1 2)) 1))
  (assert (equal? (access b (foo2 1 2)) 2))
  (assert (equal? ((access bar (foo2 1 2)) 3) '(1 2 3)))
)


(define (test-predicates)
  
  (assert (not #f))
  
  (assert (bound? 'foo2))
  (assert (not (bound? 'foo3)))
  
  (assert (null? ()))
  (assert (not (null? 'a)))
  
  (assert (atom? ()))
  (assert (atom? 1))
  (assert (atom? 1.0))
  (assert (atom? "string"))
  (assert (atom? #\a))
  (assert (atom? #()))
  (assert (not (atom? (list 1))))
  
  (assert (list? ()))
  (assert (list? (cons 1 2)))
  (assert (list? (list 1 2 3 4)))
  (assert (not (list? 1)))
  
  (assert (pair? (cons 1 2)))
  (assert (not (pair? '())))
  
  (assert (number? 1))
  (assert (number? 1.0))
  (assert (not (number? "abc")))
  (assert (not (number? #())))
  
  (assert (boolean? #t))
  (assert (boolean? #f))
  (assert (not (boolean? 1)))
  
  (assert (symbol? 'a))
  (assert (not (symbol? 1)))
  
  (assert (integer? 1))
  (assert (not (integer? 1.0)))
  (assert (real? 1.0))
  (assert (not (real? 1)))
  
  (assert (char? #\a))
  (assert (not (char? "a")))
  
  (assert (not (string? #\a)))
  (assert (string? "a"))
  
  (assert (vector? #()))
  (assert (not (vector? '(1 2))))
  
  (assert (byte-vector? (byte-vector 1 2)))
  (assert (not (byte-vector? #())))
  
  (assert (closure? foo2))
  (assert (not (closure? car)))
  
  (assert (procedure? car))
  (assert (procedure? foo2))
  
  (assert (not (environment? '())) )
  (assert (environment? (foo2 1 2)))
  
  (assert (zero? 0))
  (assert (not (zero? 10)))
  (assert (positive? 1))
  (assert (not (positive? 0)))
  (assert (not (positive? -1)))
  (assert (negative? -1))
  (assert (not (negative? 0)))
  (assert (not (negative? 1)))
  
  (assert (odd? 1))
  (assert (odd? 51))
  (assert (odd? -51))
  (assert (not (odd? 0)))
  (assert (even? 0))
  (assert (even? -50))
  (assert (not (even? 1)))
)


(define (test-strings)

  (set! s1 "abc")
  (set! s2 "abc")
  (set! s3 "axy")
  
  (assert (equal? (string-length s1) 3))
  (assert (equal? (string-append s1 s2) "abcabc"))
  (assert (equal? (string-ref s1 0) #\a))
  
  (assert (equal? (substring s1 0 1) "a"))
  (assert (equal? (substring s1 1 0) ""))
  (assert (equal? (substring s1 1 1) ""))
  (assert (equal? (substring s1 1 2) "b"))
  (assert (equal? (substring s1 1 3) "bc"))
  (assert (equal? (substring s1 0 (string-length s1)) s1))
  
  ;;
  ;; string comparison -- case senstive
  ;;
  
  (assert (string=? s1 s1))
  (assert (string=? s1 s2))
  (assert (not (string=? s1 s3)))
  
  (assert (string<? "abc" "abx"))
  (assert (string<? "abc" "ax"))
  (assert (string<? "abc" "x"))
  (assert (string<? "" "a"))
  (assert (not (string<? "a" "a")))
  (assert (not (string<? "a" "")))
  
  (assert (string<=? "abc" "abx"))
  (assert (string<=? "abc" "ax"))
  (assert (string<=? "abc" "x"))
  (assert (string<=? "" "a"))
  (assert (string<=? "a" "a"))
  (assert (not (string<=? "a" "")))
  
  (assert (string>? "abx" "abc"))
  (assert (string>? "ax" "abc"))
  (assert (string>? "x" "abc"))
  (assert (string>? "a" ""))
  (assert (not (string>? "a" "a")))
  (assert (not (string>? "" "a")))
  
  (assert (string>=? "abx" "abc"))
  (assert (string>=? "ax" "abc"))
  (assert (string>=? "x" "abc"))
  (assert (string>=? "a" "a"))
  (assert (not (string>=? "" "a")))
  
  ;;
  ;; string comparison -- case insenstive
  ;;
  
  (assert (string-ci=? "abc" "abc"))
  (assert (string-ci=? "abc" "ABC"))
  (assert (not (string-ci=? "AbC" "AxY")))
  
  (assert (string-ci<? "abc" "ABX"))
  (assert (string-ci<? "abc" "AX"))
  (assert (string-ci<? "abc" "X"))
  (assert (string-ci<? "" "A"))
  (assert (not (string-ci<? "a" "A")))
  (assert (not (string-ci<? "a" "")))
  
  (assert (string-ci<=? "abc" "ABX"))
  (assert (string-ci<=? "abc" "AX"))
  (assert (string-ci<=? "abc" "X"))
  (assert (string-ci<=? "" "A"))
  (assert (string-ci<=? "a" "A"))
  (assert (not (string-ci<=? "a" "")))
  
  (assert (string-ci>? "abx" "ABC"))
  (assert (string-ci>? "ax" "ABC"))
  (assert (string-ci>? "x" "ABC"))
  (assert (string-ci>? "a" ""))
  (assert (not (string-ci>? "a" "A")))
  (assert (not (string-ci>? "" "A")))
  
  (assert (string-ci>=? "abx" "ABC"))
  (assert (string-ci>=? "ax" "ABC"))
  (assert (string-ci>=? "x" "ABC"))
  (assert (string-ci>=? "a" "A"))
  (assert (not (string-ci>=? "" "A")))
  
  ;; swap case of 1st and 2nd arguments
  
  (assert (string-ci=? "ABC" "abc"))
  (assert (not (string-ci=? "ABC" "axy")))
  
  (assert (string-ci<? "ABC" "abx"))
  (assert (string-ci<? "ABC" "ax"))
  (assert (string-ci<? "ABC" "x"))
  (assert (string-ci<? "" "A"))
  (assert (not (string-ci<? "A" "a")))
  (assert (not (string-ci<? "A" "")))
  
  (assert (string-ci<=? "ABC" "abx"))
  (assert (string-ci<=? "ABC" "ax"))
  (assert (string-ci<=? "ABC" "x"))
  (assert (string-ci<=? "" "a"))
  (assert (string-ci<=? "A" "a"))
  (assert (not (string-ci<=? "A" "")))
  
  (assert (string-ci>? "ABX" "abc"))
  (assert (string-ci>? "AX" "abc"))
  (assert (string-ci>? "X" "abc"))
  (assert (string-ci>? "A" ""))
  (assert (not (string-ci>? "A" "a")))
  (assert (not (string-ci>? "" "a")))
  
  (assert (string-ci>=? "ABX" "abc"))
  (assert (string-ci>=? "AX" "abc"))
  (assert (string-ci>=? "X" "abc"))
  (assert (string-ci>=? "A" "a"))
  (assert (not (string-ci>=? "" "a")))
)


(define (test-chars)

  (assert (char-ci=? #\a #\a))
  (assert (not (char-ci=? #\a #\b)))
  
  (assert (char-ci<? #\a #\b))
  (assert (char-ci<=? #\a #\a))
  (assert (char-ci<=? #\a #\b))
  (assert (char-ci>? #\b #\a))
  (assert (char-ci>=? #\b #\b))
  (assert (char-ci>=? #\b #\a))
  
)

(define (test-conversions)

  (assert (equal? (integer->string 10) "10"))
  (assert (equal? (string->integer "-10") -10))
  (assert (equal? (string->integer (integer->string 10)) 10))
)


(define prom1 nil)

(define (test-promises)

  (let ((value 1000))
    (set! prom1 (delay value))
    (eqv? value (force prom1))
    )
)


(define (test-lets)
  (let ((a 1) (b 2))
    (assert (equal? (+ a b) 3))
    )
  (letrec ((a 1) (b a))
    (assert (not (null? b)))
    (if (number? b)
      (assert (equal? (+ a b) 2)))
    )
)


(define (test-ports)
  (let ((pin (open-input-file "escheme.scm"))
	(pout (open-output-file "/tmp/dump")))
    (let ((x (read pin)))
      (while (not (eof-object? x))
	(print x pout)
	(set! x (read pin))))
    (close-port pin)
    (close-port pout)))


(if #f (begin

	(define x '(letrec ((a 0) (b (lambda (n) n)) (c (lambda () (b a))))
			   (c)))
	
	(define e1 (let ((a 1) (b 2)) (the-environment)))
	(define e2 (letrec ((a 1) (b 2)) (the-environment)))
	
	(define e3 (letrec ((x 0)
			    (getx (lambda () x))) 
			   (print (getx)) 
			   (the-environment)))
	
	(define eb environment-bindings)
	
	(eb e1)
	(eb e2)
	(eb e3)
	
	(define x
	  '(letrec ((qq-lev 10)
		    (foo (lambda (a)
			   (let ((b 0))
			     (+ b qq-lev)))))
		   (foo 1)))
	))

;;
;; The following function classes have not been tested:
;;
;;   string ports
;;   sockets
;;

(define (dotest)
  (run-the-test))

(define (repeat f n)
   (while (> n 0)
      (f)
      ;;(display n) 
      ;;(display " ")
      ;;(flush-output *standard-output*)
      (set! n (- n 1))))

(define (%sum x sum)
  (if (null? x)
      sum
    (%sum (cdr x) (+ (car x) sum))))

(define (avg x)    
  (let ((n (length x))
	(s (%sum x 0)))
    (/ s (* 1.0 n))))

(define (time-it f)
  (let ((start-time (gettime))
	end-time)
    (f)
    (set! end-time (gettime))
    (let ((secs (- (car end-time) (car start-time)))
	  (nsecs (- (cdr end-time) (cdr start-time)))
	  etime)
      (set! etime (+ (* secs 1000000000) nsecs))
      etime
    )))

(define (test1) (repeat dotest 10))
(define (timed-test) (time-it test1))

(define (run-n-times f n)
  (let ((results nil))
    (while (> n 0)
      (display n)
      (display " ")
      (flush-output *standard-output*)
      (set! results (cons (f) results))
      (set! n (- n 1)))
    (display "-- ")
    results
    ))

(define (perform-timed-run n)
  (let ((factor 1000000000))
    (print (/ (avg (run-n-times timed-test n)) factor)))
  (newline)
  (display "gc: ") (print (gc))
  (display-done)
)

(load "./tests/gc.scm")





