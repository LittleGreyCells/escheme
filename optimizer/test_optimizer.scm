(define opt-verbose #f)
(define enable-clambda #t)
(define enable-grs #t)
(define enable-cset #t)

(if #f
    (begin
     ;; the optimizer defines 'optimize'
      (load "./optimizer/optimizer.scm")
      
     ;; the test apparatus
     (load "./optimizer/test_optimizer.scm")

     (define x '(let ((a 1)) a))
     (define x '(let (a) a))
     (define xab '(let ((a 1)(b 2)) b))
     (define x1 '(let ((a 1)) (+ a 1)))
     (define x2 '(let ((a 1)) (let ((b 2)) (+ a b))))
     (define x2e '(let ((a 1)) (let ((b 2)) (the-environment))))
     (define x3 '(letrec ((a 1) (b 2)) (+ a b)))
     (define cx (optimize x '()))
     (define cxab (optimize xab '()))
     (define cx1 (optimize x1 '()))
     (define cx2 (optimize x2 '()))
     (define cx2e (optimize x2e '()))
     (define cx3 (optimize x3 '()))
     (eval (optimize x '()) )
     (eval (optimize xab '()) )
     (eval (optimize x1 '()) )
     (eval (optimize x2 '()) )
     (eval (optimize x2e '()) )
     (eval (optimize x3 '()) )

     (define x '(lambda (n) n) '())
     (define y '(lambda (n) ((lambda (n) (* n n)) n)) '())
     (define z '(lambda (n) ((lambda (m) (* m n)) n)) '())
     (define cx (optimize x '()))
     (define cy (optimize y '()))
     (define cz (optimize z '()))

     (define a 100)
     (define a1 '(set! x (* a 2)))
     (define ca1 (optimize a1 '()))
     (define ca1g (optimize 'x '()))

     (define frameset1 '(lambda (n) (set! n (* n 2)) n))
     (define frameset2 '(lambda (n) (let ((m (* n 2))) (set! n (+ n m)) n)))
     (define cframeset1 (optimize frameset1 '()))
     (define cframeset2 (optimize frameset2 '()))
     ((eval cframeset1) 10)
     ((eval cframeset2) 10)

     (set! a 10)
     (set! a 100)
     (set! a -200)
     (define if1 '(if (> a 0) a (- 0 a)))
     (define cif1 (optimize if1 '()))
     (eval cif1)

     (define while1 '(while (> a 0) (print a) (set! a (- a 1))))
     (define cwhile1 (optimize while1 '()))
     (eval cwhile1)

     (define e1 '(define env1 (let ((a 1) (b 2)) (the-environment))))
     (define ce1 (optimize e1 '()))
     (eval ce1)

     (define acc1 '(access a env1))
     (define acc2 '(access b env1))
     (define cacc1 (optimize acc1 '()))
     (define cacc2 (optimize acc2 '()))
     (eval cacc1)
     (eval cacc2)

     ;; 
     ;; a performance study
     ;;

     (define g1 10)
     (define g2 20)

     ;; repeat
     (define define-repeat 
       '(define repeat 
	  (lambda (f n) 
	    (while (> n 0) 
	      (f) 
	      (set! n (-1+ n))))))

     ;; define foo
     (define define-foo 
       '(define foo 
	  (lambda (a b) 
	    (let ((x (+ a g1)) 
		  (y (* b g2))) 
	      (+ x y g1 g2)))))

     ;; define call foo
     (define define-call-foo 
       '(define call-foo 
	  (lambda () 
	    (foo 10 20))))

     ;; define repeat call foo
     (define define-repeat-call-foo 
       '(define repeat-call-foo 
	  (lambda () 
	    (repeat call-foo 10000))))

     ;; purely intepreted version

     (eval define-repeat)
     (eval define-foo)
     (eval define-call-foo)
     (eval define-repeat-call-foo)

     (time-it repeat-call-foo)

     (define (timed-test) (time-it repeat-call-foo))
     (perform-timed-run 10)

     ;; semi-optimized verion
    
     (define define-crepeat (optimize define-repeat '()))
     (define define-cfoo (optimize define-foo '()))
     (define define-ccall-foo (optimize define-call-foo '()))
     (define define-repeat-ccall-foo (optimize define-repeat-call-foo '()))

     (eval define-crepeat)
     (eval define-cfoo)
     (eval define-ccall-foo)
     (eval define-repeat-ccall-foo)

     ;; don't forget to optimize the timing functions as well!
     (set! time-it (eval ( optimize time-it '() )))
     (set! avg (eval ( optimize avg '() )))
     (set! run-n-times (eval ( optimize run-n-times '() )))
     (set! perform-timed-run (eval ( optimize perform-timed-run '() )))

     (time-it repeat-call-foo)

     (define (timed-test) (time-it repeat-call-foo))
     (perform-timed-run 10)

     ))

(if #f (begin

(define foo 100)
(define a   200)
(define x '(let ((a 1) (b a)) (+ a b))) 
(define x '(letrec ((a 1) (b a)) (+ a b)))
(define x '(let ((a 1)) (let ((foo a)) (+ a foo))))

))

;;
;; Timing
;;

(if #f
    (begin
     '(define (foo a b)
       (define (mul x y) (* x y))
       (define (add x y) (+ x y))
       (mul a (add a b))
       )
     '(define bar (lambda (a b)
		    (define (mul x y) (* x y))
		    (define (add x y) (+ x y))
		    (mul a (add a b))
		   ))
     ))

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

(define (avg x)    
  (let ((n (length x))
	(s (apply + x)))
    (/ s (* 1.0 n))))

(define (run-n-times-aux f n results)
  (print n)
  (if (> n 0)
      (run-n-times-aux f (- n 1) (cons (f) results))
    results))

(define (run-n-times f n)
  (let ((results nil))
    (while (> n 0)
      (set! results (cons (f) results))
      (set! n (- n 1)))
    results
    ))

(if #f
(begin
(define (run-n-times f n)
  (display "run-n-times:") (print n)
  (run-n-times-aux f n nil))
))

(define (perform-timed-run n)
  (let ((factor 1000000000))
    (/ (avg (run-n-times timed-test n)) factor)))

;; [EOF]
