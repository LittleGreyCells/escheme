;;
;; standard functions
;;

;;
;; error
;;

(define (error msg . object)
  (display "error: " *stderr*)
  (display msg *stderr*)
  (if object
      (begin (display " [" *stderr*) 
             (display (car object) *stderr*)
             (display "]" *stderr*) ))
  (newline *stderr*)
  (*toplevel*))

(define (fatal msg . object)
  (display "fatal error: " *stderr*)
  (display msg *stderr*)
  (if object
      (begin (display " [" *stderr*) 
             (display (car object) *stderr*)
             (display "]" *stderr*) ))
  (newline *stderr*)
  (exit))

;;
;; lists
;;

;; cdr past the 1st n elements and return car of remaining list
(define (list-ref list n)
  (car (list-tail list n)))

(define first car)
(define second cadr)
(define third caddr)

;;
;; arrays
;;

(define (make-array d . r)
  ((lambda (v) 
     (if r 
	 ((lambda (i) 
	    (while (< i d) 
	      (vector-set! v i (apply make-array r)) 
	      (set! i (+ i 1)))) 0) ) v)
   (make-vector d)) )

(define (array-ref a i . r)
  ((lambda (e) 
     (if r 
	 (apply array-ref (list* e r)) e)) 
   (vector-ref a i)) )

(define (array-set! a i v . r)
  (if r
      (apply array-set! (list* (vector-ref a i) v r))
      (vector-set! a i v)))

;;
;; terminal/history
;;

(define history show-history)

;; [EOF]
