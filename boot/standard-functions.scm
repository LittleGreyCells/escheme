;;
;; standard functions
;;

;;
;; error
;;

(define (error msg . object)
  (display "error: ")
  (display msg)
  (if object
      (begin (display " [") 
             (display object)
             (display "]")))
  (newline)
  (*toplevel*))

(define (fatal msg . object)
  (display "fatal error: ")
  (display msg)
  (if object
      (begin (display " [") 
             (display object)
             (display "]")))
  (newline)
  (exit))

;;
;; lists
;;

;; cdr past the 1st n elements and return car of remaining list
(define (list-ref list n)
  (car (list-tail list n)))

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

;; terminal/history

(define history show-history)

;; [EOF]
