(define *version* "v1.0")
(set-prompt "rep> ")

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

;; cdr past the 1st n elements and return car of remaining list
(define (list-ref list n)
  (car (list-tail list n)))

;; arrays

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

;;
;; special forms macros
;;

(let ((args (getargs))
      (macros "./macros/load-macros.scm"))
  (if (= (vector-length args) 1)
      (load macros)
      (if (> (vector-length args) 2)
	  (fatal "too many arguments" args)
	  (let ((arg (vector-ref args 1)))
	    (cond ((memv arg (list "-m" "--macros"))
		   (load macros))
		  ((memv arg (list "--nomacros"))
		   nil)
		  (else
		   (fatal "unrecognized arg" arg)))))))

;; [EOF]
