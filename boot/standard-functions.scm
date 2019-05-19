;;
;; standard functions
;;

(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))

(define caaar (lambda (x) (car (car (car x)))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdaar (lambda (x) (cdr (car (car x)))))
(define cdadr (lambda (x) (cdr (car (cdr x)))))
(define cddar (lambda (x) (cdr (cdr (car x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))

(define caaaar (lambda (x) (car (car (car (car x))))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))
(define caadar (lambda (x) (car (car (cdr (car x))))))
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
(define cadaar (lambda (x) (car (cdr (car (car x))))))
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define cdaaar (lambda (x) (cdr (car (car (car x))))))
(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))
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

(define list*
  (lambda d
    (letrec ((f (lambda (d)
                  (if (= (length d) 1)
                      (car d)
                      (cons (car d) (f (cdr d)))))))
      (if (null? d)
          d
          (f d)))))

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
