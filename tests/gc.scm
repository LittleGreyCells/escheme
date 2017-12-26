;;; -*- Mode: Lisp -*-

(define (adorn v i adornment)
  (vector-set! v i (cons adornment (vector-ref v i))))

(define (print-vector v)
  (let ((i 0))
    (while (< i (vector-length v))
      (let ((x (vector-ref v i)))
	(if (vector? x)
	    (print-vector x)
	  (print (vector-ref v i))))
      (set! i (+ i 1)))))
  
(define (gc-verbose)
  (let ((stats (gc))
	(adorn (lambda (v i a)
		 (vector-set! v i (cons a (vector-ref v i))))))
    (let ((vk (vector-ref stats 3)))
      (if vk
	  (begin
	   (adorn vk 0 'free)

	   (adorn vk 1 'null)
	   (adorn vk 2 'symbol)
	   (adorn vk 3 'fixnum)
	   (adorn vk 4 'flonum)
	   (adorn vk 5 'char)

	   (adorn vk 6 'string)
	   (adorn vk 7 'cons)
	   (adorn vk 8 'vector)
	   (adorn vk 9 'bvec)
	   (adorn vk 10 'env)

	   (adorn vk 11 'promise)
	   (adorn vk 12 'closure)
	   (adorn vk 13 'continuation)
	   (adorn vk 14 'port)
	   (adorn vk 15 'string-port)

	   (adorn vk 16 'func)
	   (adorn vk 17 'eval)
	   (adorn vk 18 'apply)
	   (adorn vk 19 'callcc)
	   (adorn vk 20 'map)

	   (adorn vk 21 'for)
	   (adorn vk 22 'force)
	   (adorn vk 23 'gref)
	   (adorn vk 24 'fref)
	   ))
      (display (list "collections" (vector-ref stats 0)))
      (newline)
      (display (list "total-nodes" (vector-ref stats 1)))
      (newline)
      (display (list "free-nodes" (vector-ref stats 2)))
      (newline)
      (display "last-collection")
      (newline)
      (print-vector vk)
      #t
      )))
