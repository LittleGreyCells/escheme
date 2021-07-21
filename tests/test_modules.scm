(if #f
    (begin

      (load "boot/modules.scm")

      (module foo
	      (define (double n) (* n 2))
	      (define (square n) (* n n))
	      )

      (module bar
	      (define (twice f) (lambda (n) (f (f n))))
	      )

      (define foo (find-module 'foo))
      (define bar (find-module 'bar))

      (define double (access double foo))
      (define square (access square foo))
      (define twice (access twice bar))
      
      (double 100)
      (square 5)
      (square (square 5))

      ((twice square) 5)
      (double ((twice square) 5))
      
      ))
