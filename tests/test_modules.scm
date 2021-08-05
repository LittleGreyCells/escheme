(if #f
    (begin

      (load "boot/modules.scm")

      (define (assert v)
	(if v
	    (print "assertion passed")
	    (print "assertion failure")))

      (module foo
	      (define (double n) (* n 2))
	      (define (square n) (* n n))
	      )
      (define foo (find-module 'foo))
      (print (environment-bindings foo))

      (module bar
	      (define (twice f) (lambda (n) (f (f n))))
	      )
      (define bar (find-module 'bar))
      (print (environment-bindings bar))

      (define double (access double foo))
      (define square (access square foo))
      (define twice (access twice bar))
      
      (assert (= (double 100) 200))
      (assert (= (square 5) 25))
      (assert (= (square (square 5)) 625))

      (assert (= ((twice square) 5) 625))
      (assert (= (double ((twice square) 5)) (* 2 625)))
      
      ))
