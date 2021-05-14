(if #f
    (begin

      ;; (load "./catch-throw.scm")

      ;; no throw
      (catch 'bill
	     (cons 1 2))
      (catch-stack)

      ;; no throw target
      (catch 'bill
	     (throw 'mary 1))
      (catch-stack)

      ;; shallow throw
      (catch 'bar
	     (let ((x 1) (y 2))
	       (throw 'bar (+ x y))))
      (catch-stack)

      ;; deeper throw
      (catch 'foo
	     (letrec ((fact
		       (lambda (n)
			 (if (<= n 1)
			     (throw 'foo "abandoning fact")
			     (* n (fact (- n 1)))))))
	       (fact 10)))
      (catch-stack)
      
      ;; no target for the second throw, since the first removed it
      (catch 'mary
	     (let ((x 1) (y 2))
	       (throw 'mary (print (+ x y)) (throw 'mary "bye!"))))
      (catch-stack)

      ;; protect with finally clause; no throw
      (unwind-protect (+ 1 2 3) (print "finally"))
      (catch-stack)

      (catch 'marty
	     (unwind-protect (throw 'marty 1) (print "finally")))
      (catch-stack)
      
      (catch 'marty
	     (let ((x "finally from parent scope"))
	       (unwind-protect (throw 'marty 10) (print x))))
      (catch-stack)

      (catch 'hitchcok
	     (let ((x 100) (y 200))
	       (unwind-protect (throw-error (cons x y)) (print (list "x" x)) (print (list "y" y)))))
      (catch-stack)

      (define x (catch 'sherlock (throw 'sherlock "Watson!")))
      (print x)
      (catch-stack)

      ;; a single severe error
      (catch 'poirot
	     (let ((a 123))
	       (unwind-protect (car a) (print a))))
      (catch-stack)
      
      ;; two severe errors -- one during cleanup
      (catch 'hastings
	     (let ((a 2021))
	       (unwind-protect (car a) (print a) (cdr a))))
      (catch-stack)
      
      ))
