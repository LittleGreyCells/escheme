
(load "./experimental/modules.scm")

(define (show <mod>)
  (print (assoc (module-name <mod>) (all-modules)))
  (print (list "xps:" (%get-exports <mod>)))
  (print (list "ips:" (%get-imports <mod>)))
  (print (list "sym:" (%get-symbols <mod>))))

(module foo)

(module bar
	(display (cons (eq? (current-module) (find-module 'bar))
		       (eq? (current-module) (find-module 'escheme))))
	(newline))

(module bob
	(define (echo x) x)
	(export echo))

(module bobx2
	(define (double x) (* x 2))
	(export double))

(module mary
	(import bob bobx2)
	(define (square x) (* (echo x) (echo x)))
	(define (quad x) (* (double x) (double x)))
	(export square quad))

(module fran
	(import bob)
	(define foo echo)
	(export foo))

(show foo)
(show bar)
(show bob)
(show bobx2)
(show mary)
(show fran)

(symbol-value 'echo bob)
(symbol-value 'double bobx2)
(symbol-value 'square mary)

(print ((symbol-value 'echo bob) 10))
(print ((symbol-value 'double bobx2) 10))
(print ((symbol-value 'square mary) 10))

(symbol-value 'echo mary)  ;; should cause an error -- echo not defined in mary
(symbol-value 'echo mary 'default)
(symbol-value 'echo fran)  ;; should cause an error -- echo not defined in fran
(symbol-value 'echo fran 'default)
(symbol-value* 'echo fran) 
(symbol-value* 'echo fran 'default) 

(show escheme)

(import bob)
(import bobx2)
(import mary)
(show escheme)

(print (echo 10))
(print (double 10))
(print (square 10))
(print (quad 10))

(select-module foo)
(define x 10)
(define y 20)
(show foo)

;;
;; end
;;
