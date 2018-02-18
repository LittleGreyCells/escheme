;;
;; bootstrap the optimization system
;;

(load "./optimizer/compiler.scm")

(define eval-backup eval)

(define (eval exp . env)
  (if (null? env)
      (eval-backup (compile exp '()))
      (eval-backup (compile exp (car env)))))

(define (rep)
  (while #t
     (display "ece> ")
     (flush-output)
     (let ((sexpr (read)))
       (print (eval sexpr)))))

(rep)

