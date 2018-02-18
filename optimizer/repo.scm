;;
;; bootstrap the optimization system
;;

(load "./optimizer/optimizer.scm")

(define eval-backup eval)

(define (eval exp . env)
  (if (null? env)
      (eval-backup (optimize exp '()))
      (eval-backup (optimize exp (car env)))))

(define (rep)
  (while #t
     (display "repo> ")
     (flush-output)
     (let ((sexpr (read)))
       (print (eval sexpr)))))

(rep)

