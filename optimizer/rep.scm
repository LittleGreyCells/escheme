
;;
;; Optimizer REP Loop
;;

(load "./optimizer/optimizer.scm")

(set! opt-verbose #f)
(set! enable-clambda #t)
(set! enable-grs #t)
(set! enable-cset #t)

(define %%optimize optimize)

(define (optimize expr . env)
  (if (null? env)
    (%%optimize (%expand-macros expr))
    (%%optimize (%expand-macros expr) (car env)) (car env)))

(define (optimize-file file . noisily)
  (if (not (string? file))
      (error "filename is not a string")
      (let ((port (open-input-file file)))
        (if port
          (let ((sexpr (read port)))
            (while (not (eof-object? sexpr))
		   (if noisily (begin (display ">> ") (print sexpr)))
		   (set! sexpr (optimize sexpr nil))
		   (eval sexpr)
		   (set! sexpr (read port)))
            (close-port port)))
        port)))

(define (rep)
  (set-prompt "optimizer> ")
  ;; all errors will return here
  (call/cc (lambda (cc) (set! *toplevel* cc)))
  (while #t
     ;; incorporate realine with history
     (let ((sexpr (read *terminal*)))
       (add-history sexpr)
       (set! sexpr (optimize sexpr nil))
       (print (eval sexpr)))))

(if #f
    (begin

      (define opt-verbose #t)
      (define enable-clambda #f)
      (define enable-grs #f)
      (define enable-cset #f)

      (load "./optimizer/optimizer.scm")
      
      (load "./optimizer/rep.scm")

      (optimize-file "./optimizer/opt1.scm" 1)
      
      (optimize-file "./optimizer/opt1a.scm" 1)
      (optimize-file "./optimizer/opt1b.scm" 1)
      (optimize-file "./optimizer/opt1c.scm" 1)

      ;; note: resume here with file 1c_1.
      ;;       there is a problem with parsing let bindings.
      ;;       singletons are not parsing (sym).
      ;;       only pairs are ((sym value)).
      (optimize-file "./optimizer/opt1c_1.scm" 1)
      (optimize-file "./optimizer/opt1c_2.scm" 1)
      
      (optimize-file "./optimizer/opt2.scm" 1)
      (optimize-file "./optimizer/opt3.scm" 1)

      ))

;; [EOF]
