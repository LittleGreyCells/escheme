;;
;; escheme configurer
;;

(define (split-args args options files)
  (if (null? args)
      (cons options files)
      (let ((arg (car args)))
        (if (equal? (substring arg 0 1) "-")
            (split-args (cdr args) (cons arg options) files)
            (split-args (cdr args) options (cons arg files))
            ))))

(let ((args (cdr (vector->list (getargs))))
      (usage
       (lambda ()
	 (display "usage: escheme [-h | --help ] | [files...]")
	 (newline)))
      )
  (let ((args (split-args args nil nil))
        options)
    (set! options (car args)) 
    (while options
       (let ((option (car options)))
         (cond ((or (equal? "--help" option)
                    (equal? "-h" option))
                (begin
                  (usage)
                  (exit)))
               (else
                (display "unknown option: ")
                (display option)
                (newline)
                (exit))))
       (set! options (cdr options)))
    ;; boot the intepreter
    (set! *version* "-- an explicit control evaluator")
    (set-prompt "ece> ")
    (load (system-path "boot/standard-functions.scm"))
    (load (system-path "macros/qquote.scm"))
    (load (system-path "macros/macros-interpreter.scm"))
    (load (system-path "boot/standard-macros.scm"))
    (load (system-path "boot/catch-throw.scm"))
    ;; (load (system-path "boot/modules.scm"))
    (let ((try-load
           (lambda (file)
             (let ((result (load file)))
               (if (null? result)
                   (error "load failed for" file))))))
      (for-each try-load (cdr args)))
    ))

;; [EOF]
