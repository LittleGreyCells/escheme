(define *version* "(interpeter)")
(set-prompt "ece> ")

(load "./boot/standard-functions.scm")

(let ((args (cdr (vector->list (getargs))))
      (macros #t)
      (usage
       (lambda ()
	 (display "usage: escheme [--nomacros | --help] [files...]")
	 (newline)))
      (boot-macros 
       (lambda ()
	 (load "./macros/macros.scm")
	 (load "./macros/qquote.scm")
	 (load "./boot/macro-definitions.scm"))))
  (if (equal? (car args) "--help")
      (begin
        (usage)
        (exit)))
  (if (equal? (car args) "--nomacros")
      (begin
        (set! macros #f)
        (set! *version* "(intepreter, no macros)")
        (set! args (cdr args))))
  (if macros
      (boot-macros))
  ;; load any list files...
  (while args
         (let ((result (load (car args))))
           (if (null? result)
               (error "load failed for" (car args)))
           (set! args (cdr args))))
  )

;; [EOF]
