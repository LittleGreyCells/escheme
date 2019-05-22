(define *version* "v1.0 (interpeter)")
(set-prompt "escheme> ")

(load (system-path "boot/standard-functions.scm"))

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
	 (display "usage: escheme [--help | --nomacros] | [files...]")
	 (newline)))
      (boot-macros 
       (lambda ()
	 (load (system-path "macros/macros.scm"))
	 (load (system-path "macros/qquote.scm"))
	 (load (system-path "boot/macro-definitions.scm")))))
  (let ((args (split-args args nil nil))
        (macro-rep #t)
        options)
    (set! options (car args))
    (while options
       (let ((option (car options)))
         (cond ((or (equal? "--help" option)
                    (equal? "-h" option))
                (begin
                  (usage)
                  (exit)))
               ((or (equal? "--nomacros" option)
                    (equal? "-n" option))
                (set! macro-rep #f))
               (else
                (display "unknown option: ")
                (display option)
                (newline)
                (exit))))
       (set! options (cdr options)))
    (if macro-rep
        (boot-macros)
        (set-prompt "nomac> "))
    (let ((try-load
           (lambda (file)
             (let ((result (load file)))
               (if (null? result)
                   (error "load failed for" file))))))
      (for-each try-load (cdr args)))
    ))

;; [EOF]
