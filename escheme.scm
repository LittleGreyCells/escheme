(define *version* "v1.0 (interpeter)")
(set-prompt "ece> ")

(load (system-path "boot/standard-functions.scm"))

(define (accume-options args options)
  (if (null? args)
      options
      (let ((arg (car args)))
        (if (equal? (substring arg 0 1) "-")
            (accume-options (cdr args) (cons arg options))
            (accume-options (cdr args) options)))))

(let ((args (cdr (vector->list (getargs))))
      (usage
       (lambda ()
	 (display "usage: escheme [(-h | --help)] | [files...]")
	 (newline)))
      (boot-macros 
       (lambda ()
	 (load (system-path "macros/macros.scm"))
	 (load (system-path "macros/qquote.scm"))
	 (load (system-path "boot/macro-definitions.scm")))))
  (if (or (memv "--help" args) (memv "-h" args))
      (begin
        (usage)
        (exit)))
  (let ((options (accume-options args nil)))
    (if options
        (begin
          (display "extra options not expected ")
          (display options)
          (newline)
          (usage)
          (exit))))
  (boot-macros)
  (let ((try-load
         (lambda (file)
           (let ((result (load file)))
             (if (null? result)
                 (error "load failed for" file))))))
    (for-each try-load args))
  )

;; [EOF]
