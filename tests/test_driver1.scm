(define displayln
  (lambda (s)
    (display s)
    (newline)))

(define display-header
  (lambda (s n)
    (display s)
    (display " (")
    (display n)
    (display ") ")
    (newline)))
    
(define (test1 n)
  (display-header "test1" n)
  (load "tests/escheme_test_suite.scm")
  (display "  suite1 ")
  (perform-timed-run n)
  (load "tests/escheme_test_suite2.scm")
  (display "  suite2 ")
  (perform-timed-run n)
  )

(define (test2 n)
  (display-header "test2" n)
  (load "syntax-help.scm")
  (let ((double-reverse
         (lambda ()
           (let ((p (syntax-read "syntax.scm")))
             (if (not (equal? p (reverse (reverse p))))
                 (error "p != rev(rev(p))")
                 )))))
    (let ()
      (while (> n 0)
             (double-reverse)
             (set! n (dec n))))
    ))

(define (test3 n)
  (display-header "test3" n)
  (load "syntax-help.scm")
  (let ((help-fn
         (lambda (fn)
           (let ((matches (reverse (syntax-find fn))))
             (if (not matches)
                 (error "couldn't find" fn)))))
        (fns '( car cdr cons list vector lambda define set! access abs add-history
                    append apply bound? call/cc ceiling char? close-port closure?
                    code? compile complex? cond dec default-object? display eq?
                    eval even? exit floor force gcd gc getenv get if inc integer?
                    last-pair let letrec list? member min negative? not null?
                    max memq number? odd? <= = * + open-append-file or output-port?
                    pair? port? print procedure? promise? put quotient random
                    rank read-char read remainder reverse round sequence set-car!
                    setenv shift-left shift-right string-append string-fill!
                    string->symbol substring symbol? symbol->string system
                    the-environment truncate unsetenv vector-copy! vector?
                    vector-length vector->list while write-char write zero?
                    )))
    (let ()
      (while (> n 0)
            (map help-fn (reverse (append fns fns fns fns fns)))
            (set! n (dec n))))
    ))

(define (driver n)
  (let ()
    (while (> n 0)
           (display "================================> ")
           (display n)
           (newline)
           (test1 200)
           (test2 200)
           (test3 10)
           (print (gc))
           (set! n (dec n))
           )))

(driver 5)
(exit)

