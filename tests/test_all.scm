(if #f
    (begin
      (load "./tests/test_all.scm")
      ))

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
  (load "help/syntax-help.scm")
  (let ((double-reverse
         (lambda ()
           (let ((p (syntax-read "help/syntax.scm")))
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
  (load "help/syntax-help.scm")
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
                    read-char read remainder reverse round sequence set-car!
                    setenv shift-left shift-right string-append string-fill!
                    string->symbol substring symbol? symbol->string system
                    the-environment truncate unsetenv vector-copy! vector?
                    vector-length vector->list while write-char write zero?
                    )))
    (let ()
      (while (> n 0)
             (map help-fn (reverse (append fns fns fns fns fns
                                           fns fns fns fns fns
                                           )))
            (set! n (dec n))))
    ))

(define (test4 n)
  (display-header "test4" n)
  (make-vector 10000)
  (make-vector 10000)
  (make-byte-vector 10000)
  (make-byte-vector 10000)
  (while (> n 0)
         (make-vector 1000)
         (make-byte-vector 1000)
         (set! n (dec n))))

(define (random-count factor base)
  (* (+ (random factor) 1) base))

(define (run n)
  (let ()
    (while (> n 0)
           (display "================================> ")
           (display n)
           (newline)
           (test1 (random-count 15 150))
           (test2 (random-count 15 150))
           (test3 (random-count 5 2))
           (test4 1000)
           (display "gc: ")
           (print (gc))
           (display "fs: ")
           (print (fs))
           (set! n (dec n))
           )))

(run 10)

;;(exit)

