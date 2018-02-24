(define *version* "v1.0")
(set-prompt "ece> ")

(load "./boot/standard-functions.scm")

(let ((args (getargs))
      (usage
       (lambda ()
	 (display "usage: escheme [--nomacros] [-h | --help]")
	 (newline)
	 (exit)))
      (boot-macros 
       (lambda ()
	 (load "./macros/macros.scm")
	 (load "./macros/qquote.scm")
	 (load "./boot/macro-definitions.scm"))))
  (if (= (vector-length args) 1)
      (boot-macros)
      (if (> (vector-length args) 2)
	  (begin
	    (display "too many arguments given -- ")
	    (display args)
	    (newline)
	    (usage))
	  (let ((arg (vector-ref args 1)))
	    (cond ((memv arg (list "-h" "--help"))
		   (usage))
		  ((memv arg (list "-m" "--macros"))
		   (boot-macros))
		  ((memv arg (list "--nomacros"))
		   (set! *version* "v1.0 (no macros)")
		   nil)
		  (else
		   (display "unrecognized arg [" )
		   (display arg)
		   (display " ]")
		   (newline)
		   (usage)))))))

;; [EOF]
