Scheme Macros -- A User's Guide
===============================

## Introduction

Macros are a powerful syntactic feature of LISP-like languages. Since programs are indistinguishable from
data, they can be manipulated, constructed and reconstructed. Macros in Scheme work behind the scenes
transforming symbolic expressions into new symbolic forms before evaluation. Let me state that again: The
result of macro expansion is then passed to eval to evaluate. The usual read/eval/print loop is
replaced with read/expand/eval/print loop.

escheme provides macro support through the following two files:
```
 macros.scm
 qquote.scm
```

These are borrowed from xscheme with suitible adaptation for the escheme interpreter.

The first (macros.scm) implements macros proper. The second provides a very useful and powerful quasi-quote
expander.

## How to Write Macros

Use the "macro" special form.
```
 (macro <macro-name> <macro-definition>)
```

The macro system will store the definition of the macro on the property list of the <macro-name> symbol, 
property %macro.

Example

Define a break. (Make sure macros.scm and qquote.scm are loaded).
```
 >(macro break (lambda (form) (list '*break-handler* '(the-environment))))
 %macro
```

Examine the property list for 'break.
```
 >(symbol-plist 'break)
 (%macro { closure:737a00 })

 >(get 'break '%macro)
 { closure:737a00 }
```

The procedure is the evaluated <macro-definition>:
```
 (lambda (form) (list '*break-handler* '(the-environment)))
```

The new eval will check whether each symbol in the function position is a macro. If so, it will fetch
the property (%macro) value ({ closure:737a00 }) and pass the rest of the macro as the bound argument
to the function.
```
 (break)
```

This invocation of break simply expands into '(*break-handler* (the-environment)) which is then
evaluated. Let's supply a *break-handler* to see what capabilities can be imagined:
```
 (define (*break-handler* env)
    (let ((done #f))
       (letrec ((exit (lambda () (set! done #t)))     ;; redefine so we leave scheme
           (resume (lambda () (exit)))           ;; return to scheme prompt
           (help (lambda () (print-help-text)))) ;; if there is help
           (while (not done)
              (display "break-handler> ")
              (print (eval (read) (the-environment)))))))
```

The 'break would prompt the user how to procede: exit, resume, ask for help or permit the 
evalution of any lexically enclosing function--examine the system under execution with
any function.
```
 > (break)
 break-handler> ...
 break-handler> (resume)
 >
```

### 2.1 Simple Macro

Let's define a simple macro to say "Hello, Guy! Did you know ...", followed by your statement.
Let's start with just the first part:
```
 (macro greeting (lambda (form) (list 'print "Hello, Guy! Did you know ...")))

 >(greeting)
 "Hello, Guy! Did you know..."
 #t
```

Ok, let's try to do something with the macro argument. Let's print it:
```
 (macro greeting (lambda (form) (list 'print (list 'quote form))))

 >(greeting)
 (greeting)

 >(greeting Yo)
 (greeting yo)
```

So, the first thing we notice is that the argument (form) is bound to the whole macro expression.
If we want the arguments following greeting, we will have to dig into it.
```
 (macro greeting (lambda (form) (list 'print (list 'quote (cadr form)))))

 >(greeting Yo)
 yo
 #t
```

You can see we are free to manipulate the form and build what we want to evaluate. In this case
we constructed.
   
### 2.2 Let Transformer Macro

Let's create a really useful macro: let. This macro will transform the special form let into
the equivalent lambda expansion. For example:
```
  //
  // let
  //     
  // (let ((<v1> <e1>) (<v2> <e2>) ... ) <body> ) ==>
  //     
  //   ((lambda (<v1> <v2> ...) <body>) <e1> <e2> ...)
  //
```
	
Let's break it down.
1. we need to extract the list of symbols
2. we need to extract the list of values
3. assemble them in the lambda invokation

```
 (set! form '(let ((<v1> <e1>) (<v2> <e2>)) <body> ))

 (set! syms (map car (cadr form)))
 (set! exps (map cadr (cadr form)))
 (set! body (cddr form))
 (set! func (append (list 'lambda syms) body))
 (set! expansion (cons func exps))
```

Make it one expression:
```
 (define (transform-let form)
    (cons (append (list 'lambda (map car (cadr form))) (cddr form)) (map cadr (cadr form))))
```

There is one last refinement--not every binding is necessarily a symbol/value pair. It can be a 
naked symbol with an implied initial value of nil. We need to rewrite the sym and exp
collectors:
```
 (set! form '(let ((<s1> <e1>) <s2> (<s3> <e3>)) <body> ))
 (set! syms (map (lambda (x) (if (pair? x) (car x) x)) (cadr form)))  ;; (<s1> ... <sN>)
 (set! exps (map (lambda (x) (if (pair? x) (cadr x) nil)) (cadr form))) ;; (<e1> ... <eN>)
 (set! body (cddr form))
 (set! func (append (list 'lambda syms) body))
 (set! expansion (cons func exps))
```

The new transformer:
```
 (define (%transform-let form)
    (cons (append (list 'lambda (map (lambda (x) (if (pair? x) (car x) x)) (cadr form))) (cddr form)) 
          (map (lambda (x) (if (pair? x) (cadr x) nil)) (cadr form))))
```

Let's make it into a macro:
```
 (macro let (lambda (form) (%transform-let form)) )
```

Yes, it is difficult to follow.

 
### 2.3 Letrec Transformer Macro

Let's create another really useful macro: letrec. This macro will transform the special form letrec into
the equivalent lambda expansions. It's a little more complex than let, but not much more. For example:
```
  //
  // letrec
  //     
  // (let ((<v1> <e1>) (<v2> <e2>) ... ) <body> ) ==>
  //     
  //   (let (<v1> <v2> ...)
  //     (set! <v1> <e1>)
  //     (set! <v2> <e2>)
  //     ...
  //     <body> ) ==>
  //
  //     ((lambda (<v1> <v2> ...)
  //        (set! <v1> <e1>)
  //        (set! <v2> <e2>)
  //        ... 
  //        <body> )
  //        '() '() ...)
  //
```

It's similar. However, the lambda parameters are initialized by set's before the body.

Let's break it down.
1. we need to extract the list of symbols
2. we need to extract the list of values
3. assemble the symbols in the formal parameter list of the lambda
4. use nil for all actual arguments
5. prefix setters before the body
	
```
 (set! form '(letrec ((<v1> <e1>) (<v2> <e2>)) <body> ))

 (set! form '(letrec ((%map1-aux
                (lambda (proc x result)
                   (if x
                       (%map1-aux proc (cdr x) (cons (proc (car x)) result))
                       (reverse result)))))
             (%map1-aux proc x '())) )

 (set! form '(letrec ((<v1> <e1>) (<v2> <e2>)) <body> ))

 (set! syms (map (lambda (x) (if (pair? x) (car x) x)) (cadr form)))
 (set! exps (map (lambda (x) (if (pair? x) (cadr x) nil)) (cadr form)))
 (set! nils (map (lambda (x) nil) (cadr form)))
      
 (define prepend-sets (lambda (syms exps body)
                         (while syms
                            (if (not (null? (car exps)))
                                (set! body (cons (list 'set! (car syms) (car exps)) body)))
                            (set! syms (cdr syms)) 
                            (set! exps (cdr exps))
                            )
                        body))
 
 (set! body (cddr form))
 (set! func (append (list 'lambda syms) (prepend-sets syms exps body)))
 (set! expansion (cons func nils))
```

Make it one expression:
```
 (define (%transform-letrec form)
    (let ((prepend-sets (lambda (syms exps body)
                         (while syms
                            (if (not (null? (car exps)))
	                       (set! body (cons (list 'set! (car syms) (car exps)) body)))
	                    (set! syms (cdr syms)) 
	                    (set! exps (cdr exps))
                         )
                         body))
            (syms (map (lambda (x) (if (pair? x) (car x) x)) (cadr form)))
            (exps (map (lambda (x) (if (pair? x) (cadr x) nil)) (cadr form)))
            (nils (map (lambda (x) nil) (cadr form)))
            (body (cddr form)))
          (set! body (prepend-sets syms exps body))
          (cons (append (list 'lambda syms) body) nils)))
```

Let's make it into a macro:
```
 (macro let (lambda (form) (%transform-letrec form)) )
```

