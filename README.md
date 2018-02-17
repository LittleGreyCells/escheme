extensible-scheme v1.0
======================

R3RS extensible scheme interpreter utilizing an explicit control evaluator

# Introduction

extensible-scheme (aka escheme) is an aspiring R3RS implementation of the 
algorithmic language scheme, supporting approximately 90% of the standard. 
Further, many additional functions are added to support environments, 
input/output, the host OS (linux/unix) and access escheme internals.

Abelson and Sussman's SICP* describes a meta-circular scheme interpreter (Ch4).
A later chapter (Ch5) describes an approach for building a tail recursive 
explicit control evalutor. That material provided the impetus for escheme.
The escheme interpreter implementation takes the SICP model explicit 
control evaluator and extends it beyond the scheme core, adding a number of 
special forms (cond, while, access, let and letrec).

(*) Structure and Interpretation of Computer Programs (aka SICP)
    by Harold Abelson and Gerald Sussman
    MIT Press (1984)

# Implementation 

escheme is implemented in C++11, using a modest set of language features
exploiting class initialization/finalization semantics for deferred execution 
and cleanup. Templates have proven beneficial in control stack and argument 
stack implementation. Exceptions replace the standard library's setjmp/longjmp, 
while honoring C++ block finalization.

## Highlights

- escheme objects are created from a uniformly sized descriminated union type
    (not from a class heirarchy)
- escheme objects are allocated from a segmented memory pool
- escheme uses a simple mark/sweep garbage collector
- escheme uses array based stacks for its interpreter state stacks
- escheme uses array based stack for its argument stack
- escheme represents continuations as vectors of saved state
   (more expensive than a link list implementation, but the decision
    was made to favor a more efficient runtime for function calls)
- escheme implements the global environment as a hash table (single instance)
- escheme implements other environments as indexable "frames"
- escheme uses a tail recursive evaluator (eceval) to interpret s-expressions
- escheme uses a tail recursive evaluator (bceval) to evaluate compiled code

## C++11 Source Files

| File             | Description                                        |
| ---------------- | ---------------------------------------------------|
|  [evaluator]     |                                                    |
|    eval.*xx      | evaluator base                                     |
|    eceval.cxx    | explicit control evaluator (interpreter)           |
|  [core]          |                                                    |
|    argstack.*xx  | function argument stack                            |
|    error.*xx     | error handler                                      |
|    escheme.cxx   | system bootstrap                                   |
|    framestore.hxx| frames                                             |
|    func.*xx      | general function library                           |
|    funtab.*xx    | function table                                     |
|    intstack.*xx  | execution state stack (integer valued)             |
|    ipcsoc.*xx    | socket library                                     |
|    math.*xx      | math library                                       |
|    memory.*xx    | object allocator/garbage collector                 |
|    pio.*xx       | port I/O                                           |
|    tio.*xx       | terminal I/O                                       |
|    printer.*xx   | s-expression printer                               |
|    reader.*xx    | s-expression reader                                |
|    regstack.*xx  | execution state stack (s-expression valued)        |
|    rep.*xx	   | read/eval/print loop and initial system definition |
|    sexpr.*xx	   | escheme object definitions                         |
|    symtab.*xx	   | global environment                                 |
|    tstack.*xx    | stack template                                     | 

## Escheme Source Files

| File             | Description                                        |
| ---------------- | ---------------------------------------------------|
|  escheme.scm     |   escheme bootstrap code
|  [macros]||
|    macros.scm    |    macro system|
|    qquote.scm    |     quasiquote/backquote system|

# Miscellaneous Directories
  
| Directory        | Description                                        |
| ---------------- | ---------------------------------------------------|
|  compiler  | optimizer source; an attempt to improve interpreter
                 performance by "compiling" s-expressions into
                 s-expressions with various optimizations applied.|
|  docs |      documentation which may or may not be accurate|
|  linenoise  readline package|
|  macros     macro and quasiquote|
|  src        evaluator and core sourses|
|  tests      files for escheme regression testing|

# Other Files

-README             -- this file
-Acknowledgments    -- shout out
-BuildingAndRunning -- getting started


