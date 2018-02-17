extensible-scheme v1.0
======================

R3RS extensible scheme interpreter utilizing an explicit control evaluator

## Introduction

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

## Implementation 

escheme is implemented in C++11, using a modest set of language features
exploiting class initialization/finalization semantics for deferred execution 
and cleanup. Templates have proven beneficial in control stack and argument 
stack implementation. Exceptions replace the standard library's setjmp/longjmp, 
while honoring C++ block finalization.

### Highlights

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

### Content
  
| Directory        | Description                                        |
| ---------------- | ---------------------------------------------------|
|  src    |    evaluator and core sourses|
|  macros   |  macro and quasiquote|
|  linenoise|  readline package|
|  tests  |    files for escheme regression testing|
|  docs |      documentation which may or may not be accurate|

## Getting Started

Consult the file _BuildingAndRunning_ to build the interpreter.

For a complete list of escheme functions consult _escheme_syntax.txt_ in the root directory.



