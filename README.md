escheme
=======

R3RS scheme interpreter utilizing an explicit control evaluator

## Introduction

escheme is an aspiring R3RS implementation of the algorithmic language scheme, 
supporting approximately 90% of the standard. Many additional 
functions are added to support environments, input/output, the host OS 
(linux/unix) and access escheme internals. Bignums are not supported.

Abelson and Sussman's SICP* in Ch5 describes an explicit control evalutor for 
scheme. That material provided the impetus for escheme.  It is not related
to emacs or eLisp in any way.

(*) Structure and Interpretation of Computer Programs (aka SICP)
    by Harold Abelson and Gerald Sussman
    MIT Press (1984)

## Implementation 

escheme is implemented in C++11, using a modest set of language features
exploiting class initialization/finalization semantics for deferred execution 
and cleanup. Templates are used for control stack and argument stack 
implementation. Exceptions replace the standard library's setjmp/longjmp, 
while honoring C++ block finalization.

### Content
  
| Directory        | Description                                        |
| ---------------- | ---------------------------------------------------|
|  src/eval    |    evaluator|
|  src/core    |    core sources|
|  src/linenoise|  readline package|
|  boot      | bootstrapping files |
|  macros   |  macro and quasiquote|
|  tests  |    files for escheme regression testing|
|  docs |      documentation which may or may not be accurate|

## Extending escheme

See _docs/bindings_guide.txt_ to learn how to extend escheme with ones own 
primitive functions implemented in C or C++. It covers both passing escheme 
values and constructing simple or structured objects (lists and vectors) back 
to escheme.

## Getting Started

Consult the file _BuildingAndRunning_ to build the interpreter.

For a complete list of escheme functions (and a help system) consult syntax.scm/syntax-help.scm in the 
root directory.



