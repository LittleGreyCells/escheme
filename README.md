escheme v1.0
============

R3RS scheme interpreter utilizing an explicit control evaluator

## Introduction

escheme is an aspiring R3RS implementation of the algorithmic language scheme, 
supporting approximately 90% of the standard. Further, many additional 
functions are added to support environments, input/output, the host OS 
(linux/unix) and access escheme internals.

Abelson and Sussman's SICP* in Ch5 describes a tail recursive explicit 
control evalutor for scheme. That material provided the impetus for escheme.
The name "ece-scheme" was a mouthful, so it was shortened to "escheme." It
is not related to emacs or eLisp in any way.

The escheme interpreter implementation takes SICP's explicit control 
evaluator and extends it beyond its simple core, adding a number of 
special forms (cond, while, access, let and letrec).

(*) Structure and Interpretation of Computer Programs (aka SICP)
    by Harold Abelson and Gerald Sussman
    MIT Press (1984)

## Implementation 

escheme is implemented in C++11, using a modest set of language features
exploiting class initialization/finalization semantics for deferred execution 
and cleanup. Templates are used for control stack and argument stack 
implementation. Exceptions replace the standard library's setjmp/longjmp, 
while honoring C++ block finalization.

### Highlights

- escheme objects are created from a uniformly sized descriminated union
    (not from a class heirarchy)
- escheme uses a simple mark/sweep garbage collector
- escheme uses array based stacks for its interpreter state stacks
- escheme uses an array based stack for its argument stack
- escheme represents continuations as vectors of saved state
   (more expensive than a link list implementation, but favors a more efficient runtime for function calls)
- escheme implements the global environment as a hash table (single instance)
- escheme implements other environments as indexable "frames"

### Content
  
| Directory        | Description                                        |
| ---------------- | ---------------------------------------------------|
|  src/eval    |    evaluator|
|  src/core    |    core sourses|
|  boot      | bootstrapping files to select interpreter configurations |
|  optimizer | s-expression transformer |
|  macros   |  macro and quasiquote|
|  linenoise|  readline package|
|  tests  |    files for escheme regression testing|
|  docs |      documentation which may or may not be accurate|

## Extending escheme

The utility of an extensible, lightweight, embedded dynamic language cannot be 
over-emphasized. One has the full scope of the underlying operating system and 
applications base to choose from for adding new capabities to escheme. 
Applications can be enhanced. Disparate applications and be integrated 
utilizing a scheme REP loop opened on a socket or secure tunnel. Data types 
and structuring are solved (s-expressions). Values can be communicated. 
Programs as values can get sent and evaluated at their target location -- 
something more than RPC.

See _docs/bindings_guide.txt_ to learn how to extend escheme with ones own 
primitive functions implemented in C or C++. It covers both passing escheme 
values and constructing simple or structured objects (lists and vectors) back 
to escheme.

## Getting Started

Consult the file _BuildingAndRunning_ to build the interpreter.

For a complete list of escheme functions consult _escheme_syntax.txt_ in the 
root directory.



