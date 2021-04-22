escheme
=======

A scheme interpreter utilizing an explicit control evaluator

## Introduction

_escheme_ is an aspiring implementation of the algorithmic language scheme, 
supporting approximately 90% of the R3RS standard. It has been principally an experimental
testbed for exploring models of evaluation, interpreter design and construction.
Along the way _escheme_ has become a capable programming language, not a toy.
Many additional functions are added to support environments, input/output, the host OS 
(linux/unix) and access _escheme_ internals.

Abelson and Sussman's _Structure and Interpretation of Computer Programs_
chapter 5 describes an explicit control evalutor for scheme. That material has provided the impetus
for _escheme's_ design and implementation approach as well as for its name. "ece-scheme" was a bit much
to type, so the name was shortened to "escheme".
_escheme_ is not related to emacs or eLisp in any way.

## Implementation 

_escheme_ is implemented in C++14, using a modest set of language features
exploiting class initialization/finalization semantics for deferred execution 
and cleanup. Templates are used for control stack and argument stack 
implementation. Exceptions are used for non-local returns, 
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
|  help |      escheme syntax related files|

## Extending escheme

See _docs/bindings_guide.txt_ to learn how to extend _escheme_ with additional 
primitives implemented in C++. It covers both passing _escheme_ 
values and constructing simple or structured objects (lists and vectors) to
return back to _escheme_.

## Getting Started

Consult the file _BuildingAndRunning_ to build the interpreter.

For a complete list of _escheme_ functions (and a help system) consult syntax-help.scm in the 
help directory.



