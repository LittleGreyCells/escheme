escheme
=======

A scheme interpreter utilizing an explicit control evaluator

## Introduction

_escheme_ is an aspiring implementation of the algorithmic language scheme, 
supporting approximately 90% of the R3RS standard. Granted, the R3RS standard is dated,
a scheme report from the late 1980s.
This is due both to the time frame of escheme's inception as well as from a desire to
constrain the design target. As such it has been principally an experimental
testbed for exploring models of evaluation, interpreter design and construction.

Abelson and Sussman's _Structure and Interpretation of Computer Programs_
chapter 5 describes an explicit control evalutor for scheme. That material has provided the impetus
for _escheme's_ design and implementation approach as well as for its name. "ece-scheme" was tiresome
to type and was shortened to "escheme".

_escheme_ is not related to emacs or eLisp in any way.

## Implementation 

_escheme_ is implemented in C++14, using a modest set of language features,
exploiting class initialization/finalization for deferred execution 
and cleanup. Templates are used primarily for evaluator stack 
implementations. Exceptions are used for non-local returns. 

The implementation is organized into 21 related spec/body groupings, constituting
8 domains: object representations, reader, printer, port and terminal
IO, memory manager, symbol table and, lastly, the interpreter with its supporting
apparatus. While efficiency has been an important consideration, intelligibility has
been paramount in order to facilitate maintenance and enhancement of a long-lived system.


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



