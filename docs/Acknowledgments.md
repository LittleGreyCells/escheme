Acknowledgments
===============

escheme would not be possible without the prior work of notable scheme
engineering and educational efforts.

## Predecessors

### XLISP and XSCHEME (David Betz)

Betz's contributions, XLISP and XSCHEME, were widely known in the industry
during the 80s and 90s. I used an early version of his XLISP interpreter 
in an AI course. That marked the beginning of my interest in LISP and 
LISP dialects. I have used his XSCHEME compiler-based system to construct
frameworks for software simulation and test.

His XSCHEME system has a coherent and understandable structure. It has been
hard not to immitate such coherence. However, the implemention of escheme 
diverges in many areas. Principally it is an interperter-based system. 
And the implementation of the explicit control evaluator is due 
to my own protracted efforts over a period of several years.

### SICP Explicit Control Evaluator (Harold Abelson and Gerald Sussman)

While browsing The Aerospace Corporation library in the early 90s I discovered 
the MIT textbook "The Structure and Interpretation of Computer Programs",
aka SICP.
 
How I had wished this text had been used for my undergraduate work in computer
science. I devoured the text and relished Chapters 4 and 5 which describe
approaches to implementing interpreters and a compiler for scheme. I have 
used the explicit control evaluator described in Chapter 5 as a starting
point for escheme. It has been expanded to handle many more special forms.

## Addendum

### escheme-compiled

Although I started with SICP model compiler in Chapter 5, I have modified it 
considerably. The virtual machine I designed for byte code evaluation is far
different from that described in SICP. The resulting compiler keeps pace.
