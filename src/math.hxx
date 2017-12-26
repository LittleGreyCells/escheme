#ifndef math_hxx
#define math_hxx

#include "sexpr.hxx"

namespace MATH
{
   // arithmetic ops
   SEXPR add();
   SEXPR sub();
   SEXPR mul();
   SEXPR div();

   // relational ops
   SEXPR eq();
   SEXPR lt();
   SEXPR le();
   SEXPR gt();
   SEXPR ge();

   // misc
   SEXPR truncate();
   SEXPR floor();
   SEXPR ceiling();
   SEXPR round();
   SEXPR inc();
   SEXPR dec();
   SEXPR abs();
   SEXPR gcd();
   SEXPR random();
   SEXPR quotient();
   SEXPR remainder();
   SEXPR min();
   SEXPR max();

   // bitwise
   SEXPR logand();
   SEXPR logior();
   SEXPR logxor();
   SEXPR lognot();
   SEXPR rsh();
   SEXPR ars();
   SEXPR lsh();

};

#endif
