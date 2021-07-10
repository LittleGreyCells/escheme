#ifndef equality_hxx
#define equality_hxx

#include "sexpr.hxx"

namespace escheme
{
   bool eq( SEXPR e1, SEXPR e2 );
   bool eqv( SEXPR e1, SEXPR e2 );
   bool equal( SEXPR e1, SEXPR e2 );
}

#endif
