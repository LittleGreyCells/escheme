#ifndef GENSCAV_HXX
#define GENSCAV_HXX

#include "sexpr.hxx"
#include "tstack.hxx"

namespace GENSCAV
{
   void mark( SEXPR& n );
   void mark( TSTACK<SEXPR>& s );

   SEXPR newnode( NodeKind kind );

   void gc();
};

#endif
