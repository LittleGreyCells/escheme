#ifndef error_hxx
#define error_hxx

#include "sexpr.hxx"

namespace ERROR
{
   struct SevereError {};
   struct FatalError {};
   struct Exit {};
   
   void severe( const char*, SEXPR exp1 = nullptr, SEXPR exp2 = nullptr );
   void fatal( const char* );
   void warning( const char*, SEXPR exp = nullptr );
}

#endif

