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

   void print_frame( SEXPR env );
   void print_active_frame();
   void print_stacktrace();
}

#endif

