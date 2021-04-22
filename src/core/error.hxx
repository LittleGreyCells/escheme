#ifndef error_hxx
#define error_hxx

#include "sexpr.hxx"

namespace escheme
{

namespace ERROR
{
   struct SevereError{};
   struct FatalError{};
   struct Exit{};
   
   SEXPR severe( const char*, SEXPR exp1 = nullptr, SEXPR exp2 = nullptr );
   void warning( const char*, SEXPR exp = nullptr );
   SEXPR fatal( const char* );
   SEXPR exit();
}

}

#endif

