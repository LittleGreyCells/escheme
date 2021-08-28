#ifndef error_hxx
#define error_hxx

#include <string>

#include "sexpr.hxx"

namespace escheme
{

namespace ERROR
{
   struct SevereError{};
   struct FatalError{};
   struct Exit{};
   
   SEXPR severe( const char*, SEXPR exp1 = nullptr, SEXPR exp2 = nullptr );
   SEXPR severe( const std::string&, SEXPR exp1 = nullptr, SEXPR exp2 = nullptr );
   
   void warning( const char*, SEXPR exp = nullptr );
   void warning( const std::string& , SEXPR exp = nullptr );
   
   SEXPR fatal( const char* );
   SEXPR fatal( const std::string& );
   
   SEXPR exit();
}

}

#endif

