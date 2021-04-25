#include <cstdio>
#include <cstdlib>

#include "error.hxx"
#include "printer.hxx"
#include "pio.hxx"

#include "eval/eval.hxx"

namespace escheme
{
   using PIO::put;
   using PRINTER::print;
      
  SEXPR ERROR::severe( const char* s, SEXPR exp1, SEXPR exp2 )
   {
      put("error: ");
      put( s );
      
      if ( exp1 != nullptr )
      {
	 put(" '");
	 print(exp1);
	 put("'");
      }
      
      if ( exp2 != nullptr )
      {
	 put(" '");
	 print(exp2);
	 put("'");
      }
      
      put("\n");
      
      throw SevereError();
   }
   
   void ERROR::warning( const char* s, SEXPR exp )
   {
      put("warning: ");
      put( s );
      
      if ( exp != nullptr )
      {
	 put(" '");
	 print(exp);
	 put("'");
      }
      
      put("\n");
   }
   
   SEXPR ERROR::fatal( const char* s )
   {
      put("fatal error: ");
      put( s );
      put("\n");
      
      throw FatalError();
   }
   
   SEXPR ERROR::exit()
   {
      throw Exit();
   }
   
}
