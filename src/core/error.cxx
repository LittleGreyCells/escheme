#include <cstdio>
#include <cstdlib>

#include "error.hxx"
#include "printer.hxx"
#include "pio.hxx"

#include "eval/eval.hxx"

namespace escheme
{

void ERROR::severe( const char* s, SEXPR exp1, SEXPR exp2 )
{
   using PIO::put;
   using PRINTER::print;
   
   put("error: ");
   put( s );
   
   if ( exp1 != nullptr )
   {
      put(" [");
      print(exp1);
      put("]");
   }
   
   if ( exp2 != nullptr )
   {
      put(" [");
      print(exp2);
      put("]");
   }
   
   put("\n");

   throw SevereError();
}

void ERROR::fatal( const char* s )
{
   using PIO::put;

   put("fatal error: ");
   put( s );
   put("\n");
   
   throw FatalError();
}

void ERROR::warning( const char* s, SEXPR exp )
{
   using PIO::put;
   using PRINTER::print;

   put("warning: ");
   put( s );
   
   if ( exp != nullptr )
   {
      put(" [");
      print(exp);
      put("]");
   }
   
   put("\n");
}

}
