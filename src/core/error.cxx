#include <cstdio>
#include <cstdlib>

#include "error.hxx"
#include "printer.hxx"
#include "pio.hxx"
#include "symtab.hxx"
#include "regstack.hxx"

#include "eval/eval.hxx"

namespace escheme
{

void ERROR::severe( const char* s, SEXPR exp1, SEXPR exp2 )
{
   PIO::put("error: ");
   PIO::put( s );
   
   if ( exp1 != nullptr )
   {
      PIO::put(" [");
      PRINTER::print(exp1);
      PIO::put("]");

      SYMTAB::enter( "%%error-object", exp1 );
   }
   
   if ( exp2 != nullptr )
   {
      PIO::put(" [");
      PRINTER::print(exp2);
      PIO::put("]");
   }
   
   PIO::put("\n");

   throw SevereError();
}

void ERROR::fatal( const char* s )
{
   PIO::put("fatal error: ");
   PIO::put( s );
   PIO::put("\n");
   
   throw FatalError();
}

void ERROR::warning( const char* s, SEXPR exp )
{
   PIO::put("warning: ");
   PIO::put( s );
   
   if ( exp != nullptr )
   {
      PIO::put(" [");
      PRINTER::print(exp);
      PIO::put("]");

      SYMTAB::enter( "%%warning-object", exp );
   }
   
   PIO::put("\n");
}

void ERROR::print_frame( SEXPR env )
{
   if ( anyp(env) && envp(env) )
   {
      FRAME frame = getenvframe(env);
      SEXPR closure = getframeclosure(frame);
      
      if ( closurep(closure) )
      {
	 PRINTER::print( getclosurevars(closure) );
         PIO::put( ", " );
	 PRINTER::print( getclosurecode(closure) );
      }
   }
}

void ERROR::print_active_frame()
{
   PIO::put( "active frame\n" );

   SEXPR env = EVAL::env;

   if ( nullp(env) )
   {
      PIO::put( "  ()\n" );
      return;
   }

   for ( int i = 0; anyp(env); ++i )
   {
      char buffer[80];
      sprintf( buffer, "  level %d ", i );
      PIO::put( buffer );
      print_frame( env );
      PIO::put( "\n" );
      env = getenvbase(env);
   }
}

void ERROR::print_stacktrace()
{
   const int top = regstack.gettop();
   int n = 0;

   char buffer[80];
   sprintf( buffer, "stacktrace (depth=%d)\n", top+1 );
   PIO::put( buffer );

   for ( int i = top; i >= 0; --i )
   {
      SEXPR item = regstack[i];

      sprintf( buffer, "  depth %d ", top-i );
      PIO::put( buffer );
      PRINTER::print( item );
      PIO::put( "\n" );

      if ( envp(item) )
      {
	 sprintf( buffer, "  frame %d ", n++ );
         PIO::put( buffer );
	 print_frame( item );
         PIO::put( "\n" );
      }
   }
}

}
