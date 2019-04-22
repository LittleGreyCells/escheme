#include <cstdio>
#include <cstdlib>

#include "error.hxx"
#include "printer.hxx"
#include "symtab.hxx"
#include "regstack.hxx"

#include "eval/eval.hxx"

void ERROR::severe( const char* s, SEXPR exp1, SEXPR exp2 )
{
   printf("error: %s", s);
   
   if ( exp1 != nullptr )
   {
      printf(" [");
      PRINTER::print(exp1);
      printf("]");

      SYMTAB::enter( "%%error-object", exp1 );
   }
   
   if ( exp2 != nullptr )
   {
      printf(" [");
      PRINTER::print(exp2);
      printf("]");
   }
   
   printf("\n");

#if 0
   PRINTER::print( EVAL::get_evaluator_state() );
   printf("\n");
#endif
   throw SevereError();
}

void ERROR::fatal( const char* s )
{
   printf("fatal error: %s\n", s);
   
   throw FatalError();
}

void ERROR::warning( const char* s, SEXPR exp )
{
   printf("warning: %s", s);
   
   if ( exp != nullptr )
   {
      printf(" [");
      PRINTER::print(exp);
      printf("]");

      SYMTAB::enter( "%%warning-object", exp );
   }
   
   printf("\n");
}

void ERROR::print_frame( SEXPR env )
{
   if ( anyp(env) && envp(env) )
   {
      SEXPR frame = getenvframe(env);
      SEXPR closure = getframeclosure(frame);
      
      if ( closurep(closure) )
      {
	 PRINTER::print( getclosurevars(closure) );
	 printf( ", " );
	 PRINTER::print( getclosurecode(closure) );
      }
   }
}

void ERROR::print_active_frame()
{
   printf( "active frame\n" );

   SEXPR env = EVAL::env;

   if ( nullp(env) )
   {
      printf( "  ()\n" );
      return;
   }

   for ( int i = 0; anyp(env); ++i )
   {
      printf( "  level %d ", i );
      print_frame( env );
      printf( "\n" );
      env = getenvbase(env);
   }
}

void ERROR::print_stacktrace()
{
   const int top = regstack.gettop();
   int n = 0;

   printf( "stacktrace (depth=%d)\n", top+1 );

   for ( int i = top; i >= 0; --i )
   {
      SEXPR item = regstack[i];

      printf( "  depth %d ", top-i );
      PRINTER::print( item );
      printf( "\n" );

      if ( envp(item) )
      {
	 printf( "  frame %d ", n++ );
	 print_frame( item );
	 printf( "\n" );
      }
   }
}
