#include <cstdio>
#include <cstdlib>

#include "error.hxx"
#include "printer.hxx"
#include "eval.hxx"
#include "symtab.hxx"


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
   SEXPR evs = EVAL::get_evaluator_state();
   PRINTER::print( vectorref( evs, 0 ) );
   printf("\n");
   PRINTER::print( vectorref( evs, 1 ) );
   printf("\n");
   PRINTER::print( vectorref( evs, 2 ) );
   printf("\n");
#endif
#if 0
   EVAL::show_ece_history();
#endif
   throw Severe;
}

void ERROR::fatal( const char* s )
{
   printf("fatal error: %s\n", s);
   
   throw Fatal;
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

