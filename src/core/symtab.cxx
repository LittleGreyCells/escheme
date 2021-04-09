#include "symtab.hxx"

#include <array>
#include <string.h>

#include "memory.hxx"
#include "regstack.hxx"

namespace escheme
{

namespace
{
   constexpr int NBUCKETS = 64;
   std::array<SEXPR, NBUCKETS> table;
}

// canonical truth and other symbols
SEXPR symbol_unbound;
SEXPR symbol_false;
SEXPR symbol_true;
SEXPR symbol_quasiquote;
SEXPR symbol_unquote;
SEXPR symbol_unquotesplicing;

// special forms
SEXPR symbol_quote;
SEXPR symbol_delay;
SEXPR symbol_define;
SEXPR symbol_set;
SEXPR symbol_lambda;
SEXPR symbol_if;
SEXPR symbol_while;
SEXPR symbol_cond;
SEXPR symbol_else;
SEXPR symbol_and;
SEXPR symbol_or;
SEXPR symbol_begin;
SEXPR symbol_sequence;
SEXPR symbol_let;
SEXPR symbol_letrec;
SEXPR symbol_access;

static unsigned hash( const char* s )
{
   unsigned i = 0;
   while ( *s )
      i = (i << 2) ^ *s++;
   return i % NBUCKETS;
}

SEXPR SYMTAB::enter( const char* name, SEXPR value )
{
   SEXPR s = SYMTAB::enter(name);
   setvalue(s, value);
   return s;
}

SEXPR SYMTAB::enter( const char* symbol_name )
{
   auto& element = table[ hash(symbol_name) ];

   if ( anyp(element) )
   {
      for ( SEXPR n = element; anyp(n); n = getcdr(n) )
      {
	 SEXPR s = getcar(n);
	 if ( strcmp(name(s), symbol_name) == 0 )
	    return s;
      }
   }

   regstack.push( MEMORY::symbol(symbol_name) );
   setvalue( regstack.top(), symbol_unbound );
   element = MEMORY::cons( regstack.top(), element );
   return regstack.pop();
}

SEXPR SYMTAB::enter( const std::string& name, SEXPR value )
{
   return enter( name.c_str(), value );
}

SEXPR SYMTAB::enter( const std::string& symbol_name )
{
   return enter( symbol_name.c_str() );
}

SEXPR SYMTAB::symbols()
{
   SEXPR symbols = MEMORY::vector(NBUCKETS);

   for ( int i = 0; i < table.size(); ++i )
      vectorset( symbols, i, table[i] );
   
   return symbols;
}

static void symtab_marker()
{
   for ( auto element : table )
      MEMORY::mark( element );
}

void SYMTAB::initialize()
{
   for ( auto& element : table )
      element = null;

   // initialize the unbound symbol first
   symbol_unbound  	 = enter("*unbound*");
   setvalue(symbol_unbound, symbol_unbound);

   symbol_false  	 = enter("#f");
   symbol_true   	 = enter("#t"); 
   setvalue(symbol_true, symbol_true);
   setvalue(symbol_false, symbol_false);

   // create other special symbols/objects
   symbol_quasiquote      = enter("quasiquote");
   symbol_unquote         = enter("unquote");
   symbol_unquotesplicing = enter("unquote-splicing");

   // special forms
   symbol_quote  	 = enter("quote");
   symbol_delay  	 = enter("delay");
   symbol_define 	 = enter("define");
   symbol_set    	 = enter("set!");
   symbol_lambda 	 = enter("lambda");
   symbol_if     	 = enter("if");
   symbol_while     	 = enter("while");
   symbol_cond   	 = enter("cond");
   symbol_else   	 = enter("else");
   symbol_and    	 = enter("and");
   symbol_or     	 = enter("or");
   symbol_begin  	 = enter("begin");
   symbol_sequence  	 = enter("sequence");
   symbol_let		 = enter("let");
   symbol_letrec   	 = enter("letrec");
   symbol_access  	 = enter("access");

   enter("t", symbol_true);
   enter("nil", null);
   
   MEMORY::register_marker( symtab_marker );
}

}
