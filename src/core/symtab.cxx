#include "symtab.hxx"

#include <array>
#include <string.h>

#include "memory.hxx"
#include "regstack.hxx"


namespace
{
   constexpr int NBUCKETS = 64;
   std::array<SEXPR, NBUCKETS> table;
}

// canonical truth and other symbols
SEXPR SYMTAB::symbol_unbound;
SEXPR SYMTAB::symbol_false;
SEXPR SYMTAB::symbol_true;
SEXPR SYMTAB::symbol_default;
SEXPR SYMTAB::symbol_quasiquote;
SEXPR SYMTAB::symbol_unquote;
SEXPR SYMTAB::symbol_unquotesplicing;

// special forms
SEXPR SYMTAB::symbol_quote;
SEXPR SYMTAB::symbol_delay;
SEXPR SYMTAB::symbol_define;
SEXPR SYMTAB::symbol_set;
SEXPR SYMTAB::symbol_lambda;
SEXPR SYMTAB::symbol_if;
SEXPR SYMTAB::symbol_while;
SEXPR SYMTAB::symbol_cond;
SEXPR SYMTAB::symbol_else;
SEXPR SYMTAB::symbol_and;
SEXPR SYMTAB::symbol_or;
SEXPR SYMTAB::symbol_begin;
SEXPR SYMTAB::symbol_sequence;
SEXPR SYMTAB::symbol_let;
SEXPR SYMTAB::symbol_letrec;
SEXPR SYMTAB::symbol_access;

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
   setvalue( regstack.top(), UNBOUND );
   element = MEMORY::cons( regstack.top(), element );
   return regstack.pop();
}

SEXPR SYMTAB::enter( const std::string& name, SEXPR value )
{
   SEXPR s = SYMTAB::enter(name);
   setvalue(s, value);
   return s;
}

SEXPR SYMTAB::enter( const std::string& symbol_name )
{
   const char* sn = symbol_name.c_str();
   auto& element = table[ hash(sn) ];

   if ( anyp(element) )
   {
      for ( SEXPR n = element; anyp(n); n = getcdr(n) )
      {
	 SEXPR s = getcar(n);
	 if ( strcmp(name(s), sn) == 0 )
	    return s;
      }
   }

   regstack.push( MEMORY::symbol(symbol_name) );
   setvalue( regstack.top(), UNBOUND );
   element = MEMORY::cons( regstack.top(), element );
   return regstack.pop();
}

SEXPR SYMTAB::enter( SEXPR s )
{
   if ( !symbolp(s) )
      return null;

   auto& element = table[ hash(name(s)) ];
   element = MEMORY::cons( s, element );
   return s;
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

   MEMORY::register_marker( symtab_marker );

   // initialize the unbound symbol first
   symbol_unbound  	 = enter("*unbound*");
   setvalue(symbol_unbound, symbol_unbound);

   symbol_false  	 = enter("#f");
   symbol_true   	 = enter("#t"); 
   setvalue(symbol_true, symbol_true);
   setvalue(symbol_false, symbol_false);

   // create other special symbols/objects
   symbol_default         = enter("**default**");
   setvalue(symbol_default, symbol_default);
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
}
