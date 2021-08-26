#include "symtab.hxx"

#include <array>
#include <string.h>

#include "memory.hxx"
#include "regstack.hxx"
#include "hash.hxx"

namespace escheme
{

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

static std::array<SEXPR, 128> symtab;

SEXPR SYMTAB::enter( const std::string& symbol_name )
{
   auto& element = symtab[ hash(symbol_name) % symtab.size() ];

   if ( anyp(element) )
   {
      for ( auto n = element; anyp(n); n = getcdr(n) )
      {
	 auto s = getcar(n);
	 if ( symbol_name == name(s) )
	    return s;
      }
   }

   regstack.push( MEMORY::symbol(symbol_name) );
   
   setvalue( regstack.top(), symbol_unbound );
   element = MEMORY::cons( regstack.top(), element );
   
   return regstack.pop();
}

SEXPR SYMTAB::enter( const std::string& symbol_name, SEXPR value )
{
   auto s = SYMTAB::enter(symbol_name);
   setvalue( s, value );
   return s;
}

SEXPR SYMTAB::all_symbols()
{
   regstack.push( null );
   
   for ( auto i = 0; i < symtab.size(); ++i )
      for ( auto n = symtab[i]; anyp(n); n = getcdr(n) )
	 regstack.top() = MEMORY::cons( getcar(n), regstack.top() );
   
   return regstack.pop();
}

static void symtab_marker()
{
   for ( auto element : symtab )
      MEMORY::mark( element );
}

void SYMTAB::initialize()
{
   for ( auto& element : symtab )
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
