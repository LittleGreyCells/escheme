#include <string.h>

#include "symtab.hxx"
#include "memory.hxx"
#include "regstack.hxx"

const int NBUCKETS = 32;

// the global symbol table (vector)
SEXPR SYMTAB::table;

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
SEXPR SYMTAB::symbol_cset;
SEXPR SYMTAB::symbol_lambda;
SEXPR SYMTAB::symbol_clambda;
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

SEXPR SYMTAB::enter( const char* name, SEXPR value )
{
   SEXPR s = SYMTAB::enter(name);
   setvalue(s, value);
   return s;
}

unsigned SYMTAB::hash( const char* s )
{
   UINT32 i = 0;

   while ( *s )
      i = (i << 2) ^ *s++;
   return i % NBUCKETS;
}

// create and intern a new symbol using the name provided
SEXPR SYMTAB::enter( const char* symbol_name )
{
   const UINT32 h = hash(symbol_name);

   if (vectorref(table, h))
   {
      for (SEXPR n = vectorref(table, h); anyp(n); n = getcdr(n))
      {
	 SEXPR s = getcar(n);
	 if (strcmp(name(s), symbol_name) == 0)
	    return s;
      }
   }

   regstack.push(MEMORY::symbol(symbol_name));
   setvalue( regstack.top(), UNBOUND );
   vectorset(table, h, MEMORY::cons(regstack.top(), vectorref(table, h)));
   return regstack.pop();
}

// intern the symbol using the symbol provided
//   (why assume it is not already interned?)
SEXPR SYMTAB::enter( SEXPR s )
{
   if (!symbolp(s))
      return null;

   const UINT32 h = hash(name(s));
   vectorset(table, h, MEMORY::cons(s, vectorref(table, h)));
   return s;
}

static void symtab_marker()
{
   // mark the symbol table objects
   MEMORY::mark(SYMTAB::table);
}

void SYMTAB::initialize()
{
   table = MEMORY::vector(NBUCKETS);

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
   symbol_cset    	 = enter("cset!");
   symbol_lambda 	 = enter("lambda");
   symbol_clambda 	 = enter("clambda");
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
   enter("*symbol-table*", table);

   MEMORY::register_marker( symtab_marker );
}
