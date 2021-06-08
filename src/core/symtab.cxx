#include "symtab.hxx"

#include <array>
#include <unordered_map>

#include "memory.hxx"

namespace escheme
{

std::unordered_map<std::string, SEXPR> table;

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


SEXPR SYMTAB::enter( const std::string& name, SEXPR value )
{
   auto s = SYMTAB::enter( name );
   setvalue(s, value);
   return s;
}

SEXPR SYMTAB::enter( const std::string& symbol_name )
{
   auto pair = table.find( symbol_name );
   if ( pair == table.end() )
   {
      using PAIR = std::pair<std::string, SEXPR>;
      // item not found
      auto sym = MEMORY::symbol( symbol_name );
      setvalue( sym, symbol_unbound );
      table.insert( PAIR( symbol_name, sym ) );
      return sym;
   }
   else
   {
      // item found
      return pair->second;
   }
}

SEXPR SYMTAB::enter( const char* name )
{
   return SYMTAB::enter( std::string(name) );
}
   
SEXPR SYMTAB::enter( const char* name, SEXPR value )
{
   return SYMTAB::enter( std::string(name), value );
}
   
SEXPR SYMTAB::all_symbols()
{
   auto v = MEMORY::vector( table.size() );
   int i = 0;
   for ( auto& element : table )
      vectorset( v, i++, element.second ); 
   return v;
}

static void symtab_marker()
{
   for ( auto& element : table )
      MEMORY::mark( element.second );
}

void SYMTAB::initialize()
{
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
