#ifndef symtab_hxx
#define symtab_hxx

#include <string>

#include "sexpr.hxx"

namespace SYMTAB
{
   void initialize();

   extern SEXPR symbol_false;
   extern SEXPR symbol_true;
   extern SEXPR symbol_quote;
   extern SEXPR symbol_delay;
   extern SEXPR symbol_define;
   extern SEXPR symbol_set;
   extern SEXPR symbol_lambda;
   extern SEXPR symbol_if;
   extern SEXPR symbol_while;
   extern SEXPR symbol_cond;
   extern SEXPR symbol_else;
   extern SEXPR symbol_and;
   extern SEXPR symbol_or;
   extern SEXPR symbol_begin;
   extern SEXPR symbol_sequence;
   extern SEXPR symbol_let;
   extern SEXPR symbol_letrec;
   extern SEXPR symbol_access;
   extern SEXPR symbol_unbound;
   extern SEXPR symbol_default;
   extern SEXPR symbol_quasiquote;
   extern SEXPR symbol_unquote;
   extern SEXPR symbol_unquotesplicing;

   SEXPR enter( const char* name );
   SEXPR enter( const char* name, SEXPR value );
   
   SEXPR enter( SEXPR symbol );
   
   SEXPR enter( const std::string& name );
   SEXPR enter( const std::string& name, SEXPR value );

   SEXPR symbols();
}

using SYMTAB::symbol_true;
using SYMTAB::symbol_false;

inline bool falsep( SEXPR n ) { return n == symbol_false || n == null; }
inline bool truep( SEXPR n )  { return !falsep(n); }

#endif
