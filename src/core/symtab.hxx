#ifndef symtab_hxx
#define symtab_hxx

#include "sexpr.hxx"

#define QUOTE  		SYMTAB::symbol_quote
#define DELAY  		SYMTAB::symbol_delay
#define DEFINE 		SYMTAB::symbol_define
#define SET    		SYMTAB::symbol_set
#define CSET    	SYMTAB::symbol_cset
#define LAMBDA 		SYMTAB::symbol_lambda
#define CLAMBDA 	SYMTAB::symbol_clambda
#define IF     		SYMTAB::symbol_if
#define WHILE  		SYMTAB::symbol_while
#define COND   		SYMTAB::symbol_cond
#define ELSE   		SYMTAB::symbol_else
#define AND    		SYMTAB::symbol_and
#define OR     		SYMTAB::symbol_or
#define BEGIN  		SYMTAB::symbol_begin
#define SEQUENCE        SYMTAB::symbol_sequence
#define LET   		SYMTAB::symbol_let
#define LETREC   	SYMTAB::symbol_letrec
#define ACCESS          SYMTAB::symbol_access
#define UNBOUND         SYMTAB::symbol_unbound
#define OPTIONAL        SYMTAB::symbol_optional
#define REST            SYMTAB::symbol_rest
#define DEFAULT         SYMTAB::symbol_default
#define QUASIQUOTE      SYMTAB::symbol_quasiquote
#define UNQUOTE         SYMTAB::symbol_unquote
#define UNQUOTESPLICING SYMTAB::symbol_unquotesplicing

namespace SYMTAB
{
   void initialize();

   extern SEXPR table;

   extern SEXPR symbol_false;
   extern SEXPR symbol_true;
   extern SEXPR symbol_quote;
   extern SEXPR symbol_delay;
   extern SEXPR symbol_define;
   extern SEXPR symbol_set;
   extern SEXPR symbol_cset;
   extern SEXPR symbol_lambda;
   extern SEXPR symbol_clambda;
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
   extern SEXPR symbol_letstar;
   extern SEXPR symbol_access;
   extern SEXPR symbol_unbound;
   extern SEXPR symbol_optional;
   extern SEXPR symbol_rest;
   extern SEXPR symbol_default;
   extern SEXPR symbol_quasiquote;
   extern SEXPR symbol_unquote;
   extern SEXPR symbol_unquotesplicing;

   SEXPR enter( const char* name );
   SEXPR enter( const char* name, SEXPR value );
   SEXPR enter( SEXPR symbol );

   // private
   unsigned hash( const char* s );
}

using SYMTAB::symbol_true;
using SYMTAB::symbol_false;

inline bool falsep( SEXPR n ) { return n == symbol_false || n == null; }
inline bool truep( SEXPR n )  { return !falsep(n); }


#endif
