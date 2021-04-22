#include "func.hxx"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <unistd.h>

#include <algorithm>
#include <string>

#include "sexpr.hxx"
#include "argstack.hxx"
#include "regstack.hxx"
#include "error.hxx"
#include "symtab.hxx"
#include "printer.hxx"
#include "reader.hxx"
#include "memory.hxx"
#include "pio.hxx"
#include "tio.hxx"
#include "transcript.hxx"

#include "eval/eval.hxx"

namespace escheme
{

//
// Attention! 
//
//   Use the 'regstack' for storing intermediate values
//   from the garbage collector.  DO NOT USE argstack!
//

bool booleanp(const SEXPR s) { return s == symbol_true || s == symbol_false; }
bool notp(const SEXPR s) { return falsep(s); }
bool boundp(const SEXPR s) { return getvalue(guard(s, symbolp)) != symbol_unbound; }
bool eof_objectp(const SEXPR s) { return s == PIO::eof_object; }
bool zerop(const SEXPR s) { return getfixnum(guard(s, fixnump)) == 0; }
bool positivep(const SEXPR s) { return getfixnum(guard(s, fixnump)) > 0; }
bool negativep(const SEXPR s) { return getfixnum(guard(s, fixnump)) < 0; }
bool oddp(const SEXPR s) { return (abs(getfixnum(guard(s, fixnump))) % 2) == 1; }
bool evenp(const SEXPR s) { return (getfixnum(guard(s, fixnump)) % 2) == 0; }
bool exactp(const SEXPR) { return false; }
bool inexactp(const SEXPR) { return true; }
bool string_nullp(const SEXPR s) { return getstringlength(guard(s, stringp)) == 0; }
bool procedurep(const SEXPR s) { return primp(s) || closurep(s); }

SEXPR FUNC::predicate( PREDICATE pred )
{
   ArgstackIterator iter;
   auto arg = iter.getlast();
   return pred(arg) ? symbol_true : symbol_false;
}

SEXPR FUNC::cxr( const char* x )
{
   ArgstackIterator iter;
   auto exp = iter.getlast();
   for ( int i = 0; x[i]; ++i )
      exp = (x[i] == 'a') ? car(exp) : cdr(exp);
   return exp;
}

//
// general
//

SEXPR FUNC::exit()
{
   //
   // syntax: (exit)
   //
   argstack.noargs();
   return ERROR::exit();
}

//
// list
//

SEXPR FUNC::cons()
{
   //
   // syntax: (cons <car> <cdr>)
   //
   ArgstackIterator iter;
   auto car = iter.getarg();
   auto cdr = iter.getlast();
   return MEMORY::cons(car, cdr);
}

SEXPR FUNC::car()
{
   //
   // syntax: (car <cons>)
   //
   ArgstackIterator iter;
   return car(iter.getlast());
}

SEXPR FUNC::cdr()
{
   //
   // syntax: (cdr <cons>)
   //
   ArgstackIterator iter;
   return cdr(iter.getlast());
}

SEXPR FUNC::set_car()
{
   //
   // syntax: (set-car! <cons> <newcar>) -> <cons>
   //
   ArgstackIterator iter;
   auto cons = guard( iter.getarg(), consp );
   auto newcar = iter.getlast();
   setcar( cons, newcar );
   return cons;
}

SEXPR FUNC::set_cdr()
{
   //
   // syntax: (set-cdr! <cons> <newcdr>) -> <cons>
   //
   ArgstackIterator iter;
   auto cons = guard( iter.getarg(), consp );
   auto newcdr = iter.getlast();
   setcdr( cons, newcdr );
   return cons;
}

SEXPR FUNC::length()
{
   //
   // syntax: (length <list>)
   //
   ArgstackIterator iter;
   auto n = guard(iter.getlast(), listp);
   return MEMORY::fixnum(list_length(n));
}

SEXPR FUNC::list()
{
   //
   // syntax: (list {<sexpr>}*)
   //
   ArgstackIterator iter;
   ListBuilder list;

   while ( iter.more() )
   {
      list.add( iter.getarg() );
   }

   return list.get();
}

SEXPR FUNC::liststar()
{
   //
   // syntax: (list* {<sexpr>}*)
   //
   const int argc = argstack.getargc();

   if ( argc == 0 )
   {
      return null;
   }
   else
   {
      ArgstackIterator iter;
      if  ( argc == 1 )
	 return iter.getlast();

      auto cell = MEMORY::cons(iter.getarg(), null);
      regstack.push(cell);

      while ( true )
      {
	 auto next = iter.getarg();
	 if ( !iter.more() )
	 {
	    setcdr( cell, next );
	    return regstack.pop();
	 }
	 else
	 {
	    setcdr( cell, MEMORY::cons(next, null) );
	    cell = getcdr(cell);
	 }
      }
      return regstack.pop();
   }
}

//
// byte vector
//

SEXPR FUNC::bvector()
{
   //
   // syntax: (byte-vector <e1> <e2> ...)
   //
   ArgstackIterator iter;
   const int n = argstack.getargc();
   auto v = MEMORY::byte_vector(n);

   for ( int i = 0; i < n; ++i )
   {
      auto byte = guard(iter.getarg(), fixnump);
      bvecset( v, i, static_cast<BYTE>( getfixnum(byte) ) );
   }

   return v;
}

SEXPR FUNC::make_bvector()
{
   //
   // syntax: (make-byte-vector <size>)
   //
   ArgstackIterator iter;
   const int size = getfixnum(guard(iter.getlast(), fixnump));

   if ( size < 0 )
      ERROR::severe("byte-vector size must be non-negative");

   return MEMORY::byte_vector(size);
}

SEXPR FUNC::bvector_ref()
{
   //
   // syntax: (byte-vector-ref <vector> <index>)
   //
   ArgstackIterator iter;
   auto bv      = guard(iter.getarg(), bvecp);
   auto bvindex = guard(iter.getlast(), fixnump);
   const int index = getfixnum(bvindex);

   if ( index < 0 || index >= getbveclength(bv) )
      ERROR::severe("index out of range");

   return MEMORY::fixnum(bvecref(bv, index));
}

SEXPR FUNC::bvector_set()
{
   //
   // syntax: (byte-vector-set! <vector> <index> <value>)
   //
   ArgstackIterator iter;
   auto bv      = guard(iter.getarg(), bvecp);
   auto bvindex = guard(iter.getarg(), fixnump);
   auto bvalue  = guard(iter.getlast(), fixnump);
   const int index = getfixnum(bvindex);

   if ( index < 0 || index >= getbveclength(bv) )
      ERROR::severe("index out of range");

   bvecset( bv, index, getfixnum(bvalue) );
   return bvalue;
}

SEXPR FUNC::bvector_length()
{
   //
   // syntax: (byte-vector-length <byte-vector>)
   //
   ArgstackIterator iter;
   auto v = guard(iter.getlast(), bvecp);
   return MEMORY::fixnum(getbveclength(v));
}

//
// vector
//

SEXPR FUNC::vector()
{
   //
   // syntax: (vector <e1> <e2> ...)
   //
   ArgstackIterator iter;
   const int n = argstack.getargc();
   auto v = MEMORY::vector(n);

   for ( int i = 0; i < n; ++i )
      vectorset( v, i, iter.getarg() );

   return v;
}

SEXPR FUNC::make_vector()
{
   //
   // syntax: (make-vector <size>)
   //
   ArgstackIterator iter;
   const int size = getfixnum(guard(iter.getlast(), fixnump));

   if ( size < 0 )
      ERROR::severe("vector size must be non-negative");

   return MEMORY::vector(size);
}

SEXPR FUNC::vector_length()
{
   //
   // syntax: (vector-length <vector>)
   //
   ArgstackIterator iter;
   auto v = guard(iter.getlast(), vectorp);
   return MEMORY::fixnum(getvectorlength(v));
}

SEXPR FUNC::vector_ref()
{
   //
   // syntax: (vector-ref <vector> <index>)
   //
   ArgstackIterator iter;
   auto v      = guard(iter.getarg(), vectorp);
   auto vindex = guard(iter.getlast(), fixnump);
   const int index = getfixnum(vindex);

   if ( index < 0 || index >= getvectorlength(v) )
      ERROR::severe("index out of range");

   return vectorref( v, index );
}

SEXPR FUNC::vector_set()
{
   //
   // syntax: (vector-set! <vector> <index> <value>)
   //
   ArgstackIterator iter;
   auto v      = guard(iter.getarg(), vectorp);
   auto vindex = guard(iter.getarg(), fixnump);
   auto x      = iter.getlast();
   const int index = getfixnum(vindex);

   if ( index < 0 || index >= getvectorlength(v) )
      ERROR::severe("index out of range");

   vectorset( v, index, x );
   return x;
}

SEXPR FUNC::vector_fill()
{
   //
   // syntax: (vector-fill! <vector> <value>) -> <vector>
   //
   ArgstackIterator iter;
   auto v = guard(iter.getarg(), vectorp);
   auto x = iter.getlast();
   const int vlen = getvectorlength(v);

   for ( int i = 0; i < vlen; ++i )
      vectorset( v, i, x );

   return v;
}

SEXPR FUNC::vector_copy()
{
   // *
   // syntax: (vector-copy! <dest> <dest-start> <src> [<src-start> <src-end>]) -> <dest>
   //
   ArgstackIterator iter;
   auto dst   = guard(iter.getarg(), vectorp);
   auto dst_s = getfixnum(guard(iter.getarg(), fixnump));
   auto src   = guard(iter.getarg(), vectorp);

   if ( dst_s >= getvectorlength(dst) )
      ERROR::severe( "dst-start > dst length" );

   int src_s = 0;
   int src_e = getvectorlength(src);

   if ( iter.more() )
   {
      src_s = getfixnum(guard(iter.getarg(), fixnump));
      src_e = getfixnum(guard(iter.getlast(), fixnump));

      if ( src_s >= getvectorlength(src) )
	 ERROR::severe( "src-start >= src length" );
      if ( src_e > getvectorlength(src) )
	 ERROR::severe( "src-end > src length" );  
      if ( src_s >= src_e )
	 ERROR::severe( "src-start >= src-end" );
   }

   if ( dst_s + (src_e - src_s) > getvectorlength(dst) )
      ERROR::severe( "dest not large enough for src" );
   
   int i = dst_s;
   for ( int j = src_s; j < src_e; ++j, ++i )
      vectorset( dst, i, vectorref( src, j ) );

   return dst;
}

SEXPR FUNC::list_to_vector()
{
   //
   // syntax: (list->vector <list>) -> <vector>
   //
   ArgstackIterator iter;
   auto list     = guard(iter.getlast(), listp);
   const int len = list_length(list);
   auto v        = MEMORY::vector(len);

   for ( int i = 0; i < len; ++i, list = cdr(list) )
      vectorset(v, i, car(list));

   return v;
}

SEXPR FUNC::vector_to_list()
{
   //
   // syntax: (vector->list <vector>) -> <list>
   //
   ArgstackIterator iter;
   auto v = guard(iter.getlast(), vectorp);
   const int len = getvectorlength(v);

   regstack.push(null);

   for ( int i = len-1; i >= 0; --i )
      regstack.top() = MEMORY::cons( vectorref(v, i), regstack.top() );

   return regstack.pop();
}

//
// Equality precidates: eq?, eqv?, equal?
//

bool FUNC::eq( SEXPR e1, SEXPR e2 )
{
   return e1 == e2;
}

bool FUNC::eqv( SEXPR e1, SEXPR e2 )
{
   if ( eq(e1, e2) )
      return true;

   if ( anyp(e1) )
   {
      if ( fixnump(e1) )
      {
	 return fixnump(e2) && getfixnum(e1) == getfixnum(e2);
      }
      else if ( flonump(e1) )
      {
	 return flonump(e2) && getflonum(e1) == getflonum(e2);
      }
      else if ( charp(e1) )
      {
	 return charp(e2) && getcharacter(e1) == getcharacter(e2);
      }
      else if ( stringp(e1) )
      {
	 return stringp(e2) && ::strcmp(getstringdata(e1), getstringdata(e2)) == 0;
      }
   }

   return false;
}

bool FUNC::equal( SEXPR e1, SEXPR e2 )
{
   if ( eqv(e1, e2) )
      return true;

   if ( anyp(e1) )
   {
      if ( vectorp(e1) )
      {
	 if ( vectorp(e2) )
	 { 
	    const int vlen = getvectorlength(e1);

	    if ( vlen != getvectorlength(e2) )
	       return false;

	    for ( int i = 0; i < vlen; ++i )
	       if ( !equal(vectorref(e1, i), vectorref(e2, i)) )
		  return false;

	    return true;
	 }
      }
      else if ( consp(e1) )
      {
	 return consp(e2) && equal(car(e1), car(e2)) && equal(cdr(e1), cdr(e2));
      }
   }

   return false;
}

//
// equality
//

SEXPR FUNC::eq()
{
   //
   // syntax: (eq? <exp1> <exp2>)
   //
   ArgstackIterator iter;
   auto e1 = iter.getarg();
   auto e2 = iter.getlast();
   return  (e1 == e2) ? symbol_true : symbol_false;
}

SEXPR FUNC::eqv()
{
   //
   // syntax: (eqv? <exp1> <exp2>)
   //
   ArgstackIterator iter;
   auto e1 = iter.getarg();
   auto e2 = iter.getlast();
   return eqv(e1, e2) ? symbol_true : symbol_false;
}

SEXPR FUNC::equal()
{
   //
   // syntax: (equal? <exp1> <exp2>)
   //
   ArgstackIterator iter;
   auto e1 = iter.getarg();
   auto e2 = iter.getlast();
   return equal(e1, e2) ? symbol_true : symbol_false;
}

//
// symbol
//

SEXPR FUNC::string_to_symbol()
{
   //
   // syntax: (string->symbol <str>) -> interned symbol
   //
   ArgstackIterator iter;
   auto s = guard(iter.getlast(), stringp);
   return SYMTAB::enter(getstringdata(s));
}

SEXPR FUNC::symbol_to_string()
{
   //
   // syntax: (symbol->string <sym>)
   //
   ArgstackIterator iter;
   auto s = guard(iter.getlast(), symbolp);
   return MEMORY::string(getname(s));
}

static unsigned gensym_number = 0;

SEXPR FUNC::gensym()
{
   //
   // syntax: (gensym [<sym>|<str>|<fix>]) -> uninterned symbol
   //
   ArgstackIterator iter;
   
   std::string new_sym = "g";

   if ( iter.more() )
   {
      const auto arg = iter.getlast();

      if ( symbolp(arg) )
      {
         new_sym = getname(arg);
      }
      else if ( stringp(arg) )
      {
	 new_sym = getstringdata(arg);
      }
      else if ( fixnump(arg) )
      {
	 gensym_number = static_cast<unsigned>(getfixnum(arg));
      }
      else
	 ERROR::severe("gensym requires [sym|str|fix]");
   }

   char number[80];
   SPRINTF( number, "%u", gensym_number++ );
   new_sym += number;
  
   return MEMORY::symbol( new_sym );
}

SEXPR FUNC::symbol_value()
{
   //
   // syntax: (symbol-value <sym-expr>)
   //
   ArgstackIterator iter;
   auto s = guard(iter.getlast(), symbolp);
   return getvalue(s);
}

SEXPR FUNC::set_symbol_value()
{
   //
   // syntax: (set-symbol-value! <sym-expr> <value>)
   //
   ArgstackIterator iter;
   auto s = guard(iter.getarg(), symbolp);
   auto v = iter.getlast();
   setvalue(s, v);
   return v;
}

SEXPR FUNC::symbol_plist()
{
   //
   // syntax: (symbol-plist <sym-expr>)
   //
   ArgstackIterator iter;
   auto s = guard(iter.getlast(), symbolp);
   return getplist(s);
}

SEXPR FUNC::set_symbol_plist()
{
   //
   // syntax: (set-symbol-plist! <sym-expr> <plist>)
   //
   ArgstackIterator iter;
   auto s = guard(iter.getarg(), symbolp);
   auto p = iter.getlast();
   setplist(s, p);
   return p;
}

SEXPR FUNC::get_property()
{
   //
   // syntax: (get <sym> <prop>)
   //
   ArgstackIterator iter;
   auto s = guard(iter.getarg(), symbolp);
   auto p = guard(iter.getlast(), symbolp);
   auto plist = getplist(s);

   while ( anyp(plist) )
   {
      if ( eq( p, car(plist) ) )
	 return car(cdr(plist));
      plist = cdr(cdr(plist));
   }
  
   return null;
}

SEXPR FUNC::put_property()
{
   //
   // syntax: (put <sym> <prop> <value>)
   //
   ArgstackIterator iter;
   auto s = guard(iter.getarg(), symbolp);
   auto p = guard(iter.getarg(), symbolp);
   auto v = iter.getlast();
   auto plist = getplist(s);

   while ( anyp(plist) )
   {
      if ( eq( p, car(plist) ) )
      {
	 setcar( guard(cdr(plist), consp), v );
	 return p;
      }
      plist = cdr(cdr(plist));
   }
  
   // if we got here, then there is no such property
   regstack.push( MEMORY::cons(v, getplist(s)) );  // protect
   setplist( s, MEMORY::cons(p, regstack.top()) );
   regstack.pop();

   return p;
}

SEXPR FUNC::symbols()
{
   //
   // syntax: (symbols) -> <vector>
   //
   argstack.noargs();
   return SYMTAB::symbols();
}


//
// input/output
//

SEXPR FUNC::read()
{
   //
   // syntax: (read [<inport>])
   //
   ArgstackIterator iter;
   if ( iter.more() )
   { 
      auto port = guard(iter.getlast(), anyinportp);
      return READER::read(port); 
   }
   else
      return READER::read();
}

//
// syntax: (print <sexpr> [<outport>]) -> #t
// syntax: (write <sexpr> [<outport>]) -> #t
//

static SEXPR basic_print( int newline )
{
   ArgstackIterator iter;
   auto s = iter.getarg();
   auto port = iter.more() ? guard(iter.getlast(), anyoutportp) : PIO::stdout_port;
   
   PRINTER::print( port, s );
   if ( newline )
      PRINTER::newline(port);
   return symbol_true;
}

SEXPR FUNC::print() { return basic_print(1); }
SEXPR FUNC::write() { return basic_print(0); }

SEXPR FUNC::display()
{
   //
   // syntax: (display <sexpr> [<outport>]) -> #t
   //
   ArgstackIterator iter;
   auto s = iter.getarg();
   auto port = iter.more() ? guard(iter.getlast(), anyoutportp) : PIO::stdout_port;

   PRINTER::print( port, s, PRINTER::NO_QUOTE );
   return symbol_true;
}

SEXPR FUNC::newline()
{
   //
   // syntax: (newline [<outport>]) -> #t
   //
   ArgstackIterator iter;
   auto port = iter.more() ? guard(iter.getlast(), anyoutportp) : PIO::stdout_port;

   PRINTER::newline( port );
   return symbol_true;
}

SEXPR FUNC::read_char()
{
   //
   // syntax: (read-char [<inport>])
   //
   ArgstackIterator iter;
   auto port = (iter.more()) ? guard(iter.getlast(), anyinportp) : PIO::stdin_port;

   const char ch = PIO::get(port);
   return (ch == EOF) ? PIO::eof_object : MEMORY::character(ch);
}

SEXPR FUNC::write_char()
{
   //
   // syntax: (write-char <sexpr> [<outport>]) -> #t
   //
   ArgstackIterator iter;
   const char ch = getcharacter(guard(iter.getarg(), charp));
   auto port = (iter.more()) ? guard(iter.getlast(), anyinportp) : PIO::stdout_port;

   PIO::put( port, ch );
   return symbol_true;
}

//
// memory management
//

static SEXPR gc_stats()
{
   // nodes stats
#ifdef GC_STATISTICS_DETAILED
   regstack.push( MEMORY::vector(4) );
#else
   regstack.push( MEMORY::vector(3) );
#endif
   
   vectorset( regstack.top(), 0, MEMORY::fixnum( MEMORY::CollectionCount ) );
   vectorset( regstack.top(), 1, MEMORY::fixnum( MEMORY::TotalNodeCount ) );
   vectorset( regstack.top(), 2, MEMORY::fixnum( MEMORY::FreeNodeCount ) );
   
#ifdef GC_STATISTICS_DETAILED
   const int N = MEMORY::ReclamationCounts.size();
   regstack.push( MEMORY::vector(N) );
   for ( int i = 0; i < N; ++i )
      vectorset( regstack.top(), i, MEMORY::fixnum( MEMORY::ReclamationCounts[i]) );
   auto reclamations = regstack.pop();
   vectorset( regstack.top(), 3, reclamations );
#endif
   
   return regstack.pop();
}

SEXPR FUNC::gc()
{
   //
   // syntax: (gc) -> <statistics>
   //
   argstack.noargs();
   MEMORY::gc();
   return gc_stats();
}

SEXPR FUNC::mm()
{
   //
   // syntax: (mm) -> <statistics>
   //
   argstack.noargs();
   return gc_stats();
}

SEXPR FUNC::fs()
{
   //
   // syntax: (fs) -> <framestore-statistics>
   //
   argstack.noargs();

   const int N = MEMORY::frameStore.count.size();
   regstack.push( MEMORY::vector(N) );

   for ( int i = 0; i < N; ++i )
      vectorset( regstack.top(), i, MEMORY::fixnum( MEMORY::frameStore.count[i] ) );
   
   return regstack.pop();
}

//
// environment
//

SEXPR FUNC::the_environment()
{
   //
   // syntax: (the-environment)
   //
   argstack.noargs();
   return EVAL::the_environment();
}

SEXPR FUNC::proc_environment()
{
   //
   // syntax: (procedure-environment <closure>)
   //
   ArgstackIterator iter;
   const SEXPR closure = guard(iter.getlast(), closurep);
   return getclosurebenv(closure);
}

SEXPR FUNC::env_parent()
{
   //
   // syntax: (environment-parent <env>)
   //
   ArgstackIterator iter;
   auto env = guard(iter.getlast(), envp);
   return getenvbase(env);
}

SEXPR FUNC::env_bindings()
{
   //
   // syntax: (environment-bindings <env>) -> (<pair1> <pair2> ...)
   //
   ArgstackIterator iter;
   const auto arg = iter.getlast();

   // treat the empty list of bindings as a null env
   if ( nullp(arg) )
      return null;

   const auto env = guard(arg, envp);

   // convert a frame into a list of bindings
   const auto frame = getenvframe(env);
   auto vars = getframevars(frame);
   
   ListBuilder bindings;

   for ( int i = 0; anyp(vars); ++i )
   {
      regstack.push( MEMORY::cons( getcar(vars), frameref(frame, i)) );
      bindings.add( regstack.top() );
      regstack.pop();
      vars = getcdr(vars);
   }
   
   return bindings.get();
}

SEXPR FUNC::make_environment()
{
   //
   // syntax: (%make-environment <pairs> <baseenv>)
   //
   ArgstackIterator iter;
   auto pairs = guard(iter.getarg(), listp);
   const auto benv = iter.getlast();

   if ( !(nullp(benv) || envp(benv)) )
      ERROR::severe( "expected a base environment", benv );
  
   // convert a list of bindings into a frame
   const int len = list_length(pairs);

   // if empty, extend base environment w/ empty frame

   const auto env = MEMORY::environment( len, null, benv );
   regstack.push( env );
   {
      ListBuilder vars;
      
      for ( int i = 0; anyp(pairs); ++i )
      {
         auto x = car(pairs);
         
         if ( consp(x) )
         {
            // ( <var> . <val> )
            vars.add( car(x) );
            frameset( getenvframe(env), i, cdr(x) );
         }
         else if ( symbolp(x) )
         {
            // <var>
            vars.add( x );
            frameset( getenvframe(env), i, null );
         }
         else
         {
            ERROR::severe( "expected a symbol or (symbol . val)", x );
         }
         pairs = cdr(pairs);
      }
      setframevars( getenvframe(env), vars.get() );
   }
   return regstack.pop();
}

//
// closure
//

SEXPR FUNC::make_closure()
{
   //
   // syntax: (%make-closure <code> <params> <env>)
   //
   ArgstackIterator iter;
   auto code = guard(iter.getarg(), listp);
   auto params = guard(iter.getarg(), listp);
   const auto env = iter.getlast();

   if ( !(nullp(env) || envp(env)) )
      ERROR::severe( "expected an environment", env );

   const auto closure = MEMORY::closure( code, env );
   regstack.push( closure );
   EVAL::parse_formals( params, 
                        getclosurevars(closure),
                        getclosurenumv(closure),
                        getclosurerargs(closure) );
   return regstack.pop();
}

SEXPR FUNC::parse_formals()
{
   //
   // syntax: (%parse-formals <params>)
   //
   ArgstackIterator iter;
   auto params = iter.getlast();

   auto v = MEMORY::vector( 3 );
   regstack.push( v );

   SEXPR vars;
   INT32 numv;
   bool rargs;
   EVAL::parse_formals( params, vars, numv, rargs );

   vset( v, 0, vars );
   vset( v, 1, MEMORY::fixnum(numv) );
   vset( v, 2, rargs ? symbol_true : symbol_false );
   
   return regstack.pop();
}

#ifdef BYTE_CODE_EVALUATOR

SEXPR FUNC::make_code()
{
   //
   // syntax: (%make-code <bvec> <vec>)
   //
   ArgstackIterator iter;
   auto bcodes = guard(iter.getarg(), bvecp);
   auto sexprs = guard(iter.getlast(), vectorp);
   return MEMORY::code( bcodes, sexprs );
}

SEXPR FUNC::get_bcodes()
{
   //
   // syntax: (%get-bcodes <code>)
   //
   ArgstackIterator iter;
   auto code = guard(iter.getlast(), codep);
   return code_getbcodes(code);
}

SEXPR FUNC::get_sexprs()
{
   //
   // syntax: (%get-sexprs <code>)
   //
   ArgstackIterator iter;
   auto code = guard(iter.getlast(), codep);
   return code_getsexprs(code);
}

#endif

//
// unix
//

int unix_argc;
char** unix_argv;

SEXPR FUNC::unix_system()
{
   //
   // syntax: (system <string>) -> <fixnum>
   //
   ArgstackIterator iter;
   auto cmd = guard(iter.getlast(), stringp);
   return MEMORY::fixnum( system(getstringdata(cmd)) );
}

SEXPR FUNC::unix_getargs()
{
   //
   // syntax: (getargs) -> #(<string0> <string1> ... <stringc-1>)
   //
   argstack.noargs();

   const auto es_argv = MEMORY::vector(unix_argc);
   regstack.push(es_argv);
   for ( int i = 0; i < unix_argc; ++i )
      vset( es_argv, i, MEMORY::string(unix_argv[i]) );
   return regstack.pop();
}

SEXPR FUNC::unix_getenv()
{
   //
   // syntax: (getenv <var>) -> <string-value>
   //
   ArgstackIterator iter;
   auto var = guard(iter.getlast(), stringp);
   auto val = ::getenv( getstringdata(var) );
   return ( val == nullptr ) ? MEMORY::string_null : MEMORY::string(val);
}

SEXPR FUNC::unix_setenv()
{
   //
   // syntax: (setenv <var> <val> <replace>) -> <fixnum>
   //
   ArgstackIterator iter;
   auto var = guard(iter.getarg(), stringp);
   auto val = guard(iter.getarg(), stringp);
   int replace = 1;
   if ( iter.more() )
      replace = getfixnum( guard(iter.getlast(), fixnump) );

   const int result = ::setenv( getstringdata(var),
                                getstringdata(val),
                                replace );
   return MEMORY::fixnum( result );
}

SEXPR FUNC::unix_unsetenv()
{
   //
   // syntax: (unsetenv <var>) -> <fixnum>
   //
   ArgstackIterator iter;
   auto var = guard(iter.getlast(), stringp);
   const int result = ::unsetenv( getstringdata(var) );
   return MEMORY::fixnum( result );
}

SEXPR FUNC::unix_gettime()
{
   //
   // syntax: (gettime) -> (<seconds> . <nanoseconds>)
   //
   argstack.noargs();

   auto time = MEMORY::cons(null, null);
   regstack.push( time );

   struct timespec ts;

   clock_gettime( CLOCK_MONOTONIC, &ts );
   setcar( time, MEMORY::fixnum( ts.tv_sec ) );
   setcdr( time, MEMORY::fixnum( ts.tv_nsec ) );

   return regstack.pop();
}

SEXPR FUNC::unix_change_dir()
{
   //
   // syntax: (chdir <string>) -> <fixnum>
   //
   ArgstackIterator iter;
   auto path = guard(iter.getlast(), stringp);
   const int result = ::chdir( getstringdata(path) );
   return MEMORY::fixnum((FIXNUM)result);
}

SEXPR FUNC::unix_current_dir()
{
   //
   // syntax: (getcwd) -> <string>
   //
   argstack.noargs();
   char buffer[200];
   const auto result = ::getcwd( buffer, sizeof(buffer) );
   if ( result == NULL )
      return null;
   else
      return MEMORY::string( buffer );
}

//
// port
//

SEXPR FUNC::open_input_file()
{
   //
   // syntax: (open-input-file <str>)
   //
   ArgstackIterator iter;
   auto fname = guard(iter.getlast(), stringp);
   return PIO::open( fname, pm_input, "r" );
}

SEXPR FUNC::open_output_file()
{
   //
   // syntax: (open-output-file <str>)
   //
   ArgstackIterator iter;
   const SEXPR fname = guard(iter.getlast(), stringp);
   return PIO::open( fname, pm_output, "w" );
}

SEXPR FUNC::open_append_file()
{
   //
   // syntax: (open-append-file <str>)
   //
   ArgstackIterator iter;
   auto fname = guard(iter.getlast(), stringp);
   return PIO::open( fname, pm_output, "a" );
}

SEXPR FUNC::open_update_file()
{
   //
   // syntax: (open-update-file <str>)
   //
   ArgstackIterator iter;
   auto fname = guard(iter.getlast(), stringp);
   return PIO::open( fname, pm_input|pm_output, "r+" );
}

SEXPR FUNC::close_port()
{
   //
   // syntax: (close-port <port>) -> #t
   //
   ArgstackIterator iter;
   auto port = guard(iter.getlast(), portp);
   PIO::close( port );
   return symbol_true;
}

SEXPR FUNC::close_input_port()
{
   //
   // syntax: (close-input-port <port>) -> #t
   //
   ArgstackIterator iter;
   auto port = guard(iter.getlast(), inportp);
   PIO::close( port );
   return symbol_true;
}

SEXPR FUNC::close_output_port()
{
   //
   // syntax: (close-output-port <port>) -> #t
   //
   ArgstackIterator iter;
   auto port = guard(iter.getlast(), outportp);
   PIO::close( port );
   return symbol_true;
}

SEXPR FUNC::set_file_position()
{
   //
   // syntax: (set-file-position <port> <pos>) -> #t
   //
   ArgstackIterator iter;
   auto port = guard(iter.getarg(), portp);
   auto pos = guard(iter.getlast(), fixnump);
   PIO::set_position( port, pos );
   return symbol_true;
}

SEXPR FUNC::get_file_position()
{
   //
   // syntax: (get-file-position <port>)
   //
   ArgstackIterator iter;
   auto port = guard(iter.getlast(), portp);
   return PIO::get_position( port );
}

SEXPR FUNC::flush_output_port()
{
   //
   // syntax: (flush-output [<outport>]) -> #t
   //
   ArgstackIterator iter;
   if ( iter.more() )
   {
      auto port = guard(iter.getlast(), outportp);
      PIO::flush( port );
   }
   else
      PIO::flush( PIO::stdout_port );
   return symbol_true;
}

//
// string port
//

SEXPR FUNC::open_input_string()
{
   //
   // syntax: (open-input-string <str>)
   //
   ArgstackIterator iter;
   auto str = guard(iter.getlast(), stringp);
   return PIO::open_on_string( str, pm_input );
}

SEXPR FUNC::open_output_string()
{
   //
   // syntax: (open-output-string)
   //
   argstack.noargs();
   return PIO::open_on_string( MEMORY::string_null, pm_output );
}

SEXPR FUNC::get_output_string()
{
   //
   // syntax: (get-output-string <stringport>)
   //
   ArgstackIterator iter;
   auto port = guard(iter.getlast(), stringportp);
   auto str = getstringportstring(port);
   return MEMORY::string( *str );
}

//
// strings
//

SEXPR FUNC::string_length()
{
   ArgstackIterator iter;
   auto s = guard(iter.getlast(), stringp);
   return MEMORY::fixnum(getstringlength(s));
}

SEXPR FUNC::string_append()
{
   //
   // syntax: (string-append <s1> <s2> ... <sn>) -> <string>
   //
   ArgstackIterator iter;
   std::string ss;

   while ( iter.more() )
   {
      auto s = guard(iter.getarg(), stringp);
      ss.append( getstringdata(s) );
   }

   return MEMORY::string( ss );
}

SEXPR FUNC::string_ref()
{
   //
   // syntax: (string-ref <s> <index>) -> <char>
   //
   ArgstackIterator iter;
   auto s = guard(iter.getarg(), stringp);
   const int n = getfixnum(guard(iter.getlast(), fixnump));

   if ( n < 0 || n >= getstringlength(s) )
      ERROR::severe("index out of string bounds");
      
   return MEMORY::character(getstringdata(s)[n]);
}

SEXPR FUNC::string_set()
{
   //
   // syntax: (string-set! <s> <index> <ch>) -> <string>
   //
   ArgstackIterator iter;
   SEXPR s = guard(iter.getarg(), stringp);
   const int n = getfixnum(guard(iter.getarg(), fixnump));
   const int ch = getcharacter(guard(iter.getlast(), charp));

   if ( n < 0 || n >= getstringlength(s) )
      ERROR::severe("index out of string bounds");
      
   getstringdata(s)[n] = ch;
   return s;
}

SEXPR FUNC::substring()
{
   //
   // syntax: (substring <s> <start> <end>) -> <string>
   //
   ArgstackIterator iter;
   auto s = guard(iter.getarg(), stringp);
   const int start = getfixnum(guard(iter.getarg(), fixnump));
   const int end   = getfixnum(guard(iter.getlast(), fixnump));

   if ( !(start < static_cast<int>(getstringlength(s)) &&
          end <= static_cast<int>(getstringlength(s))) )
      ERROR::severe("index out of string bounds");

   if ( start >= end )
      return MEMORY::string_null;
   
   const int slen = end - start;
   std::string ss( &getstringdata(s)[start], slen );
   return MEMORY::string( ss );
}

SEXPR FUNC::string_fill()
{
   //
   // syntax: (string-fill! <s> <ch>) -> <string>
   //
   ArgstackIterator iter;
   auto s = guard(iter.getarg(), stringp);
   const char ch = getcharacter(guard(iter.getlast(), charp));

   for ( int i = 0; i < getstringlength(s); ++i )
      getstringdata(s)[i] = ch;
   return s;
}

SEXPR FUNC::string_copy()
{
   // *
   // syntax: (string-copy! <dest> <dest-start> <src> [<src-start> <src-end>]) -> <dest>
   //
   //   <dest> == s_dst
   //   <src>  == s_src
   //
   ArgstackIterator iter;
   const SEXPR dst   = guard(iter.getarg(), stringp);
   const int   dst_s = getfixnum(guard(iter.getarg(), fixnump));
   const SEXPR src   = guard(iter.getarg(), stringp);

   if ( dst_s >= getstringlength(dst) )
      ERROR::severe( "string dst-start > dst length" );

   int src_s = 0;
   int src_e = getstringlength(src);

   if ( iter.more() )
   {
      src_s = getfixnum(guard(iter.getarg(), fixnump));
      src_e = getfixnum(guard(iter.getlast(), fixnump));

      if ( src_s >= getstringlength(src) )
	 ERROR::severe( "string src-start >= src length" );
      if ( src_e > getstringlength(src) )
	 ERROR::severe( "string src-end > src length" );  
      if ( src_s >= src_e )
	 ERROR::severe( "string src-start >= src-end" );
   }

   if ( dst_s + (src_e - src_s) > getstringlength(dst) )
      ERROR::severe( "string dest not large enough for src" );
   
   int i = dst_s;
   for ( int j = src_s; j < src_e; ++j, ++i )
      getstringdata(dst)[i] = getstringdata(src)[j];

   return dst;
}

SEXPR FUNC::list_to_string()
{
   //
   // syntax: (list->string <list-of-chars>) -> <string>
   //
   ArgstackIterator iter;
   auto list = guard(iter.getarg(), listp);
   const int len = list_length(list);

   std::string s;
   
   for ( int i = 0; i < len; ++i, list = cdr(list) )
      s.push_back( getcharacter(guard(car(list), charp)) );

   return MEMORY::string( s );
}

SEXPR FUNC::string_to_list()
{
   //
   // syntax: (string->list <string>) -> <list-of-chars>
   //
   ArgstackIterator iter;
   auto s = guard(iter.getlast(), stringp);
   const int len = getstringlength(s);

   regstack.push(null);
   const int p = regstack.gettop();

   for ( int i = len-1; i >= 0; --i )
   {
      regstack.push( MEMORY::character(getstringdata(s)[i]) );
      regstack[p] = MEMORY::cons( regstack.top(), regstack[p] );
      regstack.pop();
   }

   return regstack.pop();
}

enum RelOp { EQop, LTop, LEop, GTop, GEop };

using StrCmpFuncType = int (*)( const char*, const char* );

static SEXPR string_compare( RelOp op, StrCmpFuncType compare )
{
   ArgstackIterator iter;
   const auto s1 = getstringdata(guard(iter.getarg(), stringp));
   const auto s2 = getstringdata(guard(iter.getlast(), stringp));

   switch (op)
   {
      case EQop: return (compare(s1, s2) == 0) ? symbol_true : symbol_false;
      case LTop: return (compare(s1, s2) <  0) ? symbol_true : symbol_false;
      case LEop: return (compare(s1, s2) <= 0) ? symbol_true : symbol_false;
      case GTop: return (compare(s1, s2) >  0) ? symbol_true : symbol_false;
      case GEop: return (compare(s1, s2) >= 0) ? symbol_true : symbol_false;
      default:
	 return ERROR::fatal("bad string comparison operator");
   }
}

SEXPR FUNC::string_EQ() { return string_compare(EQop, ::strcmp); }
SEXPR FUNC::string_LT() { return string_compare(LTop, ::strcmp); }
SEXPR FUNC::string_LE() { return string_compare(LEop, ::strcmp); }
SEXPR FUNC::string_GT() { return string_compare(GTop, ::strcmp); }
SEXPR FUNC::string_GE() { return string_compare(GEop, ::strcmp); }
SEXPR FUNC::string_EQci() { return string_compare(EQop, ::strcasecmp); }
SEXPR FUNC::string_LTci() { return string_compare(LTop, ::strcasecmp); }
SEXPR FUNC::string_LEci() { return string_compare(LEop, ::strcasecmp); }
SEXPR FUNC::string_GTci() { return string_compare(GTop, ::strcasecmp); }
SEXPR FUNC::string_GEci() { return string_compare(GEop, ::strcasecmp); }

//
// chars
//

static SEXPR char_compare( RelOp op, int ci )
{
   ArgstackIterator iter;
   char c1 = getcharacter(guard(iter.getarg(), charp));
   char c2 = getcharacter(guard(iter.getlast(), charp));

   if ( ci )
   {
      c1 = toupper(c1);
      c2 = toupper(c2);
   }

   switch ( op )
   {
      case EQop: return (c1 == c2) ? symbol_true : symbol_false;
      case LTop: return (c1 <  c2) ? symbol_true : symbol_false;
      case LEop: return (c1 <= c2) ? symbol_true : symbol_false;
      case GTop: return (c1 >  c2) ? symbol_true : symbol_false;
      case GEop: return (c1 >= c2) ? symbol_true : symbol_false;
      default:
	 return ERROR::fatal("bad character comparison operator");
   }
}

SEXPR FUNC::char_EQ() { return char_compare(EQop, 0); }
SEXPR FUNC::char_LT() { return char_compare(LTop, 0); }
SEXPR FUNC::char_LE() { return char_compare(LEop, 0); }
SEXPR FUNC::char_GT() { return char_compare(GTop, 0); }
SEXPR FUNC::char_GE() { return char_compare(GEop, 0); }
SEXPR FUNC::char_EQci() { return char_compare(EQop, 1); }
SEXPR FUNC::char_LTci() { return char_compare(LTop, 1); }
SEXPR FUNC::char_LEci() { return char_compare(LEop, 1); }
SEXPR FUNC::char_GTci() { return char_compare(GTop, 1); }
SEXPR FUNC::char_GEci() { return char_compare(GEop, 1); }

//
// other conversion
//

SEXPR FUNC::integer_to_string()
{
   //
   // syntax: (integer->string <fixnum>) -> <string>
   //
   ArgstackIterator iter;
   auto s = guard(iter.getlast(), fixnump);

   char buf[MAX_IMAGE_LENGTH];
   SPRINTF( buf, "%ld", getfixnum(s) );
  
   return MEMORY::string( buf );
}

SEXPR FUNC::string_to_integer()
{
   //
   // syntax: (string->integer <string>) -> <fixnum>
   //
   ArgstackIterator iter;
   auto s = guard(iter.getlast(), stringp);
   return MEMORY::fixnum( atol(getstringdata(s)) );
}

//
// Member Functions
//

//  (define (%member-search eqtest? expr list)
//    (while (and list (not (eqtest? expr (car list))))
//      (set! list (cdr list)))
//    list)
//
//  ;; find an element in a list: (e1 e2 ...)
//  (define (member expr list) (%member-search equal? expr list))
//  (define (memv expr list) (%member-search eqv? expr list))
//  (define (memq expr list) (%member-search eq? expr list))
//

static SEXPR member_search( bool(*eqtest)(SEXPR, SEXPR), SEXPR exp, SEXPR list )
{
   while ( anyp(list) && !eqtest( exp, car(list) ) )
      list = cdr(list);
   return list;
}

SEXPR FUNC::member()
{
   //
   // syntax: (member <exp> <list>) -> <exp> or null
   //
   ArgstackIterator iter;
   auto exp = iter.getarg();
   auto list = guard(iter.getlast(), listp);
   return member_search( equal, exp, list );
}

SEXPR FUNC::memv()
{
   //
   // syntax: (memv <exp> <list>) -> <exp> or null
   //
   ArgstackIterator iter;
   auto exp = iter.getarg();
   auto list = guard(iter.getlast(), listp);
   return member_search( eqv, exp, list );
}

SEXPR FUNC::memq()
{
   //
   // syntax: (memq <exp> <list>) -> <exp> or null
   //
   ArgstackIterator iter;
   auto exp = iter.getarg();
   auto list = guard(iter.getlast(), listp);
   return member_search( eq, exp, list );
}

//
// Assoc Functions
//
//  (define (%assoc-search eqtest? expr list)
//    (while (and list
//              (let ((x (car list)))
//	           (if (pair? x) (not (eqtest? expr (car x))) #t)))
//      (set! list (cdr list)))
//    (car list))
//
//  ;; find an element in a association list ((e1 . v1) (e2 . v2) ...)
//  (define (assoc expr alist) (%assoc-search equal? expr alist))
//  (define (assv expr alist) (%assoc-search eqv? expr alist))
//  (define (assq expr alist) (%assoc-search eq? expr alist))
//

static SEXPR assoc_search( bool(*eqtest)(SEXPR, SEXPR), SEXPR exp, SEXPR list )
{
   while ( anyp(list) )
   {
      auto x = car(list);
      if ( consp(x) && eqtest( exp, car(x) ) )
	 return x;
      list = cdr(list);
   }
   return null;
}

SEXPR FUNC::assoc()
{
   //
   // syntax: (assoc <exp> <alist>) -> (<exp> . <value>) or null
   //
   ArgstackIterator iter;
   auto exp = iter.getarg();
   auto list = guard(iter.getlast(), listp);
   return assoc_search( equal, exp, list );
}

SEXPR FUNC::assv()
{
   //
   // syntax: (assv <exp> <alist>) -> (<exp> . <value>) or null
   //
   ArgstackIterator iter;
   auto exp = iter.getarg();
   auto list = guard(iter.getlast(), listp);
   return assoc_search( eqv, exp, list );
}

SEXPR FUNC::assq()
{
   //
   // syntax: (assq <exp> <alist>) -> (<exp> . <value>) or null
   //
   ArgstackIterator iter;
   auto exp = iter.getarg();
   auto list = guard(iter.getlast(), listp);
   return assoc_search( eq, exp, list );
}


//
//  Append
//
//  (define (append . z)
//    (let ((a (cons '() '())))
//      (let ((x a) 
// 	       b 
//	      (c z))
//        (while c
//	    (set! b (car c))
//	    (set! c (cdr c))
//	    (while b
//	      (set-cdr! a (cons (car b) '()))
//	      (set! a (cdr a))
//	      (set! b (cdr b))))
//        (cdr x))))
//

SEXPR FUNC::append()
{
   //
   // syntax: (append <list1> <list2> ... <listn>) -> <list>
   //
   ArgstackIterator iter;
   ListBuilder list;

   while ( iter.more() )
   {
      auto b = guard(iter.getarg(), listp);

      while ( anyp(b) )
      {
	 list.add( car(b) );
	 b = cdr(b);
      }
   }
   return list.get();
}

//
// Reverse
//
//  (define (reverse list)
//    ((lambda (x) 
//      (while list 
//        (set! x (cons (car list) x)) 
//        (set! list (cdr list))) 
//      x) 
//    nil))
//

SEXPR FUNC::reverse()
{
   //
   // syntax: (reverse <list>) -> <list>
   //
   ArgstackIterator iter;
   auto list = guard(iter.getlast(), listp);

   // protect the structure under construction
   regstack.push(null);
   while ( anyp(list) )
   {
      regstack.top() = MEMORY::cons( car(list), regstack.top() );
      list = cdr(list);
   }
   return regstack.pop();
}

//
// Last pair
//
//  (define (last-pair list)
//    (while (pair? (cdr list))
//      (set! list (cdr list)))
//    list)
//

SEXPR FUNC::last_pair()
{
   //
   // syntax: (last-pair <list>) -> <pair> or null
   //
   ArgstackIterator iter;
   auto list = guard(iter.getlast(), listp);

   while ( consp(cdr(list)) )
      list = cdr(list);
   return list;
}

//
// List tail
//
//  (define (list-tail list n)
//    (if (< n 0)
//        (error "index out of range" n))
//    ((lambda (i) 
//       (while (and list (> i 0)) 
//         (set! i (- i 1)) 
//         (set! list (cdr list))) 
//     n)
//    list)
//

SEXPR FUNC::list_tail()
{
   //
   // syntax: (list-tail <list> <n>) -> <list> or null
   //
   ArgstackIterator iter;
   auto list = guard(iter.getarg(), listp);
   int n = getfixnum(guard(iter.getlast(), fixnump));
  
   if ( n < 0 )
      ERROR::severe("index out of range", list, MEMORY::fixnum(n));

   while ( anyp(list) && n > 0 )
   {
      n -= 1;
      list = cdr(list);
   }
   return list;
}

//
// Closures
//

SEXPR FUNC::closure_code()
{
   //
   // syntax: (%closure-code <closure>) -> <list>
   //
   ArgstackIterator iter;
   auto closure = guard(iter.getlast(), closurep);
   return getclosurecode(closure);
}

#ifdef BYTE_CODE_EVALUATOR

SEXPR FUNC::closure_code_set()
{
   //
   // syntax: (%closure-code-set! <closure> <code>) -> <closure>
   //
   ArgstackIterator iter;
   auto closure = guard(iter.getarg(), closurep);
   auto code = guard(iter.getlast(), codep);
   setclosurecode( closure, code );
   return closure;
}

#endif

SEXPR FUNC::closure_benv()
{
   //
   // syntax: (%closure-benv <closure>) -> <env>
   //
   ArgstackIterator iter;
   auto closure = guard(iter.getlast(), closurep);
   return getclosurebenv(closure);
}

SEXPR FUNC::closure_vars()
{
   //
   // syntax: (%closure-vars <closure>) -> <list>
   //
   ArgstackIterator iter;
   auto closure = guard(iter.getlast(), closurep);
   return getclosurevars(closure);
}

SEXPR FUNC::closure_numv()
{
   //
   // syntax: (%closure-numv <closure>) -> <fixnum>
   //
   ArgstackIterator iter;
   auto closure = guard(iter.getlast(), closurep);
   return MEMORY::fixnum(getclosurenumv(closure));
}

SEXPR FUNC::closure_rest()
{
   //
   // syntax: (%closure-rest <closure>) -> <boolean>
   //
   ArgstackIterator iter;
   auto closure = guard(iter.getlast(), closurep);
   return getclosurerargs(closure) ? symbol_true : symbol_false;
}


// transcript

SEXPR FUNC::transcript_on()
{
   ArgstackIterator iter;
   auto fname = guard(iter.getlast(), stringp);
   TRANSCRIPT::on( fname );
   return symbol_true;
}

SEXPR FUNC::transcript_off()
{
   argstack.noargs();
   TRANSCRIPT::off();
   return symbol_true;
}

// history

SEXPR FUNC::history_add()
{
   //
   // syntax: (history-add <sexpr>) -> #t
   //
   ArgstackIterator iter;
   auto sexpr = iter.getlast();
   TIO::history_add( sexpr );
   return symbol_true;
}

SEXPR FUNC::history_clear()
{
   //
   // syntax: (history-clear) -> #t
   //
   argstack.noargs();
   TIO::history_clear();
   return symbol_true;
}

SEXPR FUNC::history_show()
{
   //
   // syntax: (history-show) -> #t
   //
   argstack.noargs();
   TIO::history_show();
   return symbol_true;
}

SEXPR FUNC::set_prompt()
{
   //
   // syntax: (set-prompt <string>) -> #t
   //
   ArgstackIterator iter;
   auto str = guard(iter.getlast(), stringp);
   TIO::set_prompt( getstringdata(str) ); 
   return symbol_true;
}

//
// char functions
//

SEXPR FUNC::char_alphabeticp()
{
   //
   // syntax: (char-alphabetic? <char>) -> <boolean>
   //
   ArgstackIterator iter;
   auto ch = guard(iter.getlast(), charp);
   return isalpha(getcharacter(ch)) ? symbol_true : symbol_false;
}

SEXPR FUNC::char_numericp()
{
   //
   // syntax: (char-numeric? <char>) -> <boolean>
   //
   ArgstackIterator iter;
   auto ch = guard(iter.getlast(), charp);
   return isdigit(getcharacter(ch)) ? symbol_true : symbol_false;
}

SEXPR FUNC::char_whitespacep()
{
   //
   // syntax: (char-whitespace? <char>) -> <boolean>
   //
   ArgstackIterator iter;
   auto ch = guard(iter.getlast(), charp);
   return isspace(getcharacter(ch)) ? symbol_true : symbol_false;
}

SEXPR FUNC::char_upper_casep()
{
   //
   // syntax: (char-upper-case? <char>) -> <boolean>
   //
   ArgstackIterator iter;
   const SEXPR ch = guard(iter.getlast(), charp);
   return isupper(getcharacter(ch)) ? symbol_true : symbol_false;
}

SEXPR FUNC::char_lower_casep()
{
   //
   // syntax: (char-lower-case? <char>) -> <boolean>
   //
   ArgstackIterator iter;
   auto ch = guard(iter.getlast(), charp);
   return islower(getcharacter(ch)) ? symbol_true : symbol_false;
}

SEXPR FUNC::char_upcase()
{
   //
   // syntax: (char-upcase <char>) -> <char>
   //
   ArgstackIterator iter;
   auto ch = guard(iter.getlast(), charp);
   return MEMORY::character(toupper(getcharacter(ch)));
}

SEXPR FUNC::char_downcase()
{
   //
   // syntax: (char-downcase <char>) -> <char>
   //
   ArgstackIterator iter;
   auto ch = guard(iter.getlast(), charp);
   return MEMORY::character(tolower(getcharacter(ch)));
}

SEXPR FUNC::char_to_integer()
{
   //
   // syntax: (char->integer <char>) -> <fixnum>
   //
   ArgstackIterator iter;
   const SEXPR ch = guard(iter.getlast(), charp);
   return MEMORY::fixnum( static_cast<FIXNUM>(getcharacter(ch)) );
}

SEXPR FUNC::integer_to_char()
{
   //
   // syntax: (integer->char <integer>) -> <char>
   //
   ArgstackIterator iter;
   auto num = guard(iter.getlast(), fixnump);
   return MEMORY::character( static_cast<CHAR>(getfixnum(num)) );
}

SEXPR FUNC::objaddr()
{
   //
   // syntax: (%object-address <object>) -> <fixnum>
   //
   ArgstackIterator iter;
   auto obj = iter.getlast();
   return MEMORY::fixnum( reinterpret_cast<FIXNUM>(obj) );
}

//
// string
//

SEXPR FUNC::find()
{
   ArgstackIterator iter;
   auto s1 = guard(iter.getarg(), stringp);
   auto ss = guard(iter.getlast(), stringp);

   auto pos = ::strstr( getstringdata(s1), getstringdata(ss) );

   if ( pos == NULL )
   {
      return null;
   }
   else
   {
      auto offset = pos - getstringdata(s1);
      return MEMORY::fixnum( reinterpret_cast<FIXNUM>(offset) );
   }
}

   
}
