#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <unistd.h>
#include <algorithm>

#include "sexpr.hxx"
#include "func.hxx"
#include "eval.hxx"
#include "argstack.hxx"
#include "regstack.hxx"
#include "error.hxx"
#include "symtab.hxx"
#include "printer.hxx"
#include "reader.hxx"
#include "memory.hxx"
#include "pio.hxx"
#include "tio.hxx"

//
// Attention! 
//
//   Use the 'regstack' for storing intermediate values
//   from the garbage collector.  DO NOT USE argstack!
//

#define push_reg(s) regstack.push(s)
#define pop_reg() regstack.pop()
#define top_reg() regstack.top()

#define PRED_IMP(name)					\
   SEXPR PRED_FUN(name)()				\
   {							\
      ArgstackIterator iter;				\
      const SEXPR arg = iter.getlast();			\
      return name(arg) ? symbol_true : symbol_false;	\
   }

bool booleanp(const SEXPR s) { return s == symbol_true || s == symbol_false; }

inline bool notp(const SEXPR s) { return falsep(s); }
inline bool boundp(const SEXPR s) { return getvalue(guard(s, symbolp)) != UNBOUND; }
inline bool eof_objectp(const SEXPR s) { return s == PIO::eof_object; }
inline bool default_objectp(const SEXPR s) { return s == DEFAULT; }
inline bool zerop(const SEXPR s) { return getfixnum(guard(s, fixnump)) == 0; }
inline bool positivep(const SEXPR s) { return getfixnum(guard(s, fixnump)) > 0; }
inline bool negativep(const SEXPR s) { return getfixnum(guard(s, fixnump)) < 0; }
inline bool oddp(const SEXPR s) { return (abs(getfixnum(guard(s, fixnump))) % 2) == 1; }
inline bool evenp(const SEXPR s) { return (getfixnum(guard(s, fixnump)) % 2) == 0; }
inline bool exactp(const SEXPR) { return false; }
inline bool inexactp(const SEXPR) { return true; }

//
// general
//

SEXPR FUNC::exit()
{
   // *
   // syntax: (exit)
   //
   argstack.noargs();
   throw ERROR::Exit;
}

//
// list
//

SEXPR FUNC::cons()
{
   // *
   // syntax: (cons <car> <cdr>)
   //
   ArgstackIterator iter;
   const SEXPR car = iter.getarg();
   const SEXPR cdr = iter.getlast();
   return MEMORY::cons(car, cdr);
}

SEXPR FUNC::car()
{
   //
   // syntax: (car <cons>)
   //
   ArgstackIterator iter;
   return ::car(iter.getlast());
}

SEXPR FUNC::cdr()
{
   // *
   // syntax: (cdr <cons>)
   //
   ArgstackIterator iter;
   return ::cdr(iter.getlast());
}

SEXPR FUNC::set_car()
{
   // *
   // syntax: (set-car! <cons> <newcar>) -> <cons>
   //
   ArgstackIterator iter;
   const SEXPR cons = iter.getarg();
   const SEXPR newcar = iter.getlast();
   rplaca(cons, newcar);
   return cons;
}

SEXPR FUNC::set_cdr()
{
   // *
   // syntax: (set-cdr! <cons> <newcdr>) -> <cons>
   //
   ArgstackIterator iter;
   const SEXPR cons = iter.getarg();
   const SEXPR newcdr = iter.getlast();
   rplacd(cons, newcdr);
   return cons;
}

SEXPR FUNC::length()
{
   // *
   // syntax: (length <list>)
   //
   ArgstackIterator iter;
   const SEXPR n = guard(iter.getlast(), listp);
   return MEMORY::fixnum(list_length(n));
}

SEXPR FUNC::list()
{
   // *
   // syntax: (list {<sexpr>}*)
   //
   ArgstackIterator iter;

#if 0

   if (!iter.more())
   {
      return null;  // empty list
   }
   else
   {
      SEXPR cell = MEMORY::cons(null, null);
      push_reg(cell);

      while (true)
      {
	 setcar(cell, iter.getarg());
	 if (!iter.more())
	    break;
	 SEXPR newcell = MEMORY::cons(null, null);
	 setcdr(cell, newcell);
	 cell = newcell;
      }

      return pop_reg();
   }

#else

   SEXPR a = EVAL::listbuilder;
   setcdr(a, null);

   while ( iter.more() )
   {
      setcdr( a, MEMORY::cons( iter.getarg(), null ) );
      a = getcdr( a );
   }

   return getcdr( EVAL::listbuilder );

#endif
}

SEXPR FUNC::liststar()
{
   //
   // syntax: (list* {<sexpr>}*)
   //
   const int argc = argstack.getargc();

   if (argc == 0)
   {
      return null;
   }
   else
   {
      ArgstackIterator iter;
      if (argc == 1)
	 return iter.getlast();

      SEXPR cell = MEMORY::cons(iter.getarg(), null);
      push_reg(cell);

      while (true)
      {
	 SEXPR next = iter.getarg();
	 if (!iter.more())
	 {
	    //rplacd(cell, next);
	    setcdr(cell, next);
	    return pop_reg();
	 }
	 else
	 {
	    //rplacd(cell, MEMORY::cons(next, null));
	    //cell = ::cdr(cell);
	    setcdr(cell, MEMORY::cons(next, null));
	    cell = getcdr(cell);
	 }
      }
      return pop_reg();
   }
}

// *
// cxr
//

static SEXPR cxr( const char* x, const int n )
{
   ArgstackIterator iter;
   SEXPR exp = iter.getlast();

   for (int i = n; i > 0; --i)
      exp = (x[i] == 'a') ? car(exp) : cdr(exp);

   return exp;
}

#define FNCXR(name, len) SEXPR FUNC::name() { return cxr(#name, len); }

FNCXR(caar, 2)
FNCXR(cadr, 2)
FNCXR(cdar, 2)
FNCXR(cddr, 2)

FNCXR(caaar, 3)
FNCXR(caadr, 3)
FNCXR(cadar, 3)
FNCXR(caddr, 3)
FNCXR(cdaar, 3)
FNCXR(cdadr, 3)
FNCXR(cddar, 3)
FNCXR(cdddr, 3)

FNCXR(caaaar, 4)
FNCXR(caaadr, 4)
FNCXR(caadar, 4)
FNCXR(caaddr, 4)
FNCXR(cadaar, 4)
FNCXR(cadadr, 4)
FNCXR(caddar, 4)
FNCXR(cadddr, 4)
FNCXR(cdaaar, 4)
FNCXR(cdaadr, 4)
FNCXR(cdadar, 4)
FNCXR(cdaddr, 4)
FNCXR(cddaar, 4)
FNCXR(cddadr, 4)
FNCXR(cdddar, 4)
FNCXR(cddddr, 4)

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
   SEXPR v = MEMORY::byte_vector(n);

   for (int i = 0; i < n; ++i)
   {
      const SEXPR byte = guard(iter.getarg(), fixnump);
      bvecset(v, i, static_cast<BYTE>( getfixnum(byte) ));
   }

   return v;
}

SEXPR FUNC::make_bvector()
{
   // *
   // syntax: (make-byte-vector <size>)
   //
   ArgstackIterator iter;
   const int size = getfixnum(guard(iter.getlast(), fixnump));

   if (size < 0)
      ERROR::severe("byte-vector size must be non-negative");

   return MEMORY::byte_vector(size);
}

SEXPR FUNC::bvector_ref()
{
   // *
   // syntax: (byte-vector-ref <vector> <index>)
   //
   ArgstackIterator iter;
   const SEXPR bv      = guard(iter.getarg(), bvecp);
   const SEXPR bvindex = guard(iter.getlast(), fixnump);
   const int   index   = getfixnum(bvindex);

   if (index < 0 || index >= static_cast<int>(getbveclength(bv)))
      ERROR::severe("index out of range");

   return MEMORY::fixnum(bvecref(bv, index));
}

SEXPR FUNC::bvector_set()
{
   // *
   // syntax: (byte-vector-set! <vector> <index> <value>)
   //
   ArgstackIterator iter;
   const SEXPR bv      = guard(iter.getarg(), bvecp);
   const SEXPR bvindex = guard(iter.getarg(), fixnump);
   const SEXPR bvalue  = guard(iter.getlast(), fixnump);
   const int   index   = getfixnum(bvindex);

   if (index < 0 || index >= static_cast<int>(getbveclength(bv)))
      ERROR::severe("index out of range");

   bvecset( bv, index, getfixnum(bvalue) );
   return bvalue;
}

SEXPR FUNC::bvector_length()
{
   // *
   // syntax: (byte-vector-length <byte-vector>)
   //
   ArgstackIterator iter;
   const SEXPR v = guard(iter.getlast(), bvecp);
   return MEMORY::fixnum(getbveclength(v));
}

//
// vector
//

SEXPR FUNC::vector()
{
   // *
   // syntax: (vector <e1> <e2> ...)
   //
   ArgstackIterator iter;
   const int n = argstack.getargc();
   SEXPR v = MEMORY::vector(n);

   for (int i = 0; i < n; ++i)
      vectorset( v, i, iter.getarg() );

   return v;
}

SEXPR FUNC::make_vector()
{
   // *
   // syntax: (make-vector <size>)
   //
   ArgstackIterator iter;
   const int size = getfixnum(guard(iter.getlast(), fixnump));

   if (size < 0)
      ERROR::severe("vector size must be non-negative");

   return MEMORY::vector(size);
}

SEXPR FUNC::vector_length()
{
   // *
   // syntax: (vector-length <vector>)
   //
   ArgstackIterator iter;
   const SEXPR v = guard(iter.getlast(), vectorp);
   return MEMORY::fixnum(getvectorlength(v));
}

SEXPR FUNC::vector_ref()
{
   // *
   // syntax: (vector-ref <vector> <index>)
   //
   ArgstackIterator iter;
   const SEXPR v      = guard(iter.getarg(), vectorp);
   const SEXPR vindex = guard(iter.getlast(), fixnump);
   const int   index  = getfixnum(vindex);

   if (index < 0 || index >= static_cast<int>(getvectorlength(v)))
      ERROR::severe("index out of range");

   return vectorref( v, index );
}

SEXPR FUNC::vector_set()
{
   // *
   // syntax: (vector-set! <vector> <index> <value>)
   //
   ArgstackIterator iter;
   const SEXPR v      = guard(iter.getarg(), vectorp);
   const SEXPR vindex = guard(iter.getarg(), fixnump);
   const SEXPR x      = iter.getlast();
   const int   index  = getfixnum(vindex);

   if (index < 0 || index >= static_cast<int>(getvectorlength(v)))
      ERROR::severe("index out of range");

   vectorset( v, index, x );
   return x;
}

SEXPR FUNC::vector_fill()
{
   // *
   // syntax: (vector-fill! <vector> <value>) -> <vector>
   //
   ArgstackIterator iter;
   const SEXPR v      = guard(iter.getarg(), vectorp);
   const SEXPR x      = iter.getlast();

   for ( unsigned i = 0; i < getvectorlength(v); ++i )
      vectorset( v, i, x );

   return v;
}

SEXPR FUNC::vector_copy()
{
   // *
   // syntax: (vector-copy! <dest> <dest-start> <src> [<src-start> <src-end>]) -> <dest>
   //
   ArgstackIterator iter;
   const SEXPR dst   = guard(iter.getarg(), vectorp);
   const int   dst_s = getfixnum(guard(iter.getarg(), fixnump));
   const SEXPR src   = guard(iter.getarg(), vectorp);
   int src_s;
   int src_e;

   if ( dst_s >= static_cast<int>(getvectorlength(dst)) )
      ERROR::severe( "dst-start > dst length" );

   if ( iter.more() )
   {
      src_s = getfixnum(guard(iter.getarg(), fixnump));
      src_e = getfixnum(guard(iter.getlast(), fixnump));

      if ( src_s >= static_cast<int>(getvectorlength(src)) )
	 ERROR::severe( "src-start >= src length" );
      if ( src_e > static_cast<int>(getvectorlength(src)) )
	 ERROR::severe( "src-end > src length" );  
      if ( src_s >= src_e )
	 ERROR::severe( "src-start >= src-end" );
   }
   else
   {
      src_s = 0;
      src_e = getvectorlength(src);
   }

   if ( dst_s + (src_e - src_s) > static_cast<int>(getvectorlength(dst)) )
      ERROR::severe( "dest not large enough for src" );
   
   int i = dst_s;
   for ( int j = src_s; j < src_e; ++j, ++i )
      vectorset( dst, i, vectorref( src, j ) );

   return dst;
}

SEXPR FUNC::find_index()
{
   // *
   // syntax: (find-index <item> <vector> [<limit>])
   //
   ArgstackIterator iter;
   const SEXPR x = iter.getarg();
   const SEXPR v = guard(iter.getarg(), vectorp);
   int limit = getvectorlength(v);
   
   if ( iter.more() )
      limit = std::min( limit, static_cast<int>(getfixnum(guard(iter.getlast(), fixnump))) );

   for ( int index = 0; index < limit; ++index )
   {
      if ( eqv( x, vectorref(v, index) ) )
	 return MEMORY::fixnum(index);
   }
   return symbol_false;
}

SEXPR FUNC::rank()
{
   // *
   // syntax: (rank <item> <list>) -> fixnum | nil
   //
   ArgstackIterator iter;
   const SEXPR x = guard(iter.getarg(), symbolp);
   SEXPR y = guard(iter.getlast(), listp);
   int n = 0;

   while ( true )
   {
      if ( nullp(y) )
      {
	 return null;
      }
      else if ( eq( x, car(y) ) )
      {
	 return MEMORY::fixnum(n);
      }
      else
      {
	 n += 1;
	 y = cdr(y);
      }
   }
}

SEXPR FUNC::list_to_vector()
{
   // *
   // syntax: (list->vector <list>) -> <vector>
   //
   ArgstackIterator iter;
   SEXPR list    = guard(iter.getlast(), listp);
   const int len = list_length(list);
   SEXPR v = MEMORY::vector(len);

   for (int i = 0; i < len; ++i, list = ::cdr(list))
      vectorset(v, i, ::car(list));

   return v;
}

SEXPR FUNC::vector_to_list()
{
   // *
   // syntax: (vector->list <vector>) -> <list>
   //
   ArgstackIterator iter;
   SEXPR v = guard(iter.getlast(), vectorp);
   const int len = getvectorlength(v);

   push_reg(null);

   for (int i = len-1; i >= 0; --i)
   {
      top_reg() = MEMORY::cons( vectorref(v, i), top_reg() );
   }

   return pop_reg();
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

   if (anyp(e1))
   {
      if (fixnump(e1))
      {
	 return fixnump(e2) && getfixnum(e1) == getfixnum(e2);
      }
      else if (flonump(e1))
      {
	 return flonump(e2) && getflonum(e1) == getflonum(e2);
      }
      else if (charp(e1))
      {
	 return charp(e2) && getcharacter(e1) == getcharacter(e2);
      }
      else if (stringp(e1))
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

   if (anyp(e1))
   {
      if (vectorp(e1))
      {
	 if (vectorp(e2))
	 { 
	    const unsigned vlen = getvectorlength(e1);

	    if (vlen != getvectorlength(e2))
	       return false;

	    for ( unsigned i = 0; i < vlen; ++i )
	       if ( !equal(vectorref(e1, i), vectorref(e2, i)) )
		  return false;

	    return true;
	 }
      }
      else if (consp(e1))
      {
	 return consp(e2) && 
	    equal(car(e1), car(e2)) && 
	    equal(cdr(e1), cdr(e2));
      }
   }

   return false;
}

//
// equality
//

SEXPR FUNC::eq()
{
   // *
   // syntax: (eq? <exp1> <exp2>)
   //
   ArgstackIterator iter;
   const SEXPR e1 = iter.getarg();
   const SEXPR e2 = iter.getlast();
   return  (e1 == e2) ? symbol_true : symbol_false;
}

SEXPR FUNC::eqv()
{
   // *
   // syntax: (eqv? <exp1> <exp2>)
   //
   ArgstackIterator iter;
   const SEXPR e1 = iter.getarg();
   const SEXPR e2 = iter.getlast();
   return eqv(e1, e2) ? symbol_true : symbol_false;
}

SEXPR FUNC::equal()
{
   // *
   // syntax: (equal? <exp1> <exp2>)
   //
   ArgstackIterator iter;
   const SEXPR e1 = iter.getarg();
   const SEXPR e2 = iter.getlast();
   return equal(e1, e2) ? symbol_true : symbol_false;
}

//
// symbol
//

SEXPR FUNC::string_to_symbol()
{
   // *
   // syntax: (string->symbol <str>) -> interned symbol
   //
   ArgstackIterator iter;
   const SEXPR s = guard(iter.getlast(), stringp);
   return SYMTAB::enter(getstringdata(s));
}

SEXPR FUNC::symbol_to_string()
{
   // *
   // syntax: (symbol->string <sym>)
   //
   ArgstackIterator iter;
   const SEXPR s = guard(iter.getlast(), symbolp);
   return MEMORY::string(getname(s));
}

static auto gensym_number = 0u;
static char gensym_prefix[MAX_SYMBOL_LENGTH+1] = "g";

SEXPR FUNC::gensym()
{
   //
   // syntax: (gensym [<sym>|<str>|<fix>]) -> uninterned symbol
   //
   ArgstackIterator iter;

   if (iter.more())
   {
      const SEXPR arg = iter.getlast();

      if (symbolp(arg))
      {
	 strncpy(gensym_prefix, getname(arg), MAX_SYMBOL_LENGTH);
	 gensym_prefix[MAX_SYMBOL_LENGTH] = '\0';
      }
      else if (stringp(arg))
      {
	 strncpy(gensym_prefix, getstringdata(arg), MAX_SYMBOL_LENGTH);
	 gensym_prefix[MAX_SYMBOL_LENGTH] = '\0';
      }
      else if (fixnump(arg))
      {
	 gensym_number = static_cast<decltype(gensym_number)>(getfixnum(arg));
      }
      else
	 ERROR::severe("gensym requires [sym|str|fix]");
   }

   char new_sym[MAX_SYMBOL_LENGTH];

   SPRINTF(new_sym, "%s%u", gensym_prefix, gensym_number++);
  
   return MEMORY::symbol(new_sym);
}

SEXPR FUNC::symbol_value()
{
   // *
   // syntax: (symbol-value <sym-expr>)
   //
   ArgstackIterator iter;
   const SEXPR s = guard(iter.getlast(), symbolp);
   return getvalue(s);
}

SEXPR FUNC::set_symbol_value()
{
   // *
   // syntax: (set-symbol-value! <sym-expr> <value>)
   //
   ArgstackIterator iter;
   const SEXPR s = guard(iter.getarg(), symbolp);
   const SEXPR v = iter.getlast();
   setvalue(s, v);
   return v;
}

SEXPR FUNC::symbol_plist()
{
   // *
   // syntax: (symbol-plist <sym-expr>)
   //
   ArgstackIterator iter;
   const SEXPR s = guard(iter.getlast(), symbolp);
   return getplist(s);
}

SEXPR FUNC::set_symbol_plist()
{
   // *
   // syntax: (set-symbol-plist! <sym-expr> <plist>)
   //
   ArgstackIterator iter;
   const SEXPR s = guard(iter.getarg(), symbolp);
   const SEXPR p = iter.getlast();
   setplist(s, p);
   return p;
}

SEXPR FUNC::get_property()
{
   // *
   // syntax: (get <sym> <prop>)
   //
   ArgstackIterator iter;
   SEXPR s = guard(iter.getarg(), symbolp);
   SEXPR p = guard(iter.getlast(), symbolp);

   SEXPR plist = getplist(s);

   while (anyp(plist))
   {
      if ( eq( p, ::car(plist) ) )
	 return ::car(::cdr(plist));
      plist = ::cdr(::cdr(plist));
   }
  
   return null;
}

SEXPR FUNC::put_property()
{
   // *
   // syntax: (put <sym> <prop> <value>)
   //
   ArgstackIterator iter;
   SEXPR s = guard(iter.getarg(), symbolp);
   SEXPR p = guard(iter.getarg(), symbolp);
   SEXPR v = iter.getlast();

   SEXPR plist = getplist(s);

   while (anyp(plist))
   {
      if ( eq( p, ::car(plist) ) )
      {
	 rplaca(::cdr(plist), v);
	 return p;
      }
      plist = ::cdr(::cdr(plist));
   }
  
   // if we got here, then there is no such property
   push_reg(MEMORY::cons(v, getplist(s)));  // protect
   setplist(s, MEMORY::cons(p, top_reg()));
   pop_reg();

   return p;
}


//
// input/output
//

SEXPR FUNC::read()
{
   // *
   // syntax: (read [<inport>])
   //
   ArgstackIterator iter;
   if (iter.more())
   { 
      const SEXPR port = guard(iter.getlast(), anyinportp);
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
   const SEXPR s = iter.getarg();
   const SEXPR port = iter.more() ? guard(iter.getlast(), anyoutportp) : PIO::stdout_port;

   PRINTER::print(port, s);
   if (newline)
      PRINTER::newline(port);
   return symbol_true;
}

SEXPR FUNC::print() { return basic_print(1); }
SEXPR FUNC::write() { return basic_print(0); }

SEXPR FUNC::display()
{
   // *
   // syntax: (display <sexpr> [<outport>]) -> #t
   //
   ArgstackIterator iter;
   const SEXPR s = iter.getarg();
   const SEXPR port = iter.more() ? guard(iter.getlast(), anyoutportp) : PIO::stdout_port;

   PRINTER::print(port, s, 0);
   return symbol_true;
}

SEXPR FUNC::newline()
{
   // *
   // syntax: (newline [<outport>]) -> #t
   //
   ArgstackIterator iter;
   const SEXPR port = iter.more() ? guard(iter.getlast(), anyoutportp) : PIO::stdout_port;

   PRINTER::newline(port);
   return symbol_true;
}

SEXPR FUNC::read_char()
{
   // *
   // syntax: (read-char [<inport>])
   //
   ArgstackIterator iter;
   const SEXPR port = (iter.more()) ? guard(iter.getlast(), anyinportp) : PIO::stdout_port;

   const char ch = PIO::get(port);
   return (ch == EOF) ? PIO::eof_object : MEMORY::character(ch);
}

SEXPR FUNC::write_char()
{
   // *
   // syntax: (write-char <sexpr> [<outport>]) -> #t
   //
   ArgstackIterator iter;
   const char  ch = getcharacter(guard(iter.getarg(), charp));
   const SEXPR port = (iter.more()) ? guard(iter.getlast(), anyinportp) : PIO::stdout_port;

   PIO::put(port, ch);

   return symbol_true;
}

//
// memory management
//

SEXPR FUNC::gc()
{
   // *
   // syntax: (gc) -> #t
   //
   argstack.noargs();

   MEMORY::gc();

   push_reg( MEMORY::vector(4) );

   vectorset( top_reg(), 0, MEMORY::fixnum( MEMORY::CollectionCount ) );
   vectorset( top_reg(), 1, MEMORY::fixnum( MEMORY::TotalNodeCount ) );
   vectorset( top_reg(), 2, MEMORY::fixnum( MEMORY::FreeNodeCount ) );

#ifdef GC_STATISTICS_DETAILED
   const int N = MEMORY::ReclamationCounts.size();
   SEXPR v = MEMORY::vector(N);

   for (int i = 0; i < N; ++i)
      vectorset( v, i, MEMORY::fixnum( MEMORY::ReclamationCounts[i]) );

   vectorset( top_reg(), 3, v );
#endif

   return pop_reg();
}

//
// environment
//

SEXPR FUNC::the_environment()
{
   // *
   // syntax: (the-environment)
   //
   argstack.noargs();
   return EVAL::the_environment();
}

SEXPR FUNC::proc_environment()
{
   // *
   // syntax: (procedure-environment <closure>)
   //
   ArgstackIterator iter;
   const SEXPR closure = guard(iter.getlast(), closurep);
   return getclosurebenv(closure);
}

SEXPR FUNC::env_parent()
{
   // *
   // syntax: (environment-parent <env>)
   //
   ArgstackIterator iter;
   const SEXPR env = guard(iter.getlast(), envp);
  
   return getenvbase(env);
}

SEXPR FUNC::env_bindings()
{
   // *
   // syntax: (environment-bindings <env>) -> (<pair1> <pair2> ...)
   //
   REGSTACK_CHECKER("env-bindings");

   ArgstackIterator iter;
   const SEXPR arg = iter.getlast();

   // treat the empty list of bindings as a null env
   if (nullp(arg))
      return null;

   const SEXPR env = guard(arg, envp);

   // convert a frame into a list of bindings
   FRAME frame = getenvframe(env);

   if (frame)
   {
      SEXPR a = EVAL::listbuilder;
      setcdr(a, null);

      SEXPR vars = getframevars(frame);
      
      for (int i = 0; anyp(vars); ++i)
      {
	 push_reg( MEMORY::cons( getcar(vars), frameref(frame, i)) );
	 setcdr( a, MEMORY::cons( top_reg(), null ) );
	 pop_reg();
	 a = getcdr( a );
	 vars = getcdr(vars);
      }
      
      return getcdr( EVAL::listbuilder );
   }
   else
   {
      return null;
   }
}

SEXPR FUNC::make_environment()
{
   // *
   // syntax: (%make-environment <pairs> <baseenv>)
   //
   REGSTACK_CHECKER("make-env");

   ArgstackIterator iter;
   SEXPR pairs = guard(iter.getarg(), listp);
   const SEXPR benv = iter.getlast();

   if (!(nullp(benv) || envp(benv)))
      ERROR::severe( "expected a base environment", benv );
  
   // convert a list of bindings into a frame
   const int len = list_length(pairs);

   // if empty, extend base environment w/ empty frame
   if (len == 0)
      return MEMORY::environment(nullptr, benv);

   FRAME frame = MEMORY::frame(len);

   push_reg( MEMORY::environment( frame, benv ) );

   SEXPR a = EVAL::listbuilder;
   setcdr(a, null);

   for (int i = 0; anyp(pairs); ++i)
   {
      SEXPR x = ::car(pairs);
      if (consp(x))
      {
	 // ( <var> . <val> )
	 setcdr( a, MEMORY::cons(::car(x), null) );
	 frameset(frame, i, ::cdr(x));
      }
      else if (symbolp(x))
      {
	 // <var>
	 setcdr( a, MEMORY::cons(x, null) );
	 frameset(frame, i, null);
      }
      else
      {
	 ERROR::severe( "expected a symbol or (symbol . val)", x );
      }
      a = getcdr(a);
      pairs = ::cdr(pairs);
   }

   setframevars( frame, getcdr(EVAL::listbuilder) );   // pop [1] vars

   return pop_reg();                          // pop [0] environment
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
   SEXPR code = guard(iter.getarg(), listp);
   SEXPR params = guard(iter.getarg(), listp);
   const SEXPR env = iter.getlast();

   if (!(nullp(env) || envp(env)))
      ERROR::severe( "expected an environment", env );

   SEXPR closure = MEMORY::closure( code, env );
   push_reg( closure );

   EVAL::set_closure_attributes( closure, params );

   return pop_reg();
}

SEXPR FUNC::parse_formals()
{
   //
   // syntax: (%parse-formals <params>)
   //
   REGSTACK_CHECKER("parse-formals");

   ArgstackIterator iter;
   SEXPR params = iter.getlast();

   SEXPR v = MEMORY::vector( 3 );
   push_reg( v );

   SEXPR vars;
   BYTE numv;
   BYTE rargs;

   EVAL::parse_formals( params, vars, numv, rargs );

   vset( v, 0, vars );
   vset( v, 1, MEMORY::fixnum(numv) );
   vset( v, 2, rargs ? symbol_true : symbol_false );

   return pop_reg();
}

#ifdef BCE_COMPILER

SEXPR FUNC::make_code()
{
   //
   // syntax: (%make-code <bvec> <vec>)
   //
   ArgstackIterator iter;
   SEXPR bcodes = guard(iter.getarg(), bvecp);
   SEXPR sexprs = guard(iter.getlast(), vectorp);

   return MEMORY::code( bcodes, sexprs );
}

SEXPR FUNC::get_bcodes()
{
   //
   // syntax: (%get-bcodes <code>)
   //
   ArgstackIterator iter;
   SEXPR code = guard(iter.getlast(), codep);

   return code_getbcodes(code);
}

SEXPR FUNC::get_sexprs()
{
   //
   // syntax: (%get-sexprs <code>)
   //
   ArgstackIterator iter;
   SEXPR code = guard(iter.getlast(), codep);

   return code_getsexprs(code);
}

#endif

//
// unix
//

SEXPR FUNC::unix_system()
{
   //
   // syntax: (system <string>) -> <fixnum>
   //
   ArgstackIterator iter;
   const SEXPR cmd = guard(iter.getlast(), stringp);
   const int result = system(getstringdata(cmd));
   return MEMORY::fixnum( result );
}

extern int unix_argc;
extern char** unix_argv;

SEXPR FUNC::unix_getargs()
{
   //
   // syntax: (getargs) -> #(<string0> <string1> ... <stringc-1>)
   //
   argstack.noargs();

   SEXPR es_argv = MEMORY::vector(unix_argc);
   push_reg(es_argv);

   for (int i = 0; i < unix_argc; ++i)
      vset( es_argv, i, MEMORY::string(unix_argv[i]) );

   return pop_reg();
}

SEXPR FUNC::unix_gettime()
{
   //
   // syntax: (gettime) -> (<seconds> . <nanoseconds>)
   //
   argstack.noargs();

   SEXPR time = MEMORY::cons(null, null);
   push_reg( time );

   struct timespec ts;

   clock_gettime( CLOCK_MONOTONIC, &ts );
   setcar( time, MEMORY::fixnum( ts.tv_sec ) );
   setcdr( time, MEMORY::fixnum( ts.tv_nsec ) );

   return pop_reg();
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
   const SEXPR fname = guard(iter.getlast(), stringp);
   return PIO::open(getstringdata(fname), pm_input, "r");
}

SEXPR FUNC::open_output_file()
{
   //
   // syntax: (open-output-file <str>)
   //
   ArgstackIterator iter;
   const SEXPR fname = guard(iter.getlast(), stringp);
   return PIO::open(getstringdata(fname), pm_output, "w");
}

SEXPR FUNC::open_append_file()
{
   //
   // syntax: (open-append-file <str>)
   //
   ArgstackIterator iter;
   const SEXPR fname = guard(iter.getlast(), stringp);
   return PIO::open(getstringdata(fname), pm_output, "a");
}

SEXPR FUNC::open_update_file()
{
   //
   // syntax: (open-update-file <str>)
   //
   ArgstackIterator iter;
   const SEXPR fname = guard(iter.getlast(), stringp);
   return PIO::open(getstringdata(fname), pm_input|pm_output, "r+");
}

SEXPR FUNC::close_port()
{
   //
   // syntax: (close-port <port>) -> #t
   //
   ArgstackIterator iter;
   const SEXPR port = guard(iter.getlast(), portp);
   PIO::close(port);
   return symbol_true;
}

SEXPR FUNC::close_input_port()
{
   //
   // syntax: (close-input-port <port>) -> #t
   //
   ArgstackIterator iter;
   const SEXPR port = guard(iter.getlast(), inportp);
   PIO::close(port);
   return symbol_true;
}

SEXPR FUNC::close_output_port()
{
   //
   // syntax: (close-output-port <port>) -> #t
   //
   ArgstackIterator iter;
   const SEXPR port = guard(iter.getlast(), outportp);
   PIO::close(port);
   return symbol_true;
}

SEXPR FUNC::set_file_position()
{
   //
   // syntax: (set-file-position <port> <pos>) -> #t
   //
   ArgstackIterator iter;
   const SEXPR port = guard(iter.getarg(), portp);
   const SEXPR pos = guard(iter.getlast(), fixnump);
   PIO::set_position(port, pos);
   return symbol_true;
}

SEXPR FUNC::get_file_position()
{
   //
   // syntax: (get-file-position <port>)
   //
   ArgstackIterator iter;
   const SEXPR port = guard(iter.getlast(), portp);
   return PIO::get_position(port);
}

SEXPR FUNC::flush_output_port()
{
   //
   // syntax: (flush-output [<outport>]) -> #t
   //
   ArgstackIterator iter;
   if (iter.more())
   {
      const SEXPR port = guard(iter.getlast(), outportp);
      PIO::flush(port);
   }
   else
      PIO::flush(PIO::stdout_port);
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
   const SEXPR str = guard(iter.getlast(), stringp);
   return PIO::open_on_string( str, pm_input );
}

SEXPR FUNC::open_output_string()
{
   //
   // syntax: (open-output-string)
   //
   argstack.noargs();
   return PIO::open_on_string( MEMORY::string(2000), pm_output );
}

SEXPR FUNC::get_output_string()
{
   //
   // syntax: (open-output-string <stringport>)
   //
   ArgstackIterator iter;
   const SEXPR port = guard(iter.getlast(), stringportp);
   return getstringportstring(port);
}


//
// Predicates
//
//
// syntax: (null? <exp>)
// syntax: (symbol? <exp>)
// syntax: (integer? <exp>)
// syntax: (real? <exp>)
// syntax: (number? <exp>)
// syntax: (string? <exp>)
// syntax: (vector? <exp>)
// syntax: (pair? <exp>)
// syntax: (port? <exp>)
// syntax: (input-port? <exp>)
// syntax: (output-port? <exp>)
// syntax: (string-port? <exp>)
// syntax: (input-string-port? <exp>)
// syntax: (output-string-port? <exp>)
// syntax: (closure? <exp>)
// syntax: (procedure? <exp>)
// syntax: (continuation? <exp>)
// syntax: (environment? <exp>)
// syntax: (list? <exp>)
// syntax: (atom? <exp>)
// syntax: (bound? <exp>)
// syntax: (boolean? <exp>)
// syntax: (promise? <exp>)
//
PRED_IMP(nullp)
PRED_IMP(symbolp)
PRED_IMP(fixnump)
PRED_IMP(flonump)
PRED_IMP(numberp)
PRED_IMP(charp)
PRED_IMP(stringp)
PRED_IMP(vectorp)
PRED_IMP(bvecp)
PRED_IMP(consp)
PRED_IMP(funcp)
PRED_IMP(portp)
PRED_IMP(inportp)
PRED_IMP(outportp)
PRED_IMP(stringportp)
PRED_IMP(instringportp)
PRED_IMP(outstringportp)
PRED_IMP(closurep)
PRED_IMP(contp)
PRED_IMP(envp)
PRED_IMP(listp)
PRED_IMP(atomp)
PRED_IMP(promisep)
#ifdef BCE_COMPILER
PRED_IMP(codep)
#endif

PRED_IMP(boundp)
PRED_IMP(booleanp)
PRED_IMP(notp)
PRED_IMP(eof_objectp)
PRED_IMP(default_objectp)
PRED_IMP(zerop)
PRED_IMP(positivep)
PRED_IMP(negativep)
PRED_IMP(oddp)
PRED_IMP(evenp)
PRED_IMP(exactp)
PRED_IMP(inexactp)

SEXPR FUNC::procedurep()
{
   ArgstackIterator iter;
   const SEXPR arg = iter.getlast();
   return (funcp(arg) || specialp(arg) || closurep(arg)) ? symbol_true : symbol_false;
}

inline int string_nullp(SEXPR s)
{
   return getstringlength(guard(s, stringp)) == 0;
}

PRED_IMP(string_nullp)

SEXPR FUNC::string_length()
{
   ArgstackIterator iter;
   const SEXPR s = guard(iter.getlast(), stringp);
   return MEMORY::fixnum(getstringlength(s));
}

SEXPR FUNC::string_append()
{
   //
   // syntax: (string-append <s1> <s2> ... <sn>) -> <string>
   //
   ArgstackIterator iter;

   if (!iter.more())
      return MEMORY::string_null;

   auto slen = 0;

   while ( iter.more() )
   {
      SEXPR s = guard(iter.getarg(), stringp);
      slen += getstringlength(s);
   }

   iter.reset();

   SEXPR n = MEMORY::string(slen);
   while ( iter.more() )
   {
      SEXPR s = iter.getarg();
      strcat(getstringdata(n), getstringdata(s));
   }

   return n;
}

SEXPR FUNC::string_ref()
{
   //
   // syntax: (string-ref <s> <index>) -> <char>
   //
   ArgstackIterator iter;
   SEXPR s = guard(iter.getarg(), stringp);
   const int n = getfixnum(guard(iter.getlast(), fixnump));

   if (n >= 0 && n < static_cast<int>(getstringlength(s)))
      return MEMORY::character(getstringdata(s)[n]);
   else
      ERROR::severe("index out of string bounds");

   return s;
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

   if (n >= 0 && n < static_cast<int>(getstringlength(s)))
      getstringdata(s)[n] = ch;
   else
      ERROR::severe("index out of string bounds");

   return s;
}

SEXPR FUNC::substring()
{
   //
   // syntax: (substring <s> <start> <end>) -> <string>
   //
   ArgstackIterator iter;
   SEXPR s = guard(iter.getarg(), stringp);
   const int start = getfixnum(guard(iter.getarg(), fixnump));
   const int end   = getfixnum(guard(iter.getlast(), fixnump));

   if (start < static_cast<int>(getstringlength(s)) &&
       end <= static_cast<int>(getstringlength(s)))
   {
      if (start >= end)
	 return MEMORY::string_null;
      else
      {
	 const int slen = end - start;
	 SEXPR ss = MEMORY::string(slen);
	 strncpy(getstringdata(ss), &getstringdata(s)[start], slen);
	 getstringdata(ss)[slen] = '\0';
	 return ss;
      }
   }
   else
   {
      ERROR::severe("index out of string bounds");
      return null;
   }
}

SEXPR FUNC::string_fill()
{
   //
   // syntax: (string-fill! <s> <ch>) -> <string>
   //
   ArgstackIterator iter;
   SEXPR s = guard(iter.getarg(), stringp);
   const int ch = getcharacter(guard(iter.getlast(), charp));

   for ( unsigned i = 0; i < getstringlength(s); ++i )
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
   int src_s;
   int src_e;

   if ( dst_s >= static_cast<int>(getstringlength(dst)) )
      ERROR::severe( "string dst-start > dst length" );

   if ( iter.more() )
   {
      src_s = getfixnum(guard(iter.getarg(), fixnump));
      src_e = getfixnum(guard(iter.getlast(), fixnump));

      if ( src_s >= static_cast<int>(getstringlength(src)) )
	 ERROR::severe( "string src-start >= src length" );
      if ( src_e > static_cast<int>(getstringlength(src)) )
	 ERROR::severe( "string src-end > src length" );  
      if ( src_s >= src_e )
	 ERROR::severe( "string src-start >= src-end" );
   }
   else
   {
      src_s = 0;
      src_e = getstringlength(src);
   }

   if ( dst_s + (src_e - src_s) > static_cast<int>(getstringlength(dst)) )
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
   SEXPR list = guard(iter.getarg(), listp);
   const int len = list_length(list);

   SEXPR s = MEMORY::string(len);       // string(len) allocates room for '\0'

   for (int i = 0; i < len; ++i, list = ::cdr(list))
      getstringdata(s)[i] = getcharacter(guard(::car(list), charp));

   getstringdata(s)[len] = '\0';  // null terminate the string data

   return s;
}

SEXPR FUNC::string_to_list()
{
   //
   // syntax: (string->list <string>) -> <list-of-chars>
   //
   REGSTACK_CHECKER("string->list");

   ArgstackIterator iter;
   SEXPR s = guard(iter.getlast(), stringp);
   const int len = getstringlength(s);

   regstack.push(null);
   const int p = regstack.gettop();

   for (int i = len-1; i >= 0; --i)
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
   const char* s1 = getstringdata(guard(iter.getarg(), stringp));
   const char* s2 = getstringdata(guard(iter.getlast(), stringp));

   switch (op)
   {
      case EQop: return (compare(s1, s2) == 0) ? symbol_true : symbol_false;
      case LTop: return (compare(s1, s2) <  0) ? symbol_true : symbol_false;
      case LEop: return (compare(s1, s2) <= 0) ? symbol_true : symbol_false;
      case GTop: return (compare(s1, s2) >  0) ? symbol_true : symbol_false;
      case GEop: return (compare(s1, s2) >= 0) ? symbol_true : symbol_false;
      default:
      {
	 ERROR::fatal("bad string comparison operator");
	 return null;
      }
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

static SEXPR char_compare( RelOp op, int ci )
{
   ArgstackIterator iter;
   char c1 = getcharacter(guard(iter.getarg(), charp));
   char c2 = getcharacter(guard(iter.getlast(), charp));

   if (ci)
   {
      c1 = toupper(c1);
      c2 = toupper(c2);
   }

   switch (op)
   {
      case EQop: return (c1 == c2) ? symbol_true : symbol_false;
      case LTop: return (c1 <  c2) ? symbol_true : symbol_false;
      case LEop: return (c1 <= c2) ? symbol_true : symbol_false;
      case GTop: return (c1 >  c2) ? symbol_true : symbol_false;
      case GEop: return (c1 >= c2) ? symbol_true : symbol_false;
      default:
      {
	 ERROR::fatal("bad character comparison operator");
	 return null;
      }
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
   const SEXPR s = guard(iter.getlast(), fixnump);

   char image[MAX_INTEGER_LENGTH];
   SPRINTF(image, "%ld", getfixnum(s));
  
   return MEMORY::string(image);
}

SEXPR FUNC::string_to_integer()
{
   //
   // syntax: (string->integer <string>) -> <fixnum>
   //
   ArgstackIterator iter;
   const SEXPR s = guard(iter.getlast(), stringp);
   return MEMORY::fixnum( atoi(getstringdata(s)) );
}

//
// let transformer(s)
//
//   let*
//

SEXPR FUNC::transform_letstar()
{
   //
   // syntax: (%transform-let*  <exp>) -> <exp>
   //
   ArgstackIterator iter;
   SEXPR exp = iter.getlast();

   if (!(consp(exp) && ::car(exp) == LETSTAR))
      ERROR::severe( "not a let* expression", exp );

   return EVAL::transform_letstar( exp );
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
   while ( anyp(list) && !eqtest( exp, ::car(list) ) )
      list = ::cdr(list);

   return list;
}

SEXPR FUNC::member()
{
   //
   // syntax: (member <exp> <list>) -> <exp> or null
   //
   ArgstackIterator iter;
   const SEXPR exp = iter.getarg();
   const SEXPR list = guard(iter.getlast(), listp);
   return member_search( equal, exp, list );
}

SEXPR FUNC::memv()
{
   //
   // syntax: (memv <exp> <list>) -> <exp> or null
   //
   ArgstackIterator iter;
   const SEXPR exp = iter.getarg();
   const SEXPR list = guard(iter.getlast(), listp);
   return member_search( eqv, exp, list );
}

SEXPR FUNC::memq()
{
   //
   // syntax: (memq <exp> <list>) -> <exp> or null
   //
   ArgstackIterator iter;
   const SEXPR exp = iter.getarg();
   const SEXPR list = guard(iter.getlast(), listp);
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
      SEXPR x = ::car(list);
      if ( consp(x) && eqtest( exp, ::car(x) ) )
	 return x;
      list = ::cdr(list);
   }

   return null;
}

SEXPR FUNC::assoc()
{
   //
   // syntax: (assoc <exp> <alist>) -> (<exp> . <value>) or null
   //
   ArgstackIterator iter;
   const SEXPR exp = iter.getarg();
   const SEXPR list = guard(iter.getlast(), listp);
   return assoc_search( equal, exp, list );
}

SEXPR FUNC::assv()
{
   //
   // syntax: (assv <exp> <alist>) -> (<exp> . <value>) or null
   //
   ArgstackIterator iter;
   const SEXPR exp = iter.getarg();
   const SEXPR list = guard(iter.getlast(), listp);
   return assoc_search( eqv, exp, list );
}

SEXPR FUNC::assq()
{
   //
   // syntax: (assq <exp> <alist>) -> (<exp> . <value>) or null
   //
   ArgstackIterator iter;
   const SEXPR exp = iter.getarg();
   const SEXPR list = guard(iter.getlast(), listp);
   return assoc_search( eq, exp, list );
}


//
// Other list functions
//

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
   REGSTACK_CHECKER("append");

   ArgstackIterator iter;

   SEXPR a = EVAL::listbuilder;
   setcdr(a, null);

   while (iter.more())
   {
      SEXPR b = guard(iter.getarg(), listp);

      while (anyp(b))
      {
	 setcdr( a, MEMORY::cons(::car(b), null) );
	 a = getcdr(a);
	 b = ::cdr(b);
      }
   }

   return getcdr( EVAL::listbuilder );
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
   REGSTACK_CHECKER("reverse");

   ArgstackIterator iter;
   SEXPR list = guard(iter.getlast(), listp);

   // protect the structure under construction
   push_reg(null);

   while (anyp(list))
   {
      top_reg() = MEMORY::cons(::car(list), top_reg());
      list = ::cdr(list);
   }

   return pop_reg();
}

//
// Last pair
//
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
   SEXPR list = guard(iter.getlast(), listp);

   while (consp(::cdr(list)))
      list = ::cdr(list);

   return list;
}

//
// List tail
//
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
   SEXPR list = guard(iter.getarg(), listp);
   auto n = getfixnum(guard(iter.getlast(), fixnump));
  
   if (n < 0)
      ERROR::severe("index out of range", list, MEMORY::fixnum(n));

   while (anyp(list) && n > 0)
   {
      n -= 1;
      list = ::cdr(list);
   }

   return list;
}

//
// Symbol Reference
//

SEXPR FUNC::gref()
{
   //
   // syntax: (%gref <symbol>) -> <gref>
   //
   ArgstackIterator iter;
   SEXPR symbol = guard(iter.getlast(), symbolp);
   return MEMORY::gref(symbol);
}

SEXPR FUNC::fref()
{
   //
   // syntax: (%fref <depth> <index>) -> <fref>
   //
   ArgstackIterator iter;
   const int depth = getfixnum(guard(iter.getarg(), fixnump));
   const int index = getfixnum(guard(iter.getlast(), fixnump));
   return MEMORY::fref(depth, index);
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
   SEXPR closure = guard(iter.getlast(), closurep);
   return getclosurecode(closure);
}

#ifdef BCE_COMPILER

SEXPR FUNC::closure_code_set()
{
   //
   // syntax: (%closure-code-set! <closure> <code>) -> <closure>
   //
   ArgstackIterator iter;
   SEXPR closure = guard(iter.getarg(), closurep);
   SEXPR code = guard(iter.getlast(), codep);
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
   SEXPR closure = guard(iter.getlast(), closurep);
   return getclosurebenv(closure);
}

SEXPR FUNC::closure_vars()
{
   //
   // syntax: (%closure-vars <closure>) -> <list>
   //
   ArgstackIterator iter;
   SEXPR closure = guard(iter.getlast(), closurep);
   return getclosurevars(closure);
}

SEXPR FUNC::closure_numv()
{
   //
   // syntax: (%closure-numv <closure>) -> <fixnum>
   //
   ArgstackIterator iter;
   SEXPR closure = guard(iter.getlast(), closurep);
   return MEMORY::fixnum(getclosurenumv(closure));
}

SEXPR FUNC::closure_rest()
{
   //
   // syntax: (%closure-rest <closure>) -> <boolean>
   //
   ArgstackIterator iter;
   SEXPR closure = guard(iter.getlast(), closurep);
   return getclosurerargs(closure) ? symbol_true : symbol_false;
}


// transcript

SEXPR FUNC::transcript_on()
{
   ArgstackIterator iter;
   SEXPR fname = guard(iter.getlast(), stringp);
   PIO::transcript_on( getstringdata(fname) );
   return null;
}

SEXPR FUNC::transcript_off()
{
   argstack.noargs();
   PIO::transcript_off();
   return null;
}

// history

SEXPR FUNC::history_add()
{
   //
   // syntax: (history-add <sexpr>) -> #t
   //
   ArgstackIterator iter;
   const SEXPR sexpr = iter.getlast();
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
   const SEXPR str = guard(iter.getlast(), stringp);
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
   const SEXPR ch = guard(iter.getlast(), charp);
   return isalpha(getcharacter(ch)) ? symbol_true : symbol_false;
}

SEXPR FUNC::char_numericp()
{
   //
   // syntax: (char-numeric? <char>) -> <boolean>
   //
   ArgstackIterator iter;
   const SEXPR ch = guard(iter.getlast(), charp);
   return isdigit(getcharacter(ch)) ? symbol_true : symbol_false;
}

SEXPR FUNC::char_whitespacep()
{
   //
   // syntax: (char-whitespace? <char>) -> <boolean>
   //
   ArgstackIterator iter;
   const SEXPR ch = guard(iter.getlast(), charp);
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
   const SEXPR ch = guard(iter.getlast(), charp);
   return islower(getcharacter(ch)) ? symbol_true : symbol_false;
}

SEXPR FUNC::char_upcase()
{
   //
   // syntax: (char-upcase <char>) -> <char>
   //
   ArgstackIterator iter;
   const SEXPR ch = guard(iter.getlast(), charp);
   return MEMORY::character(toupper(getcharacter(ch)));
}

SEXPR FUNC::char_downcase()
{
   //
   // syntax: (char-downcase <char>) -> <char>
   //
   ArgstackIterator iter;
   const SEXPR ch = guard(iter.getlast(), charp);
   return MEMORY::character(tolower(getcharacter(ch)));
}

SEXPR FUNC::char_to_integer()
{
   //
   // syntax: (char->integer <char>) -> <fixnum>
   //
   ArgstackIterator iter;
   const SEXPR ch = guard(iter.getlast(), charp);
   return MEMORY::fixnum((FIXNUM)getcharacter(ch));
}

SEXPR FUNC::integer_to_char()
{
   //
   // syntax: (integer->char <integer>) -> <char>
   //
   ArgstackIterator iter;
   const SEXPR num = guard(iter.getlast(), fixnump);
   return MEMORY::character((CHAR)getfixnum(num));
}

SEXPR FUNC::objaddr()
{
   //
   // syntax: (%object-address <object>) -> <fixnum>
   //
   ArgstackIterator iter;
   const SEXPR obj = iter.getlast();
   return MEMORY::fixnum((FIXNUM)obj);
}

SEXPR FUNC::change_dir()
{
   //
   // syntax: (chdir <string>) -> <fixnum>
   //
   ArgstackIterator iter;
   const SEXPR path = guard(iter.getlast(), stringp);
   const int result = ::chdir( getstringdata(path) );
   return MEMORY::fixnum((FIXNUM)result);
}

SEXPR FUNC::current_dir()
{
   //
   // syntax: (getcwd) -> <string>
   //
   argstack.noargs();
   char buffer[200];
   const char* result = ::getcwd( buffer, sizeof(buffer) );
   if ( result == NULL )
      return null;
   else
      return MEMORY::string( buffer );
}
