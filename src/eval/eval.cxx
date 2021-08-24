#include "eval.hxx"

#include <cstdlib>

#ifdef BYTE_CODE_EVALUATOR
#include "code.hxx"
#endif

#include "core/error.hxx"
#include "core/symtab.hxx"
#include "core/memory.hxx"
#include "core/printer.hxx"
#include "core/format.hxx"
#include "core/symdict.hxx"

namespace escheme
{

using MEMORY::cons;
using MEMORY::fixnum;

// evaluator registers
SEXPR EVAL::exp;
SEXPR EVAL::env;
SEXPR EVAL::val;
SEXPR EVAL::aux;
SEXPR EVAL::unev;
#ifndef NO_INTERP
EVSTATE EVAL::cont;
EVSTATE EVAL::next;
#endif
#ifdef BYTE_CODE_EVALUATOR
int EVAL::pc;
SEXPR EVAL::map_code;
SEXPR EVAL::for_code;
SEXPR EVAL::fep_code;
SEXPR EVAL::rtc_code;
#ifndef NO_INTERP
SEXPR EVAL::rte_code;
#endif
#endif

SEXPR EVAL::the_environment() { return env; }
SEXPR EVAL::the_global_environment() { return null; }

//
// New: A frame-based representation
//
//   <env> = ( <frame> . <env> )
//
// The following functions are dependent upon the representation:
//
//   lookup
//   set_variable_value
//   create_bindings
//   extend_environment
//

SEXPR EVAL::lookup( SEXPR var, SEXPR env )
{
   guard( env, anyenvp );
   
   for ( ; ; env = getenvbase(env) )
   {
      if ( envp(env) )
      {
	 FRAME frame = getenvframe(env);
	 SEXPR vars = getframevars(frame);
	 
	 for ( int i = 0; i < getframenslots(frame); ++i, vars = getcdr(vars) )
	 {
	    if ( getcar(vars) == var ) 
	       return frameref(frame, i);
	 }
      }
      else if ( assocenvp(env) )
      {
	 auto dict = assocenv_getdict(env);
	 
	 if ( dict->has( var ) )
	    return dict->get( var );
      }
      else
      {
	 // global var
	 const SEXPR val = value(var);
	 
	 if ( val == symbol_unbound )
	    ERROR::severe( "symbol is undefined", var );
	 
	 return val;
      }
   }
}

SEXPR EVAL::is_bound( SEXPR var, SEXPR env )
{
   guard( env, anyenvp );
   
   for ( ; ; env = getenvbase(env) )
   {
      if ( envp(env) )
      {
	 FRAME frame = getenvframe(env);
	 SEXPR vars = getframevars(frame);
	 
	 for ( int i = 0; i < getframenslots(frame); ++i, vars = getcdr(vars) )
	 {
	    if ( getcar(vars) == var ) 
	       return symbol_true;
	 }
      }
      else if ( assocenvp(env) )
      {
	 auto dict = assocenv_getdict(env);
	 
	 if ( dict->has( var ) )
	    return symbol_true;
      }
      else
      {
	 // global var
	 const SEXPR val = value(var);
	 
	 return ( val == symbol_unbound ) ? symbol_false : symbol_true;
      }
   }
}

void EVAL::set_variable_value( SEXPR var, SEXPR val, SEXPR env )
{
   guard( env, anyenvp );
   
   for ( ; ; env = getenvbase(env) )
   {
      if ( envp(env) )
      {
	 FRAME frame = getenvframe(env);  
	 SEXPR vars = getframevars(frame);
	 
	 for ( int i = 0; i < getframenslots(frame); ++i, vars = getcdr(vars) )
	 {
	    if ( getcar(vars) == var )
	    {
	       frameset( frame, i, val );
	       return;
	    }
	 }
      }
      else if ( assocenvp(env) )
      {
	 auto dict = assocenv_getdict(env);
	 
	 if ( dict->has( var ) )
	 {
	    dict->set( var, val );
	    return;
	 }
      }
      else
      {	 
	 // global var
	 if ( value(var) == symbol_unbound )
	    ERROR::severe( "symbol is undefined", var );
	 
	 set( var, val );
	 return;
      }
   }
}

//
// Parse the Formal Parameters
//
//   parameter lists
//     (a ...)
//     tradition rest
//       (a . b) == (a #!rest b)
//

void EVAL::parse_formals( SEXPR formals, SEXPR& vars, INT32& numv, bool& rargs )
{
   numv = 0;
   rargs = false;

   ListBuilder varlist;

   // validate and normalize the varlist
   while ( anyp(formals) )
   {
      numv++;

      if ( symbolp(formals) )
      {
	 rargs = true;      
	 varlist.add( formals );
	 formals = null;
      }
      else
      {
	 varlist.add( guard(car(formals), symbolp) );
	 formals = cdr(formals);
      }
   }

   vars = varlist.get();
}

static void arg_error( const char* text, unsigned n1, unsigned n2, SEXPR fun )
{
   ERROR::severe( format( "%s -- actual=%u, expect=%u", text, n1, n2 ).c_str(), fun );
}

SEXPR EVAL::extend_env_fun( SEXPR closure )
{
   //
   // extend the environment with the closure's vars
   // populate the frame with argstack values
   //

   // formal parameter attributes required:
   //   (<numv> <simple-var-list>)

   const auto nactual = static_cast<int>(argstack.getargc());
   const auto nformal = static_cast<int>(getclosurenumv(closure));
   const auto benv = getclosurebenv(closure);
   const bool rargs = getclosurerargs(closure);

   // create an extended environment
   regstack.push( MEMORY::environment( nformal, getclosurevars(closure), benv ) );

   FRAME frame = getenvframe( regstack.top() );

   if ( rargs == false ) 
   {
      // case I: no rest args
      //
      //   <fargs> := (a1 a2 ...)
      //
      if ( nactual != nformal )
      {
	 if ( nactual < nformal )
	    arg_error( "too few arguments", nactual, nformal, closure );
	 else
	    arg_error( "too many arguments", nactual, nformal, closure );
      }
     
      int p = argstack.getfirstargindex();
     
      // BIND required
      for ( int i = 0; i < nactual; ++i )
	 frameset( frame, i, argstack[p++] );
   }
   else
   {
      // case II: rest arg
      //
      //   <fargs> := (a1 a2 ... aN-1 . aN)
      //
      const int nrequired = nformal - 1;

      if ( nactual < nrequired )
	 arg_error( "too few arguments", nactual, nrequired, closure );
     
      int p = argstack.getfirstargindex();
     
      // BIND required
      for ( int i = 0; i < nrequired; ++i )
	 frameset( frame, i, argstack[p++] );

      // BIND rest
      regstack.push(null);

      for ( int i = p + (nactual - nformal); i >= p; --i )
	 regstack.top() = cons( argstack[i], regstack.top() );
     
      frameset( frame, nrequired, regstack.pop() );
   }

   argstack.removeargc();

   return regstack.pop();
}

SEXPR EVAL::extend_env_vars( SEXPR bindings, SEXPR benv )
{
   //
   // extend the environment with let/letrec vars
   //   bindings = (binding ...)
   //   binding = (v e) | v
   //

   if ( nullp(bindings) )
      return benv;

   ListBuilder vars;
   int nvars = 0;

   while ( anyp(bindings) )
   {
      nvars++;
      auto v = car(bindings);
      if ( consp(v) )
	 v = car(v);
      vars.add( v );
      bindings = cdr(bindings);
   }

   return MEMORY::environment( nvars, vars.get(), benv );
}

SEXPR EVAL::get_evaluator_state()
{
   const int rs_depth = regstack.getdepth();
   const int as_depth = argstack.getdepth();
   const int is_depth = intstack.getdepth();

   regstack.push( MEMORY::vector( rs_depth ) );
   for ( int i = 0; i < rs_depth; ++i )
      vectorset( regstack.top(), i, regstack[i] );

   regstack.push( MEMORY::vector( as_depth ) );
   for ( int i = 0; i < as_depth; ++i )
      vectorset( regstack.top(), i, argstack[i] );

   regstack.push( MEMORY::vector( is_depth ) );
   for ( int i = 0; i < is_depth; ++i )
      vectorset( regstack.top(), i, MEMORY::fixnum(intstack[i]) );

   auto evs = MEMORY::vector(3);
   vectorset( evs, 2, regstack.pop() );
   vectorset( evs, 1, regstack.pop() );
   vectorset( evs, 0, regstack.pop() );
   
   return evs;
}

//
// continuations
//

// the number of single entries in the continuation
//   these precede the stack contents
const int ContSingletons = 3;

// the number of byte vector locations reserved for saved int values
const int BvReserved = 4;

SEXPR EVAL::create_continuation()
{
   const int regs_depth = regstack.getdepth();
   const int args_depth = argstack.getdepth();
   const int ints_depth = intstack.getdepth();
   const int state_len = ContSingletons + regs_depth + args_depth;

   regstack.push( MEMORY::continuation() );
   cont_setstate( regstack.top(), MEMORY::vector(state_len) );
   
   // byte vector includes the intstack and the three(3) stack depth values
   const int ByteVectorLength = ints_depth + BvReserved;
   auto bv = MEMORY::byte_vector( ByteVectorLength*sizeof(INT16) );
   
   INT16* pint16 = reinterpret_cast<INT16*>(getbvecdata(bv));

   pint16[0] = regs_depth;
   pint16[1] = args_depth;
   pint16[2] = ints_depth;
#ifdef BYTE_CODE_EVALUATOR
   pint16[3] = pc;
#endif

   // no additional allocations
   auto state = cont_getstate( regstack.top() );

   vectorset( state, 0, env );
   vectorset( state, 1, unev );
   vectorset( state, 2, bv );
   
   int j = ContSingletons;

   for ( int i = 0; i < regs_depth; ++i )
      vectorset( state, j++, regstack[i] );
   
   for ( int i = 0; i < args_depth; ++i )
      vectorset( state, j++, argstack[i] ); 
   
   for ( int i = 0; i < ints_depth; ++i )
      pint16[BvReserved+i] = intstack[i];
   
   return regstack.pop();
}

void EVAL::restore_continuation( SEXPR cc )
{
   // no allocations
   
   auto state = cont_getstate( cc );

   env = vectorref( state, 0 );
   unev = vectorref( state, 1 );
   auto bv = vectorref( state, 2 );

   const INT16* pint16 = reinterpret_cast<INT16*>(getbvecdata(bv));

   const int regs_depth = pint16[0];
   const int args_depth = pint16[1];
   const int ints_depth = pint16[2];
#ifdef BYTE_CODE_EVALUATOR
   pc                   = pint16[3];
#endif

   int j = ContSingletons;
   
   for ( int i = 0; i < regs_depth; ++i )
      regstack[i] = vectorref( state, j++ );
   
   for ( int i = 0; i < args_depth; ++i )
      argstack[i] = vectorref( state, j++ );
      
   for ( int i = 0; i < ints_depth; ++i )
      intstack[i] = pint16[BvReserved+i];
   
   regstack.newtop( regs_depth );
   argstack.newtop( args_depth );
   intstack.newtop( ints_depth );
}

//
// initialization
//

static void eval_marker()
{
   // mark the evaluator objects
   MEMORY::mark( argstack );
   MEMORY::mark( regstack );
   MEMORY::mark( EVAL::exp );
   MEMORY::mark( EVAL::env );
   MEMORY::mark( EVAL::aux );
   MEMORY::mark( EVAL::val );
   MEMORY::mark( EVAL::unev );
#ifdef BYTE_CODE_EVALUATOR
   MEMORY::mark( EVAL::map_code );
   MEMORY::mark( EVAL::for_code );
   MEMORY::mark( EVAL::fep_code );
   MEMORY::mark( EVAL::rtc_code );
#ifndef NO_INTERP
   MEMORY::mark( EVAL::rte_code );
#endif
#endif
}

void EVAL::initialize()
{
   // evaluator registers
   exp = null;
   env = null;
   val = null;
   aux = null;
   unev = null;
#ifndef NO_INTERP
   cont = EV_DONE;
   next = EV_DONE;
#endif
#ifdef BYTE_CODE_EVALUATOR
   pc = 0;
#endif

#ifndef NO_INTERP
   // set the special form dispatch value
   setform( symbol_quote,    EV_QUOTE );
   setform( symbol_delay,    EV_DELAY );
   setform( symbol_set,      EV_SET );
   setform( symbol_define,   EV_DEFINE );
   setform( symbol_if,       EV_IF );
   setform( symbol_cond,     EV_COND );
   setform( symbol_lambda,   EV_LAMBDA );
   setform( symbol_begin,    EV_BEGIN );
   setform( symbol_sequence, EV_BEGIN );
   setform( symbol_let,      EV_LET );
   setform( symbol_letrec,   EV_LETREC );
   setform( symbol_while,    EV_WHILE );
   setform( symbol_and,      EV_AND );
   setform( symbol_or,       EV_OR );
   setform( symbol_access,   EV_ACCESS );
   setform( null,            EV_APPLICATION );
#endif

#ifdef BYTE_CODE_EVALUATOR
   //
   // create code fragments
   //
   auto map_bcodes = MEMORY::byte_vector( 5 );
   auto for_bcodes = MEMORY::byte_vector( 5 );
   auto rtc_bcodes = MEMORY::byte_vector( 1 );
   auto fep_bcodes = MEMORY::byte_vector( 2 );;
#ifndef NO_INTERP
   auto rte_bcodes = MEMORY::byte_vector( 1 );
#endif

   bvecset( map_bcodes, 0, OP_MAP_INIT );
   bvecset( map_bcodes, 1, OP_MAP_APPLY );
   bvecset( map_bcodes, 2, OP_APPLY );
   bvecset( map_bcodes, 3, OP_MAP_RESULT );
   bvecset( map_bcodes, 4, OP_GOTO_CONT );

   bvecset( for_bcodes, 0, OP_FOR_INIT );
   bvecset( for_bcodes, 1, OP_FOR_APPLY );
   bvecset( for_bcodes, 2, OP_APPLY );
   bvecset( for_bcodes, 3, OP_FOR_RESULT );
   bvecset( for_bcodes, 4, OP_GOTO_CONT );

   bvecset( fep_bcodes, 0, OP_FORCE_VALUE );
   bvecset( fep_bcodes, 1, OP_GOTO_CONT );

   bvecset( rtc_bcodes, 0, OP_RTC );
#ifndef NO_INTERP
   bvecset( rte_bcodes, 0, OP_RTE );
#endif

   auto vector_null = MEMORY::vector(0);
   map_code = MEMORY::code( map_bcodes, vector_null );
   for_code = MEMORY::code( for_bcodes, vector_null );
   fep_code = MEMORY::code( fep_bcodes, vector_null );
   rtc_code = MEMORY::code( rtc_bcodes, vector_null );
#ifndef NO_INTERP
   rte_code = MEMORY::code( rte_bcodes, vector_null );
#endif

   SYMTAB::enter( "%%map-code", map_code );
   SYMTAB::enter( "%%for-code", for_code );
   SYMTAB::enter( "%%fep-code", fep_code );
   SYMTAB::enter( "%%rtc-code", rtc_code );
#ifndef NO_INTERP
   SYMTAB::enter( "%%rte-code", rte_code );
#endif
#endif

   MEMORY::register_marker( eval_marker );
}

}
