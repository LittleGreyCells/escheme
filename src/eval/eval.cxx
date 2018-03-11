#include <cstdlib>

#include "eval.hxx"
#include "error.hxx"
#include "symtab.hxx"
#include "memory.hxx"
#include "printer.hxx"

using MEMORY::cons;
using MEMORY::fixnum;

// evaluator registers
SEXPR EVAL::exp;
SEXPR EVAL::env;
SEXPR EVAL::val;
SEXPR EVAL::aux;
SEXPR EVAL::unev;
EVSTATE EVAL::cont;
EVSTATE EVAL::next;
SEXPR EVAL::theGlobalEnv;

//
// New: A frame-based representation
//
//   <env> = ( <frame> . <env> )
//   <frame> = Frame( nslots, slots[ <var> <val> <var> <val> ...] )
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
   for (; anyp(env); env = getenvbase(env))
   {
      FRAME frame = getenvframe(env);

      if (frame)
      {
	 SEXPR vars = getframevars(frame);

	 for (int i = 0; anyp(vars); ++i, vars = getcdr(vars))
	 {
	    if (getcar(vars) == var) 
	       return frameref(frame, i);
	 }
      }
   }

   // global var
   const SEXPR val = value(var);

   if (val == SYMTAB::symbol_unbound)
      ERROR::severe("symbol is unbound", var);

   return val;
}

void EVAL::set_variable_value( SEXPR var, SEXPR val, SEXPR env )
{
   if (anyp(env))
      guard(env, envp);

   for (; anyp(env); env = getenvbase(env))
   {
      FRAME frame = getenvframe(env);

      if (frame)
      {
	 SEXPR vars = getframevars(frame);

	 for (int i = 0; anyp(vars); ++i, vars = getcdr(vars))
	 {
	    if (getcar(vars) == var)
	    {
	       frameset(frame, i, val);
	       return;
	    }
	 }
      }
   }

   // global var
   set(var, val);
}

//
// normalize_definition
// 
//   (define x <exp>)
//       var == x
//       val == <exp>
//   (define (x <args>) <body>)  -> (define x (lambda <args> <body>))
//       var == x
//       val == (lambda (<args> <body>))
//

void EVAL::normalize_definition( SEXPR exp, SEXPR& var, SEXPR& val )
{
   const SEXPR cdr_exp = cdr(exp);
   const SEXPR cadr_exp = car(cdr_exp);

   if (_symbolp(cadr_exp))
   {
      // (define x <exp>)
      var = cadr_exp;                // var = x
      val = car(cdr(cdr_exp));       // val = <exp>
   }
   else
   { 
      // (define (x <args>) ...)
      var = car(cadr_exp);           // var = x
      regstack.push( cons(cdr(cadr_exp), cdr(cdr_exp)) );  // ((<args>)...)
      val = cons(LAMBDA, null);      // val = (lambda)
      setcdr(val, regstack.pop());   //     = (lambda (<args> ...)
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

void EVAL::parse_formals( SEXPR formals, SEXPR& vars, BYTE& numv, BYTE& rargs )
{
   numv = 0;
   rargs = false;

   if (nullp(formals))
   {
      vars = null;
      return;
   }

   // varlist = (())
   SEXPR varlist = cons(null, null);

   // protect the varlist
   regstack.push(varlist);

   // validate and normalize the varlist
   while (anyp(formals))
   {
      SEXPR item;

      numv++;

      if (_symbolp(formals))
      {
	 rargs = true;      
	 item = cons(formals, null);
	 formals = null;
      }
      else
      {
	 const SEXPR fv = guard(car(formals), symbolp);
	 item = cons(fv, null);
	 formals = cdr(formals);
      }

      if (nullp(getcar(varlist)))
      {
	 // first item
	 setcar(varlist, item);
	 setcdr(varlist, item);
      }
      else
      {
	 // subsequent item
	 setcdr(getcdr(varlist), item);
	 setcdr(varlist, item);
      }
   }

   vars = getcar(varlist);

   regstack.pop();
}

void EVAL::set_closure_attributes( SEXPR closure, SEXPR formals )
{
   //
   // set the formal variable descriptor fields of the closure object:
   //
   //   #(... (<v1> <v2> ... <vN>))
   //
   //   Note: Position is zero-based.
   //
   parse_formals( formals, 
		  getclosurevars(closure),
		  getclosurenumv(closure),
		  getclosurerargs(closure) );
}

FRAME EVAL::create_frame( int nvars, SEXPR vars )
{
   // create a frame of size (nvars) with vars (vars)

   if ( nvars > 0 )
   {
      FRAME frame = MEMORY::frame(nvars);
      setframevars( frame, vars );
      return frame;
   }
   else
   {
      return nullptr;
   }
} 

static void arg_error( const char* text, unsigned n1, unsigned n2 )
{
   char msg[80];
   SPRINTF( msg, "%s -- actual=%u, expected=%u", text, n1, n2 );
   ERROR::severe( msg );
}

SEXPR EVAL::extend_env_fun( SEXPR closure )
{
   REGSTACK_CHECKER("extend-env-fun");
   //
   // extend the environment with the closure's vars
   // populate the frame with argstack values
   //

   // formal parameter attributes required:
   //   (<numv> <simple-var-list>)

   const auto nactual = static_cast<UINT32>(argstack.getargc());
   const auto nformal = static_cast<UINT32>(getclosurenumv(closure));
   const SEXPR benv = getclosurebenv(closure);
   const bool rargs = getclosurerargs(closure);

   if (nformal == 0)
   {
      if (nactual > 0)
	 arg_error( "too many arguments", nactual, nformal );

      return MEMORY::environment(nullptr, benv);
   }

   // create an extended environment
   SEXPR env = MEMORY::environment(nullptr, benv);
   regstack.push(env); 

   // create a new frame
   FRAME frame = create_frame(nformal, getclosurevars(closure));
   setenvframe(env, frame);

   if (rargs == false) 
   {
      // case I: no rest args
      //
      //   <fargs> := (a1 a2 ...)
      //
      if (nactual != nformal)
      {
	 if (nactual < nformal)
	    arg_error( "too few arguments", nactual, nformal );
	 else
	    arg_error( "too many arguments", nactual, nformal );
      }
     
      int p = argstack.getfirstargindex();
     
      // BIND required
      for (unsigned i = 0; i < nactual; ++i)
	 frameset(frame, i, argstack[p++]);
   }
   else
   {
      // case II: rest arg
      //
      //   <fargs> := (a1 a2 ... aN-1 . aN)
      //
      const unsigned nrequired = nformal - 1;

      if (nactual < nrequired)
	 arg_error( "too few arguments", nactual, nrequired );
     
      int p = argstack.getfirstargindex();
     
      // BIND required
      for (unsigned i = 0; i < nrequired; ++i)
	 frameset(frame, i, argstack[p++]);

      // BIND rest
      regstack.push(null);
 
      for (int i = p + (nactual - nformal); i >= p; --i)
	 regstack.top() = cons(argstack[i], regstack.top());
     
      frameset(frame, nrequired, regstack.pop());
   }

   argstack.removeargc();

   return regstack.pop();
}

SEXPR EVAL::extend_env_vars( SEXPR bindings, SEXPR benv )
{
   REGSTACK_CHECKER("extend-env-vars");
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
      SEXPR v = car(bindings);
      if ( consp(v) )
	 v = car(v);
      vars.add( v );
      bindings = cdr(bindings);
   }

   FRAME frame = create_frame( nvars, vars.get() );
   SEXPR xenv = MEMORY::environment( frame, benv );

   return xenv;
}


void EVAL::register_check( int id, PREDICATE pred, SEXPR reg )
{
   if ( !pred(reg) )
   {
      printf( "\ncheck(%u) failed: %p(k=%d)\n", id, reg->id(), nodekind(reg) );
   }
}


SEXPR EVAL::get_evaluator_state()
{
   const int rs_depth = regstack.getdepth();
   const int as_depth = argstack.getdepth();
   const int is_depth = intstack.getdepth();

   SEXPR evs = MEMORY::vector(3);
   regstack.push( evs );
   
   SEXPR regs = MEMORY::vector( rs_depth  );
   vectorset( evs, 0, regs );

   for (int i = 0; i < rs_depth; ++i )
      vectorset( regs, i, regstack[i] );

   SEXPR args = MEMORY::vector( as_depth );
   vectorset( evs, 1, args );

   for (int i = 0; i < as_depth; ++i )
      vectorset( args, i, argstack[i] );

   SEXPR ints = MEMORY::vector( is_depth );
   vectorset( evs, 2, ints );

   for (int i = 0; i < is_depth; ++i )
      vectorset( ints, i, MEMORY::fixnum(intstack[i]) );

   return regstack.pop();
}

static void eval_marker()
{
   // mark the evaluator objects
   MEMORY::mark(argstack);
   MEMORY::mark(regstack);
   MEMORY::mark(EVAL::exp);
   MEMORY::mark(EVAL::env);
   MEMORY::mark(EVAL::aux);
   MEMORY::mark(EVAL::val);
   MEMORY::mark(EVAL::unev);
}

void EVAL::initialize()
{
   // evaluator registers
   exp = null;
   env = null;
   val = null;
   aux = null;
   unev = null;

   cont = EV_DONE;
   next = EV_DONE;

   theGlobalEnv = null;

   // set the special form dispatch value
   setform( QUOTE,    EV_QUOTE );
   setform( DELAY,    EV_DELAY );
   setform( SET,      EV_SET );
   setform( CSET,     EV_CSET );
   setform( DEFINE,   EV_DEFINE );
   setform( IF,       EV_IF );
   setform( COND,     EV_COND );
   setform( LAMBDA,   EV_LAMBDA );
   setform( CLAMBDA,  EV_CLAMBDA );
   setform( BEGIN,    EV_BEGIN );
   setform( SEQUENCE, EV_BEGIN );
   setform( LET,      EV_LET );
   setform( LETREC,   EV_LETREC );
   setform( WHILE,    EV_WHILE );
   setform( AND,      EV_AND );
   setform( OR,       EV_OR );
   setform( ACCESS,   EV_ACCESS );
   setform( null,     EV_APPLICATION );

   MEMORY::register_marker( eval_marker );
}
