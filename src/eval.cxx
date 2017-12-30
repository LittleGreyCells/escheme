#include <cstdlib>

#include "eval.hxx"
#include "error.hxx"
#include "symtab.hxx"
#include "memory.hxx"
#include "printer.hxx"

#define TRACE_LET
#undef TRACE_LET

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

SEXPR EVAL::listbuilder;

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
	 SEXPR vars = frame->vars;

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
	 SEXPR vars = frame->vars;

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
      frame->vars = vars;

      for (int i = 0; i < nvars; ++i)
	 frameset(frame, i, null);

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
	 else if (nactual > nformal)
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

#if 0
static void print_bindings( SEXPR env )
{
   int depth = 0;

   while ( anyp(env) )
   {
      FRAME frame = getenvframe(env);

      if (frame)
      {
	 SEXPR vars = getframevars(frame);

	 for ( int i = 0; i < static_cast<int>(getframenslots(frame)); ++i )
	 {
	    printf("slot[%d:%d]: ", depth, i);
	    PRINTER::print( car(vars) );
	    printf(" ");
	    PRINTER::print( frameref(frame,i) );
	    PRINTER::newline();
	    vars = cdr(vars);
	 }
      }

      printf( "--\n" );
      env = getenvbase(env);
      depth += 1;
   }
}
#endif

SEXPR EVAL::extend_env_vars( SEXPR bindings, SEXPR benv )
{
   REGSTACK_CHECKER("extend-env-vars");
   //
   // extend the environment with let/letrec vars
   //   bindings = (<binding> ...)
   //   binding = (v e) | v
   //

   if ( nullp(bindings) )
      return benv;

   SEXPR vars = listbuilder;
   setcdr(vars, null);

   int nvars = 0;

   while ( anyp(bindings) )
   {
      nvars++;
      SEXPR v = car(bindings);
      if ( consp(v) )
	 v = car(v);
      setcdr( vars, cons(v, null) );
      vars = getcdr(vars);
      bindings = cdr(bindings);
   }

   FRAME frame = create_frame( nvars, getcdr(listbuilder) );
   SEXPR xenv = MEMORY::environment( frame, benv );

   return xenv;
}


///////////////////////////////////////////////
//
// let transformers
//
//////////////////////////////////////////////

SEXPR EVAL::transform_let( SEXPR exp )
{
   //
   // let
   //     
   // (let ((<v1> <e1>) (<v2> <e2>) ... ) 
   //    <body> )
   //
   //   ==>
   //     
   //   ((lambda (<v1> <v2> ...) <body>) <e1> <e2> ...)
   //

   REGSTACK_CHECKER("transform-let");

   const SEXPR cdr_exp = cdr(exp);

   SEXPR bindings = car(cdr_exp);
   SEXPR body = cdr(cdr_exp);

   // reserve space for the application
   const SEXPR application = cons(null, null);
   regstack.push(application);                     // [1] ( () )

   // construct the (lambda)
   const SEXPR lambda = cons(LAMBDA, null);
   // rplaca(regstack.top(), lambda);                 // [1] ( (lambda) )
   rplaca(application, lambda);                    // [1] ( (lambda) )

   // reserve space for (<formals> . <body>)
   const SEXPR lambda2 = cons(null, null);
   rplacd(lambda, lambda2);                        // [1] ( (lambda ()) ) 

   // reserve space for <actuals>
   regstack.push(null);                            // [2] ()
   const int a_index = regstack.gettop();

   // reserve space for <formals>
   regstack.push(null);                            // [3] ()
   const int f_index = regstack.gettop();

   int n = 0;

   for (; anyp(bindings); bindings = cdr(bindings) )
   {
      n++;
      const SEXPR b = car(bindings);

      if (consp(b))
      {
	 regstack.push(car(b));                    // <v> = (car '(<v> <e>))
	 regstack.push(car(cdr(b)));               // <e> = (cadr '(<v> <e>))
      }
      else
      {
	 regstack.push(b);                         // <v>
	 regstack.push(null);                      // '()
      }
   }
  
   while (n-- > 0)
   {
      const SEXPR actual = regstack.top();
      regstack[a_index] = cons(actual, regstack[a_index]);  // accume actuals
      regstack.pop();

      const SEXPR formal = regstack.top();
      regstack[f_index] = cons(formal, regstack[f_index]);  // accume formals
      regstack.pop();
   }
 
   // assemble it!
  
   // construct the lambda -- (lambda <formals> <body>)
   rplaca(lambda2, regstack[f_index]);
   rplacd(lambda2, body);

   // construct the application
   // ( (lambda <formals> <body>) <actuals> )
   rplacd(application, regstack[a_index]);

   regstack.pop();        // remove [3] formals
   regstack.pop();        // remove [2] actuals
   return regstack.pop(); // remove [1] application
}

SEXPR EVAL::transform_letrec( SEXPR exp )
{
   //
   // letrec
   //     
   // (letrec ((<v1> <e1>) (<v2> <e2>) ... (<vn> <en>) ) 
   //   <body> )
   //
   //   ==>
   //     
   //   (let (<vn> ... <v2> <v1>)
   //     (set! <v1> <e1>)
   //     (set! <v2> <e2>)
   //     ...
   //     (set! <vn> <en>)
   //     <body> )
   //

   REGSTACK_CHECKER("transform-letrec2");

   const SEXPR cdr_exp = cdr(exp);
   SEXPR bindings = car(cdr_exp);
   SEXPR body = cdr(cdr_exp);

   // reserve space for <actuals>
   regstack.push(null);                            // [1] ()
   const int a_index = regstack.gettop();

   // reserve space for <formals>
   regstack.push(null);                            // [2] ()
   const int f_index = regstack.gettop();

   // reserve space for extended <body>
   regstack.push(body);                            // [3] <body>
   const int b_index = regstack.gettop();

   int n = 0;

   for (; anyp(bindings); bindings = cdr(bindings) )
   {
      n++;
      const SEXPR b = car(bindings);

      if (consp(b))
      {
	 regstack.push(car(b));                    // <v> = (car '(<v> <e>))
	 regstack.push(car(cdr(b)));               // <e> = (cadr '(<v> <e>))
      }
      else
      {
	 regstack.push(b);                         // <v>
	 regstack.push(null);                      // '()
      }
   }
  
   while ( n-- > 0 )
   {
      const SEXPR actual = regstack.top();
      regstack[a_index] = cons(actual, regstack[a_index]);  // accume actuals
      regstack.pop();

      const SEXPR formal = regstack.top();
      regstack[f_index] = cons(formal, regstack[f_index]);  // accume formals
      regstack.pop();

      // (set! formal actual)
      regstack.push(cons(SET, null));
      regstack.push(cons(formal, null));
      rplacd(regstack.top(), cons(actual, null));
      
      const SEXPR p = regstack.pop();
      rplacd(regstack.top(), p);
      
      // body = cons( set, body );
      regstack[b_index] = cons(regstack.top(), regstack[b_index]);
      regstack.pop();
   }
 
   // assemble the let

   // (let <formals> <body>)
   SEXPR let = cons(LET, null);

   regstack.push( let );  // [4] <let>
   rplacd( let, cons(regstack[f_index], regstack[b_index]) );

   regstack.pop();        // remove [4] <let>
   regstack.pop();        // remove [3] <body>
   regstack.pop();        // remove [2] <formals>
   regstack.pop();        // remove [1] <actuals>

   return let;
}

SEXPR EVAL::transform_letstar( SEXPR exp )
{
   //
   // let*
   //     
   // (let* ((<v1> <e1>) (<v2> <e2>) ... ) <body> ) ==>
   //     
   //   (let ((<v1> <e1>))
   //     (let ((<v2> <e2>))
   //        ...
   //       (begin <body>) ))
   //
   // note: we transform (let* () <body>) -> (begin <body>)
   //
  
   REGSTACK_CHECKER("transform-let*");

   SEXPR cdr_exp = cdr(exp);
   SEXPR bindings = car(cdr_exp);
   SEXPR body = cdr(cdr_exp);

   // preserve body
   regstack.push(cons(BEGIN, body));                         // [1] (BEGIN <body>)
   const int p = regstack.gettop();

   int n = 0;

   for ( ; anyp(bindings); bindings = cdr(bindings))
   {
      n++;
      regstack.push(car(bindings));                          // (si vi)
   }
  
   // Address the rest
   for ( int i = 0; i < n; ++i )
   {
      // new binding at level
      regstack.top() = cons(regstack.top(), null);           // ((si vi))
      regstack[p]    = cons(regstack[p], null);              // (<rest>)
      regstack.top() = cons(regstack.top(), regstack[p]);    // (((si vi)) <rest>)
      regstack.top() = cons(LET, regstack.top());            // (LET <binding> <rest>)
      regstack[p]    = regstack.pop();
   }

   return regstack.pop();
}


#if 0
#define CASEN(state, label) case state: return #label;

static const char* image( EVSTATE state )
{
   switch (state)
   {
      CASEN(EV_APPLICATION, Apply);
      CASEN(EVAL_ARGS, Args);
      CASEN(EVAL_ARG_LOOP, ArgLoop);
      CASEN(ACCUMULATE_ARG, AccumArg);
      CASEN(ACCUMULATE_LAST_ARG, AccumLast);
      CASEN(EVAL_DISPATCH, Dispatch);
      CASEN(EV_QUOTE, Quote);
      CASEN(EV_DEFINE, Def);
      CASEN(EV_DEFINE_VALUE, DefValue);
      CASEN(EV_SET, Set);
      CASEN(EV_SET_VALUE, SetValue);
      CASEN(EV_SETACCESS_ENV, SetAccessEnv);
      CASEN(EV_SETACCESS_VALUE, SetAccessValue);
      CASEN(EV_CSET, CSet);
      CASEN(EV_CSET_VALUE, CSetValue);
      CASEN(EV_LAMBDA, Lambda);
      CASEN(EV_CLAMBDA, CLambda);
      CASEN(EV_IF, If);
      CASEN(EVIF_DECIDE, IfDecide);
      CASEN(EV_AND, And);
      CASEN(EV_OR, Or);
      CASEN(EVAL_ANDSEQ, AndSeq);	
      CASEN(EVAL_ANDSEQ_FORK, AndSeqFork);	
      CASEN(EVAL_ORSEQ, OrSeq);	
      CASEN(EVAL_ORSEQ_FORK, OrSeqFork);	
      CASEN(EV_COND, Cond);
      CASEN(EVCOND_PRED, CondOred);
      CASEN(EVCOND_DECIDE, CondDecide);
      CASEN(EV_BEGIN, Begin);
      CASEN(APPLY_DISPATCH, ApplyDispatch);
      CASEN(EVAL_SEQUENCE, Seq);
      CASEN(EVAL_SEQUENCE_BODY, SeqBody);
      CASEN(EV_LET, Let);
      CASEN(EV_LETREC, Letrec);
      CASEN(EV_LETSTAR, Letstar);
      CASEN(EV_WHILE, While);
      CASEN(EVAL_WHILE_COND, WhileCond);
      CASEN(EVAL_WHILE_BODY, WhileBody);
      CASEN(EV_ACCESS, Access);
      CASEN(EV_ACCESS_VALUE, AccessValue);
      CASEN(EV_MAP_APPLY, MapApply);
      CASEN(EV_MAP_RESULT, MapResult);
      CASEN(EV_FOR_APPLY, ForApply);
      CASEN(EV_FOR_RESULT, ForResult);
      CASEN(EV_DELAY, Delay);
      CASEN(EV_FORCE_VALUE, ForceValue);
      CASEN(EV_DONE, Done);
      default:
	 return "<unknown-evstate>";
   }
}
#endif

void EVAL::check( int id, PREDICATE pred, SEXPR reg )
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
   MEMORY::mark(EVAL::listbuilder);
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
   setform( LETSTAR,  EV_LETSTAR );
   setform( WHILE,    EV_WHILE );
   setform( AND,      EV_AND );
   setform( OR,       EV_OR );
   setform( ACCESS,   EV_ACCESS );
   setform( null,     EV_APPLICATION );

   listbuilder = MEMORY::cons(null, null);

   MEMORY::register_marker( eval_marker );
}
