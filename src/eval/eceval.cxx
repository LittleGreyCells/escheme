#include "eval.hxx"
#include "symtab.hxx"
#include "memory.hxx"
#include "printer.hxx"

#define DO_CHECK
#undef DO_CHECK

#ifdef DO_CHECK
#define CHECK( id, pred, reg ) check( id, pred, reg )
#else
#define CHECK( id, pred, reg )
#endif

/////////////////////////////////////////////////////////////
//
//           Explicit Control Evaluator (ECE)
//
//   Developed from the model evaluator described in SICP
//
//   "The Structure and Interpretation of Computer Programs"
//       by Abelson and Sussman
//
/////////////////////////////////////////////////////////////

//
// Unique surrogates for special primitives
//
//   syntax: (eval <expr> [<env>])
//   syntax: (apply <fn> <arglist>)
//   syntax: (call/cc <fn>)
//   syntax: (map <fn> <list>)
//   syntax: (foreach <fn> <list>)
//   syntax: (force <promise>)
//
// These functions are never called. The evaluator instead reconfigures
// itself and iterates. See apply_dispatch.
//

SEXPR EVAL::eval()    { return null; }
SEXPR EVAL::apply()   { return null; }
SEXPR EVAL::callcc()  { return null; }
SEXPR EVAL::map()     { return null; }
SEXPR EVAL::foreach() { return null; }
SEXPR EVAL::force()   { return null; }

//
// The Explicit Control Evaluator ENTRY
//
//   (see README file for discussion on efficiency)
//

#ifdef DO_CHECK
static bool anyenvp( SEXPR n ) { return nullp(n) || envp(n); }
#endif

static int frameindex = 0;

SEXPR EVAL::eceval( SEXPR sexpr )
{
   // sexpr valued
   exp = sexpr;
   env = theGlobalEnv;
   val = null;
   aux = null;
   unev = null;

   // integer valued
   cont = EV_DONE;
   next = EVAL_DISPATCH;

   regstack.flush();
   argstack.flush();
   intstack.flush();

   //
   // When analyzing forms *always* use the type-safe accessors:
   //   cons:   car, cdr 
   //   vector: vref
   //   symbol: value
   //
   
   while ( true )
   {
      switch ( next )
      {
	 case EVAL_DISPATCH:
	 {
            //
            // syntax: <symbol>
            // syntax: <self-evaluating>
            // syntax: ( ... )
            //
	    switch ( nodekind(exp) )
	    {
	       case n_symbol:
	       {
		  val = lookup(exp, env);
		  next = cont;
		  break;
	       }

	       case n_gref:
	       {
		  val = value(gref_getsymbol(exp));
		  if (val == SYMTAB::symbol_unbound)
		     ERROR::severe("symbol is unbound", gref_getsymbol(exp));
		  next = cont;
		  break;
	       }

	       case n_fref:
	       {
		  int d = fref_getdepth(exp);
		  SEXPR e = env;
		  while (d-- > 0)
		     e = getenvbase(e);
		  val = fref(getenvframe(e), fref_getindex(exp));
		  next = cont;
		  break;
	       }

	       case n_cons:
	       {
		  // special form or function application
		  next = static_cast<EVSTATE>(getform(car(exp)));
		  break;
	       }

	       default:
	       {
		  // self evaluating
		  val = exp;
		  next = cont;
		  break;
	       }
	    }
	    break;
	 }

	 case EV_QUOTE:
	 {
	    val = car(cdr(exp));
	    next = cont;
	    break;
	 }
	    
	 case EV_DELAY:
	 {
	    val = MEMORY::promise( car(cdr(exp)) );
	    next = cont;
	    break;
	 }
	    
	 ////////////////////////////////////////////////
	 //
	 // Function Application
	 //
	 ////////////////////////////////////////////////
	 //
	 // syntax: (<fun-expr> <farg>...)
	 //
	 case EV_APPLICATION:
	 {
	    save_evs(cont);
	    unev = cdr(exp);      // args
	    exp = car(exp);       // callable
	    save_reg(env);
	    save_reg(unev);
	    cont = EVAL_ARGS;
	    next = EVAL_DISPATCH;
	    break;
	 }
	    
	 case EVAL_ARGS:
	 {
	    restore_reg(unev);
	    restore_reg(env);
	    CHECK( 0, anyenvp, env );
	    argstack.argc = 0;
	    if ( nullp(unev) )
	    {
	       next = APPLY_DISPATCH;
	    }
	    else
	    {
	       save_reg(val);
	       next = EVAL_ARG_LOOP;
	    }
	    break;
	 }
	    
	 case EVAL_ARG_LOOP:
	 {
	    exp = car(unev);
	    if ( _lastp(unev) )
	    {
	       save_int(argstack.argc);
	       cont = ACCUMULATE_LAST_ARG;
	       next = EVAL_DISPATCH;
	    }
	    else
	    {
	       save_int(argstack.argc);
	       save_reg(env);
	       save_reg(unev);
	       cont = ACCUMULATE_ARG;
	       next = EVAL_DISPATCH;
	    }
	    break;
	 }
	    
	 case ACCUMULATE_ARG:
	 {
	    restore_reg(unev);
	    restore_reg(env);
	    CHECK( 1, anyenvp, env );
	    restore_int(argstack.argc);
	    argstack.push(val);
	    unev = cdr(unev);
	    next = EVAL_ARG_LOOP;
	    break;
	 }

	 case ACCUMULATE_LAST_ARG:
	 {
	    restore_int(argstack.argc);
	    argstack.push(val);
	    restore_reg(val);               // restore FUN
	    next = APPLY_DISPATCH;
	    break;
	 }
      
	 case APPLY_DISPATCH:
	 {
            //
            // syntax: (<primitive> ...)
            // syntax: (call/cc <expr>)
            // syntax: (apply <fn> <list>)
            // syntax: (eval <expr> [<env>])
            // syntax: (<continuation> [<value>])
            // syntax: (<closure> ...)
            // syntax: (map <func> <list>)
            // syntax: (foreach <func> <list>)
            // syntax: (force <promise>)
 	    //
	    switch ( nodekind(val) )
	    {
	       case n_func:
	       {
		  val = getfunc(val)();
		  argstack.removeargc();
		  restore_evs(cont);
		  next = cont;
		  break;
	       }

	       case n_closure:
	       {
		  env = extend_env_fun(val);
		  unev = getclosurecode(val);
		  next = EVAL_SEQUENCE;
		  break;
	       }

	       case n_apply:
	       {
		  ArgstackIterator iter;
		  val = iter.getarg();
		  SEXPR args = guard(iter.getlast(), listp);
		  argstack.removeargc();
		  for (; anyp(args); args = cdr(args))
		     argstack.push(car(args));
		  next = APPLY_DISPATCH;
		  break;
	       }

	       case n_eval:
	       {
		  ArgstackIterator iter;
		  exp = iter.getarg();
		  if (iter.more())
		  {
		     env = iter.getlast();
		     if (anyp(env))
			guard(env, envp); 
		  }
		  else
		  {
		     env = theGlobalEnv;
		  }
		  argstack.removeargc();
		  restore_evs(cont);
		  next = EVAL_DISPATCH;
		  break;
	       }

	       case n_callcc:
	       {
		  ArgstackIterator iter;
		  val = guard(iter.getlast(), closurep);
		  argstack.removeargc();
		  argstack.push( create_continuation() );
		  next = APPLY_DISPATCH;
		  break;
	       }

	       case n_continuation:
	       {
		  ArgstackIterator iter;
		  SEXPR ccresult = iter.more() ? iter.getlast() : null;
		  argstack.removeargc();
		  restore_continuation(val);
		  val = ccresult;
		  restore_evs(cont);
		  next = cont;
		  break;
	       }

	       case n_map:
	       {
		  if (argstack.argc != 2)
		     ERROR::severe( "map requires two arguments" );
		  save_int( argstack.argc );
		  save_reg( MEMORY::cons(null, null) );  // accume == (())
		  next = EV_MAP_APPLY;
		  break;
	       }

	       case n_foreach:
	       {
		  if (argstack.argc != 2)
		     ERROR::severe("foreach requires 2 arguments");
		  save_int( argstack.argc );
		  save_reg( null );              // no accume == ()
		  next = EV_FOR_APPLY;
		  break;
	       }

	       case n_force:
	       {
		  ArgstackIterator iter;
		  SEXPR promise = guard(iter.getlast(), promisep);
		  argstack.removeargc();
		  if ( nullp(getcar(promise)) )
		  {
		     // already forced
		     val = getcdr(promise);
		     restore_evs(cont);
		     next = cont;   
		  }
		  else
		  {
		     // force the evaluation...
		     save_reg( promise );
		     exp = getcar(promise);
		     cont = EV_FORCE_VALUE;
		     next = EVAL_DISPATCH;
		  }
		  break;
	       }

	       default:
	       {
		  ERROR::severe("[ece] not a callable", val);
		  break;
	       }
	    }
	    break;
	 }
	    
	 case EV_FORCE_VALUE:
	 {
	    // cache and return the value
	    restore_reg( exp );
	    CHECK( 2, promisep, exp );
	    setcar(exp, null);
	    setcdr(exp, val);
	    restore_evs(cont);
	    next = cont;
	    break;
	 }

	 ////////////////////////////////////////////////
	 // End Function Application
	 ////////////////////////////////////////////////
	 
	 //
	 // syntax: (map <func> <list>)
	 //
	 case EV_MAP_APPLY:
	 {
	    if ( nullp(argstack.top()) )
	    {
	       restore_reg( val );            // val == (<list> . <last>)
	       val = car(val);                // val == <list>
	       restore_int( argstack.argc );
	       argstack.removeargc();
	       restore_evs( cont );          // cont
	       next = cont;
	    }
	    else
	    {
	       // setup an application
	       argstack.argc = 0;
	       const int top = argstack.gettop();
	       val = argstack[top-1];                  // FUN
	       argstack.push( car(argstack[top]) );
	       save_evs( EV_MAP_RESULT );
	       next = APPLY_DISPATCH;
	    }
	    break;
	 }

	 case EV_MAP_RESULT:
	 {
	    // result is in regstack[top]
	    SEXPR x = MEMORY::cons(val, null);
	    const int top = regstack.gettop();
	    if ( nullp(car(regstack[top])) )
	    {
	       // val == (() . ())
	       setcar( regstack[top], x );
	       setcdr( regstack[top], x );
	       // val == (<firstpair> . <firstpair>)
	    }
	    else
	    {
	       // val == ((...<lastpair>) . <lastpair>)
	       setcdr( getcdr(regstack[top]), x );
	       setcdr( regstack[top], x );
	    }
	    argstack.top() = cdr( argstack.top() );
	    next = EV_MAP_APPLY;
	    break;
	 }
  
	 //
	 // syntax: (foreach <func> <list>)
	 //
	 case EV_FOR_APPLY:
	 {
	    if ( nullp(argstack.top()) )
	    {
	       restore_reg( val );
	       restore_int( argstack.argc );
	       argstack.removeargc();
	       restore_evs( cont );
	       next = cont;
	    }
	    else
	    {
	       // setup an application
	       argstack.argc = 0;
	       const int top = argstack.gettop();
	       val = argstack[top-1];                    // FUN
	       argstack.push( car(argstack[top]) );
	       save_evs( EV_FOR_RESULT );
	       next = APPLY_DISPATCH;
	    }
	    break;
	 }
	    
	 case EV_FOR_RESULT:
	 {
	    argstack.top() = cdr( argstack.top() );
	    next = EV_FOR_APPLY;
	    break;
	 }
	    
	 //
	 // syntax: (begin <sequence>)
	 // syntax: (sequence <sequence>)
	 //
	 case EV_BEGIN:
	 {
	    save_evs(cont);
	    unev = cdr(exp);
	    next = EVAL_SEQUENCE;
	    break;
	 }

	 //
	 // <exp>...
	 //
	 case EVAL_SEQUENCE:
	 {
	    exp = car(unev);
	    if (nullp(unev) || _lastp(unev))
	    {
	       restore_evs(cont);
	       next = EVAL_DISPATCH;
	    }
	    else
	    {
	       save_reg(unev);
	       save_reg(env);
	       cont = EVAL_SEQUENCE_BODY;
	       next = EVAL_DISPATCH;
	    }
	    break;
	 }
	    
	 case EVAL_SEQUENCE_BODY:
	 {
	    restore_reg(env);
	    restore_reg(unev);
	    CHECK( 3, anyenvp, env );
	    unev = cdr(unev);
	    next = EVAL_SEQUENCE;
	    break;
	 }
	    
	 //
	 // syntax: (while <cond> <sequence>)
	 //
	 case EV_WHILE:
	 {
	    save_evs(cont);
	    unev = cdr(exp);                      // (<cond> <sequence>)
	    save_reg(env);                        // prep for cond eval
	    save_reg(unev);
	    next = EVAL_WHILE_COND;
	    break;
	 }
  
	 case EVAL_WHILE_COND:
	 {
	    restore_reg(unev);                    // FETCH <cond> evaluation context
	    restore_reg(env);                     // 
	    CHECK( 4, anyenvp, env );
	    exp = car(unev);                      // exp = <cond>
	    save_reg(env);                        // SAVE the <cond> evaluation context
	    save_reg(unev);                       // (<sequence>)
	    cont = EVAL_WHILE_BODY;
	    next = EVAL_DISPATCH;
	    break;
	 }
  
	 case EVAL_WHILE_BODY:
	 {
	    restore_reg(unev);                    // (<cond> <sequence>)
	    restore_reg(env);                     // RESTORE cond evaluation context
	    CHECK( 5, anyenvp, env );
	    if ( truep(val) )
	    {
	       save_reg(env);                     // SAVE the cond evaluation ENV
	       save_reg(unev);                    // save (<cond> <sequence>)
	       exp = cdr(unev);
	       save_evs(EVAL_WHILE_COND);         // setup EVAL_SEQUENCE to return above
	       next = EVAL_SEQUENCE;
	    }
	    else
	    {
	       restore_evs(cont);
	       next = cont;
	    }
	    break;
	 }
  
	 //
	 // syntax: (set! <var> <exp>)
	 // syntax: (set! (access <var> <env2>) <exp>)
	 //
	 case EV_SET:
	 {
	    unev = cdr(exp);                     // (<var> <exp>) | ((access <var> <env2>) <exp>)
	    const SEXPR var_exp = car(unev);
	    
	    if (_symbolp(var_exp))
	    {
	       // (<var> <exp>)
	       exp = car(cdr(unev));             // <exp>
	       unev = var_exp;                   // unev == <var>
	       save_reg(unev);
	       save_reg(env);
	       save_evs(cont);
	       cont = EV_SET_VALUE;
	       next = EVAL_DISPATCH;
	    }
	    else if (_consp(var_exp) && getcar(var_exp) == ACCESS)
	    {
	       // ((access <var> <env2>) <exp>)
	       exp = car(cdr(cdr(var_exp)));     // exp = <env2>
	       save_evs(cont);
	       save_reg(unev);
	       save_reg(env);
	       cont = EV_SETACCESS_ENV;
	       next = EVAL_DISPATCH;             // evaluate <env2>
	    }
	    else
	       ERROR::severe("not a valid target for set!");  
	    break;
	 }
	 
	 case EV_SET_VALUE:
	 {
	    restore_evs(cont);
	    restore_reg(env);
	    restore_reg(unev);
	    CHECK( 6, anyenvp, env );
	    set_variable_value(unev, val, env);
	    next = cont;
	    break;
	 }
  
	 case EV_SETACCESS_ENV:
	 {
	    restore_reg(env);
	    restore_reg(unev);                   // unev == ((access <var> <env2>) <exp>)
	    CHECK( 7, anyenvp, env );
	    exp = car(cdr(unev));                // exp = <exp>
	    unev = car(cdr(car(unev)));          // unev = <var>
	    save_reg(val);                       // save(eval(<env2>))
	    save_reg(unev);                      // save(<var>)
	    save_reg(env);                       // save(<env>)
	    cont = EV_SETACCESS_VALUE;           // evaluate <exp>
	    next = EVAL_DISPATCH;
	    break;
	 }
    
	 case EV_SETACCESS_VALUE:
	 {
	    restore_reg(env);                    // restore(<env>)
	    restore_reg(unev);                   // restore(<var>)
	    restore_reg(exp);                    // restore(eval(<env2>))
	    restore_evs(cont);
	    CHECK( 8, anyenvp, env );
	    CHECK( 9, anyenvp, exp );
	    set_variable_value(unev, val, exp);
	    next = cont;
	    break;
	 }	    

	 //
	 // syntax: (access <symbol> <env>)
	 //
	 case EV_ACCESS:
	 {
	    {
	       const SEXPR cdr_exp = cdr(exp);
	       unev = car(cdr_exp);
	       exp = car(cdr(cdr_exp));
	    }
	    save_reg(unev);
	    save_reg(env);
	    save_evs(cont);
	    cont = EV_ACCESS_VALUE;
	    next = EVAL_DISPATCH;
	    break;
	 }

	 case EV_ACCESS_VALUE:
	 {
	    restore_evs(cont);
	    restore_reg(env);
	    restore_reg(unev);
	    CHECK( 10, anyenvp, env );
	    val = lookup(unev, val);   // unev=symbol, val=env
	    next = cont;
	    break;
	 }
	 
	 //
	 // syntax: (cset! <ref> <exp>)
	 //
	 case EV_CSET:
	 {
	    const SEXPR cdr_exp = cdr(exp);
	    const SEXPR ref_exp = car(cdr_exp);
	    
	    unev = ref_exp;                   // unev == <ref>
	    exp = car(cdr(cdr_exp));
	    save_reg(unev);
	    save_reg(env);
	    save_evs(cont);
	    cont = EV_CSET_VALUE;
	    next = EVAL_DISPATCH;
	    break;
	 }

	 case EV_CSET_VALUE:
	 {
	    restore_evs(cont);
	    restore_reg(env);
	    restore_reg(unev);
	    CHECK( 11, anyenvp, env );
	    // determine if the unev is a gref or fref
	    //   make the assignment
	    if (nodekind(unev) == n_gref)
	    {
	       set(gref_getsymbol(unev), val);
	    }
	    else
	    {
	       int d = fref_getdepth(unev);
	       SEXPR e = env;
	       while (d-- > 0)
		  e = getenvbase(e);
	       fset(getenvframe(e), fref_getindex(unev), val);
	    }	 
	    next = cont;
	    break;
	 }
  
	 //
	 // syntax: (define <var> <exp>>
	 //
	 case EV_DEFINE:
	 {
	    normalize_definition(exp, unev, exp);
	    save_reg(unev);
	    save_reg(env);
	    save_evs(cont);
	    cont = EV_DEFINE_VALUE;
	    next = EVAL_DISPATCH;
	    break;
	 }
  
	 case EV_DEFINE_VALUE:
	 {
	    restore_evs(cont);
	    restore_reg(env);
	    restore_reg(unev);
	    CHECK( 12, anyenvp, env );
	    if (nullp(env))
	    {
	       // set in the global environment [()]
	       set(unev, val);
	    }
	    else
	    {
	       ERROR::severe("nested defines not supported");
	    }
	    val = unev;
	    next = cont;
	    break;
	 }
  
	 //
	 // syntax: (lambda <fargs> <sequence>)
	 //
	 case EV_LAMBDA:
	 {
	    const SEXPR cdr_exp = cdr(exp);
	    const SEXPR params = car(cdr_exp);
	    const SEXPR code = cdr(cdr_exp);
	    val = MEMORY::closure(code, env);     // <code> <benv>
	    set_closure_attributes(val, params);
	    next = cont;
	    break;
	 }
	    
	 //
	 // syntax: (clambda <closure>)
	 //
	 case EV_CLAMBDA:
	 {
	    // compiled lambda
	    //   the argument <closure> must be an evaluated lambda/closure
	    //   there is a significant performance gain by pre-processing
	    //   the lambda, principally the parameter descriptor.
	    //   this is done once rather than on each invocation
	    SEXPR proc = guard(car(cdr(exp)), closurep);
	    // clone it 
	    val = MEMORY::closure(getclosurecode(proc), env);
	    setclosurevars(val, getclosurevars(proc));
	    setclosurenumv(val, getclosurenumv(proc));
	    setclosurerargs(val, getclosurerargs(proc));
	    next = cont;
	    break;
	 }
	    
	 //
	 // syntax: (cond (<exp> <sequence>)...)
	 //
	 case EV_COND:
	 {
	    save_evs(cont);
	    unev = cdr(exp);
	    cont = EVCOND_DECIDE;
	    next = EVCOND_PRED;
	    break;
	 }
	    
	 case EVCOND_PRED:
	 {
	    if (nullp(unev))
	    {
	       restore_evs(cont);
	       val = null;
	       next = cont;
	    }
	    else
	    {
	       exp = car(unev);
	       if (car(exp) == ELSE)
	       {
		  unev = cdr(exp);
		  next = EVAL_SEQUENCE;
	       }
	       else
	       { 
		  save_reg(env);
		  save_reg(unev);
		  exp = car(exp);
		  cont = EVCOND_DECIDE;
		  next = EVAL_DISPATCH;
	       }
	    }
	    break;
	 }
  
	 case EVCOND_DECIDE:
	 {
	    restore_reg(unev);
	    restore_reg(env);
	    CHECK( 13, anyenvp, env );
	    if (truep(val))
	    {
	       exp = car(unev);
	       unev = cdr(exp);
	       next = EVAL_SEQUENCE;
	    }
	    else
	    {
	       unev = cdr(unev);
	       next = EVCOND_PRED;
	    }
	    break;
	 }
  
	 //
	 // syntax: (if <expr> <then-expr> <else-expr>)
	 //
	 case EV_IF:
	 {
	    save_evs(cont);
	    cont = EVIF_DECIDE;
	    unev = cdr(exp);
	    exp = car(unev);
	    unev = cdr(unev);
	    save_reg(env);
	    save_reg(unev);
	    next = EVAL_DISPATCH;
	    break;
	 }
  
	 case EVIF_DECIDE:
	 {
	    restore_reg(unev);
	    restore_reg(env);
	    restore_evs(cont);
	    CHECK( 14, anyenvp, env );
	    exp = truep(val) ? car(unev) : car(cdr(unev));
	    next = EVAL_DISPATCH;
	    break;
	 }	    
	  
	 //
	 // syntax: (and <sequence>)
	 //
	 case EV_AND:
	 {
	    save_evs(cont);
	    unev = cdr(exp);
	    next = EVAL_ANDSEQ;
	    break;
	 }
  
	 case EVAL_ANDSEQ:
	 {
	    exp = car(unev);
	    if (nullp(unev) || _lastp(unev))
	    {
	       restore_evs(cont);
	       next = EVAL_DISPATCH;
	    }
	    else
	    {
	       save_reg(unev);
	       save_reg(env);
	       cont = EVAL_ANDSEQ_FORK;
	       next = EVAL_DISPATCH;
	    }
	    break;
	 }
  
	 case EVAL_ANDSEQ_FORK:
	 {
	    restore_reg(env);
	    restore_reg(unev);
	    CHECK( 15, anyenvp, env );
	    if (falsep(val))
	    {
	       restore_evs(cont);
	       next = cont;
	    }
	    else
	    {
	       unev = cdr(unev);
	       next = EVAL_ANDSEQ;
	    }
	    break;
	 }
  
	 //
	 // syntax: (or <sequence>)
	 //
	 case EV_OR:
	 {
	    save_evs(cont);
	    unev = cdr(exp);
	    next = EVAL_ORSEQ;
	    break;
	 }
  
	 case EVAL_ORSEQ:
	 {
	    exp = car(unev);
	    if (nullp(unev) || _lastp(unev))
	    {
	       restore_evs(cont);
	       next = EVAL_DISPATCH;
	    }
	    else
	    {
	       save_reg(unev);
	       save_reg(env);
	       cont = EVAL_ORSEQ_FORK;
	       next = EVAL_DISPATCH;
	    }
	    break;
	 }
  
	 case EVAL_ORSEQ_FORK:
	 {
	    restore_reg(env);
	    restore_reg(unev);
	    CHECK( 16, anyenvp, env );
	    if (truep(val))
	    {
	       restore_evs(cont);
	       next = cont;
	    }
	    else
	    {
	       unev = cdr(unev);
	       next = EVAL_ORSEQ;
	    }
	    break;
	 }
	    
	 //
	 // syntax: (let <bindings> <body>)
	 // syntax: (letrec <bindings> <body>)
	 //
	 //   let and letrec can use the same arg processing.
	 //   the essential difference is when the new env
	 //   is assigned -- before or after arg evaluation.
	 //
	 case EV_LET:
	 case EV_LETREC:
	 {
	    save_evs(cont);
	    const SEXPR cdr_exp = cdr(exp);
	    unev = car(cdr_exp);                      // bindings; ((v1 e1) (v2 e2) ...)
	    exp = cdr(cdr_exp);                       // body: (<body>)       
	    save_reg( exp );                          // save the body
	    save_reg( extend_env_vars( unev, env ) ); // save xenv
	    if ( next == EV_LETREC )
	       env = regstack.top();
	    frameindex = 0;
	    next = nullp(unev) ? EV_LET_BODY : EV_LET_ARG_LOOP;
	    break;
	 }
	 	 
	 case EV_LET_ARG_LOOP:
	 {
	    exp = car(unev);               // exp = (v e) | v
	    if ( consp(exp) )
	    {
	       // exp == (v e)
	       exp = car(cdr(exp));        // e
	    }
	    else
	    {
	       // exp == v
	       exp = null;                 // ()
	    }
	    if ( _lastp(unev) )
	    {
	       save_int(frameindex);
	       cont = EV_LET_ACCUM_LAST_ARG;
	       next = EVAL_DISPATCH;
	    }
	    else
	    {
	       save_int(frameindex);
	       save_reg(env);
	       save_reg(unev);
	       cont = EV_LET_ACCUM_ARG;
	       next = EVAL_DISPATCH;
	    }  
	    break;
	 }
	 
	 case EV_LET_ACCUM_ARG:
	 {
	    restore_reg(unev);
	    restore_reg(env);
	    CHECK( 20, envp, env );
	    restore_int(frameindex);
	    if ( envp(regstack.top()) )
	       frameset( getenvframe(regstack.top()), frameindex, val );
	    frameindex += 1;
	    unev = cdr(unev);             // ((v2 e2) ...)
	    next = EV_LET_ARG_LOOP;
	    break;
	 }
	 
	 case EV_LET_ACCUM_LAST_ARG:
	 {
	    restore_int(frameindex);
	    if ( envp(regstack.top()) )
	       frameset( getenvframe(regstack.top()), frameindex, val );
	    next = EV_LET_BODY;
	    break;
	 }
	 
	 case EV_LET_BODY:
	 {
	    restore_reg(env);            // assign env (benign for letrec)
	    restore_reg(unev);           // restore (<body>)
	    CHECK( 21, anyenvp, env );
	    next = EVAL_SEQUENCE;
	    break;
	 }

	 case EV_DONE:
	 {
	    return val;
	 }
	    
	 default:
	 {
	    ERROR::severe("unknown/unexpected evaluation state", MEMORY::fixnum(next));
	    break;
	 }
      }
   }
   
   return null;
}

// the number of single entries in the continuation
//   these precede the stack contents
const int ContSingletons = 3;

// the number of byte vector locations reserved for saved int values
const int BvReserved = 3;


SEXPR EVAL::create_continuation()
{
   // allocate and populate the 'continuation'
   //   (it is assumed that 'cont' is protected from gc)
   const int regs_depth = regstack.getdepth();
   const int args_depth = argstack.getdepth();
   const int ints_depth = intstack.getdepth();

   const int cont_len = ContSingletons + regs_depth + args_depth;

   SEXPR cc = MEMORY::continuation(cont_len);
   regstack.push(cc);
   
   // byte vector
   //   length accomodates the intstack and the three(3) stack depth values
   const int ByteVectorLength = ints_depth + BvReserved;
   SEXPR bv = MEMORY::byte_vector( ByteVectorLength*sizeof(INT16) );
   INT16* pint16 = reinterpret_cast<INT16*>(getbvecdata(bv));

   pint16[0] = regs_depth;
   pint16[1] = args_depth;
   pint16[2] = ints_depth;

   vectorset(cc, 0, env);
   vectorset(cc, 1, unev);
   vectorset(cc, 2, bv);
   
   int j = ContSingletons;

   for (int i = 0; i < regs_depth; ++i)
      vectorset(cc, j++, regstack[i]);
   
   for (int i = 0; i < args_depth; ++i)
      vectorset(cc, j++, argstack[i]); 
   
   for (int i = 0; i < ints_depth; ++i)
      pint16[BvReserved+i] = intstack[i];
   
   return regstack.pop();
}

void EVAL::restore_continuation( SEXPR cc )
{
   env = vectorref(cc, 0);
   unev = vectorref(cc, 1);
   SEXPR bv = vectorref(cc, 2);

   const INT16* pint16 = reinterpret_cast<INT16*>(getbvecdata(bv));

   const int regs_depth = pint16[0];
   const int args_depth = pint16[1];
   const int ints_depth = pint16[2];

   int j = ContSingletons;
   
   for (int i = 0; i < regs_depth; ++i)
      regstack[i] = vectorref(cc, j++);
   
   for (int i = 0; i < args_depth; ++i)
      argstack[i] = vectorref(cc, j++);
      
   for (int i = 0; i < ints_depth; ++i)
      intstack[i] = pint16[BvReserved+i];
   
   regstack.newtop(regs_depth);
   argstack.newtop(args_depth);
   intstack.newtop(ints_depth);
}
