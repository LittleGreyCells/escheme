#include "eval.hxx"

#include "core/symtab.hxx"
#include "core/memory.hxx"
#include "core/printer.hxx"
#include "core/dict.hxx"

namespace escheme
{

static int frameindex = 0;

SEXPR EVAL::eceval( SEXPR sexpr )
{
   // sexpr valued
   exp = sexpr;
   env = null;
   val = null;
   aux = null;
   unev = null;

   // integer valued
   cont = EV_DONE;
   next = EVAL_DISPATCH;
   
#ifdef BYTE_CODE_EVALUATOR
   pc   = 0;
#endif

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
#ifdef BYTE_CODE_EVALUATOR
	 case EVAL_RETURN:
	    RESTORE_BCE_REGISTERS();
	    bceval();
	    break;

	 case EVAL_CODE:
	    bceval();
	    break;
#endif
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
		  val = lookup( exp, env );
		  next = cont;
		  break;
	       }

	       case n_cons:
	       {
		  // special form or function application
		  next = static_cast<EVSTATE>(getform(car(exp)));
		  break;
	       }

#ifdef BYTE_CODE_EVALUATOR
	       case n_code:
	       {
		  // compiled code evaluation
		  //   compiled code expects to exit with goto-cont
		  save( cont );
		  unev = exp;
		  pc = 0;
		  next = EVAL_CODE;
		  SAVE_RTE();
		  break;
	       }
#endif
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
	    save( cont );
	    unev = cdr(exp);      // args
	    exp = car(exp);       // callable
	    save( env );
	    save( unev );
	    cont = EVAL_ARGS;
	    next = EVAL_DISPATCH;
	    break;
	 }
	    
	 case EVAL_ARGS:
	 {
	    restore( unev );
	    restore( env );
	    argstack.argc = 0;
	    if ( nullp(unev) )
	    {
	       next = APPLY_DISPATCH;
	    }
	    else
	    {
	       save( val );
	       next = EVAL_ARG_LOOP;
	    }
	    break;
	 }
	    
	 case EVAL_ARG_LOOP:
	 {
	    exp = car(unev);
	    if ( lastp(unev) )
	    {
	       save( argstack.argc );
	       cont = ACCUMULATE_LAST_ARG;
	       next = EVAL_DISPATCH;
	    }
	    else
	    {
	       save( argstack.argc );
	       save( env );
	       save( unev );
	       cont = ACCUMULATE_ARG;
	       next = EVAL_DISPATCH;
	    }
	    break;
	 }
	    
	 case ACCUMULATE_ARG:
	 {
	    restore( unev );
	    restore( env );
	    restore( argstack.argc );
	    argstack.push(val);
	    unev = cdr(unev);
	    next = EVAL_ARG_LOOP;
	    break;
	 }

	 case ACCUMULATE_LAST_ARG:
	 {
	    restore( argstack.argc );
	    argstack.push(val);
	    restore( val );               // restore FUN
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
		  try
		  {
		     val = getprimfunc(val)();
		  }
		  catch ( ERROR::SevereError& )
		  {
		     PRINTER::print( val );
		     PRINTER::newline();
		     throw;
		  }
		  argstack.removeargc();
		  restore( cont );
		  next = cont;
		  break;
	       }

	       case n_closure:
	       {
		  env = extend_env_fun(val);
		  unev = getclosurecode(val);
#ifdef BYTE_CODE_EVALUATOR
		  if ( codep(unev) )
		  {
		     pc = 0;
		     next = EVAL_CODE;
		     SAVE_RTE();
		  }
		  else
		  {
		     next = EVAL_SEQUENCE;
		  }
#else
		  next = EVAL_SEQUENCE;
#endif
		  break;
	       }

	       case n_apply:
	       {
		  ArgstackIterator iter;
		  val = iter.getarg();
		  auto args = guard(iter.getlast(), listp);
		  argstack.removeargc();
		  for ( ; anyp(args); args = cdr(args) )
		     argstack.push( car(args) );
		  next = APPLY_DISPATCH;
		  break;
	       }

	       case n_eval:
	       {
		  ArgstackIterator iter;
		  exp = iter.getarg();
		  env = iter.more() ? guard(iter.getlast(), anyenvp) : null;
		  argstack.removeargc();
		  restore( cont );
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
		  auto ccresult = iter.more() ? iter.getlast() : null;
		  argstack.removeargc();
		  restore_continuation(val);
		  val = ccresult;
#ifdef BYTE_CODE_EVALUATOR
		  // determine if the continuation should resume here or in the BCE
		  if ( codep( regstack.top() ) )
		  {
		     next = EVAL_RETURN;
		  }
		  else
		  {
		     restore( cont );
		     next = cont;
		  }
#else
		  restore( cont );
		  next = cont;
#endif
		  break;
	       }

	       case n_map:
	       {
		  if ( argstack.argc < 2 )
		     ERROR::severe( "map requires two or more arguments" );
		  save( argstack.argc );
		  save( MEMORY::cons(null, null) );  // accume == (())
		  next = EV_MAP_APPLY;
		  break;
	       }

	       case n_foreach:
	       {
		  if ( argstack.argc < 2 )
		     ERROR::severe( "foreach requires two or more arguments" );
		  save( argstack.argc );
		  save( null );              // no accume == ()
		  next = EV_FOR_APPLY;
		  break;
	       }

	       case n_force:
	       {
		  ArgstackIterator iter;
		  auto promise = guard(iter.getlast(), promisep);
		  argstack.removeargc();
		  if ( nullp(promise_getexp(promise)) )
		  {
		     // already forced
		     val = promise_getval(promise);
		     restore( cont );
		     next = cont;   
		  }
		  else
		  {
		     // force the evaluation...
		     save( promise );
		     exp = promise_getexp(promise);
		     cont = EV_FORCE_VALUE;
		     next = EVAL_DISPATCH;
		  }
		  break;
	       }

	       default:
	       {
		  ERROR::severe( "[ece] not a callable", val );
		  break;
	       }
	    }
	    break;
	 }
	    
	 case EV_FORCE_VALUE:
	 {
	    // cache and return the value
	    restore( exp );
	    promise_setexp(exp, null);
	    promise_setval(exp, val);
	    restore( cont );
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
	       restore( val );            // val == (<list> . <last>)
	       val = car(val);                // val == <list>
	       restore( argstack.argc );
	       argstack.removeargc();
	       restore( cont );          // cont
	       next = cont;
	    }
	    else
	    {
	       // setup an application
	       const int argc = intstack.top();
	       const int p = argstack.gettop() - argc + 1;
	       val = argstack[p];             // FUN
	       argstack.argc = 0;
	       for ( int i = 1; i < argc; ++i )
	       {
		   const auto arg = argstack[p+i];
		   argstack.push( car(arg) );
		   argstack[p+i] = cdr(arg);
	       }
	       save( EV_MAP_RESULT );
	       next = APPLY_DISPATCH;
	    }
	    break;
	 }

	 case EV_MAP_RESULT:
	 {
	    // result is in regstack[top]
	    const auto x = MEMORY::cons(val, null);
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
	       restore( val );
	       restore( argstack.argc );
	       argstack.removeargc();
	       restore( cont );
	       next = cont;
	    }
	    else
	    {
	       // setup an application
	       const int argc = intstack.top();
	       const int p = argstack.gettop() - argc + 1;
	       val = argstack[p];             // FUN
	       argstack.argc = 0;
	       for ( int i = 1; i < argc; ++i )
	       {
		   const auto arg = argstack[p+i];
		   argstack.push( car(arg) );
		   argstack[p+i] = cdr(arg);
	       }
	       save( EV_FOR_APPLY );
	       next = APPLY_DISPATCH;
	    }
	    break;
	 }
	    
	 //
	 // syntax: (begin <sequence>)
	 // syntax: (sequence <sequence>)
	 //
	 case EV_BEGIN:
	 {
	    save( cont );
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
	    if ( nullp(unev) || lastp(unev) )
	    {
	       restore( cont );
	       next = EVAL_DISPATCH;
	    }
	    else
	    {
	       save( unev );
	       save( env );
	       cont = EVAL_SEQUENCE_BODY;
	       next = EVAL_DISPATCH;
	    }
	    break;
	 }
	    
	 case EVAL_SEQUENCE_BODY:
	 {
	    restore( env );
	    restore( unev );
	    unev = cdr(unev);
	    next = EVAL_SEQUENCE;
	    break;
	 }
	    
	 //
	 // syntax: (while <cond> <sequence>)
	 //
	 case EV_WHILE:
	 {
	    save( cont );
	    unev = cdr(exp);                  // (<cond> <sequence>)
	    save( env );                      // prep for cond eval
	    save( unev );
	    next = EVAL_WHILE_COND;
	    break;
	 }
  
	 case EVAL_WHILE_COND:
	 {
	    restore( unev );                    // FETCH <cond> evaluation context
	    restore( env );                     // 
	    exp = car(unev);                    // exp = <cond>
	    save( env );                        // SAVE the <cond> evaluation context
	    save( unev );                       // (<sequence>)
	    cont = EVAL_WHILE_BODY;
	    next = EVAL_DISPATCH;
	    break;
	 }
  
	 case EVAL_WHILE_BODY:
	 {
	    restore( unev );                    // (<cond> <sequence>)
	    restore( env );                     // RESTORE cond evaluation context
	    if ( truep(val) )
	    {
	       save( env );                     // SAVE the cond evaluation ENV
	       save( unev );                    // save (<cond> <sequence>)
	       exp = cdr(unev);
	       save( EVAL_WHILE_COND );         // setup EVAL_SEQUENCE to return above
	       next = EVAL_SEQUENCE;
	    }
	    else
	    {
	       restore( cont );
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
	    const auto var_exp = car(unev);
	    
	    if ( symbolp(var_exp) )
	    {
	       // (<var> <exp>)
	       exp = car(cdr(unev));             // <exp>
	       unev = var_exp;                   // unev == <var>
	       save( unev );
	       save( env );
	       save( cont );
	       cont = EV_SET_VALUE;
	       next = EVAL_DISPATCH;
	    }
	    else if ( consp(var_exp) && getcar(var_exp) == symbol_access )
	    {
	       // ((access <var> <env2>) <exp>)
	       exp = car(cdr(cdr(var_exp)));     // exp = <env2>
	       save( cont );
	       save( unev );
	       save( env );
	       cont = EV_SETACCESS_ENV;
	       next = EVAL_DISPATCH;             // evaluate <env2>
	    }
	    else
	       ERROR::severe( "not a valid target for set!" );  
	    break;
	 }
	 
	 case EV_SET_VALUE:
	 {
	    restore( cont );
	    restore( env );
	    restore( unev );
	    set_variable_value( unev, val, env );
	    next = cont;
	    break;
	 }
  
	 case EV_SETACCESS_ENV:
	 {
	    restore( env );
	    restore( unev );                   // unev == ((access <var> <env2>) <exp>)
	    exp = car(cdr(unev));              // exp = <exp>
	    unev = car(cdr(car(unev)));        // unev = <var>
	    save( val );                       // save(eval(<env2>))
	    save( unev );                      // save(<var>)
	    save( env );                       // save(<env>)
	    cont = EV_SETACCESS_VALUE;         // evaluate <exp>
	    next = EVAL_DISPATCH;
	    break;
	 }
    
	 case EV_SETACCESS_VALUE:
	 {
	    restore( env );                    // restore(<env>)
	    restore( unev );                   // restore(<var>)
	    restore( exp );                    // restore(eval(<env2>))
	    restore( cont );
	    set_variable_value( unev, val, exp );
	    next = cont;
	    break;
	 }	    

	 //
	 // syntax: (access <symbol> <env>)
	 //
	 case EV_ACCESS:
	 {
            const auto cdr_exp = cdr(exp);
            unev = car(cdr_exp);      // <symbol>
            exp = car(cdr(cdr_exp));  // <env>
	    save( unev );
	    save( env );
	    save( cont );
	    cont = EV_ACCESS_VALUE;
	    next = EVAL_DISPATCH;
	    break;
	 }

	 case EV_ACCESS_VALUE:
	 {
	    restore( cont );
	    restore( env );
	    restore( unev );
	    val = lookup( unev, val );   // unev=symbol, val=env
	    next = cont;
	    break;
	 }
	 
	 //
	 // syntax: (define <var> <exp>>
	 // syntax: (define (<var> [<param>...]) [<exp> ...])
	 //
	 case EV_DEFINE:
	 {
	    auto cdr_exp = cdr(exp);
            auto cadr_exp = car(cdr_exp);
	   
            if ( symbolp(cadr_exp) )
            {
                // (define <var> <exp>)
                unev = cadr_exp;
                exp = car(cdr(cdr_exp));
                save( unev );
                save( env );
                save( cont );
                cont = EV_DEFINE_VALUE;
                next = EVAL_DISPATCH;
            }
            else if ( consp(cadr_exp) )
            {
                // (define (<var> [<param>...]) [<exp> ...])
                unev = car(cadr_exp);                 // <var>
                save( unev );
                save( env );
                save( cont );
                // perform accelerated lambda creation
                unev = cdr(cadr_exp);             // params: ([<param>...])
                exp = cdr(cdr_exp);               // code: ([<exp>...])
                val = MEMORY::closure(exp, env);  // <code> <benv>
                parse_formals( unev, 
                               getclosurevars(val),
                               getclosurenumv(val),
                               getclosurerargs(val) );
                next = EV_DEFINE_VALUE;
            }
            else
                ERROR::severe( "ill-formed define", exp );
            break;
	 }
  
	 case EV_DEFINE_VALUE:
	 {
	    restore( cont );
	    restore( env );
	    restore( unev );
	    if ( nullp(env) )
	    {
	       // set in the global environment [()]
	       set( unev, val );
	    }
	    else if ( assocenvp(env) )
	    {
	       // set in the associate environment
	       dict_set( assocenv_getdict(env), unev, val );
	    }
	    else
	    {
	       ERROR::severe( "internal defines are not supported", unev );
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
            // params = car(cdr(exp))
            // code == cdr(cdr(exp))
	    exp = cdr(exp);
	    val = MEMORY::closure( cdr(exp), env );     // <code> <benv>
            parse_formals( car(exp), 
                           getclosurevars(val),
                           getclosurenumv(val),
                           getclosurerargs(val) );
	    next = cont;
	    break;
	 }
	    
	 //
	 // syntax: (cond (<exp> <sequence>)...)
	 //
	 case EV_COND:
	 {
	    save( cont );
	    unev = cdr(exp);
	    cont = EVCOND_DECIDE;
	    next = EVCOND_PRED;
	    break;
	 }
	    
	 case EVCOND_PRED:
	 {
	    if ( nullp(unev) )
	    {
	       restore( cont );
	       val = null;
	       next = cont;
	    }
	    else
	    {
	       exp = car(unev);
	       if ( car(exp) == symbol_else )
	       {
		  unev = cdr(exp);
		  next = EVAL_SEQUENCE;
	       }
	       else
	       { 
		  save( env );
		  save( unev );
		  exp = car(exp);
		  cont = EVCOND_DECIDE;
		  next = EVAL_DISPATCH;
	       }
	    }
	    break;
	 }
  
	 case EVCOND_DECIDE:
	 {
	    restore( unev );
	    restore( env );
	    if ( truep(val) )
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
	    save( cont );
	    cont = EVIF_DECIDE;
	    unev = cdr(exp);
	    exp = car(unev);
	    unev = cdr(unev);
	    save( env );
	    save( unev );
	    next = EVAL_DISPATCH;
	    break;
	 }
  
	 case EVIF_DECIDE:
	 {
	    restore( unev );
	    restore( env );
	    restore( cont );
	    exp = truep(val) ? car(unev) : car(cdr(unev));
	    next = EVAL_DISPATCH;
	    break;
	 }	    
	  
	 //
	 // syntax: (and <sequence>)
	 //
	 case EV_AND:
	 {
	    save( cont );
	    unev = cdr(exp);
	    next = EVAL_ANDSEQ;
	    break;
	 }
  
	 case EVAL_ANDSEQ:
	 {
	    exp = car(unev);
	    if ( nullp(unev) || lastp(unev) )
	    {
	       restore( cont );
	       next = EVAL_DISPATCH;
	    }
	    else
	    {
	       save( unev );
	       save( env );
	       cont = EVAL_ANDSEQ_FORK;
	       next = EVAL_DISPATCH;
	    }
	    break;
	 }
  
	 case EVAL_ANDSEQ_FORK:
	 {
	    restore( env );
	    restore( unev );
	    if ( falsep(val) )
	    {
	       restore( cont );
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
	    save( cont );
	    unev = cdr(exp);
	    next = EVAL_ORSEQ;
	    break;
	 }
  
	 case EVAL_ORSEQ:
	 {
	    exp = car(unev);
	    if ( nullp(unev) || lastp(unev) )
	    {
	       restore( cont );
	       next = EVAL_DISPATCH;
	    }
	    else
	    {
	       save( unev );
	       save( env );
	       cont = EVAL_ORSEQ_FORK;
	       next = EVAL_DISPATCH;
	    }
	    break;
	 }
  
	 case EVAL_ORSEQ_FORK:
	 {
	    restore( env );
	    restore( unev );
	    if ( truep(val) )
	    {
	       restore( cont );
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
	    save( cont );
	    exp = cdr(exp);
	    unev = car(exp);                      // bindings; ((v1 e1) (v2 e2) ...)
	    exp = cdr(exp);                       // body: (<body>)       
	    save( exp );                          // save the body
	    save( extend_env_vars( unev, env ) ); // save xenv
            // no additional allocations
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
	    if ( lastp(unev) )
	    {
	       save( frameindex );
	       cont = EV_LET_ACCUM_LAST_ARG;
	       next = EVAL_DISPATCH;
	    }
	    else
	    {
	       save( frameindex );
	       save( env );
	       save( unev );
	       cont = EV_LET_ACCUM_ARG;
	       next = EVAL_DISPATCH;
	    }  
	    break;
	 }
	 
	 case EV_LET_ACCUM_ARG:
	 {
	    restore( unev );
	    restore( env );
	    restore( frameindex );
	    if ( envp(regstack.top()) )
	       frameset( getenvframe(regstack.top()), frameindex, val );
	    frameindex += 1;
	    unev = cdr(unev);             // ((v2 e2) ...)
	    next = EV_LET_ARG_LOOP;
	    break;
	 }
	 
	 case EV_LET_ACCUM_LAST_ARG:
	 {
	    restore( frameindex );
	    if ( envp(regstack.top()) )
	       frameset( getenvframe(regstack.top()), frameindex, val );
	    next = EV_LET_BODY;
	    break;
	 }
	 
	 case EV_LET_BODY:
	 {
	    restore( env );            // assign env (benign for letrec)
	    restore( unev );           // restore (<body>)
	    next = EVAL_SEQUENCE;
	    break;
	 }

	 case EV_DONE:
	 {
	    return val;
	 }
	    
	 default:
	 {
	    return ERROR::severe("unknown/unexpected evaluation state", MEMORY::fixnum(next));
	 }
      }
   }
   
   return null;
}

}
