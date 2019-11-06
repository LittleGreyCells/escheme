//
// Evaluator
//

#ifndef eval_hxx
#define eval_hxx

#include "core/sexpr.hxx"
#include "core/symtab.hxx"
#include "core/argstack.hxx"
#include "core/regstack.hxx"
#include "core/intstack.hxx"

namespace escheme
{

//
// Evaluator States
//
enum EVSTATE
{
   EV_APPLICATION = 0,   // keep this first
   EVAL_ARGS,
   EVAL_ARG_LOOP,
   ACCUMULATE_ARG,
   ACCUMULATE_LAST_ARG,
   EVAL_DISPATCH,
   EV_QUOTE,
   EV_DEFINE,
   EV_DEFINE_VALUE,
   EV_SET,
   EV_SET_VALUE,
   EV_SETACCESS_ENV,
   EV_SETACCESS_VALUE,
   EV_LAMBDA,
   EV_IF,
   EVIF_DECIDE,
   EV_AND,
   EV_OR,
   EVAL_ANDSEQ,	
   EVAL_ANDSEQ_FORK,	
   EVAL_ORSEQ,	
   EVAL_ORSEQ_FORK,
   EV_COND,
   EVCOND_PRED,
   EVCOND_DECIDE,
   EV_BEGIN,
   APPLY_DISPATCH,
   EVAL_SEQUENCE,
   EVAL_SEQUENCE_BODY,
   EV_LET,
   EV_LET_ARG_LOOP,
   EV_LET_ACCUM_ARG,
   EV_LET_ACCUM_LAST_ARG,
   EV_LET_BODY,
   EV_LETREC,
   EV_WHILE,
   EVAL_WHILE_COND,
   EVAL_WHILE_BODY,
   EV_ACCESS,
   EV_ACCESS_VALUE,
   EV_MAP_APPLY,
   EV_MAP_RESULT,
   EV_FOR_APPLY,
   EV_DELAY,
   EV_FORCE_VALUE,
   EV_DONE,
   EV_SIZE
};
 
namespace EVAL
{
   // private:
   extern SEXPR exp;
   extern SEXPR env;
   extern SEXPR val;
   extern SEXPR aux;
   extern SEXPR unev;
   extern EVSTATE cont;
   extern EVSTATE next;

   extern SEXPR theGlobalEnv;

   void initialize();
  
   SEXPR eceval( SEXPR sexpr );

   inline SEXPR the_environment() { return env; }

   void statistics();
   SEXPR get_evaluator_state();

   SEXPR lookup( SEXPR var, SEXPR env );

   void set_variable_value( SEXPR var, SEXPR val, SEXPR env );
   void parse_formals( SEXPR formals, SEXPR& vars, BYTE& numv, BYTE& rargs );
   void set_closure_attributes( SEXPR closure, SEXPR fvars );

   SEXPR extend_env_fun( SEXPR closure );
   SEXPR extend_env_vars( SEXPR bindings, SEXPR benv );

   SEXPR create_continuation();
   void restore_continuation( SEXPR continuation );

   void register_check( int id, PREDICATE pre, SEXPR reg );
}

inline void save_evs( EVSTATE x )  { intstack.push(int(x)); }
inline void save_int( int x )  { intstack.push(x); }
inline void save_reg( SEXPR x ) { regstack.push(x); }
inline void restore_evs( EVSTATE& x ) { x = EVSTATE( intstack.pop() ); }
inline void restore_int( int& x ) { x = intstack.pop(); }
inline void restore_reg( SEXPR& x ) { x = regstack.pop(); }

}

#endif

