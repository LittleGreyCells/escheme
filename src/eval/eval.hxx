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

#ifndef NO_INTERP
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
#ifdef BYTE_CODE_EVALUATOR
   EVAL_CODE,
   EVAL_RETURN,
#endif
   EV_MODULE,
   EVAL_MODULE,
   EVAL_MODULE_BODY,
   EV_DONE,
   EV_SIZE
};
#endif


namespace EVAL
{
   // private:
   extern SEXPR exp;
   extern SEXPR env;
   extern SEXPR val;
   extern SEXPR aux;
   extern SEXPR unev;
#ifndef NO_INTERP
   extern EVSTATE cont;
   extern EVSTATE next;
#endif
#ifdef BYTE_CODE_EVALUATOR
   extern int pc;
   extern SEXPR map_code;
   extern SEXPR for_code;
   extern SEXPR fep_code;
   extern SEXPR rtc_code;
#ifndef NO_INTERP
   extern SEXPR rte_code;
#endif
#endif

   void initialize();
  
#ifndef NO_INTERP
   SEXPR eceval( SEXPR sexpr );
#endif
#ifdef BYTE_CODE_EVALUATOR
   void bceval();
   void bceval( SEXPR sexpr );
#endif

   inline SEXPR the_environment() { return env; }

   void statistics();
   SEXPR get_evaluator_state();

   SEXPR lookup( SEXPR var, SEXPR env );

   void set_variable_value( SEXPR var, SEXPR val, SEXPR env );
   void parse_formals( SEXPR formals, SEXPR& vars, INT32& numv, bool& rargs );

   SEXPR extend_env_fun( SEXPR closure );
   SEXPR extend_env_vars( SEXPR bindings, SEXPR benv );

   SEXPR create_continuation();
   void restore_continuation( SEXPR continuation );
}

#ifndef NO_INTERP
inline void save( EVSTATE x )  { intstack.push(int(x)); }
#endif
inline void save( int x )  { intstack.push(x); }
inline void save( SEXPR x ) { regstack.push(x); }
   
#ifndef NO_INTERP
inline void restore( EVSTATE& x ) { x = EVSTATE( intstack.pop() ); }
#endif
inline void restore( int& x ) { x = intstack.pop(); }
inline void restore( SEXPR& x ) { x = regstack.pop(); }

#ifdef BYTE_CODE_EVALUATOR
//
// Save and Restore the Byte Code Evaluator Registers
//

inline void SAVE_BCE_REGISTERS()
{
   save( EVAL::env );
   save( EVAL::unev );
   save( EVAL::pc );
}

inline void RESTORE_BCE_REGISTERS()
{
   restore( EVAL::pc );
   restore( EVAL::unev );
   restore( EVAL::env );
}

inline void SAVE_RTC()
{
   save( EVAL::env );
   save( EVAL::rtc_code );
   save( 0 );
}

#ifndef NO_INTERP
inline void SAVE_RTE()
{
   save( EVAL::env );
   save( EVAL::rte_code );
   save( 0 );
}
#endif
   
#endif

}

#endif

