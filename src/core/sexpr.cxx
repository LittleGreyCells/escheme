#include <cstdio>
#include <cstring>
#include <vector>

#include "sexpr.hxx"
#include "memory.hxx"
#include "error.hxx"
#include "format.hxx"

namespace escheme
{

/////////////////////////////////////////////////////////////////
//
// Accessors/Modifiers
//
/////////////////////////////////////////////////////////////////

char* name( const SEXPR n ) { return getname(guard(n, symbolp)); }

FIXNUM fixnum( const SEXPR n ) { return getfixnum(guard(n, fixnump)); }
FLONUM flonum( const SEXPR n ) { return getflonum(guard(n, flonump)); }

SEXPR car( const SEXPR n ) { return anyp(n) ? getcar(guard(n, consp)) : null; }
SEXPR cdr( const SEXPR n ) { return anyp(n) ? getcdr(guard(n, consp)) : null; }

SEXPR nthcar( const SEXPR list, UINT32 n )
{
   SEXPR s = list;
   for (int i = static_cast<int>(n); i > 0 && consp(s); s = getcdr(s))
      --i;
   return car(s);
}

SEXPR nthcdr( const SEXPR list, UINT32 n )
{
   SEXPR s = list;
   for (int i = static_cast<int>(n); i > 0 && consp(s); s = getcdr(s))
      --i;
   return cdr(s);
}

void  vset( SEXPR v, UINT32 index, SEXPR value ) { vectorset(guard(v,vectorp), index, value); }
SEXPR vref( SEXPR v, UINT32 index ) { return vectorref(guard(v,vectorp), index); }

SEXPR value( SEXPR n ) { return getvalue(guard(n, symbolp)); }

SEXPR set( SEXPR n, SEXPR value ) 
{ 
   setvalue(guard(n, symbolp), value);
   return n;
}

UINT32 list_length( const SEXPR x )
{
   SEXPR s = x;
   UINT32 length = 0;
  
   // while a pair
   for ( ; consp(s); s = getcdr(s) )
      ++length;

   return length;
}

void fset( FRAME frame, UINT32 index, SEXPR value )
{
   if ( index >= getframenslots(frame) )
      ERROR::severe( "fset range error");

   frameset( frame, index, value );
}

SEXPR fref( FRAME frame, UINT32 index )
{
   if ( index >= getframenslots(frame) )
      ERROR::severe( "fref range error");

   return frameref( frame, index );
}

/////////////////////////////////////////////////////////////////
//
// Predicates
//
/////////////////////////////////////////////////////////////////

bool symbolp( const SEXPR n ) { return n->kind == n_symbol; }

bool fixnump( const SEXPR n ) { return n->kind == n_fixnum; }
bool flonump( const SEXPR n ) { return n->kind == n_flonum; }
bool numberp( const SEXPR n ) { return n->kind == n_fixnum || n->kind == n_flonum; }

bool stringp( const SEXPR n ) { return n->kind == n_string; }
bool charp( const SEXPR n ) { return n->kind == n_char; }
bool vectorp( const SEXPR n ) { return n->kind == n_vector; }
bool consp( const SEXPR n ) { return n->kind == n_cons; }
bool funcp( const SEXPR n ) { return n->kind == n_func; }
bool portp( const SEXPR n ) { return n->kind == n_port; }
bool stringportp( const SEXPR n ) { return n->kind == n_string_port; }

bool closurep( const SEXPR n ) { return n->kind == n_closure; }

bool specialp( const SEXPR n ) { return n->kind == n_eval || 
                                        n->kind == n_callcc || 
                                        n->kind == n_apply || 
                                        n->kind == n_map ||
                                        n->kind == n_foreach ||
                                        n->kind == n_force; }

bool primp( const SEXPR n ) { return funcp(n) || specialp(n); }

bool contp( const SEXPR n ) { return n->kind == n_continuation; }
bool envp( const SEXPR n ) { return n->kind == n_environment; }
bool bvecp( const SEXPR n ) { return n->kind == n_bvec; }
bool listp( const SEXPR n ) { return nullp(n) || n->kind == n_cons; }
bool atomp( const SEXPR n ) { return nullp(n) || n->kind != n_cons; }

bool inportp( const SEXPR n ) { return portp(n) && (getmode(n) & pm_input); }
bool outportp( const SEXPR n ) { return portp(n) && (getmode(n) & pm_output); }
bool instringportp( const SEXPR n ) { return stringportp(n) && (getmode(n) & pm_input); }
bool outstringportp( const SEXPR n ) { return stringportp(n) && (getmode(n) & pm_output); }
bool anyinportp( const SEXPR n ) { return inportp(n) || instringportp(n); }
bool anyoutportp( const SEXPR n ) { return outportp(n) || outstringportp(n); }

bool promisep( SEXPR n )  { return n->kind == n_promise; }
bool codep( const SEXPR n ) { return n->kind == n_code; }
bool dictp( const SEXPR n ) { return n->kind == n_dict; }
bool assocenvp( const SEXPR n ) { return n->kind == n_assoc_env; }
   
bool anyenvp( const SEXPR n ) { return nullp(n) || envp(n) || assocenvp(n); }

struct PredMap { PREDICATE pred; const char* name; };

std::vector<PredMap> predicate_map =
{
   { symbolp, "symbol" },
   { fixnump, "fixnum" },
   { flonump, "flonum" },
   { numberp, "number" },
   { stringp, "string" },
   { charp, "char" },
   { vectorp, "vector" },
   { consp, "pair" },
   { funcp, "func" },
   { portp, "port" },
   { stringportp, "string port" },
   { closurep, "closure" },
   { specialp, "special" },
   { contp, "continuation" },
   { envp, "environment" },
   { bvecp, "byte vector" },
   { listp, "list" },
   { atomp, "atom" },
   { inportp, "input port" },
   { outportp, "output port" },
   { instringportp, "input string port" },
   { outstringportp, "output string port" },
   { anyinportp, "any input port" },
   { anyoutportp, "any output port" },
   { lastp, "last argument" },
   { promisep, "promise" },
   { primp, "func or special" },
   { anyenvp, "any environment" },
   { codep, "compiled code" },
   { dictp, "dictionary" },
   { assocenvp, "associative environment" },
};

SEXPR guard( SEXPR s, PREDICATE predicate )
{
   if ( !predicate(s) )
   {
      const char* expected = "<unknown>";

      for ( auto& x : predicate_map )
      {
	 if ( x.pred == predicate )
	 {
	    expected = x.name;
	    break;
	 }
      }

      ERROR::severe( format( "argument wrong type--expected %s, not", expected).c_str(), s );
   }

   return s;
}

}
