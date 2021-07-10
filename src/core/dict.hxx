#ifndef dict_hxx
#define dict_hxx

#include "sexpr.hxx"

namespace escheme
{
   bool has_key( SEXPR dict, SEXPR key );
   SEXPR dict_ref( SEXPR dict, SEXPR key );
   SEXPR dict_set( SEXPR dict, SEXPR key, SEXPR val );
}

#endif
