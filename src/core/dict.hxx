#ifndef dict_hxx
#define dict_hxx

#include "sexpr.hxx"

namespace escheme
{
   bool has_key( SEXPR dict, SEXPR key );
   
   SEXPR dict_ref( SEXPR dict, SEXPR key );
   SEXPR dict_set( SEXPR dict, SEXPR key, SEXPR val );
   SEXPR dict_items( SEXPR dict );

   SEXPR dict_rem( SEXPR dict, SEXPR key );
   SEXPR dict_empty( SEXPR dict );
}

#endif
