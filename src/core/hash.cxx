#include "sexpr.hxx"

namespace escheme
{
   size_t hash( const char* s )
   {
      size_t hval = 0;
      while ( *s )
	 hval = (hval << 2) ^ *s++;
      return hval;
   }

   size_t hash( SEXPR val )
   {
      if ( nullp(val) )
	 return 0;
      else
	 switch ( nodekind(val) )
	 {
	    case n_symbol:
	       return hash( getname(val) );
	    case n_string:
	       return hash( getstringdata(val) );
	    case n_fixnum:
	       return static_cast<size_t>( getfixnum(val) );
	    case n_flonum:
	       return static_cast<size_t>( getflonum(val) );
	    default:
	       return reinterpret_cast<size_t>( val );
	 }
   }
}


