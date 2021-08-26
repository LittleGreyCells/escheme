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

   size_t hash( const std::string& s )
   {
      size_t hval = 0;
      for ( auto ch : s )
	 hval = (hval << 2) ^ ch;
      return hval;
   }

   size_t hash( SEXPR val )
   {
      switch ( nodekind(val) )
      {
	 case n_symbol:
	    return reinterpret_cast<size_t>( val );
	 case n_string:
	    return hash( getstringdata(val) );
	 case n_fixnum:
	    return static_cast<size_t>( getfixnum(val) );
	 case n_flonum:
	    return static_cast<size_t>( getflonum(val) );
	 case n_char:
	    return static_cast<size_t>( getcharacter(val) );
	 default:
	    return reinterpret_cast<size_t>( val );
      }
   }
}


