#include "equality.hxx"

#include <string.h>

namespace escheme
{
   bool eq( SEXPR e1, SEXPR e2 )
   {
      return e1 == e2;
   }
   
   bool eqv( SEXPR e1, SEXPR e2 )
   {
      if ( eq(e1, e2) )
	 return true;
      
      if ( anyp(e1) )
      {
	 if ( fixnump(e1) )
	 {
	    return fixnump(e2) && getfixnum(e1) == getfixnum(e2);
	 }
	 else if ( flonump(e1) )
	 {
	    return flonump(e2) && getflonum(e1) == getflonum(e2);
	 }
	 else if ( charp(e1) )
	 {
	    return charp(e2) && getcharacter(e1) == getcharacter(e2);
	 }
	 else if ( stringp(e1) )
	 {
	    return stringp(e2) && ::strcmp(getstringdata(e1), getstringdata(e2)) == 0;
	 }
      }
      
      return false;
   }
   
   bool equal( SEXPR e1, SEXPR e2 )
   {
      if ( eqv(e1, e2) )
	 return true;
      
      if ( anyp(e1) )
      {
	 if ( vectorp(e1) )
	 {
	    if ( vectorp(e2) )
	    { 
	       const int vlen = getvectorlength(e1);
	       
	       if ( vlen != getvectorlength(e2) )
		  return false;
	       
	       for ( int i = 0; i < vlen; ++i )
		  if ( !equal(vectorref(e1, i), vectorref(e2, i)) )
		     return false;
	       
	       return true;
	    }
	 }
	 else if ( consp(e1) )
	 {
	    return consp(e2) && equal(car(e1), car(e2)) && equal(cdr(e1), cdr(e2));
	 }
      }
      
      return false;
   }
   
}
