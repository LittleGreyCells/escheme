#include <cstdio>
#include <cstdlib>

#include "error.hxx"
#include "printer.hxx"
#include "pio.hxx"

#include "eval/eval.hxx"

namespace escheme
{
   using PIO::put;
   using PIO::stderr_port;
   using PRINTER::print;
      
   SEXPR ERROR::severe( const char* s, SEXPR exp1, SEXPR exp2 )
   {
      put( stderr_port, "error: " );
      put( stderr_port, s );
      
      if ( exp1 != nullptr )
      {
	 put( stderr_port, " '" );
	 print( stderr_port, exp1 );
	 put( stderr_port, "'" );
      }
      
      if ( exp2 != nullptr )
      {
	 put( stderr_port, " '" );
	 print( stderr_port, exp2 );
	 put( stderr_port, "'" );
      }
      
      put( stderr_port, "\n" );
      
      throw SevereError();
   }
   
   SEXPR ERROR::severe( const std::string& s, SEXPR exp1, SEXPR exp2 )
   {
      return severe( s.c_str(), exp1, exp2 );
   }
   
   void ERROR::warning( const char* s, SEXPR exp )
   {
      put( stderr_port, "warning: " );
      put( stderr_port, s );
      
      if ( exp != nullptr )
      {
	 put( stderr_port, " '" );
	 print( stderr_port, exp );
	 put( stderr_port, "'" );
      }
      
      put( stderr_port, "\n" );
   }
   
   void ERROR::warning( const std::string& s, SEXPR exp )
   {
      return warning( s.c_str(), exp );
   }
   
   SEXPR ERROR::fatal( const char* s )
   {
      put( stderr_port, "fatal error: " );
      put( stderr_port, s );
      put( stderr_port, "\n" );
      
      throw FatalError();
   }
   
   SEXPR ERROR::fatal( const std::string& s )
   {
      return fatal( s.c_str() );
   }
   
   SEXPR ERROR::exit()
   {
      throw Exit();
   }
   
}
