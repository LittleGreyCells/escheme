#ifndef reader_hxx
#define reader_hxx

#include <cstdio>
#include <string>

#include "sexpr.hxx"
#include "pio.hxx"

namespace escheme
{

namespace READER
{
   void initialize();

   SEXPR read( SEXPR inport = PIO::stdin_port );

   int eof_objectp( const SEXPR n );

   void mark();

   // private
   extern SEXPR symbol_dot;

   SEXPR read_sexpr( SEXPR inport );
   void  read_comment( SEXPR inport );
   SEXPR read_list( SEXPR inport, char terminator );
   SEXPR read_string( SEXPR inport );
   SEXPR read_symbol( SEXPR inport );
   SEXPR read_special( SEXPR inport );
   SEXPR read_vector( SEXPR inport, char terminator );
   SEXPR read_quote( SEXPR inport, SEXPR flavor );
   SEXPR read_comma( SEXPR inport );
   SEXPR read_fixnum( SEXPR inport, int base );

   int scan( SEXPR inport );
}

}

#endif
