#ifndef printer_hxx
#define printer_hxx

#include <cstdio>
#include "sexpr.hxx"
#include "pio.hxx"

namespace escheme
{

namespace PRINTER
{
   // quote styles
   //   NO_QUOTE: do not quote (display)
   //   QUOTE: do quote or symbolic form (print)
   
   enum QuoteStyle
   {
      NO_QUOTE,
      QUOTE,
   };

   void print( SEXPR outport, const SEXPR s, QuoteStyle style = QUOTE );
   
   void newline( SEXPR outport = PIO::stdout_port );

   // standard output
   void print( const SEXPR s, QuoteStyle style = QUOTE );

   // private
   void print_sexpr( SEXPR outport, const SEXPR s, QuoteStyle style = QUOTE );
   void print_list( SEXPR outport, const SEXPR s, QuoteStyle style = QUOTE );
   void print_vector( SEXPR outport, const SEXPR s, QuoteStyle style = QUOTE );
   void print_closure( SEXPR outport, const SEXPR s, QuoteStyle style = QUOTE );
   void print_string( SEXPR outport, const char* p, QuoteStyle style );
}

}

#endif
