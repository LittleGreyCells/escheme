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
   //   0: do not quote (display)
   //   1: do quote (print)

   void print( SEXPR outport, const SEXPR s, int style = 1 );
   
   void newline( SEXPR outport = PIO::stdout_port );

   // standard output
   void print( const SEXPR s, int style = 1 );

   // private
   void print_sexpr( SEXPR outport, const SEXPR s, int style = 1 );
   void print_list( SEXPR outport, const SEXPR s, int style = 1 );
   void print_vector( SEXPR outport, const SEXPR s, int style = 1 );
   void print_closure( SEXPR outport, const SEXPR s, int style = 1 );
}

}

#endif
