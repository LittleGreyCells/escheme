#ifndef printer_hxx
#define printer_hxx

#include <cstdio>
#include "sexpr.hxx"
#include "pio.hxx"

namespace PRINTER
{
   // quote styles
   //   0: do not quote (display)
   //   1: do quote (print)

   void print( SEXPR outport, const SEXPR s, int style = 1 );
   void newline( SEXPR outport );

   // standard output

   inline 
   void print( const SEXPR s, int style = 1 ) 
   { 
      print(PIO::stdout_port, s, style); 
   }

   inline 
   void newline() { newline(PIO::stdout_port); }

   // private
   void print_sexpr( SEXPR outport, const SEXPR s, int style = 1 );
   void print_list( SEXPR outport, const SEXPR s, int style = 1 );
   void print_vector( SEXPR outport, const SEXPR s, int style = 1 );
   void print_closure( SEXPR outport, const SEXPR s, int style = 1 );
}

#endif
