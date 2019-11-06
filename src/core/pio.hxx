#ifndef pio_hxx
#define pio_hxx

#include "sexpr.hxx"

//
// Standard/File and String Ports
//

namespace escheme
{

namespace PIO
{
   extern SEXPR stdin_port;
   extern SEXPR stdout_port;
   extern SEXPR stderr_port;
   extern SEXPR terminal_port;
   extern SEXPR eof_object;
   
   extern FILE* transcript;
   
   void initialize();
   
   SEXPR open( SEXPR fname, short mode, const char* ftype );
   SEXPR open_on_string( SEXPR str, short mode );
   
   void close( SEXPR port );
   void remove( const char* name );
   
   void set_position( SEXPR port, SEXPR pos );
   SEXPR get_position( SEXPR port );
   
   void flush( SEXPR port );
   
   int get( SEXPR inport );
   void unget( SEXPR inport, int ch );
   
   void put( SEXPR outport, const char* s );
   void put( SEXPR outport, int ch );
   
   inline void put( const char* s ) { put(stdout_port, s); }
   inline void put( int ch ) { put(stdout_port, ch); }

   void transcript_on( SEXPR fname );
   void transcript_off();
}

}

#endif
