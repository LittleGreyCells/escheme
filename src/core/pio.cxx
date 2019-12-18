#include "pio.hxx"

#include <string>
#include <cstdio>
#include <cstring>

#include "tio.hxx"
#include "memory.hxx"
#include "symtab.hxx"
#include "error.hxx"
#include "transcript.hxx"

namespace escheme
{

SEXPR PIO::stdin_port;
SEXPR PIO::stdout_port;
SEXPR PIO::stderr_port;
SEXPR PIO::terminal_port;
SEXPR PIO::eof_object;


SEXPR PIO::open( SEXPR name, short mode, const char* ftype )
{
   auto file = fopen( getstringdata(name), ftype );
   return ( file == 0 ) ? null : MEMORY::port( file, mode );
}

SEXPR PIO::open_on_string( SEXPR str, short mode )
{
   return MEMORY::string_port( str, mode );
}

void PIO::set_position( SEXPR port, SEXPR pos )
{
   // portp
   if ( getfile(port) == 0 )
      ERROR::severe( "set position on closed port", port );

   fseek( getfile(port), getfixnum(pos), SEEK_SET );
}

SEXPR PIO::get_position( SEXPR port )
{
   // portp
   if ( getfile(port) == 0 )
      ERROR::severe( "get position on closed port", port );

   return MEMORY::fixnum( ftell( getfile(port) ) );
}

void PIO::close( SEXPR port )
{
   // portp
   if ( port == terminal_port ||
        port == stdin_port ||
        port == stdout_port ||
        port == stderr_port )
   {
      ERROR::warning( "close on a system port ignored", port );
      return;
   }

   if ( getfile(port) == 0 )
      ERROR::severe( "close on closed port", port );

   fclose( getfile(port) );
   setfile( port, 0 );
}
   
void PIO::flush( SEXPR port )
{
   // portp
   if ( getfile(port) == 0 )
      ERROR::severe( "flush on closed port", port );
   
   fflush( getfile(port) );
}

//
// Dual Use (portp and stringportp)
//

int PIO::get( SEXPR port )
{
   // portp or stringportp
   if ( portp(port) )
   {
      // file
      if ( getfile(port) == 0 )
	 ERROR::severe( "get on closed port", port );

      if ( port == terminal_port )
	 return TIO::terminal_getch();
      else
	 return fgetc( getfile(port) );
   }
   else
   {
      // string input
      auto str = getstringportstring(port);
      auto& index = getstringportindex(port);
      if ( index < str->length() )
         return str->at( index++ );
      else
         return EOF;
   }
}

void PIO::unget( SEXPR port, int ch )
{
   if ( portp(port) )
   {
      // file
      if ( getfile(port) == 0 )
	 ERROR::severe( "unget on closed port", port );

      if ( port == terminal_port )
	 TIO::terminal_unget( ch );
      else
	 ungetc( ch, getfile(port) );
   }
   else
   {
      // string input
      if ( ch != EOF )
      {
         auto& index = getstringportindex(port);
	 // string input (adjust)
	 if ( index > 0)
	    index--;
      }
   }
}

void PIO::put( SEXPR port, int ch )
{
   if ( portp(port) )
   {
      // file
      if ( getfile(port) == 0 )
	 ERROR::severe( "put on closed port", port );

      fputc( ch, getfile(port) );

      using TRANSCRIPT::transcript;
      if ( transcript && (port == stdout_port) )
	 fputc( ch, transcript );
   }
   else
   {
      // string
      getstringportstring(port)->push_back( ch );
   }
}

void PIO::put( SEXPR port, const char* s )
{
   if ( portp(port) )
   {
      // file
      if ( getfile(port) == 0 )
	 ERROR::severe( "put on closed port", port );

      fputs( s, getfile(port) );

      using TRANSCRIPT::transcript;
      if ( transcript && (port == stdout_port) )
	 fputs( s, transcript );
   }
   else
   {
      // string
      getstringportstring(port)->append( s );
   }
}

static void pio_marker()
{
   // mark the PIO objects
   MEMORY::mark( PIO::stdin_port );
   MEMORY::mark( PIO::stdout_port );
   MEMORY::mark( PIO::stderr_port );
   MEMORY::mark( PIO::terminal_port );
   MEMORY::mark( PIO::eof_object );
}

void PIO::initialize()
{
   stdin_port = MEMORY::port( stdin, pm_input );
   stdout_port = MEMORY::port( stdout, pm_output );
   stderr_port = MEMORY::port( stderr, pm_output );
   terminal_port = MEMORY::port( stdin, pm_input );

   SYMTAB::enter( "*standard-input*", stdin_port );
   SYMTAB::enter( "*standard-output*", stdout_port );
   SYMTAB::enter( "*standard-error*", stderr_port );
   
   SYMTAB::enter( "*stdin*", stdin_port );
   SYMTAB::enter( "*stdout*", stdout_port );
   SYMTAB::enter( "*stderr*", stderr_port );
   
   SYMTAB::enter( "*terminal*", terminal_port );

   TIO::history_init();

   eof_object = SYMTAB::enter( "**eof**" );
   setvalue( eof_object, eof_object );

   MEMORY::register_marker( pio_marker );
}

}
