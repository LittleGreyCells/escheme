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
   if ( port == terminal_port )
   {
      ERROR::warning( "set_position on terminal port", port );
      return;
   }

   if ( getfile(port) == 0 )
      ERROR::severe( "set_position on closed port", port );

   if (-1 == fseek(getfile(port), getfixnum(pos), SEEK_SET))
      ERROR::severe("seek error on file");
}

SEXPR PIO::get_position( SEXPR port )
{
   // portp
   if ( port == terminal_port )
   {
      ERROR::warning( "get_position on terminal port", port );
      return MEMORY::fixnum(0);
   }

   if ( getfile(port) == 0 )
      ERROR::severe( "get_position on closed port", port );

   return MEMORY::fixnum( static_cast<FIXNUM>(ftell(getfile(port))) );
}

void PIO::close( SEXPR port )
{
   // portp
   if ( port == terminal_port )
   {
      ERROR::warning( "close on terminal port", port );
      return;
   }

   if ( getfile(port) == 0 )
      ERROR::severe( "close on closed port", port );

   fclose( getfile(port) );
   setfile( port, 0 );
}

void PIO::remove( const char* name )
{
   ::remove(name);
}

void PIO::flush( SEXPR port )
{
   // outportp
   if ( port == terminal_port )
   {
      ERROR::warning( "flush on terminal port", port );
      return;
   }

   if ( outportp(port) )
   {
      if ( getfile(port) == 0 )
         ERROR::severe( "flush on closed port", port );
      
      fflush( getfile(port) );
   }
}

//
// Dual Use (standard ports and string ports)
//

int PIO::get( SEXPR port )
{
   // anyportp
   if ( inportp(port) )
   {
      // file
      if ( getfile(port) == NULL )
	 ERROR::severe( "get on closed port", port );

      if ( port == terminal_port )
	 return TIO::terminal_getch();
      else
	 return fgetc( getfile(port) );
   }
   else if ( instringportp(port) )
   {
      // string input
      auto str = getstringportstring(port);
      auto& index = getstringportindex(port);
      if ( index < str->length() )
         return str->at( index++ );
      else
         return EOF;
   }
   else
   {
      ERROR::severe( "not an input port", port );
      return 0;
   }
}

void PIO::unget( SEXPR port, int ch )
{
   // anyportp
   if ( inportp(port) )
   {
      // file
      if ( getfile(port) == NULL )
	 ERROR::severe( "unget on closed port", port );

      if ( port == terminal_port )
	 TIO::terminal_unget( ch );
      else
	 ungetc( ch, getfile(port) );
   }
   else if ( instringportp(port) )
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
   else
   {
      ERROR::severe( "not an input port", port );
   }
}

void PIO::put( SEXPR port, const char* s )
{
   // anyportp
   if ( outportp(port) )
   {
      // file
      if ( getfile(port) == NULL )
	 ERROR::severe( "put on closed port", port );

      fputs( s, getfile(port) );

      using TRANSCRIPT::transcript;
      if ( transcript && (port == stdout_port) )
	 fputs( s, transcript );
   }
   else if ( outstringportp(port) )
   {
      // string
      getstringportstring(port)->append( s );
   }
   else
   {
      ERROR::severe( "not an output port", port );
   }
}

void PIO::put( SEXPR port, int ch )
{
   // anyportp
   if ( outportp(port) )
   {
      // file
      if ( getfile(port) == NULL )
	 ERROR::severe( "put on closed port", port );

      fputc( ch, getfile(port) );

      using TRANSCRIPT::transcript;
      if ( transcript && (port == stdout_port) )
	 fputc( ch, transcript );
   }
   else if ( outstringportp(port) )
   {
      // string
      getstringportstring(port)->push_back( ch );
   }
   else
   {
      ERROR::severe( "not an output port", port );
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
   SYMTAB::enter( "*standard-input*", stdin_port );

   stdout_port = MEMORY::port( stdout, pm_output );
   SYMTAB::enter( "*standard-output*", stdout_port );

   stderr_port = MEMORY::port( stderr, pm_output );
   SYMTAB::enter( "*standard-error*", stderr_port );

   terminal_port = MEMORY::port( stdin, pm_input );
   SYMTAB::enter( "*terminal*", terminal_port );

   TIO::history_init();

   eof_object = SYMTAB::enter( "**eof**" );
   setvalue( eof_object, eof_object );

   MEMORY::register_marker( pio_marker );
}

}
