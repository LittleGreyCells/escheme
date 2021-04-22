#include "tio.hxx"

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <string>

#include "pio.hxx"
#include "error.hxx"
#include "memory.hxx"
#include "printer.hxx"
#include "regstack.hxx"
#include "transcript.hxx"

#include "linenoise/linenoise.h"

namespace escheme
{

static const char* history = "history.txt";
static const int history_max_length = 100;
static std::string prompt = "> ";

static std::string term_line;
static unsigned term_index;

int TIO::terminal_getch()
{
   if ( term_index >= term_line.size() )
   {
      char* line = linenoise( prompt.c_str() );

      if ( !line )
	 ERROR::exit();

      term_index = 0;
      term_line = line;

      linenoiseFree( line );

      // append whitespace to satisfy scheme tokenizer
      term_line.push_back('\n');

      if ( TRANSCRIPT::transcript )
      {
	 fputs( prompt.c_str(), TRANSCRIPT::transcript );
	 fputs( term_line.c_str(), TRANSCRIPT::transcript );
      }
   }

   return term_line[term_index++];
}

void TIO::terminal_unget( int )
{
   if ( term_index > 0 )
      term_index -= 1;
}

void TIO::history_init()
{
   linenoiseHistoryLoad( history );
   linenoiseHistorySetMaxLen( history_max_length );
}

void TIO::history_add( SEXPR sexpr )
{
   const SEXPR port = PIO::open_on_string( MEMORY::string_null, pm_output );
   PRINTER::print( port, sexpr );

   // linenoise makes a copy when it adds it to the history
   linenoiseHistoryAdd( getstringportstring(port)->c_str() );
   linenoiseHistorySave( history );
}

void TIO::history_clear()
{
   linenoiseHistorySetMaxLen( 1 );
   linenoiseHistorySetMaxLen( history_max_length );
   linenoiseHistorySave( history );
}

void TIO::history_show()
{
   FILE* file = fopen( history, "r" );

   if ( file == NULL )
      return;

   while ( true )
   {
      char buffer[2000];
      const char* s = fgets( buffer, sizeof(buffer), file );

      if ( feof(file) )
	 break;
      
      if ( s != NULL && strlen( s ) > 0 )
	 printf( "%s", s );
   }

   fclose( file );
}

void TIO::set_prompt( const char* new_prompt )
{
   prompt = new_prompt;
}

}
