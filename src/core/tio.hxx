#ifndef TIO_HXX
#define TIO_HXX

#include <string>

#include "sexpr.hxx"

namespace escheme
{

namespace TIO
{
   // readline with history
   int terminal_getch();
   void terminal_unget( int ch );

   void history_init();

   void history_add( SEXPR sexpr );
   void history_show();
   void history_clear();

   void set_prompt( const std::string& prompt );
}

}

#endif
