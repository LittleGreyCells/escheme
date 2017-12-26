#ifndef TIO_HXX
#define TIO_HXX

#include "sexpr.hxx"

namespace TIO
{
   // line-oriented standard input
   int getch();
   void unget( int ch );

   // readline with history
   int terminal_getch();
   void terminal_unget( int ch );

   void history_init();

   void history_add( SEXPR sexpr );
   void history_show();
   void history_clear();

   void set_prompt( const char* prompt );
};

#endif
