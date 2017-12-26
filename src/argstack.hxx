#ifndef argstack_hxx
#define argstack_hxx

#include "tstack.hxx"
#include "sexpr.hxx"
#include "error.hxx"

class ARGSTACK : public TSTACK<SEXPR>
{
public:
   ARGSTACK( int stacksize ) : TSTACK<SEXPR>(stacksize), argc(0) {}

   void push( SEXPR n ) { ++argc; TSTACK<SEXPR>::push(n); }
   SEXPR pop() { --argc; return TSTACK<SEXPR>::pop(); }

   void removeargc() { top_index -= argc; argc = 0; }
   void noargs() { if (argc != 0) ERROR::severe("no arguments expected"); }

   int getargc() { return argc; }
   int getfirstargindex() { return top_index-argc+1; }

   void flush() { TSTACK<SEXPR>::flush(); argc = 0; }

   virtual void overflow() { ERROR::severe("argstack overflow"); }
   virtual void underflow() { ERROR::severe("argstack underflow"); }

   int argc;
};

extern ARGSTACK argstack;

class ArgstackIterator
{
public:
   ArgstackIterator() : argc(argstack.argc), argp(argstack.getfirstargindex()) {}

   int more() { return argc > 0; }

   void done() 
   { 
      if (argc != 0) 
	 ERROR::severe("too many arguments"); 
   }

   SEXPR getarg() 
   {
      if (argc <= 0)
	 ERROR::severe("too few arguments");
      return --argc, argstack[argp++];
   }

   SEXPR getlast()
   {
      if (argc != 1)
      {
	 if (argc < 1)
	    ERROR::severe("too few arguments");
	 else
	    ERROR::severe("too many arguments");
      }
      return --argc, argstack[argp];
   }

   void reset();
   void show();

   int argc;
   int argp;
};
  
#endif


