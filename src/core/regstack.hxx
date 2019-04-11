#ifndef regstack_hxx
#define regstack_hxx

#include "tstack.hxx"
#include "error.hxx"

class REGSTACK : public TSTACK<SEXPR>
{
public:
   explicit REGSTACK( int stacksize ) : TSTACK<SEXPR>(stacksize) {}

   virtual void underflow() { ERROR::severe("regstack underflow"); }
   virtual void overflow() { ERROR::severe("regstack overflow"); }
};

extern REGSTACK regstack;


class RegstackChecker
{
   const char* name;
   int start;
public:
   explicit RegstackChecker( const char* s ) : name(s) 
   {
      start = regstack.gettop();
   }
   ~RegstackChecker()
   {
      const int end = regstack.gettop();
      if ( start != end )
      {
	 char message[80];
	 SPRINTF( message, "%s: regstack disparity (start=%d, end=%d)\n", name, start, end );
	 ERROR::severe( message );
      }
   }
};

#ifdef DO_REGSTACK_CHECK
#define REGSTACK_CHECKER(s) RegstackChecker rsc(s)
#else
#define REGSTACK_CHECKER(s)
#endif

#endif

