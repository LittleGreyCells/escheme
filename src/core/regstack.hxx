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

#endif

