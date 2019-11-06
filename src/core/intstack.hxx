#ifndef intstack_hxx
#define intstack_hxx

#include "tstack.hxx"
#include "error.hxx"

namespace escheme
{

class INTSTACK : public TSTACK<int>
{
public:
   explicit INTSTACK( int stacksize ) : TSTACK<int>(stacksize) {}

   virtual void underflow() { ERROR::severe("control stack underflow"); }
   virtual void overflow() { ERROR::severe("control stack overflow"); }
};

extern INTSTACK intstack;

}

#endif



