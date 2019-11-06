#include <cstdio>

#include "argstack.hxx"

namespace escheme
{
   
//
// Argument Stack
//

ARGSTACK argstack( ARGSTACK_SIZE );

//
// Argument Stack Iterator
//

void ArgstackIterator::reset()
{
   argc = argstack.argc;
   argp = argstack.getfirstargindex();
}  
  
}

