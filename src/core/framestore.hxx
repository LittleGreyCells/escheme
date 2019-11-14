#ifndef framestore_hxx
#define framestore_hxx

#include <array>

#include "sexpr.hxx"

namespace escheme
{

namespace MEMORY
{

class FrameStore
{
   std::array<FRAME, 10> store;
  
public: 
   FrameStore();
  
   FRAME alloc( UINT32 nslots );
   FRAME clone( FRAME fr );
   void free( FRAME frame );
   
   std::array<UINT32, 10> count;
};

}  // MEMORY

}

#endif
