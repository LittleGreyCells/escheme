#ifndef framestore_hxx
#define framestore_hxx

#include <array>

#include "sexpr.hxx"

namespace MEMORY

{

class FrameStore
{
   std::array<FRAME, 10> store = {nullptr};

public: 
   FrameStore() {}

   FRAME alloc( UINT32 nslots );
   void free( FRAME frame );

   std::array<UINT32, 10> count = {0};
};

}  // MEMORY

#endif
