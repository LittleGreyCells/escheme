#ifndef framestore_hxx
#define framestore_hxx

#include <array>

#include "sexpr.hxx"

class FrameStore
{
   std::array<FRAME, 10> store = {nullptr};

public: 
   FrameStore() {}

   FRAME alloc( UINT32 nslots );
   void free( FRAME frame );

   std::array<UINT32, 10> count = {0};
};

#endif
