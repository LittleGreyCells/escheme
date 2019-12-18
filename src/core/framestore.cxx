#include "framestore.hxx"

#include <cstring>

#include "sexpr.hxx"
#include "error.hxx"

//
// Frame Store
//

namespace escheme
{

namespace MEMORY
{

FrameStore::FrameStore() : store{nullptr}, count{0} {}

FRAME FrameStore::alloc( UINT32 nslots )
{
   // allocate a frame with all slots defined
   //    framenslots = nslots
   //    framevars = null
   //    frameclosure = null
   //    frameslots = {null}
   //    framesize = sizeof(header) + sizeof(slots)
   FRAME frame;
   
   if ( (nslots < store.size()) && store[nslots] )
   {
      // reuse an existing frame
      frame = store[nslots];
      store[nslots] = frame->next;
      count[nslots] -= 1;
      
      if ( getframenslots(frame) != nslots )
         ERROR::fatal( "recycled frame size inconsistent with request" );
   }
   else
   {
      // allocate a new frame from heap
      frame = new Frame();
      frame->slot = ( nslots > 0 ) ? new SEXPR[nslots] : nullptr;
      setframenslots( frame, nslots );
   }
   
   setframevars( frame, null );
   setframeclosure( frame, null );
   
   for ( int i = 0; i < nslots; ++i )
      frameset( frame, i, null );
   
   return frame;
}

void FrameStore::free( FRAME frame )
{
   const auto nslots = frame->nslots;
   
   if ( nslots < store.size() )
   {
      frame->next = store[nslots];
      store[nslots] = frame;
      count[nslots] += 1;
   }
   else
   {
      if ( frame->slot )
         delete[] frame->slot;
      delete[] frame;
   }
}

} // MEMORY

}
