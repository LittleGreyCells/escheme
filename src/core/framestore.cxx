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

FRAME FrameStore::alloc( UINT32 nslots, SEXPR vars )
{
   // allocate a frame with all slots defined
   //    framenslots = nslots
   //    framevars = null
   //    frameslots = {null}
   //    framesize = sizeof(header) + sizeof(slots)
   
   if ( (nslots < store.size()) && store[nslots] )
   {
      // reuse an existing frame
      auto frame = store[nslots];
      store[nslots] = frame->next;
      count[nslots] -= 1;

      // reinit
      setframevars( frame, vars );
      for ( int i = 0; i < nslots; ++i )
	 frameset( frame, i, null );

      return frame;
   }
   else
   {
      // allocate a new frame from heap
      return new Frame( nslots, vars );
   }
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
      delete[] frame->slot;
      delete[] frame;
   }
}

} // MEMORY

}
