#include "framestore.hxx"

#include <cstring>

#include "sexpr.hxx"
#include "error.hxx"

//
// Frame Store
//

namespace MEMORY
{

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
      const auto frameSize = FRAMESIZE( nslots );   
      frame = reinterpret_cast<FRAME>( new DWORD[frameSize] );
      
      setframesize( frame, frameSize );
      setframenslots( frame, nslots );
   }
   
   setframevars( frame, null );
   setframeclosure( frame, null );
   
   for ( int i = 0; i < nslots; ++i )
      frameset( frame, i, null );
   
   return frame;
}

FRAME FrameStore::clone( FRAME fr )
{
   FRAME frame;
   const auto nslots = getframenslots(fr);
   
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
      frame = reinterpret_cast<FRAME>( new DWORD[getframesize(fr)] );
   }
      
   std::memcpy( frame, fr, NBYTES(getframesize(fr)) );

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
      delete[] frame;
   }
}

} // MEMORY
