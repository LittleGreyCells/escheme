#include "framestore.hxx"

#include "sexpr.hxx"
#include "error.hxx"

//
// Frame Store
//

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
      const size_t frameSize = FRAMESIZE_NB( nslots );   
      frame = (FRAME) ::operator new (frameSize);
      
      setframesize( frame, (UINT32)frameSize );
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
   // some frames might be nullptrs
   
   if ( frame )
   {
      const UINT32 nslots = frame->nslots;
      
      if ( nslots < store.size() )
      {
         frame->next = store[nslots];
         store[nslots] = frame;
         count[nslots] += 1;
      }
      else
      {
         delete frame;
      }
   }
}
