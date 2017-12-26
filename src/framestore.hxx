#include <cstdio>
#include <array>

#include "sexpr.hxx"
#include "error.hxx"

//
// Frame Store
//

class FrameStore
{
   std::array<FRAME, 10> store;

public: 
   FrameStore()
   {
      for ( auto& x : store )
	 x = nullptr;
   }

   FRAME alloc( UINT32 nslots )
   {
      if ( (nslots < store.size()) && store[nslots] )
      {
	 FRAME frame = store[nslots];
	 store[nslots] = frame->next;
	 if ( getframenslots(frame) != nslots )
	    ERROR::fatal( "recycled frame size inconsistent with request" );
	 return frame;
      }
      
      const size_t frameSize = sizeof(Frame) + sizeof(SEXPR) * nslots;   
      FRAME frame = (FRAME) ::operator new (frameSize);
      
      setframenslots( frame, nslots );
      
      return frame;
   }

   void free( FRAME frame )
   {
      // some frames can be nullptr's; check

      if (frame)
      {
	 const UINT32 nslots = frame->nslots;
	 
	 if ( nslots < store.size() )
	 {
	    frame->next = store[nslots];
	    store[nslots] = frame;
	 }
	 else
	 {
	    delete frame;
	 }
      }
   }
};

