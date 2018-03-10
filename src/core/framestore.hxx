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
   INT32 nframes;
   UINT32 nzeroallocs;
#ifdef FS_STATISTICS_DETAILED
   std::array<INT32, 10> outstanding;
   std::array<INT32, 10> available;
#endif

public: 
   FrameStore()
   {
      for ( auto& x : store )
	 x = nullptr;

      nframes = 0;
      nzeroallocs = 0;

#ifdef FS_STATISTICS_DETAILED
      for ( int i = 0; i < store.size(); ++i )
      {
	 outstanding[i] = 0;
	 available[i] = 0;
      }
#endif
   }

   FRAME alloc( UINT32 nslots )
   {
      // allocate a frame with all slots defined
      //    framenslots = nslots
      //    framevars = null
      //    frameslots = {null}
      FRAME frame;

      nframes += 1;

      if ( nslots == 0 )
	 nzeroallocs += 1;

      if ( (nslots < store.size()) && store[nslots] )
      {
	 // reuse an existing frame
	 frame = store[nslots];
	 store[nslots] = frame->next;

	 if ( getframenslots(frame) != nslots )
	    ERROR::fatal( "recycled frame size inconsistent with request" );

#ifdef FS_STATISTICS_DETAILED
	 outstanding[nslots] += 1;
	 available[nslots] -= 1;
#endif
      }
      else
      {
	 // allocate a new frame from heap
	 const size_t frameSize = sizeof(Frame) + sizeof(SEXPR) * nslots - sizeof(SEXPR);   
	 frame = (FRAME) ::operator new (frameSize);
      
	 setframenslots( frame, nslots );
  
#ifdef FS_STATISTICS_DETAILED
	 if ( nslots < store.size() )
	    outstanding[nslots] += 1;
#endif
      }
      
      setframevars( frame, null );

      for (int i = 0; i < nslots; ++i )
	 frameset( frame, i, null );

      return frame;
   }

   void free( FRAME frame )
   {
      // some frames can be nullptr's; check

      if (frame)
      {
	 nframes -= 1;

	 const UINT32 nslots = frame->nslots;
	 
	 if ( nslots < store.size() )
	 {
	    frame->next = store[nslots];
	    store[nslots] = frame;
#ifdef FS_STATISTICS_DETAILED
	    outstanding[nslots] -= 1;
	    available[nslots] += 1;
#endif
	 }
	 else
	 {
	    delete frame;
	 }
      }
   }

   UINT32 size() { return store.size(); }
};

