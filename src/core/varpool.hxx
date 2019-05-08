#ifndef VARPOOL_HXX
#define VARPOOL_HXX

#include <array>
#include <algorithm>

#include <cstdio>
#include <cstring>

#include "sexpr.hxx"
#include "error.hxx"
#include "memory.hxx"

#ifdef DO_VARPOOL_TRACE
#define TRACE( code ) code;
#else
#define TRACE( code )
#endif

//
// Variable Sized Object Pools
//

namespace MEMORY
{

class VarPool
{
   const char* name;
   DWORD* active;
   DWORD* inactive;
   unsigned index;
   unsigned size;
   unsigned delta;

   void expand( unsigned nwords )
   {
      // expand var pool by max(nwords, VARPOOL_SIZE)
      //
      //   state 
      //      active has addresses in use
      //      inactive has copyback store
      //
      //   procedure
      //      [1] delete existing copyback store (inactive)
      //      [2] allocate a larger copyback store (inactive)
      //      [3] perform another gc( copy=true ) (swap)
      //      [4] delete the copyback store (inactive)
      //      [5] allocate a larger copyback store (inactive)
      //      [6] set size to new size
      //
      const unsigned new_size = std::max( size+nwords, size+delta );

      TRACE( printf( "nwords:   %u\n", nwords ) );
      TRACE( printf( "old_size: %u\n", size ) );
      TRACE( printf( "new_size: %u\n", new_size ) );

      // [1] delete existing copyback store (inactive)
      // [2] allocate a larger copyback store (inactive)
      delete[] inactive;
      inactive = new DWORD[new_size];

      // [3] perform another gc( copy=true ) (swap)
      MEMORY::gc( true );

      // [4] delete the new copyback store (inactive)
      // [5] allocate a larger copyback store (inactive)
      delete[] inactive;
      inactive = new DWORD[new_size];

      // [6] set size to new size
      size = new_size;
   }

private:

   // copying not permitted
   VarPool( const VarPool& vp );
   VarPool& operator=( const VarPool& vp );

public:

   VarPool( const char* name, unsigned size, unsigned delta=2000 ) 
      : name( name ),
	size( size ),
	index( 0 ),
	active( new DWORD[size] ),
	inactive( new DWORD[size] ),
        delta( delta )
   {
      // empty
   }

   ~VarPool()
   {
      delete[] active;
      delete[] inactive;
   }

   unsigned getsize() { return size; }
   unsigned getindex() { return index; }

   void prep()
   {
      // prep stop-and-copy.
      //   this will init the inactive partition start for copy
      // after copying and swapping partition pointers,
      //   index will be pointing to the next available 
      //   allocation point.
      index = 0;
   }

   void swap()
   {
      // swap active and inactive partition pointers
      DWORD* temp = inactive;
      inactive = active;
      active = temp;
   }

   void* alloc( unsigned nwords )
   {
      // we allocate from [active[index] .. active[size-1]].
      //   return the address of the storage location.
      //   if there is not enought room, garbage collect with copy enabled.
      if ( index + nwords >= size )
      {
	 // need to gc
	 MEMORY::gc( true );

	 if ( index + nwords >= size )
	    expand( nwords );

	 if ( index + nwords >= size )
	 {
	    char msg[80];
	    SPRINTF( msg, "(%s) insufficient varpool space", name );
	    ERROR::fatal( msg );
	 }
      }

      void* address = (void*)&active[index];
      TRACE( printf( "(%s)alloc: addr=%p, index=%u, nwords=%u\n", name, address, index, nwords ) );
      index += nwords;
      return address;
   }

   void* copy_to_inactive( void* src, unsigned nwords )
   {
      if ( index + nwords >= size )
      {
         char msg[80];
	 SPRINTF( msg, "(%s)copy_to_inactive exceeds pool: index=%u, nwords=%u\n", name, index, nwords );
         ERROR::fatal( msg );
      }
      TRACE( printf( "(%s)cp2i: src=%p, index=%u, nwords=%u\n", name, src, index, nwords ) );
      void* dst = std::memcpy( (void*)&inactive[index], src, NBYTES(nwords) );
      index += nwords;
      return dst;
   }

   void* copy_to_active( void* src, unsigned nwords )
   {
      if ( index + nwords >= size )
      {
         char msg[80];
	 SPRINTF( msg, "(%s)copy_to_active exceeds pool: index=%u, nwords=%u\n", name, index, nwords );
         ERROR::fatal( msg );
      }
      TRACE( printf( "(%s)cp2a: src=%p, index=%u, nwords=%u\n", name, src, index, nwords ) );
      void* dst = std::memcpy( (void*)&active[index], src, NBYTES(nwords) );
      index += nwords;
      return dst;
   }

};

}  // MEMORY

#endif
