#include "varpool.hxx"

#include <array>
#include <algorithm>

#include <cstdio>
#include <cstring>

#include "sexpr.hxx"
#include "error.hxx"
#include "memory.hxx"

namespace MEMORY
{

   VarPool::VarPool( const char* name, unsigned size, unsigned delta ) 
      : name( name ),
	size( size ),
	index( 0 ),
	active( new DWORD[size] ),
	inactive( new DWORD[size] ),
        delta( delta )
   {
      // empty
   }

   VarPool::~VarPool()
   {
      delete[] active;
      delete[] inactive;
   }

   void VarPool::prep()
   {
      // prep stop-and-copy.
      //   this will init the inactive partition start for copy.
      // after copying and swapping partition pointers,
      //   index will be pointing to the next available 
      //   allocation point.
      index = 0;
   }

   void VarPool::swap()
   {
      // swap active and inactive partition pointers
      DWORD* temp = inactive;
      inactive = active;
      active = temp;
   }

   void VarPool::expand( unsigned nwords )
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

   bool VarPool::not_enough_room( unsigned nwords )
   {
      return index + nwords >= size;
   }

   void* VarPool::alloc( unsigned nwords )
   {
      // we allocate from [active[index] .. active[size-1]].
      //   return the address of the storage location.
      //   if there is not enought room, garbage collect with copy enabled.
      if ( not_enough_room( nwords ) )
      {
	 MEMORY::gc( true );

	 if ( not_enough_room( nwords ) )
	    expand( nwords );

	 if ( not_enough_room( nwords ) )
	 {
	    char msg[80];
	    SPRINTF( msg, "(%s) insufficient pool space", name );
	    ERROR::fatal( msg );
	 }
      }

      void* address = (void*)&active[index];
      index += nwords;
      return address;
   }

   void* VarPool::copy_to_inactive( void* src, unsigned nwords )
   {
      if ( not_enough_room( nwords ) )
      {
         char msg[80];
	 SPRINTF( msg, "(%s)copy_to_inactive exceeds pool: index=%u, nwords=%u\n", name, index, nwords );
         ERROR::fatal( msg );
      }
      
      void* dst = std::memcpy( &inactive[index], src, NBYTES(nwords) );
      index += nwords;
      return dst;
   }

   void* VarPool::copy_to_active( void* src, unsigned nwords )
   {
      if ( not_enough_room( nwords ) )
      {
         char msg[80];
	 SPRINTF( msg, "(%s)copy_to_active exceeds pool: index=%u, nwords=%u\n", name, index, nwords );
         ERROR::fatal( msg );
      }
      
      void* dst = std::memcpy( &active[index], src, NBYTES(nwords) );
      index += nwords;
      return dst;
   }

}  // MEMORY

