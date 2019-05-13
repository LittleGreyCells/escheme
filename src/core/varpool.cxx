#include "varpool.hxx"

#include <list>
#include <algorithm>

#include <cstdio>
#include <cstring>

#include "sexpr.hxx"
#include "error.hxx"
#include "memory.hxx"

namespace MEMORY
{
   VPBlock::VPBlock( unsigned size ) : active( new DWORD[size] ),
                                       inactive( new DWORD[size] ) {}
   VPBlock::~VPBlock()
   {
      delete[] active;
      delete[] inactive;
   }
      
   VarPool::VarPool( const char* name, unsigned size ) 
      : name( name ),
	size( size ),
	index( 0 )
   {
      // initially we use a single block
      blocks.push_back( new VPBlock(size) );
      block = blocks.back();
   }

   VarPool::~VarPool()
   {
      // empty
   }

   void VarPool::prep()
   {
      it = blocks.begin();
      block = *it;
      index = 0;
   }

   void VarPool::swap()
   {
      // swap active and inactive partition pointers for all blocks.
      for ( auto& block : blocks )
      {
         DWORD* temp = block->inactive;
         block->inactive = block->active;
         block->active = temp;
      }
   }

   void VarPool::expand()
   {
      // use the next block or add one
#if 1
      // multi-block expanson is not yet supported
      char msg[80];
      SPRINTF( msg, "(%s) multi-block expansion not supported", name );
      ERROR::fatal( msg );
#else
      if ( block == blocks.back() )
      {
         // we are on the last block, so we add one
         blocks.push_back( new VPBlock(size) );
         block = blocks.back();
      }
      else
      {
         // otherwise, we are pulling from the list of allocated blocks
         ++it;
         block = *it;
      }
      index = 0;
#endif
   }

   bool VarPool::not_enough_room( unsigned nwords )
   {
      return index + nwords >= size;
   }

   void* VarPool::alloc( unsigned nwords )
   {
      // we allocate from [active[index] .. active[size-1]].
      //   return the address of the storage location.
      //   if there is not enough room, garbage collect with copy enabled.
      if ( not_enough_room( nwords ) )
      {
#ifdef OBJECT_CACHE
	 MEMORY::gc( true );
#endif
	 if ( not_enough_room( nwords ) )
         {
	    expand();
         }
      }
      void* address = (void*)&block->active[index];
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
      void* dst = std::memcpy( &block->inactive[index], src, NBYTES(nwords) );
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
      void* dst = std::memcpy( &block->active[index], src, NBYTES(nwords) );
      index += nwords;
      return dst;
   }

}  // MEMORY

