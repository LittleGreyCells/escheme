
#include <array>

#include <cstdio>
#include <cstring>

#include "sexpr.hxx"
#include "error.hxx"
#include "memory.hxx"

using DWORD = void*;

#define NDWORDS(n) (((n)+sizeof(DWORD)-1)/sizeof(DWORD))
#define NBYTES(n)  ((n)*sizeof(DWORD))

#ifdef DO_VARPOOL_TRACE
#define TRACE( code ) code;
#else
#define TRACE( code )
#endif

//
// Variable Sized Object Pools
//

static unsigned max( unsigned a, unsigned b ) { return (a > b) ? a : b; }

class NewSpace
{
   const char* name;
   DWORD* active;
   DWORD* inactive;
   unsigned index;
   unsigned size;

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
      const unsigned new_size = max( size+nwords, size+VARPOOL_EXPANSION );

      printf( "nwords:   %u\n", nwords );
      printf( "old_size: %u\n", size );
      printf( "new_size: %u\n", new_size );

      // [1] delete existing copyback store (inactive)
      // [2] allocate a larger copyback store (inactive)
      delete inactive;
      inactive = new DWORD[new_size];

      // [3] perform another gc( copy=true ) (swap)
      MEMORY::gc( true );

      // [4] delete the new copyback store (inactive)
      // [5] allocate a larger copyback store (inactive)
      delete inactive;
      inactive = new DWORD[new_size];

      // [6] set size to new size
      size = new_size;
   }

public:

   NewSpace( const char* name, unsigned size ) 
      : name( name ),
	size( size ),
	index( 0 ),
	active( new DWORD[size] ),
	inactive( new DWORD[size] )
   {
      // empty
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
	 printf( "(%s)copy_to_inactive exceeds space: index=%u, nwords=%u\n", name, index, nwords );
	 fflush(NULL);
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
	 printf( "(%s)copy_to_active exceeds space: index=%u, nwords=%u\n", name, index, nwords );
	 fflush(NULL);
      }
      TRACE( printf( "(%s)cp2a: src=%p, index=%u, nwords=%u\n", name, src, index, nwords ) );
      void* dst = std::memcpy( (void*)&active[index], src, NBYTES(nwords) );
      index += nwords;
      return dst;
   }

};

const unsigned MAGIC = 0xFFFF;

struct OSEntry
{
   unsigned magic;
   unsigned nwords;
   SEXPR object;
   DWORD data[1];   // varying
};

class OldSpace
{
   const char* name;
   DWORD* active;
   DWORD* inactive;
   unsigned index;
   unsigned size;

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
      //      [3] swap
      //      [4] delete the copyback store (inactive)
      //      [5] allocate a larger copyback store (inactive)
      //      [6] set size to new size
      //
      const unsigned new_size = max( size+nwords, size+VARPOOL_EXPANSION );

      printf( "nwords:   %u\n", nwords );
      printf( "old_size: %u\n", size );
      printf( "new_size: %u\n", new_size );

      // [1] delete existing copyback store (inactive)
      // [2] allocate a larger copyback store (inactive)
      delete inactive;
      inactive = new DWORD[new_size];

      // [3] swap
      swap();

      // [4] delete the new copyback store (inactive)
      // [5] allocate a larger copyback store (inactive)
      delete inactive;
      inactive = new DWORD[new_size];

      // [6] set size to new size
      size = new_size;
   }

public:

   OldSpace( const char* name, unsigned size ) 
      : name( name ),
	size( size ),
	index( 0 ),
	active( new DWORD[size] ),
	inactive( new DWORD[size] )
   {
      // empty
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

   void* copy_to_inactive( SEXPR n, void* src, unsigned nwords )
   {
      if ( index + nwords >= size )
      {
	 printf( "(%s)copy_to_inactive exceeds space: index=%u, nwords=%u\n", name, index, nwords );
	 fflush(NULL);
      }
      TRACE( printf( "(%s)cp2i: src=%p, index=%u, nwords=%u\n", name, src, index, nwords ) );
      void* dst = std::memcpy( (void*)&inactive[index], src, NBYTES(nwords) );
      index += nwords;
      return dst;
   }

   void* copy_to_active( SEXPR n, void* src, unsigned nwords )
   {
      const unsigned entry_size = nwords + 2;

      OSEntry* entry = (OSEntry*)alloc( entry_size );
      entry->magic = MAGIC;
      entry->nwords = nwords;
      entry->object = n;

      TRACE( printf( "(%s)cp2a: src=%p, index=%u, nwords=%u\n", name, src, index, nwords ) );
      void* dst = std::memcpy( &entry->data[0], src, NBYTES(nwords) );

      return dst;
   }

   void show()
   {
      if ( index > 0 )
      {
	 unsigned n = 0;
	 unsigned src_index = 0;

	 while ( src_index < index )
	 {
	    n += 1;

	    OSEntry* h = (OSEntry*)&active[src_index];

	    if ( h->magic != MAGIC )
	    {
	       printf( "h->magic = %0x\n", h->magic );
	       printf( "h->nwords = %u\n", h->nwords );
	       ERROR::fatal( "oldspace magic does not agree" );
	    }

	    printf( "%u: index=%u, h=%p, nwords=%u, object=%p, k=%u\n", 
		    n, 
		    src_index, 
		    (void*)h,
		    h->nwords, 
		    (void*)h->object,
		    nodekind((SEXPR)h->object) );

	    src_index += h->nwords + 2;
	 }
      }
   }

   void copy_marked()
   {
      if ( index > 0 )
      {
	 unsigned dst_index = 0;
	 unsigned src_index = 0;

	 while ( src_index < index )
	 {
	    OSEntry* h = (OSEntry*)&active[src_index];

	    if ( h->magic != MAGIC )
	       ERROR::fatal( "oldspace magic does not agree" );

	    if ( markedp(h->object) )  
	    {
	       // copy entry to inactive side
	       void* dst = std::memcpy( &inactive[dst_index], &active[src_index], NBYTES(h->nwords) );

	       // update object data pointer
	       switch ( nodekind( h->object ) )
	       {
		  case n_symbol:
		     setname( h->object, (char*)&h->data );
		     break;

		  case n_string:
		     setstringdata( h->object, (char*)&h->data );
		     break;

		  case n_vector:
		     setvectordata( h->object, (SEXPR*)&h->data );
		     break;

		  case n_bvec:
		     setbvecdata( h->object, (BYTE*)&h->data );
		     break;

		  case n_environment:
		     setenvframe( h->object, (FRAME)&h->data );
		     break;

		  default:
		     break;
	       }

	       dst_index += h->nwords + 2;
	    }

	    src_index += h->nwords + 2;
	 }

	 index = dst_index;
      }
   }
};
