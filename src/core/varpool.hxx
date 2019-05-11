#ifndef VARPOOL_HXX
#define VARPOOL_HXX

#include "sexpr.hxx"

//
// Variable Sized Object Pools
//

namespace MEMORY
{

class VarPool
{
public:
   VarPool( const char* name, unsigned size, unsigned delta=2000 );
   ~VarPool();

   unsigned getsize() { return size; }
   unsigned getindex() { return index; }

   void prep();
   void swap();

   void* alloc( unsigned nwords );
   void* copy_to_inactive( void* src, unsigned nwords );

private:
   // copying not permitted
   VarPool( const VarPool& vp );
   VarPool& operator=( const VarPool& vp );

   bool not_enough_room( unsigned nwords );
   void expand( unsigned nwords );
   void* copy_to_active( void* src, unsigned nwords );

   const char* name;
   DWORD* active;
   DWORD* inactive;
   unsigned index;
   unsigned size;
   unsigned delta;
};

}  // MEMORY

#endif
