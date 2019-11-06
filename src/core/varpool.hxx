#ifndef VARPOOL_HXX
#define VARPOOL_HXX

#include <list>

#include "sexpr.hxx"

//
// Variable Sized Object Pool
//

namespace escheme
{

namespace MEMORY
{
   struct VPBlock
   {
      VPBlock( unsigned size );
      ~VPBlock();
      DWORD* active;
      DWORD* inactive;
   };
   
   class VarPool
   {
   public:
      VarPool( const char* name, unsigned size );
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
      void expand();
      void* copy_to_active( void* src, unsigned nwords );
      
      const char* name;
      const unsigned size;
      unsigned index;
      std::list<VPBlock*> blocks;
      VPBlock* block;
      std::list<VPBlock*>::iterator it;
   };
   
}  // MEMORY

}

#endif
