#ifndef memory_hxx
#define memory_hxx

#include <cstdio>
#include <array>
#include <string>

#include "sexpr.hxx"
#include "tstack.hxx"

namespace MEMORY
{
   extern long TotalNodeCount;
   extern long FreeNodeCount;
   extern int  CollectionCount;

#ifdef GC_STATISTICS_DETAILED
   extern std::array<UINT32, NUMKINDS> ReclamationCounts;
#endif

   extern SEXPR string_null;
   extern SEXPR vector_null;
   extern SEXPR listtail;
   extern SEXPR listhead;

   void initialize();

   // allocators
   SEXPR symbol( const char* name );
   SEXPR symbol( const std::string& name );
   SEXPR fixnum( FIXNUM fixnum );
   SEXPR flonum( FLONUM flonum );
   SEXPR string( UINT32 length );
   SEXPR string( const char* string );
   SEXPR string( const std::string& string );
   SEXPR character( CHAR ch );
   SEXPR cons( SEXPR car, SEXPR cdr );
   SEXPR prim( FUNCTION func, NodeKind kind = n_func );
   SEXPR port( FILE* file, short mode );
   SEXPR closure( SEXPR code, SEXPR env );
   SEXPR environment( UINT32 nvars, SEXPR vars, SEXPR env );
   SEXPR vector( UINT32 length );
   SEXPR continuation();
   SEXPR byte_vector( UINT32 length );
   SEXPR string_port();
   SEXPR promise( SEXPR exp );
   SEXPR code( SEXPR bcodes, SEXPR sexprs );

   // modifier(s)
   void resize( SEXPR string, UINT32 delta );
  
   // garbage collection

   typedef void (*Marker)();
   void register_marker( Marker );

   extern int suspensions;
   void gc();

   void mark( SEXPR n );
   void mark( TSTACK<SEXPR>& s );
}


struct GcSuspension
{
   const char* name;
   void suspend_gc() { MEMORY::suspensions += 1; }
   void resume_gc() { MEMORY::suspensions -= 1; }

   explicit GcSuspension( const char* n ) : name(n) { suspend_gc(); }
   ~GcSuspension() { resume_gc(); }
};


struct ListBuilder
{
   ListBuilder()
   {
      MEMORY::listtail = MEMORY::listhead;
      setcdr( MEMORY::listtail, null );
   }

   ~ListBuilder()
   {
      MEMORY::listtail = null;
      setcdr( MEMORY::listhead, null );
   }

   void add( SEXPR item )
   {
      setcdr( MEMORY::listtail, MEMORY::cons(item, null) );
      MEMORY::listtail = getcdr( MEMORY::listtail );
   }

   SEXPR get()
   {
      return getcdr( MEMORY::listhead );
   }
};

#endif
