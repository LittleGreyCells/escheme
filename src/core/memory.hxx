#ifndef memory_hxx
#define memory_hxx

#include <cstdio>
#include <array>

#include "sexpr.hxx"
#include "tstack.hxx"
#include "framestore.hxx"

using std::array;

extern FrameStore frameStore;

namespace MEMORY
{
   extern long TotalNodeCount;
   extern long FreeNodeCount;
   extern int  CollectionCount;

#ifdef GC_STATISTICS_DETAILED
   extern array<UINT32, NUMKINDS> ReclamationCounts;
#endif

   extern SEXPR string_null;
   extern SEXPR vector_null;
   extern SEXPR listbuilder;

   void initialize();

   // allocators
   SEXPR symbol( const char* name );
   SEXPR fixnum( FIXNUM fixnum );
   SEXPR flonum( FLONUM flonum );
   SEXPR string( UINT32 length );
   SEXPR string( const char* string );
   SEXPR character( CHAR ch );
   SEXPR cons( SEXPR car, SEXPR cdr );
   SEXPR func( PRIMITIVE func, NodeKind kind = n_func );
   SEXPR port( FILE* file, short mode );
   SEXPR closure( SEXPR code, SEXPR env );
   SEXPR environment( FRAME frame, SEXPR env );
   SEXPR environment( UINT32 nvars, SEXPR vars, SEXPR env );
   SEXPR vector( UINT32 length );
   SEXPR continuation();
   SEXPR byte_vector( UINT32 length );
   SEXPR string_port();
   FRAME frame( UINT32 nslots );
   SEXPR gref( SEXPR symbol );
   SEXPR fref( int depth, int index );
   SEXPR promise( SEXPR exp );
   SEXPR code( SEXPR bcodes, SEXPR sexprs );

   // modifier(s)
   SEXPR string_resize( SEXPR string, UINT32 delta );
  
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

   GcSuspension( const char* n ) : name(n) { suspend_gc(); }
   ~GcSuspension() { resume_gc(); }
};


class ListBuilder
{
   SEXPR list;
public:
   ListBuilder() : list(MEMORY::listbuilder) { setcdr( list, null ); }
   ~ListBuilder() { setcdr( MEMORY::listbuilder, null ); }

   void add( SEXPR item )
   {
      setcdr( list, MEMORY::cons(item, null) );
      list = getcdr( list );
   }

   SEXPR get() { return getcdr( MEMORY::listbuilder ); }
};

#endif
