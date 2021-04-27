#ifndef memory_hxx
#define memory_hxx

#include <cstdio>
#include <array>
#include <string>

#include "sexpr.hxx"
#include "tstack.hxx"
#include "framestore.hxx"

namespace escheme
{

namespace MEMORY
{
   extern long TotalNodeCount;
   extern long FreeNodeCount;
   extern int  CollectionCount;
   extern FrameStore frameStore;

#ifdef GC_STATISTICS_DETAILED
   extern std::array<UINT32, NUMKINDS> ReclamationCounts;
#endif

   extern SEXPR string_null;

   void initialize();

   // allocators
   SEXPR character( CHAR ch );
   SEXPR fixnum( FIXNUM fixnum );
   SEXPR flonum( FLONUM flonum );
   SEXPR symbol( const char* name );
   SEXPR symbol( const std::string& name );
   SEXPR string( const char* string );
   SEXPR string( const std::string& string );
   SEXPR string( UINT32 length, char ch );
   SEXPR cons( SEXPR car, SEXPR cdr );
   SEXPR prim( const char* name, FUNCTION func, NodeKind kind = n_func );
   SEXPR port( FILE* file, short mode );
   SEXPR closure( SEXPR code, SEXPR env );
   SEXPR environment( UINT32 nvars, SEXPR vars, SEXPR env );
   SEXPR vector( UINT32 length );
   SEXPR continuation();
   SEXPR byte_vector( UINT32 length );
   SEXPR string_port( SEXPR str, short mode );
   SEXPR promise( SEXPR exp );
   SEXPR code( SEXPR bcodes, SEXPR sexprs );

   // garbage collection

   typedef void (*Marker)();
   void register_marker( Marker );

   extern int suspensions;
   void gc();
   void mark( SEXPR n );
   void mark( TSTACK<SEXPR>& s );
}


class GcSuspension
{
   const char* name;
   
public:
   explicit GcSuspension( const char* n ) : name(n) { suspend_gc(); }
   ~GcSuspension() { resume_gc(); }
   
   void suspend_gc() { MEMORY::suspensions += 1; }
   void resume_gc() { MEMORY::suspensions -= 1; }
};


class ListBuilder
{
   SEXPR tail;
public:
   ListBuilder();
   ~ListBuilder();
   void add( SEXPR item );
   SEXPR get();
};

}

#endif
