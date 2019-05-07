#include "memory.hxx"

#include <string.h>
#include <array>
#include <list>

#include "error.hxx"
#include "regstack.hxx"
#include "framestore.hxx"
#include "varpool.hxx"

#if 1
#define CACHE_BVEC
#define CACHE_VECTOR
#define CACHE_STRING
#define CACHE_SYMNAME
#define CACHE_CLOSURE
#define CACHE_FRAME
#endif

//
// the global objects
//

SEXPR MEMORY::string_null;
SEXPR MEMORY::vector_null;
SEXPR MEMORY::listtail;
SEXPR MEMORY::listhead;

#ifdef GC_STATISTICS_DETAILED
std::array<UINT32, NUMKINDS> MEMORY::ReclamationCounts;
#endif

MEMORY::FrameStore MEMORY::frameStore;

std::list<MEMORY::Marker> markers;

void MEMORY::register_marker( Marker marker )
{
   markers.push_back( marker );
}

//
// Node Block Pool
//

long  MEMORY::TotalNodeCount  = 0;
long  MEMORY::FreeNodeCount   = 0;
int   MEMORY::CollectionCount = 0;

static SEXPR FreeNodeList;

struct NodeBlock
{
   std::array<Node, NODE_BLOCK_SIZE> nodes;
};

static std::list<NodeBlock*> blocks;

static void NewNodeBlock()
{
   auto block = new NodeBlock;

   blocks.push_back( block );

   MEMORY::TotalNodeCount += block->nodes.size();
   MEMORY::FreeNodeCount += block->nodes.size();

   for ( auto& node : block->nodes )
   {
      SEXPR p = &node;
      new (p) Node( n_free, FreeNodeList );
      FreeNodeList = p;
   }
}

//
// Private Allocation/Deallocation Functions
//

static SEXPR newnode( NodeKind kind )
{
   if ( nullp(FreeNodeList) )
   {
      MEMORY::gc();

      // don't wait till 0, before allocating a new block.
      //   make the threshold 1/5 of the NODE_BLOCK_SIZE.
      if ( MEMORY::FreeNodeCount < NODE_BLOCK_SIZE / 5 )
	 NewNodeBlock();
   }

   MEMORY::FreeNodeCount -= 1;

   SEXPR n = FreeNodeList;
   FreeNodeList = FreeNodeList->getnext();
   n->kind = kind;

   return n;
}

//
// Variable Sized Object Pool
//

namespace MEMORY
{
   bool cache_copy = false;
}

MEMORY::VarPool cache( "newspace", CACHE_START_SIZE, CACHE_EXPANSION );

unsigned MEMORY::CacheSwapCount = 0;
unsigned MEMORY::CacheSize      = cache.getsize();

unsigned MEMORY::get_cache_highwater() { return cache.getindex(); }

//
// Copy To/From New Space
//

#ifdef CACHE_BVEC
inline BYTE* copy_bvec( SEXPR n ) 
{ 
   return reinterpret_cast<BYTE*>( cache.copy_to_inactive( getbvecdata(n), getndwords(n) ) );
}

inline BYTE* tenure_bvec( SEXPR n ) 
{
   BYTE* bv = new BYTE[NBYTES(getndwords(n))];
   std::memcpy( bv, getbvecdata(n), NBYTES(getndwords(n)) );
   return bv;
}
#endif

#ifdef CACHE_STRING
inline char* copy_string( SEXPR n ) 
{ 
   return reinterpret_cast<char*>( cache.copy_to_inactive( getstringdata(n), getndwords(n) ) );
}

inline char* tenure_string( SEXPR n ) 
{
   char* str = new char[NBYTES(getndwords(n))];
   std::memcpy( str, getstringdata(n), NBYTES(getndwords(n)) );
   return str;
}
#endif

#ifdef CACHE_VECTOR
inline SEXPR* copy_vector( SEXPR n )
{
   return reinterpret_cast<SEXPR*>( cache.copy_to_inactive( getvectordata(n), getvectorlength(n) ) );
}

inline SEXPR* tenure_vector( SEXPR n )
{
   SEXPR* v = new SEXPR[getvectorlength(n)];
   std::memcpy( v, getvectordata(n), NBYTES(getvectorlength(n)) );
   return v;
}
#endif

#ifdef CACHE_FRAME
inline FRAME copy_frame( SEXPR n )
{
   FRAME fr = getenvframe(n);
   return reinterpret_cast<FRAME>( cache.copy_to_inactive( fr, getframesize(fr) ) );
}

inline FRAME tenure_frame( SEXPR n )
{
   // clone the old frame
   return MEMORY::frameStore.clone( getenvframe(n) );
}
#endif

#ifdef CACHE_SYMNAME
inline char* copy_symbolname( SEXPR n )
{
   return reinterpret_cast<char*>( cache.copy_to_inactive( getname(n), getndwords(n) ) );
}

inline char* tenure_symbolname( SEXPR n )
{
   char* name = new char[NBYTES(getndwords(n))];
   std::memcpy( name, getname(n), NBYTES(getndwords(n)) );
   return name;
}
#endif

#ifdef CACHE_CLOSURE
inline SEXPR* copy_closure( SEXPR n )
{
   return reinterpret_cast<SEXPR*>( cache.copy_to_inactive( getclosuredata(n), 3 ) );
}

inline SEXPR* tenure_closure( SEXPR n )
{
   SEXPR* v = new SEXPR[3];
   std::memcpy( v, getclosuredata(n), NBYTES(3) );
   return v;
}
#endif

//
// Aging
//

inline void increment_age( SEXPR n ) 
{ 
   if ( n->nage < MAXAGE ) 
      ++n->nage; 
}

//
// Garbage Collection
//
//   GC consists of mark and sweep phases. The mark phase marks all nodes
// reachable from the execution environment. The sweep phase collects all
// unmarked objects into freelists.
//
//   Marking is a cooperative process. Marking clients have registered
// callbacks to be invoked during the marking phase of garbage collection.
// The success of this operation depends entirely upon the dutiful marking
// of client structures by the client. Failure to mark an essential object
// will lead to disaster.
//
//   Node space is managed in a NodeBlock "pool". Presently all objects
// are allocated from this uniform pool. This approached which uses a
// discriminated union is not ideal and definitely not object-oriented.
// But for now this is the implementation which stands. Adopting a
// purely object-orient approach will require considerable redesign.
//

#define markedp(n) ((n)->mark)
#define setmark(n) ((n)->mark = 1)
#define resetmark(n) ((n)->mark = 0)

int MEMORY::suspensions = 0;

static void badnode( SEXPR n )
{
   char buffer[80];
   SPRINTF(buffer, "bad node (%p,%d) during gc", n->id(), nodekind(n));
   ERROR::fatal(buffer);
}

void MEMORY::mark( SEXPR n )
{
#if 0
   if ( n == nullptr )
      ERROR::fatal( "marking nullptr; abandoning gc" );
#endif
   
   if ( nullp(n) || markedp(n) )
      return;

   switch ( nodekind(n) )
   {
      case n_cons:
	 setmark(n);
	 mark( getcar(n) );
	 mark( getcdr(n) );
	 break;
    
      case n_promise:
	 setmark(n);
	 mark( promise_getexp(n) );
	 mark( promise_getval(n) );
	 break;
    
      case n_code:
	 setmark(n);
	 mark( code_getbcodes(n) );
	 mark( code_getsexprs(n) );
	 break;
    
      case n_environment:
      {
	 setmark(n);
         // frame
#ifdef CACHE_FRAME
         if ( cache_copy )
         {
            increment_age( n );
            if ( n->nage < TENURE )
               setenvframe( n, copy_frame(n) );
            else if ( n->nage == TENURE )
               setenvframe( n, tenure_frame(n) );
         }
#endif
         auto frame = getenvframe(n);
         mark( getframevars(frame) );
         const auto nslots = getframenslots(frame);
         for ( int i = 0; i < nslots; ++i )
            mark( frameref(frame, i) );
         // benv
	 mark( getenvbase(n) );
	 break;
      }
  
      case n_string_port:
	 setmark(n);
	 mark( getstringportstring(n) );
	 break;
    
      case n_continuation:
	 setmark(n);
	 mark( cont_getstate(n) );
	 break;

      case n_vector:
      {
	 setmark(n);
	 const auto length = getvectorlength(n);
#ifdef CACHE_VECTOR
	 if ( cache_copy )
	 {
	    increment_age( n );
	    if ( n->nage < TENURE )
	       setvectordata( n, copy_vector(n) );
	    else if ( n->nage == TENURE )
	       setvectordata( n, tenure_vector(n) );
	 }
#endif
	 for ( int i = 0; i < length; ++i )
	    mark( vectorref(n, i) );
	 break;
      }
  
      case n_symbol:
	 setmark(n);
#ifdef CACHE_SYMNAME
	 if ( cache_copy )
	 {
	    increment_age( n );
	    if ( n->nage < TENURE )
	       setname( n, copy_symbolname(n) );
	    else if ( n->nage == TENURE )
	       setname( n, tenure_symbolname(n) );
	 }
#endif
	 mark( getpair(n) );
	 break;
    
      case n_closure:
      {
	 setmark(n);
#ifdef CACHE_CLOSURE
	 if ( cache_copy )
	 {
	    increment_age( n );
	    if ( n->nage < TENURE )
	       setclosuredata( n, copy_closure(n) );
	    else if ( n->nage == TENURE )
	       setclosuredata( n, tenure_closure(n) );
	 }
#endif
	 mark( getclosurecode(n) );
	 mark( getclosurebenv(n) );
	 mark( getclosurevars(n) );
	 break;
      }

      case n_bvec:
	 setmark(n);
#ifdef CACHE_BVEC
	 if ( cache_copy )
	 {
	    increment_age( n );
	    if ( n->nage < TENURE )
	       setbvecdata( n, copy_bvec(n) );
	    else if ( n->nage == TENURE )
	       setbvecdata( n, tenure_bvec(n) );
	 }
#endif
	 break;

      case n_string:
	 setmark(n);
#ifdef CACHE_STRING
	 if ( cache_copy )
	 {
	    increment_age( n );
	    if ( n->nage < TENURE )
	       setstringdata( n, copy_string(n) );
	    else if ( n->nage == TENURE )
	       setstringdata( n, tenure_string(n) );
	 }
#endif
	 break;
         
      case n_fixnum:
      case n_flonum:
      case n_port:
      case n_char:
      case n_func:
      case n_eval:
      case n_apply:
      case n_callcc:
      case n_map:
      case n_foreach:
      case n_force:
	 setmark(n);
	 break;

      case n_null:
	 // null is not allocated from node space
	 break;
   
      case n_free:
      default:
	 badnode(n);
	 break;
   }
}

void MEMORY::mark( TSTACK<SEXPR>& stack )
{
   const auto depth = stack.getdepth();
   for ( int i = 0; i < depth; ++i )
      mark( stack[i] );
}

static void sweep()
{
   FreeNodeList = null;
   MEMORY::FreeNodeCount = 0;

#ifdef GC_STATISTICS_DETAILED
   for ( auto& count : MEMORY::ReclamationCounts ) 
      count = 0;
#endif

   for ( auto block : blocks )
   {
      for ( auto& node : block->nodes )
      {
	 auto p = &node;

	 if ( markedp(p) )
	 {
	    resetmark(p);
	 }
	 else
	 {
	    // reclaim the node
	    switch ( nodekind(p) )
	    {
	       case n_symbol:
#ifdef CACHE_SYMNAME
                  if ( p->nage >= TENURE )
                     delete[] getname( p );
#else
		  delete[] getname( p );
#endif
		  break;

	       case n_closure:
#ifdef CACHE_CLOSURE
                  if ( p->nage >= TENURE )
                     delete[] getclosuredata( p );
#else
                  delete[] getclosuredata( p );
#endif
		  break;

	       case n_string:
#ifdef CACHE_STRING
                  if ( p->nage >= TENURE )
                     delete[] getstringdata( p );
#else
		  delete[] getstringdata( p );
#endif
		  break;

	       case n_vector:
#ifdef CACHE_VECTOR
                  if ( p->nage >= TENURE )
                     delete[] getvectordata( p );
#else
		  delete[] getvectordata( p );
#endif
		  break;

	       case n_bvec:
#ifdef CACHE_BVEC
                  if ( p->nage >= TENURE )
                     delete[] getbvecdata( p );
#else
		  delete[] getbvecdata( p );
#endif
		  break;

               case n_port:
                  if ( getfile(p) != NULL )
                     fclose( getfile(p) );
                  break;

               case n_environment:
#ifdef CACHE_FRAME
                  if ( p->nage >= TENURE )
                     MEMORY::frameStore.free( getenvframe(p) );
#else
                  MEMORY::frameStore.free( getenvframe(p) );
#endif
		  break;                  
                  
	       default:
		  break;
	    }

	    MEMORY::FreeNodeCount += 1;
#ifdef GC_STATISTICS_DETAILED
	    MEMORY::ReclamationCounts[nodekind(p)] += 1;
#endif
	    // minimal reinitialization and link
	    new (p) Node( n_free, FreeNodeList );

	    FreeNodeList = p;
	 }
      }
   }
}

void MEMORY::gc( bool copy )
{
   if (suspensions > 0)
      return;

   CollectionCount += 1;

   cache_copy = copy;

   if ( cache_copy )
   {
      cache.prep();
   }

   // mark memory managed roots
   mark( string_null );
   mark( vector_null );
   mark( listtail );
   mark( listhead );

   // notify all clients to mark their active roots
   for ( auto marker : markers )
      marker();

   // collect the unused nodes
   sweep();

   if ( cache_copy )
   {
      CacheSwapCount += 1;
      cache.swap();
      cache_copy = false;
   }
}

//
// Public Allocation Functions
//

SEXPR MEMORY::fixnum( FIXNUM fixnum )     // (<long int>)
{
   SEXPR n = newnode(n_fixnum);
   setfixnum(n, fixnum);
   return n;
}

SEXPR MEMORY::flonum( FLONUM flonum )     // (<double>)
{
   SEXPR n = newnode(n_flonum);
   setflonum(n, flonum);
   return n;
}

SEXPR MEMORY::character( CHAR ch )   // (<char>)
{
   SEXPR n = newnode(n_char);
   setcharacter(n, ch);
   return n;
}

namespace
{
   inline
   SEXPR new_symbol( const char* s, int length )
   {
      // cache or heap
      const auto size = length+1;
#ifdef CACHE_SYMNAME
      const auto ndwords = NDWORDS( size );
      auto name = reinterpret_cast<char*>( cache.alloc( ndwords ) );
#else
      auto name = new char[size];
#endif
      strcpy(name, s);
      // node space
      regstack.push( MEMORY::cons(null, null) );
      SEXPR n = newnode(n_symbol);
      setname(n, name);
      setpair( n, regstack.pop() );
#ifdef CACHE_SYMNAME
      setndwords( n, ndwords );
#endif
      return n;
   }
}

SEXPR MEMORY::symbol( const char* s )      // (<name> <value>  <plist>)
{
   return new_symbol( s, strlen(s) );
}

SEXPR MEMORY::symbol( const std::string& s )      // (<name> <value>  <plist>)
{
   return new_symbol( s.c_str(), s.length() );
}

SEXPR MEMORY::string( UINT32 length )        // (<length> . "")
{
   // cache or heap
   const auto size = length+1;
#ifdef CACHE_STRING
   const auto ndwords = NDWORDS( size );      // allow for null byte terminator
   auto data = reinterpret_cast<char*>( cache.alloc( ndwords ) );
#else
   auto data = new char[size]; 
#endif
   data[0] = '\0';
   // node space
   SEXPR n = newnode(n_string);
   setstringlength(n, length);
   setstringdata(n, data);
#ifdef CACHE_STRING
   setndwords(n, ndwords);
#endif
   return n;
}

namespace
{
   inline
   SEXPR new_string( const char* s, int length )
   {
      if ( length == 0 )
      {
         return MEMORY::string_null;
      }
      else
      {
         SEXPR n = MEMORY::string( length );
         strcpy( getstringdata(n), s );
         return n;
      }
   }
}

SEXPR MEMORY::string( const char* s )
{
   return new_string( s, strlen(s) );
}

SEXPR MEMORY::string( const std::string& s )
{
   return new_string( s.c_str(), s.length() );
}

SEXPR MEMORY::string_port()               // (<length> . <string>)
{
   SEXPR n = newnode(n_string_port);
   setmode(n, 0);
   setstringportstring(n, null);
   setstringportindex(n, 0);
   return n;
}

SEXPR MEMORY::cons( SEXPR car, SEXPR cdr )  // (<car> . <cdr> )
{
   SEXPR n = newnode(n_cons);
   // setform(n, EV_APPLICATION); {form(0)}
   setcar(n, car);
   setcdr(n, cdr);
   return n;
}

SEXPR MEMORY::vector( UINT32 length )         // (<length> . data[])
{
   // cache or heap
#ifdef CACHE_VECTOR
   auto data = reinterpret_cast<SEXPR*>( cache.alloc( length ) );
#else
   auto data = new SEXPR[length];
#endif
   for ( int i = 0; i < length; ++i )
      data[i] = null;
   // node space
   SEXPR n = newnode(n_vector);
   setvectorlength(n, length);
   setvectordata(n, data);
   return n;
}

void MEMORY::resize( SEXPR string, UINT32 delta )
{
   guard(string, stringp);

   const auto old_length = getstringlength(string);
   const auto new_length = old_length + delta;

   if (new_length > MAX_STRING_SIZE)
      ERROR::severe( "string length exceeds maximum size", MEMORY::fixnum(new_length) );
      
   auto& old_data = getstringdata(string);
   
#ifdef CACHE_STRING
   const auto ndwords = NDWORDS( new_length );
   auto new_data = reinterpret_cast<char*>( cache.alloc( ndwords ) );
#else
   auto new_data = new char[new_length]; 
#endif

   strcpy( new_data, old_data );

#ifndef CACHE_STRING
   delete[] old_data;
#endif

   setstringlength( string, new_length );
   setstringdata( string, new_data );
   
#ifdef CACHE_STRING
   setndwords( string, ndwords );
#endif
}

SEXPR MEMORY::continuation()
{
   SEXPR n = newnode(n_continuation);
   cont_setstate(n, null);
   return n;
}

SEXPR MEMORY::byte_vector( UINT32 length )                // (<byte-vector>)
{
   // cache or heap
#ifdef CACHE_BVEC
   const auto ndwords = NDWORDS( length );
   auto data = reinterpret_cast<BYTE*>( cache.alloc( ndwords ) );
#else
   auto data = new BYTE[length];
#endif
   for ( int i = 0; i < length; ++i )
      data[i] = 0;
   // node space
   SEXPR n = newnode(n_bvec);
   setbveclength(n, length);
   setbvecdata(n, data);
#ifdef CACHE_BVEC
   setndwords( n, ndwords );
#endif
   return n;
}

SEXPR MEMORY::prim( FUNCTION func, NodeKind kind )     // (<prim>)
{
   SEXPR n = newnode(kind);
   setfunc(n, func);
   return n;
}

SEXPR MEMORY::port( FILE* file, short mode )          // (<file>)
{
   SEXPR n = newnode(n_port);
   setfile(n, file);
   setmode(n, mode);
   return n;
}

SEXPR MEMORY::closure( SEXPR code, SEXPR env )       // ( <numv> [<code> <benv> <vars>] )
{
   // cache or heap
#ifdef CACHE_CLOSURE
   auto data = reinterpret_cast<SEXPR*>( cache.alloc( 3 ) );
#else
   auto data = new SEXPR[3];
#endif
   // node space
   SEXPR n = newnode(n_closure);
   setclosuredata(n, data);
   setclosurecode(n, code);
   setclosurebenv(n, env);
   setclosurevars(n, null);
   return n;
}

SEXPR MEMORY::environment( UINT32 nvars, SEXPR vars, SEXPR env )   // (<frame> . <env>)
{
   // cache or heap
#ifdef CACHE_FRAME
   const auto ndwords = FRAMESIZE( nvars );
   auto frame = reinterpret_cast<FRAME>( cache.alloc( ndwords ) );
   setframesize( frame, ndwords );
   setframenslots( frame, nvars );
   for ( int i = 0; i < nvars; ++i )
      frameset( frame, i, null );
#else
   auto frame = frameStore.alloc( nvars );
#endif
   setframevars(frame, vars);
   
   SEXPR n = newnode(n_environment);
   setenvbase(n, env);
   setenvframe(n, frame);
   return n;
}

SEXPR MEMORY::promise( SEXPR exp )
{
   SEXPR n = newnode(n_promise);
   promise_setexp(n, exp);
   promise_setval(n, null);
   return n;
}

SEXPR MEMORY::code( SEXPR bcodes, SEXPR sexprs )
{
   SEXPR n = newnode(n_code);
   code_setbcodes(n, bcodes);
   code_setsexprs(n, sexprs);
   return n;
}

//
// Nullities
//
//   ()  -- null
//   ""  -- null string
//   #() -- null vector
//
//   note: the null object is not allocated from node space.
//

// the null object
SEXPR null = new Node( n_null );

void MEMORY::initialize()
{
   FreeNodeList = null;
   NewNodeBlock();
   string_null = string( 0u );
   vector_null = vector( 0u );
   listtail = null;
   listhead = cons(null, null);
}
