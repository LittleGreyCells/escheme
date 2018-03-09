#include <string.h>
#include <array>
#include <list>

#include "memory.hxx"
#include "error.hxx"
#include "regstack.hxx"

using std::array;
using std::list;

//
// the global objects
//

SEXPR MEMORY::string_null;
SEXPR MEMORY::vector_null;
SEXPR MEMORY::listbuilder;

#ifdef GC_STATISTICS_DETAILED
array<UINT32, NUMKINDS> MEMORY::ReclamationCounts;
#endif

FrameStore frameStore;

list<MEMORY::Marker> markers;

void MEMORY::register_marker( Marker marker )
{
   markers.push_back( marker );
}

//////////////////////////////////////////
//
// Node Block Pool
//
//////////////////////////////////////////

long  MEMORY::TotalNodeCount  = 0;
long  MEMORY::FreeNodeCount   = 0;
int   MEMORY::CollectionCount = 0;

#define freename(n)        delete getname(n)
#define freestringdata(n)  delete getstringdata(n)
#define freevectordata(n)  delete getvectordata(n)
#define freebvecdata(n)    delete getbvecdata(n)

static SEXPR FreeNodeList;

struct NodeBlock
{
   array<Node, NODE_BLOCK_SIZE> nodes;
};

static list<NodeBlock*> blocks;

// 
// Allocate a new node block
//   Populate the FreeNodeList
//
static void NewNodeBlock()
{
   NodeBlock* block = new NodeBlock;

   blocks.push_back(block);

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
// End NodeBlockPool
//

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

int MEMORY::suspensions = 0;

static void badnode( SEXPR n )
{
   char buffer[80];
   SPRINTF(buffer, "bad node (%p,%d) during gc", n->id(), nodekind(n));
   ERROR::fatal(buffer);
}

//
// Mark this node and all reachable substructures
//

// marking Functions

#define markedp(n) ((n)->mark)
#define setmark(n) ((n)->mark = 1)
#define resetmark(n) ((n)->mark = 0)

void MEMORY::mark( SEXPR n )
{
   if ( n == nullptr )
      ERROR::fatal( "marking nullptr; abandoning gc" );

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
	 // mark the frame if not nullptr
	 FRAME frame = getenvframe(n);
	 if ( frame != nullptr )
	 {
	    mark( getframevars(frame) );
	    const int nslots = getframenslots(frame);
	    for (int i = 0; i < nslots; ++i)
	       mark( frameref(frame, i) );
	 }
	 // mark the base env
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
	 const UINT32 length = getvectorlength(n);
	 for (UINT32 i = 0; i < length; ++i)
	    mark( vectorref(n, i) );
	 break;
      }
  
      case n_symbol:
	 setmark(n);
	 mark( getsymbolpair(n) );
	 break;
    
      case n_closure:
      {
	 setmark(n);
	 mark( getclosurecode(n) );
	 mark( getclosurepair(n) );
	 break;
      }

      case n_fixnum:
      case n_flonum:
      case n_string:
      case n_port:
      case n_char:
      case n_bvec:
      case n_func:
	 setmark(n);
	 break;

      case n_eval:
      case n_apply:
      case n_callcc:
      case n_map:
      case n_foreach:
      case n_force:
	 setmark(n);
	 break;

      case n_gref:
	 setmark(n);
	 mark( gref_getsymbol(n) );
	 break;

      case n_fref:
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
   const int depth = stack.getdepth();
   for (int i = 0; i < depth; ++i)
      mark( stack[i] );
}

//
// Sweep up the unused (unreachable) nodes
//   This should be called after all reachable nodes are marked
//   All unmarked nodes are collected into the free node list
//
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
	 SEXPR p = &node;

	 if (markedp(p))
	 {
	    resetmark(p);
	 }
	 else
	 {
	    // reclaim the node
	    switch (nodekind(p))
	    {
	       case n_symbol:
		  freename(p);
		  break;

	       case n_string:
		  freestringdata(p);
		  break;

	       case n_vector:
		  freevectordata(p);
		  break;

	       case n_bvec:
		  freebvecdata(p);
		  break;

	       case n_environment:
		  if ( getenvframe(p) )
		     frameStore.free( getenvframe(p) ); 
		  break;

	       default:
		  break;
	    }

	    MEMORY::FreeNodeCount += 1;
#ifdef GC_STATISTICS_DETAILED
	    MEMORY::ReclamationCounts[nodekind(p)] += 1;
#endif
	    // minimal reinitialization
	    new (p) Node( n_free, FreeNodeList );

	    FreeNodeList = p;
	 }
      }
   }
}

void MEMORY::gc()
{
   if (suspensions > 0)
      return;

   CollectionCount += 1;

   // mark memory managed roots
   mark( string_null );
   mark( vector_null );
   mark( listbuilder );

   // notify all clients to mark their active roots
   for ( auto marker : markers )
      marker();

   // collect the unused nodes
   sweep();
}

//////////////////////////////////////////////////////
//
// Private Allocation Functions
//
/////////////////////////////////////////////////////

static SEXPR newnode( NodeKind kind )
{
   if (nullp(FreeNodeList))
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

static SEXPR* newvectordata( UINT32 length )
{
   SEXPR* v = new SEXPR[length];
   for (UINT32 i = 0; i < length; ++i)
      v[i] = null;
   return v;
}

static BYTE* newbvecdata( UINT32 length )
{
   BYTE* v = new BYTE[length];
   for (UINT32 i = 0; i < length; ++i)
      v[i] = 0;
   return v;
}

static char* newstringdata( UINT32 length ) { return new char[length]; }
static char* newsymbolname( UINT32 length ) { return new char[length]; }

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

SEXPR MEMORY::symbol( const char* s )      // (<name> <value>  <plist>)
{
   regstack.push( cons(null, null) );
   SEXPR n = newnode(n_symbol);
   char* str = newsymbolname(strlen(s)+1);
   strcpy(str, s);
   setname(n, str);
   setsymbolpair( n, regstack.pop() );
   return n;
}

SEXPR MEMORY::string( UINT32 length )        // (<length> . "")
{
   SEXPR n = newnode(n_string);
   char* str = newstringdata(length+1);
   strcpy(str, "");
   setstringlength(n, length);
   setstringindex(n, 0);
   setstringdata(n, str);
   return n;
}

SEXPR MEMORY::string( const char* s )     // (<length> . s)
{
   const UINT32 slen = strlen(s);
   if (slen == 0)
   {
      return string_null;
   }
   else
   {
      SEXPR n = MEMORY::string(slen);
      strcpy(getstringdata(n), s);
      return n;
   }
}

SEXPR MEMORY::string_port()               // (<length> . <string>)
{
   SEXPR n = newnode(n_string_port);
   setmode(n, 0);
   setstringportstring(n, null);
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
   SEXPR n = newnode(n_vector);
   setvectorlength(n, length);
   setvectordata(n, newvectordata(length));
   return n;
}

SEXPR MEMORY::string_resize( SEXPR string, UINT32 delta )
{
   guard(string, stringp);

   const UINT32 old_length = getstringlength(string);
   const UINT32 new_length = old_length + delta;

   if (new_length > MAX_STRING_SIZE)
      ERROR::severe( "string exceeds maximum size" );
      
   char* old_data = getstringdata(string);
   char* new_data = new char[new_length];
      
   strcpy(new_data, old_data);

   delete old_data;

   setstringlength(string, new_length);
   setstringdata(string, new_data);

   return string;
}

SEXPR MEMORY::continuation()
{
   SEXPR n = newnode(n_continuation);
   cont_setstate(n, null);
   return n;
}

SEXPR MEMORY::byte_vector( UINT32 length )                // (<byte-vector>)
{
   SEXPR n = newnode(n_bvec);
   setbveclength(n, length);
   setbvecdata(n, newbvecdata(length));
   return n;
}

SEXPR MEMORY::func( PRIMITIVE func, NodeKind kind )     // (<func>)
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
   regstack.push( cons(env, null) );
   SEXPR n = newnode(n_closure);
   setclosurecode(n, code);
   setclosurepair( n, regstack.pop() );
   return n;
}

SEXPR MEMORY::environment( FRAME frame, SEXPR env )   // (<frame> . <env>)
{
   SEXPR n = newnode(n_environment);
   setenvframe(n, frame);
   setenvbase(n, env);
   return n;
}

FRAME MEMORY::frame( UINT32 nslots )
{
   return frameStore.alloc( nslots );
}

SEXPR MEMORY::gref( SEXPR symbol )
{
   SEXPR n = newnode(n_gref);
   gref_setsymbol(n, symbol);
   return n;
}

SEXPR MEMORY::fref( int depth, int index )
{
   SEXPR n = newnode(n_fref);
   fref_setdepth(n, depth);
   fref_setindex(n, index);
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
   string_null = string(UINT32(0));
   vector_null = vector(UINT32(0));
   listbuilder = cons(null, null);
}
