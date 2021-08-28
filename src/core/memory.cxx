#include "memory.hxx"

#include <cstring>
#include <array>
#include <list>
#include <stack>
#include <algorithm>

#include "error.hxx"
#include "regstack.hxx"
#include "framestore.hxx"
#include "format.hxx"
#include "symdict.hxx"

namespace escheme
{

SEXPR MEMORY::string_null;

namespace MEMORY
{
   SEXPR listhead;
}

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

long MEMORY::TotalNodeCount  = 0;
long MEMORY::FreeNodeCount   = 0;
int  MEMORY::CollectionCount = 0;
int  MEMORY::MaxIterMarkDepth = 0;

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
   FreeNodeList = n->getnext();
   n->kind = kind;

   return n;
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

inline int markedp( SEXPR n ) { return n->mark; }
inline void setmark( SEXPR n ) { n->mark = 1; }
inline void resetmark( SEXPR n ) { n->mark = 0; }

int MEMORY::suspensions = 0;

static void badnode( SEXPR n )
{
   ERROR::fatal( format( "bad node (%p, %d) during gc", n->id(), (int)nodekind(n) ) );
}

//
// iterative marking function
//
void MEMORY::mark( SEXPR n )
{
   if ( markedp(n) )
      return;

   std::stack<SEXPR> sexprs;

   sexprs.push(n);

   while ( !sexprs.empty() )
   {
      #ifdef MAX_DEPTH
      MaxIterMarkDepth = std::max( MaxIterMarkDepth, (int)sexprs.size() );
      #endif
      auto n = sexprs.top();
      sexprs.pop();

     start_mark:
	 
      if ( !markedp(n) )
      {
	 setmark(n);

	 switch ( nodekind(n) )
	 {
	    case n_null:
	    case n_bvec:
	    case n_string:
	    case n_string_port:
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
	       break;
	       
	    case n_cons:
	       sexprs.push( getcar(n) );
	       n = getcdr(n);
	       goto start_mark;
	       
	    case n_promise:
	       sexprs.push( promise_getexp(n) );
	       n = promise_getval(n);
	       goto start_mark;
	       
	    case n_code:
	       setmark( code_getbcodes(n) );
	       n = code_getsexprs(n);
	       goto start_mark;
	       
	    case n_continuation:
	       n = cont_getstate(n);
	       goto start_mark;
	       
	    case n_environment:
	    {
	       // frame
	       auto frame = getenvframe(n);
	       sexprs.push( getframevars(frame) );
	       const int nslots = getframenslots(frame);
	       for ( int i = 0; i < nslots; ++i )
		  sexprs.push( frameref(frame, i) );
	       // benv
	       n = getenvbase(n);
	       goto start_mark;
	    }
	    case n_vector:
	    case n_dict:
	    {
	       const int length = getvectorlength(n);
	       for ( int i = 0; i < length; ++i )
		  sexprs.push( vectorref(n, i) );
	       break;
	    }
	    
	    case n_closure:
	    {
	       sexprs.push( getclosurecode(n) );
	       sexprs.push( getclosurebenv(n) );
	       n = getclosurevars(n);
	       goto start_mark;
	    }
	    
	    case n_symbol:
	       n = getpair(n);
	       goto start_mark;
	       
	    case n_assoc_env:
	       for ( auto& x : assocenv_getdict(n)->umap )
	       {
		  sexprs.push( x.first );
		  sexprs.push( x.second );
	       }
	       n = assocenv_getbase(n);
	       goto start_mark;
	       
	    case n_free:
	    default:
	       badnode(n);
	       break;
	 }
      }
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
	    switch ( nodekind(p) )
	    {
	       case n_symbol:
		  delete[] getname( p );
		  break;
                  
	       case n_string:
		  delete[] getstringdata( p );
		  break;
                  
	       case n_bvec:
		  delete[] getbvecdata( p );
		  break;
                  
	       case n_environment:
		  MEMORY::frameStore.free( getenvframe(p) );
		  break;                  
                  
	       case n_vector:
	       case n_dict:
		  delete[] getvectordata( p );
		  break;
                  
	       case n_closure:
		  delete[] getclosuredata( p );
		  break;
                  
	       case n_port:
		  if ( getfile(p) != NULL )
		     fclose( getfile(p) );
		  break;
                  
	       case n_string_port:
		  delete getstringportstring( p );
		  break;
                  
	       case n_assoc_env:
		  delete assocenv_getdict( p );
		  break;

	       default:
		  break;
	    }

	    MEMORY::FreeNodeCount += 1;
#ifdef GC_STATISTICS_DETAILED
	    MEMORY::ReclamationCounts[nodekind(p)] += 1;
#endif
	    // minimal reinitialization and link
	    new ( p ) Node( n_free, FreeNodeList );

	    FreeNodeList = p;
	 }
      }
   }
}

void MEMORY::gc()
{
   if ( suspensions > 0 )
      return;

   CollectionCount += 1;

   // mark memory managed roots
   mark( string_null );
   mark( listhead );

   // notify all clients to mark their active roots
   for ( auto marker : markers )
      marker();

   // collect the unused nodes
   sweep();
}

//
// Public Allocation Functions
//

SEXPR MEMORY::fixnum( FIXNUM fixnum )     // (<long int>)
{
   auto n = newnode(n_fixnum);
   setfixnum( n, fixnum );
   return n;
}

SEXPR MEMORY::flonum( FLONUM flonum )     // (<double>)
{
   auto n = newnode(n_flonum);
   setflonum( n, flonum );
   return n;
}

SEXPR MEMORY::character( CHAR ch )   // (<char>)
{
   auto n = newnode(n_char);
   setcharacter( n, ch );
   return n;
}

static char* duplicate( const char* s, int length )
{
   auto dup = new char[length+1];
   strcpy( dup, s );
   return dup;
}

SEXPR MEMORY::symbol( const std::string& s )
{
   regstack.push( MEMORY::cons( null, null ) );
   auto n = newnode(n_symbol);
   setname( n, duplicate( s.c_str(), s.length() ) );
   setpair( n, regstack.pop() );
   return n;
}

static SEXPR new_string( const char* s, int length )
{
   if ( length == 0 )
   {
      return MEMORY::string_null;
   }
   else
   {
      auto n = newnode(n_string);
      setstringlength( n, length );
      setstringdata( n, duplicate(s, length) );
      return n;
   }
}

SEXPR MEMORY::string( const std::string& s )
{
   return new_string( s.c_str(), s.length() );
}

SEXPR MEMORY::string( UINT32 length, char ch )
{
   auto s = new_string( "", length );
   for ( int i = 0; i < length; ++i )
      getstringdata(s)[i] = ch;
   getstringdata(s)[length] = '\0';
   return s;
}

SEXPR MEMORY::string_port( SEXPR str, short mode )
{
   auto n = newnode(n_string_port);
   setmode( n, mode );
   setstringportstring( n, new std::string( getstringdata(str) ) );
   setstringportindex( n, 0 );
   return n;
}

SEXPR MEMORY::cons( SEXPR car, SEXPR cdr )  // (<car> . <cdr> )
{
   auto n = newnode(n_cons);
   setcar( n, car );
   setcdr( n, cdr );
   return n;
}

SEXPR MEMORY::vector( UINT32 length )         // (<length> . data[])
{
   auto data = new SEXPR[length];
   for ( int i = 0; i < length; ++i )
      data[i] = null;
   auto n = newnode(n_vector);
   setvectorlength( n, length );
   setvectordata( n, data );
   return n;
}

SEXPR MEMORY::continuation()
{
   auto n = newnode(n_continuation);
   cont_setstate( n, null );
   return n;
}

SEXPR MEMORY::byte_vector( UINT32 length )                // (<byte-vector>)
{
   auto data = new BYTE[length];
   for ( int i = 0; i < length; ++i )
      data[i] = 0;
   auto n = newnode(n_bvec);
   setbveclength( n, length );
   setbvecdata( n, data );
   return n;
}

SEXPR MEMORY::prim( const std::string& name, FUNCTION func, NodeKind kind )     // (<prim>)
{
   // note: primitive functions are not allocated from node space
   auto n = new Node(kind);
   setprimname( n, duplicate(name.c_str(), name.length()) );
   setprimfunc( n, func );
   return n;
}

SEXPR MEMORY::port( FILE* file, short mode )          // (<file>)
{
   auto n = newnode(n_port);
   setfile( n, file );
   setmode( n, mode );
   return n;
}

SEXPR MEMORY::closure( SEXPR code, SEXPR env )       // ( <numv> [<code> <benv> <vars>] )
{
   auto data = new SEXPR[3];
   auto n = newnode(n_closure);
   setclosuredata( n, data );
   setclosurecode( n, code );
   setclosurebenv( n, env );
   setclosurevars( n, null );
   return n;
}

SEXPR MEMORY::environment( UINT32 nvars, SEXPR vars, SEXPR env )   // (<frame> . <env>)
{
   auto frame = frameStore.alloc( nvars, vars );
   auto n = newnode(n_environment);
   setenvbase( n, env );
   setenvframe( n, frame );
   return n;
}

SEXPR MEMORY::promise( SEXPR exp )
{
   auto n = newnode(n_promise);
   promise_setexp( n, exp );
   promise_setval( n, null );
   return n;
}

SEXPR MEMORY::code( SEXPR bcodes, SEXPR sexprs )
{
   auto n = newnode(n_code);
   code_setbcodes( n, bcodes );
   code_setsexprs( n, sexprs );
   return n;
}

SEXPR MEMORY::dict( UINT32 size )
{
   auto n = vector( size );
   setnodekind( n, n_dict );
   return n;
}

SEXPR MEMORY::assocenv( SEXPR env )
{
   auto n = newnode(n_assoc_env);
   assocenv_setdict( n, new SymDict() );
   assocenv_setbase( n, env );
   return n;
}

//
// Nullities
//
//   ()  -- null
//   ""  -- null string
//

// the null object is not allocated from node space
SEXPR null = new Node( n_null );

static SEXPR make_string_null()
{
   auto n = newnode(n_string);
   setstringlength( n, 0 );
   setstringdata( n, duplicate("", 0) );
   return n;
}

void MEMORY::initialize()
{
   FreeNodeList = null;
   NewNodeBlock();
   string_null = make_string_null();
   listhead = cons( null, null );
}

//
// ListBuilder
//

ListBuilder::ListBuilder()
{
   tail = MEMORY::listhead;
   setcdr( tail, null );
}

ListBuilder::~ListBuilder()
{
   setcdr( MEMORY::listhead, null );
}

void ListBuilder::add( SEXPR item )
{
   auto cell = MEMORY::cons( item, null );
   setcdr( tail, cell );
   tail = cell;
}

SEXPR ListBuilder::get()
{
   return getcdr( MEMORY::listhead );
}

}
