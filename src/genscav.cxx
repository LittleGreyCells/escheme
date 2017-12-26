#include "genscav.hxx"
#include "error.hxx"
#include "framestore.hxx"

const int PARTITION_SIZE = 5000;

static Node nodespace[PARTITION_SIZE*2];

SEXPR active = &nodespace[0];
SEXPR other  = &nodespace[PARTITION_SIZE];

static int pindex = 0;

static void swap( SEXPR& a, SEXPR& b )
{
   const SEXPR x = a;
   a = b;
   b = x;
}

#define freename(n)        
#define freestringdata(n)  delete getstringdata(n)
#define freevectordata(n)  delete getvectordata(n)
#define freebvecdata(n)    delete getbvecdata(n)

static void reclaim( SEXPR p )
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
	 
      case n_continuation:
      case n_vector:
	 freevectordata(p);
	 break;
	 
      case n_bvec:
	 freebvecdata(p);
	 break;
	 
      case n_environment:
	 frameStore.free( getenvframe(p) ); 
	 break;
	 
      default:
	 break;
   }
}

SEXPR GENSCAV::newnode( NodeKind kind )
{
   if ( pindex >= PARTITION_SIZE )
      gc();

   SEXPR n = active + pindex++;

   if ( n->copy )
   {
      // if node copied, don't destroy data
      n->copy = 0;
   }
   else
   {
      // if node not copied, do destroy data
      reclaim( n );
   }

   n->kind = kind;

   return n;
}

void GENSCAV::gc()
{
   // call markers (now copiers)
   swap( active, other );
   pindex = 0;
}

static void copy( SEXPR& n )
{
   SEXPR copy = active + pindex++;
   (*copy) = (*n);
   n->copy = copy;
   n = n->copy;
}

void GENSCAV::mark( SEXPR& n )
{
   if ( nullp(n) )
      return;

   if ( n->copy )
   {
      n = n->copy;
      return;
   }

   switch ( nodekind(n) )
   {
      case n_cons:
      case n_promise:
      {
	 copy(n);
	 mark( getcar(n) );
	 mark( getcdr(n) );
	 break;
      }
    
      case n_environment:
      {
	 copy(n);
	 FRAME frame = getenvframe(n);
	 if (frame != nullptr)
	 {
	    mark( frame->vars );
	    const int nslots = getframenslots(frame);
	    for (int i = 0; i < nslots; ++i)
	       mark( frameref(frame, i) );
	 }
	 mark( getenvbase(n) );
	 break;
      }
  
      case n_string_port:
      {
	 copy(n);
	 mark( getstringportstring(n) );
	 break;
      }

      case n_continuation:
      case n_vector:
      {
	 copy(n);
	 const UINT32 length = getvectorlength(n);
	 for (UINT32 i = 0; i < length; ++i)
	    mark( vectorref(n, i) );
	 break;
      }
  
      case n_symbol:
      {
	 copy(n);
	 mark( getvalue(n) );
	 mark( getplist(n) );
	 break;
      }
    
      case n_closure:
      {
	 copy(n);
	 mark( getclosurecode(n) );
	 mark( getclosurebenv(n) );
	 mark( getclosurevars(n) );
	 break;
      }

      case n_fixnum:
      case n_flonum:
      case n_string:
      case n_port:
      case n_char:
      case n_bvec:
      case n_func:
      case n_apply:
      case n_callcc:
      case n_eval:
      case n_map:
      case n_foreach:
      case n_force:
      {
	 copy(n);
	 break;
      }

      case n_gref:
      {
	 copy(n);
	 mark( gref_getsymbol(n) );
	 break;
      }

      case n_fref:
      {
	 copy(n);
	 break;
      }

      case n_null:
      {
	 break;
      }
   
      case n_free:
      default:
      {
	 ERROR::fatal( "bad node during gc" );
	 break;
      }
   }
}

void GENSCAV::mark( TSTACK<SEXPR>& stack )
{
   const int depth = stack.getdepth();
   for (int i = 0; i < depth; ++i)
      mark( stack[i] );
}
