#include "printer.hxx"

#include "sexpr.hxx"
#include "error.hxx"
#include "recumark.hxx"
#include "format.hxx"

namespace escheme
{

static char buffer[MAX_IMAGE_LENGTH];

void PRINTER::newline( SEXPR outport )
{
   PIO::put(outport, '\n');
}

void PRINTER::print_list( SEXPR outport, const SEXPR n, QuoteStyle style )
{
   if (markedp(n))
   {
      PIO::put(outport, "<recursive>...");
      return;
   }

   RECURSIVE_MARKER rm(n);

   SEXPR s = n;
   PIO::put(outport, '(');
   while ( anyp(s) )
   {
      print_sexpr(outport, getcar(s), style);
    
      SEXPR tail = getcdr(s);
      if (nullp(tail))
      {
	 break;
      }
      else if (consp(tail))
      {
	 PIO::put(outport, ' ');
	 s = tail;
	 if (markedp(s))
	 {
	    PIO::put(outport, "(...)");
	    break;
	 }
      }
      else
      {
	 PIO::put(outport, " . ");
	 print_sexpr(outport, tail, style);
	 break;
      }
   }
   PIO::put(outport, ')');
}

void PRINTER::print_vector( SEXPR outport, const SEXPR n, QuoteStyle style )
{
   if (markedp(n))
   {
      PIO::put(outport, "#(...)");
      return;
   }

   RECURSIVE_MARKER rm(n);

   PIO::put(outport, "#(");
   for (UINT32 i = 0; i < getvectorlength(n); ++i)
   {
      if (i != 0) 
	 PIO::put(outport, ' ');
      print_sexpr(outport, vectorref(n, i), style);
   }
   PIO::put(outport, ')');
}

void PRINTER::print_string( SEXPR outport, const char* p, QuoteStyle style )
{
   if ( style == QUOTE )
      PIO::put( outport, '"' );
   
   while ( *p )
      PIO::put( outport, *p++ );
   
   if ( style == QUOTE )
      PIO::put( outport, '"' );
}

void PRINTER::print_sexpr( SEXPR outport, const SEXPR n, QuoteStyle style )
{
   if (nullp(n))
   {
      PIO::put(outport, "()");
   }
   else
   {
      switch (nodekind(n))
      {
	 case n_cons:
	    print_list(outport, n, style);
	    break;

	 case n_vector:
	    print_vector(outport, n, style);
	    break;

	 case n_symbol:
            print_string( outport, getname(n), NO_QUOTE );
	    break;

	 case n_fixnum:
	    PIO::put( outport, format("%d", getfixnum(n)) );
	    break;

	 case n_flonum:
	    PIO::put( outport, format("%f", getflonum(n)) );
	    break;

	 case n_string:
            print_string( outport, getstringdata(n), style );
	    break;

	 case n_char:
	    if ( style == QUOTE )
	    {
	       const char ch = getcharacter(n);
	       if (ch == '\n')
	       {
		  PIO::put(outport, "#\\newline");
	       }
	       else if (ch == ' ')
	       {
		  PIO::put(outport, "#\\space");
	       }
	       else if (ch == '\t')
	       {
		  PIO::put(outport, "#\\tab");
	       }
	       else
	       {
		  PIO::put( outport, format( "#\\%c", ch ) );
	       }
	    }
	    else
	    {
	       PIO::put( outport, format( "%c", getcharacter(n) ) );
	    }
	    break;

	 case n_func:
	 case n_apply:
	 case n_callcc:
	 case n_eval:
	 case n_map:
	 case n_foreach:
	 case n_force:
	    PIO::put( outport, format( "{primitive:%s}", getprimname(n) ) );
	    break;

	 case n_port:
	    PIO::put( outport, format( "{port:%p}", n->id() ) );
	    break;

	 case n_string_port:
	    PIO::put( outport, format( "{string-port:%p}", n->id() ) );
	    break;

	 case n_closure:
	    PIO::put( outport, format( "{closure:%p}", n->id() ) );
	    break;

	 case n_continuation:
	    PIO::put( outport, format( "{continuation:%p}", n->id() ) );
	    break;

	 case n_bvec:
	    if ( 0 )
	    {
	       PIO::put( outport, format( "{byte-vector:%p}", n->id() ) );
	    }
	    else
	    {
	       PIO::put(outport, "#(");
	       for ( auto i = 0; i < getbveclength(n); )
	       {
		  const auto b = (unsigned)bvecref(n, i);
		  PIO::put(outport, format( "%d", b) );
		  i += 1;
		  if ( i < getbveclength(n) )
		     PIO::put( outport, ' ' );
	       }
	       PIO::put( outport, ')' );
	    }
	    break;

	 case n_environment:
	    PIO::put( outport, format( "{environment:%p}", n->id() ) );
	    break;

	 case n_promise:
	    PIO::put( outport, format( "{promise:%p}", n->id() ) );
	    break;

	 case n_code:
	    PIO::put( outport, format( "{code:%p}", n->id() ) );
	    break;

	 case n_dict:
	    PIO::put( outport, format( "{dict:%p}", n->id() ) );
	    break;

	 case n_free:
	    ERROR::severe( format( "{free-cell:%p}", n->id() ).c_str() );
	    break;

	 default:
	 {
	    ERROR::severe( format("bad node (%p, %d) during printing", n->id(), (int)nodekind(n)).c_str() );
	 }
	 break;
      }
   }
}

void PRINTER::print( SEXPR outport, const SEXPR n, QuoteStyle style )
{
   print_sexpr( outport, n, style );
}

void PRINTER::print( const SEXPR s, QuoteStyle style ) 
{ 
   print_sexpr( PIO::stdout_port, s, style ); 
}

}
