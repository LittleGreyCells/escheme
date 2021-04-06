#include "printer.hxx"

#include "sexpr.hxx"
#include "error.hxx"
#include "recumark.hxx"

namespace escheme
{

static char buffer[MAX_IMAGE_LENGTH];

static void error( const char* s )
{
   char message[300];
   SPRINTF(message, "PRINTER error: %s\n", s);
   ERROR::severe(message);
}

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
	    SPRINTF(buffer, "%ld", getfixnum(n));
	    PIO::put(outport, buffer);
	    break;

	 case n_flonum:
	    SPRINTF(buffer, "%lf", getflonum(n));
	    PIO::put(outport, buffer);
	    break;

	 case n_string:
            print_string( outport, getstringdata(n), style );
	    break;

	 case n_char:
	    if ( style == QUOTE )
	    {
	       const int ch = getcharacter(n);
	       if (ch == '\n')
	       {
		  PIO::put(outport, "#\\newline");
	       }
	       else if (ch == ' ')
	       {
		  PIO::put(outport, "#\\space");
	       }
	       else
	       {
		  SPRINTF(buffer, "#\\%c", ch);
		  PIO::put(outport, buffer);
	       }
	    }
	    else
	    {
	       SPRINTF(buffer, "%c", (int)getcharacter(n));
	       PIO::put(outport, buffer);
	    }
	    break;

	 case n_func:
	 case n_apply:
	 case n_callcc:
	 case n_eval:
	 case n_map:
	 case n_foreach:
	 case n_force:
	    SPRINTF(buffer, "{primitive:%s}", getprimname(n));
	    PIO::put(outport, buffer);
	    break;

	 case n_port:
	    SPRINTF(buffer, "{port:%p}", n->id() );
	    PIO::put(outport, buffer);
	    break;

	 case n_string_port:
	    SPRINTF(buffer, "{string-port:%p}", n->id() );
	    PIO::put(outport, buffer);
	    break;

	 case n_closure:
	    SPRINTF(buffer, "{closure:%p}", n->id() );
	    PIO::put(outport, buffer);
	    break;

	 case n_continuation:
	    SPRINTF(buffer, "{continuation:%p}", n->id() );
	    PIO::put(outport, buffer);
	    break;

	 case n_bvec:
	    if ( 0 )
	    {
	       SPRINTF(buffer, "{byte-vector:%p}", n->id() );
	       PIO::put(outport, buffer);
	    }
	    else
	    {
	       PIO::put(outport, "#(");
	       for (UINT32 i = 0; i < getbveclength(n);)
	       {
		  const BYTE b = bvecref(n, i);
		  SPRINTF( buffer, "%d", b );
		  PIO::put(outport, buffer);
		  i += 1;
		  if (i < getbveclength(n))
		     PIO::put(outport, " ");
	       }
	       PIO::put(outport, ")");
	    }
	    break;

	 case n_environment:
	    SPRINTF(buffer, "{environment:%p}", n->id() );
	    PIO::put(outport, buffer);
	    break;

	 case n_promise:
	    SPRINTF(buffer, "{promise:%p}", n->id() );
	    PIO::put(outport, buffer);
	    break;

	 case n_code:
	    SPRINTF(buffer, "{code:%p}", n->id() );
	    PIO::put(outport, buffer);
	    break;

	 case n_free:
	    SPRINTF(buffer, "{free-cell:%p}", n->id() );
	    PIO::put(outport, buffer);
	    break;

	 default:
	 {
	    SPRINTF( buffer, "bad node (%p, %d)", n->id(), nodekind(n));
	    error(buffer);
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
