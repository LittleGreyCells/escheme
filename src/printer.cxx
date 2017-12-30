#include "printer.hxx"
#include "error.hxx"
#include "recumark.hxx"

static char buffer[256];

static void error( const char* s )
{
   char message[80];
   SPRINTF(message, "PRINTER error: %s\n", s);
   ERROR::severe(message);
}

void PRINTER::newline( SEXPR outport )
{
   PIO::put(outport, '\n');
}

void PRINTER::print_list( SEXPR outport, const SEXPR n, int style )
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

void PRINTER::print_vector( SEXPR outport, const SEXPR n, int style )
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

void PRINTER::print_sexpr( SEXPR outport, const SEXPR n, int style )
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
	    PIO::put(outport, getname(n));
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
	    if (style)
	       SPRINTF(buffer, "\"%s\"", getstringdata(n));
	    else
	       SPRINTF(buffer, "%s", getstringdata(n));
	    PIO::put(outport, buffer);
	    break;

	 case n_char:
	    if (style)
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

	 case n_gref:
	    sprintf(buffer, "{ gref:%s }", getname(gref_getsymbol(n)) );
	    PIO::put(outport, buffer);
	    break;

	 case n_fref:
	    sprintf(buffer, "{ fref:%d,%d }", fref_getdepth(n),  fref_getindex(n) );
	    PIO::put(outport, buffer);
	    break;

	 case n_func:
	 case n_apply:
	 case n_callcc:
	 case n_eval:
	 case n_map:
	 case n_foreach:
	 case n_force:
	    SPRINTF(buffer, "{ prim:%p }", n );
	    PIO::put(outport, buffer);
	    break;

	 case n_port:
	    SPRINTF(buffer, "{ port:%p }", n );
	    PIO::put(outport, buffer);
	    break;

	 case n_string_port:
	    SPRINTF(buffer, "{ sport:%p }", n );
	    PIO::put(outport, buffer);
	    break;

	 case n_closure:
	    SPRINTF(buffer, "{ closure:%p }", n );
	    PIO::put(outport, buffer);
	    break;

	 case n_continuation:
	    SPRINTF(buffer, "{ cont:%p }", n );
	    PIO::put(outport, buffer);
	    break;

	 case n_bvec:
	    if ( 0 )
	    {
	       SPRINTF(buffer, "{ bvec:%p }", n );
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
	    SPRINTF(buffer, "{ env:%p }", n );
	    PIO::put(outport, buffer);
	    break;

	 case n_promise:
	    SPRINTF(buffer, "{ promise:%p }", n );
	    PIO::put(outport, buffer);
	    break;

	 case n_free:
	    SPRINTF(buffer, "{ free:%p }", n );
	    PIO::put(outport, buffer);
	    break;

	 default:
	 {
	    SPRINTF( buffer, "bad node (%p, %d)", n, nodekind(n));
	    error(buffer);
	 }
	 break;
      }
   }
}

void PRINTER::print( SEXPR outport, const SEXPR n, int style )
{
   print_sexpr( outport, n, style );
}
