#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <string>

#include "reader.hxx"
#include "symtab.hxx"
#include "regstack.hxx"
#include "error.hxx"
#include "memory.hxx"

namespace escheme
{

SEXPR READER::symbol_dot;

inline void push(SEXPR s) { regstack.push(s); }
inline SEXPR pop() { return regstack.pop(); }
inline SEXPR& top() { return regstack.top(); }


/////////////////////////////////////////////////////////////////
//
// Predicates
//
/////////////////////////////////////////////////////////////////

int READER::eof_objectp( const SEXPR n )
{
   return n == PIO::eof_object;
}


/////////////////////////////////////////////////////////////////
//
// Reader
//
/////////////////////////////////////////////////////////////////

int READER::scan( SEXPR inport )
{
   while (true)
   {
      const int ch = PIO::get(inport);
      if (ch == EOF || !isspace(ch))
	 return ch;
   }
}

void READER::read_comment( SEXPR inport )
{
   while (true)
   {
      const int ch = PIO::get(inport);
      if (ch == EOF)
      {
	 break;
      }
      else if (ch == '\n')
      {
	 PIO::unget(inport, ch);
	 break;
      }
   }
}

SEXPR READER::read_list( SEXPR inport, char terminator )
{
   SEXPR list = null;
   SEXPR last = null;

   while (true)
   {
      int ch = scan(inport);
      if (ch == EOF)
	 ERROR::severe("unexpected EOF");

      switch (ch)
      {
	 case ';':
	    read_comment(inport);
	    break;

	 case ')':
	    if (terminator != ch)
	       ERROR::warning("expecting ] list terminator");
	    return anyp(list) ? pop() : null;

	 case ']':
	    if (terminator != ch)
	       ERROR::warning("expecting ) list terminator");
	    return anyp(list) ? pop() : null;

	 default:
	 {
	    PIO::unget(inport, ch);
	    SEXPR n = read_sexpr(inport);
	    if (symbolp(n) && (n == symbol_dot))
	    {
	       if (nullp(last))
		  ERROR::severe("misplaced dot");

	       setcdr(last, read_sexpr(inport));

	       while ((ch = scan(inport)) == ';')
		  read_comment(inport);

	       if ( !(ch == ')' || ch == ']') )
		  ERROR::severe("missing right paren/bracket");

	       if ( ch == ')' ) 
	       {
		  if (terminator == ']')
		     ERROR::warning("expecting ] list terminator");
	       }
	       else if ( ch == ']' )
	       {
		  if (terminator == ')')
		     ERROR::warning("expecting ) list terminator");
	       }
	       return anyp(list) ? pop() : null;
	    }
	    else if (nullp(list))
	    {
	       push(n);
	       top() = MEMORY::cons(n, null);
	       last = list = top();
	    }
	    else
	    {
	       push(n);
	       n = MEMORY::cons(n, null);
	       pop();
	       setcdr(last, n);
	       last = n;
	    }
	 }
      }
   }
}

static bool issym( int ch )
{
   if (isspace(ch))
   {
      return false;
   }
   else
   {
      for (const char* p = "();[]"; *p != '\0'; )
	 if (*p++ == ch)
	    return false;
      return true;
   }
}

static SEXPR number( const std::string& s )
{
   int i = 0;

   // check for a sign
   if (s[i] == '+' || s[i] == '-')
      i++;

   // check for a string of digits
   int n = 0;
   while (isdigit(s[i])) { i++; n++; }

   // check for a decimal point
   bool d = false;
   int  m = 0;
   if (s[i] == '.')
   {
      i++;
      d = true;
      while (isdigit(s[i])) { i++; m++; }
   }

   // check for an exponent
   if ((n || m) && toupper(s[i]) == 'E')
   {
      i++;
      d = true;
      // check for a sign
      if (s[i] == '+' || s[i] == '-')
	 i++;

      // check for a string of digits
      while (isdigit(s[i])) { i++; m++; }
   }

   // make sure there was at least one digit
   if ( (n == 0 && m == 0) || (i < s.length()) )
      return null;

   return d ? MEMORY::flonum( std::stod( s ) ) :
              MEMORY::fixnum( std::stol( s ) );
}

static void getsymbolname( SEXPR inport, std::string& s )
{
   int ch;

   for ( ; (ch = PIO::get(inport)) != EOF && issym(ch); )
      s.push_back( tolower(ch) );

   PIO::unget(inport, ch);
}

SEXPR READER::read_symbol( SEXPR inport )
{
   std::string s;

   getsymbolname( inport, s );

   if ( s.length() == 0 )
   {
      return ERROR::severe("expecting symbol name");
   }
   else
   {
      SEXPR n = number( s );
      return anyp(n) ? n : SYMTAB::enter( s );
   }
}

SEXPR READER::read_string( SEXPR inport )
{
   int ch;
   std::string s;

   for ( ; ((ch = PIO::get(inport)) != EOF) && (ch != '"'); )
   {
      if (ch == '\\')
	 ch = PIO::get(inport);
      if ( ch != EOF )
	 s.push_back( ch );
   }
   return MEMORY::string( s );
}

SEXPR READER::read_vector( SEXPR inport, char terminator )
{
   SEXPR s = read_list(inport, terminator);
   unsigned n = list_length(s);

   push(s);
   SEXPR v = MEMORY::vector(n);
   pop();

   for (unsigned i = 0; i < n; ++i, s = cdr(s))
      vset(v, i, car(s));
   return v;
}

inline bool RANGE( int ch, int lo, int hi ) { return (lo <= ch) && (ch <= hi); }

static bool isbasedigit( int ch, int base )
{
   switch (base)
   {
      case 2: return RANGE( ch, '0', '1' );
      case 4: return RANGE( ch, '0', '3' );
      case 8: return RANGE( ch, '0', '7' );
      case 10: return RANGE( ch, '0', '9' );
      case 16: return RANGE( ch, '0', '9' ) || RANGE( ch, 'a', 'f' );
      default:
	 return false;
   }
}

static FIXNUM todigit( int ch )
{
   return (ch <= '9') ? static_cast<FIXNUM>(ch - '0') : static_cast<FIXNUM>((ch - 'a') + 10);
}

SEXPR READER::read_fixnum( SEXPR inport, int base )
{
   FIXNUM n = 0;
   int ch;
  
   while ( ((ch = PIO::get(inport)) != EOF) && issym(ch) ) 
   {
      if ( isupper(ch) ) 
	 ch = tolower(ch);

      if ( !isbasedigit(ch, base) )
	 ERROR::severe("invalid digit");

      n = n * base + todigit(ch);
   }
  
   PIO::unget(inport, ch);
  
   return MEMORY::fixnum(n);
}

SEXPR READER::read_special( SEXPR inport )
{
   std::string s;

   int ch = scan(inport);

   switch ( ch )
   {
      case '(':
	 return read_vector(inport, ')');

      case '[':
	 return read_vector(inport, ']');

      case '\\':
      {
	 getsymbolname( inport, s );

	 if ( s.compare( "newline" ) == 0 )
	    ch = '\n';
	 else if ( s.compare( "space" ) == 0 )
	    ch = ' ';
	 else if ( s.compare( "tab" ) == 0 )
	    ch = '\t';
	 else if ( s.length() > 1 )
	    ERROR::severe("unknown special symbol");
	 else if ( s.length() == 0 )
	    ch = PIO::get(inport);
	 else
	    ch = s[0];
	 return MEMORY::character(ch);
      }

      case 'b':
      case 'B':
	 return read_fixnum( inport, 2 );
      case 'q':
      case 'Q':
	 return read_fixnum( inport, 4 );
      case 'o':
      case 'O':
	 return read_fixnum( inport, 8 );
      case 'd':
      case 'D':
	 return read_fixnum( inport, 10 );
      case 'x':
      case 'X':
	 return read_fixnum( inport, 16 );

      case '!':
      {
	 getsymbolname( inport, s );

	 if ( s.compare( "true" ) == 0 )
	    return symbol_true;
	 else if ( s.compare( "false" ) == 0 )
	    return symbol_false;
	 else if ( s.compare( "null") == 0 )
	    return null;
	 else if ( s.length() == 0 )
	    return ERROR::severe("expected special symbol after #!");
	 else
	 {
            std::string ss = "#!";
            ss += s;
	    return SYMTAB::enter( ss );
	 }
      }

      default:
      {
	 PIO::unget(inport, ch);

	 getsymbolname( inport, s );

	 if ( s.compare( "t" ) == 0 )
	    return symbol_true;
	 else if ( s.compare( "f" ) == 0)
	    return symbol_false;
	 else
	    return ERROR::severe("unknown special symbol");
      }
   }
}

SEXPR READER::read_quote( SEXPR inport, SEXPR flavor )
{
   push(read_sexpr(inport));
   top() = MEMORY::cons(top(), null);
   top() = MEMORY::cons(flavor, top());
   return pop();
}

SEXPR READER::read_comma( SEXPR inport )
{
   const int ch = PIO::get(inport);

   if (ch == '@')
      return read_quote(inport, symbol_unquotesplicing);
   else
   {
      PIO::unget(inport, ch);
      return read_quote(inport, symbol_unquote);
   }
}

SEXPR READER::read_sexpr( SEXPR inport )
{
   while ( true )
   {
      const int ch = scan(inport);

      if ( ch == EOF )
	 return PIO::eof_object;

      switch ( ch )
      {
	 case ';':
	    read_comment(inport);
	    break;

	 case '#':
	    return read_special(inport);

	 case '(':
	    return read_list(inport, ')');

	 case '[':
	    return read_list(inport, ']');

	 case ')':
	 case ']':
	    return ERROR::severe("misplaced right paren/bracket");

	 case '\"':
	    return read_string(inport);

	 case '\'':
	    return read_quote(inport, symbol_quote);

	 case '`':
	    return read_quote(inport, symbol_quasiquote);

	 case ',':
	    return read_comma(inport);

	 default:
	    PIO::unget(inport, ch);
	    return read_symbol(inport);
      }
   }
}

SEXPR READER::read( SEXPR inport )
{
   return read_sexpr(inport);
}

void READER::initialize()
{
   symbol_dot = SYMTAB::enter(".");
}

}
