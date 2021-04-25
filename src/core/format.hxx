#ifndef FORMAT_HXX
#define FORMAT_HXX

#include <ctype.h>
#include <sstream>
#include <string>
#include <iomanip>

#include <iostream>

//
// format
//
//    variadic template.
//    minimal functionality to satisfy application needs.
//

#if 0
static bool isfmt( char ch )
{
   return ch == 'd' || ch == 'i' || ch == 'o' || ch == 'u' || ch == 's' || ch == 'x';
}
#endif

static bool isflag( char ch )
{
   return ch == '-' || ch == '+' || ch == ' ' || ch == '#' || ch == '0'; 
}

static const char* scanw( const char* p, int& width )
{
   width = 0;
   while ( *p && isdigit(*p) )
      width = width*10 + (*p++ - '0');
   return p;
}

// case argc = 0
static void format( std::stringstream& ss, const char* s ) { ss << s; }

// case argc > 0
template< typename T, typename... Args >
void format( std::stringstream& ss, const char* s, T value, Args... args )
{
   const auto percent = '%';
   while ( s && *s )
   {
      if ( *s == percent && *++s != percent )
      {
	 char flag = isflag(*s) ? *s++ : '\0';
	 
	 int width;
	 auto p = scanw( s, width );

	 if ( flag )
	    ss << std::setfill( flag );
	 
	 if ( p != s )
	 {
	    ss << std::setw( width );
	    s = p;
	 }

	 if ( *p == 'x' )
	    ss << std::hex;
	 else if ( *p == 'o' )
	    ss << std::oct;

	 ss << value;
	 
	 if ( flag )
	    ss << std::setfill( ' ' );

	 if ( *p != 'd' )
	    ss << std::dec;

	 format( ss, ++s, args... );
	 return;
      }
      ss << *s++;
   }
}

template< typename T, typename... Args >
std::string format( const char* s, T value, Args... args )
{
   std::stringstream ss;
   format( ss, s, value, std::forward<Args>(args)... );
   return ss.str();
}

#endif
