#ifndef FORMAT_HXX
#define FORMAT_HXX

#include <sstream>
#include <string>

//
// format
//
   
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
	 ss << value;
	 format( ss, ++s, args... );
	 return;
      }
      ss << *s++;
   }
   std::runtime_error( "extra arguments provided in format" );
}

template< typename T, typename... Args >
std::string format( const char* s, T value, Args... args )
{
   std::stringstream ss;
   format( ss, s, value, std::forward<Args>(args)... );
   return ss.str();
}

#endif
