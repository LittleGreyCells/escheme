#ifndef transcript_hxx
#define transcript_hxx

#include <cstdio>

#include "sexpr.hxx"

namespace escheme
{
   
namespace TRANSCRIPT
{
   extern FILE* transcript;
   
   void on( SEXPR name );
   void off();
}

}

#endif
