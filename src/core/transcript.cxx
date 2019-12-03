#include "transcript.hxx"
#include "error.hxx"

namespace escheme
{
   
namespace TRANSCRIPT
{
   extern FILE* transcript;
   
   FILE* transcript = 0;
   
   void on( SEXPR name )
   {
      if ( transcript )
         fclose( transcript );
      
      transcript = fopen( getstringdata(name), "a" );
      
      if ( transcript == 0 )
         ERROR::severe( "unable to open transcript file", name );
   }
   
   void off()
   {
      if ( transcript )
      {
         fclose( transcript );
         transcript = 0;
      }
   }
}

}
