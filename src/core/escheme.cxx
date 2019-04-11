#include "rep.hxx"
#include "error.hxx"
#include "memory.hxx"
#include "symtab.hxx"
#include "funtab.hxx"
#include "reader.hxx"
#include "pio.hxx"

#include "eval/eval.hxx"

int unix_argc;
char** unix_argv;

//
// escheme
//
//   $ escheme [-h] [-f <file1> <file2> ...] [< <redirection-input-file>]
//

int main( int argc, char** argv )
{
   unix_argc = argc;
   unix_argv = argv;

   try
   {
      GcSuspension gcs("escheme-initialization");

      // essential initialization (order is important)
      MEMORY::initialize();
      SYMTAB::initialize();
      FUNTAB::initialize();
      READER::initialize();
      PIO::initialize();
      EVAL::initialize();
   }
   catch (...)
   {
      printf( "error during initialization; terminating." );
      return 0;
   }

   // enter REP and return on controlled exit
   rep_loop();

   return 0;
}
