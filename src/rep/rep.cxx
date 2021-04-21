#include "rep.hxx"

#include <cstdio>
#include <cstdlib>

#include <string>

#include "core/error.hxx"
#include "core/reader.hxx"
#include "core/pio.hxx"
#include "core/printer.hxx"
#include "core/memory.hxx"
#include "core/symtab.hxx"
#include "core/transcript.hxx"
#include "core/regstack.hxx"

#include "eval/eval.hxx"

namespace escheme
{

// symbol names for hanging sexprs
const std::string SYSTEM_REPLOOP = "*system-rep-loop*";
const std::string SYSTEM_LOADER  = "*system-loader*";
const std::string SYSTEM_PATH    = "*system-path*";
const std::string TOPLEVEL       = "*toplevel*";

static void define_system()
{
   const std::string system = R"(
(begin
   (define *version* "<interpreter>")
   (set-prompt "noise> ")
   (let ((x 0))
     (call/cc (lambda (cc) (set! *toplevel* cc)))
     (if (= x 0)
       (begin
         (set! x 1)
         (load (system-path "escheme.scm"))
         )))
     (display "escheme ")
     (display *version*)
     (newline)
     (newline)
     (flush-output)
     (call/cc (lambda (cc) (set! *toplevel* cc)))
     (while #t
       (let ((sexpr (read *terminal*)))
         (add-history sexpr)
         (print (eval sexpr)))))

(define (load file . noisily)
  (if (not (string? file))
      (error "filename is not a string")
      (let ((port (open-input-file file)))
        (if port
          (let ((sexpr (read port)))
            (while (not (eof-object? sexpr))
              (if noisily (begin (display ">> ") (print sexpr)))
	      (eval sexpr)
	      (set! sexpr (read port)))
            (close-port port)))
        port)))

(define (system-path file)
  (let ((home (getenv "ESCHEME")))
    (if (= (string-length home) 0)
        file
        (string-append home "/" file))))
)";

   escheme::GcSuspension gcs("defsys");

   auto port = PIO::open_on_string( MEMORY::string(system), pm_input );
   
   setvalue( SYMTAB::enter(SYSTEM_REPLOOP), READER::read(port) );
   setvalue( SYMTAB::enter(SYSTEM_LOADER), READER::read(port) );
   setvalue( SYMTAB::enter(SYSTEM_PATH), READER::read(port) );
}

void rep_loop()
{
   // build the "system"

   try
   {
      define_system();
      
      EVAL::eceval( getvalue(SYMTAB::enter(SYSTEM_LOADER)) );
      EVAL::eceval( getvalue(SYMTAB::enter(SYSTEM_PATH)) );
   }
   catch (...)
   {
      printf( "error during system definition\n" );
      return;
   }

   //
   // REP Loop
   //   a single call into the interpreter.
   //   exit on exceptions and evaluate the toplevel continuation.
   //

   auto exp = getvalue( SYMTAB::enter(SYSTEM_REPLOOP) );

   while ( true )
   {
      try
      {
	 EVAL::eceval( exp );
	 return;
      }
      catch ( ERROR::SevereError& )
      {
	 exp = SYMTAB::enter(TOPLEVEL);

	 if ( contp(getvalue(exp))  )
	 {
	    // make it into an application
	    exp = MEMORY::cons( exp, null );
	 }
	 else
	 {
	    // abandon the interpreter
	    printf( "toplevel is not a continuation\n" );
	    return;
	 }
      }
      catch ( ERROR::FatalError& )
      {
	 printf( "handling fatal error\n" );
	 return;
      }
      catch ( ERROR::Exit& )
      {
	 TRANSCRIPT::off();
	 return;
      }
      catch ( ... )
      {
	 printf( "handling unexpected error\n" );
	 return;
      }
   }
}

}
