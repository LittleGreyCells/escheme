#include <cstdio>

#include "rep.hxx"
#include "error.hxx"
#include "reader.hxx"
#include "eval.hxx"
#include "pio.hxx"
#include "printer.hxx"
#include "memory.hxx"
#include "symtab.hxx"
#include "regstack.hxx"

// symbol names for hanging sexprs
const char* SYSTEM_REPLOOP       = "*system-rep-loop*";
const char* SYSTEM_LOADER        = "*system-loader*";
const char* TOPLEVEL             = "*toplevel*";

static void define_system()
{
   // define rep loop

   //
   // The following top-level reads in escheme.scm and start rep loop.
   //

   const char* system = "\
(begin\
   (define *version* \"<interpreter>\")\
   (set-prompt \"noise> \")	       \
   (let ((x 0))\
     (call/cc (lambda (cc) (set! *toplevel* cc)))\
     (if (= x 0)\
       (begin\
         (set! x 1)\
         (load \"escheme.scm\")\
          )))\
     (display \"escheme \")\
     (display *version*)\
     (newline)\
     (newline)\
     (flush-output)\
     (call/cc (lambda (cc) (set! *toplevel* cc)))\
     (while #t\
       (let ((sexpr (read *terminal*)))\
         (add-history sexpr)\
         (print (eval sexpr)))))\
\
(define (load file . noisily)\
  (if (not (string? file))\
      (error \"filename is not a string\")\
      (let ((port (open-input-file file)))\
        (if port\
          (let ((sexpr (read port)))\
            (while (not (eof-object? sexpr))\
              (if noisily (begin (display \">> \") (print sexpr)))\
	      (eval sexpr)\
	      (set! sexpr (read port)))\
            (close-port port)))\
        port)))";

   const SEXPR port = PIO::open_on_string( MEMORY::string(system), pm_input );
  
   // protect the port from gc
   regstack.push(port);

   setvalue( SYMTAB::enter(SYSTEM_REPLOOP), READER::read(port) );
   setvalue( SYMTAB::enter(SYSTEM_LOADER), READER::read(port) );

   regstack.pop();
}

void rep_loop()
{
   // build the "system"

   try
   {
      define_system();
      EVAL::eceval( getvalue(SYMTAB::enter(SYSTEM_LOADER)) );
   }
   catch (...)
   {
      printf("Error during system definition. Terminating.\n");
      return;
   }

   //
   // REP Loop
   //   a single call into the interpreter
   //   exit on exceptions
   //

   SEXPR exp = getvalue( SYMTAB::enter(SYSTEM_REPLOOP) );

   while (true)
   {
      try
      {
	 exp = EVAL::eceval(exp);
	 return;
      }
      catch ( ERROR::SevereError& )
      {
	 exp = SYMTAB::enter(TOPLEVEL);

	 const SEXPR val = getvalue(exp);
	 
	 if ( contp(val) ||
	      funcp(val) ||
	      specialp(val) ||
	      closurep(val) )
	 {
	    // make it into an application
	    exp = MEMORY::cons( exp, null );
	 }
	 else
	 {
	    // abandon the interpreter
	    printf( "toplevel is unbound\n" );
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
	 // place holder for Exit actions
	 PIO::transcript_off();
	 return;
      }
      catch ( ... )
      {
	 printf( "handling other error\n" );
	 return;
      }
   }
}

