#ifndef sexpr_hxx
#define sexpr_hxx

#include <cstdio>

//
// escheme configration
//

enum ConfigurationConstants
{
   NODE_BLOCK_SIZE    = 5000,
   ARGSTACK_SIZE      = 500,
   REGSTACK_SIZE      = 1000,
   INTSTACK_SIZE      = 1000,
   ECE_HISTORY_LENGTH = 100,
   MAX_IMAGE_LENGTH   = 256,
   MAX_STRING_SIZE    = 0xFFFFFFFE,
};

enum NodeKind
{
   n_free,
   n_null,
   n_symbol,
   n_fixnum,
   n_flonum,
   n_char,
   n_string,
   n_cons,
   n_vector,
   n_bvec,
   n_environment,
   n_promise,
   n_closure,
   n_continuation,
   n_port,
   n_string_port,
   n_func,
   n_eval,
   n_apply,
   n_callcc,
   n_map,
   n_foreach,
   n_force,
   n_code,
   NUMKINDS        // keep me last
};

enum PortMode
{ 
   pm_none   = 0x00,
   pm_input  = 0x01,
   pm_output = 0x02
};


using FIXNUM  = signed long;
using UFIXNUM = unsigned long;
using FLONUM  = double;
using CHAR    = char;
using BYTE    = unsigned char;
using INT16   = signed short;
using UINT16  = unsigned short;
using INT32   = signed int;
using UINT32  = unsigned int;

//
// Formmatted string output
//

#define SPRINTF(buffer, ...) snprintf(buffer, sizeof(buffer), __VA_ARGS__)

 
//
// Virtualizing the Node Representational Scheme
//

struct Node;
using SEXPR = Node*;

using FUNCTION = SEXPR (*)();
using PREDICATE = bool (*)( const SEXPR );

struct PRIMITIVE
{
   FUNCTION func;
   const char* name;
};

struct ENVIRON
{
   SEXPR frame;
   SEXPR baseenv;
};

struct CONSCELL
{
   SEXPR car;
   SEXPR cdr;
};

struct VECTOR
{
   UINT32 length;
   SEXPR* data;
};

struct CLOSURE
{
   BYTE numv;
   BYTE rargs;
   SEXPR* data;
};

struct SYMBOL
{
   char* name;
   SEXPR* data;
};

struct PORT
{
   BYTE mode;
   UINT32 index;
   union 
   {
      FILE* file;         // file port
      SEXPR string;       // string port
   } p;
};

struct STRING
{
   UINT32 length;
   char* data;
};

struct BVECTOR
{
   UINT32 length;
   BYTE* data;
};

struct LINKAGE
{
   SEXPR next;
};

struct CODE
{
   SEXPR bcodes;
   SEXPR sexprs;
};

struct PROMISE
{
   SEXPR exp;
   SEXPR val;
};

//
// Forematter
//
//   kind   # type tag
//   mark   # used by memory management
//   form   # used by eval for fast dispatch
//   recu   # used by printer to guard against recursive printing
//

struct Node
{
   BYTE kind;
   BYTE mark;
   BYTE form;
   BYTE recu;
   union
   {
      LINKAGE link;
      FIXNUM fixnum;
      FLONUM flonum;
      CHAR ch;
      STRING string;
      CONSCELL cons;
      VECTOR vector;
      PRIMITIVE prim;
      ENVIRON environ;
      PORT port;
      BVECTOR bvec;
      SYMBOL symbol;
      CLOSURE closure;
      CODE code;
      PROMISE promise;
   } u;

   Node()
   {}

   explicit Node( NodeKind k ) :
      kind(k), mark(0), form(0) {}

   Node( NodeKind k, SEXPR next ) :
      kind(k), mark(0), form(0) { u.link.next = next; }

   void setnext( SEXPR next ) { u.link.next = next; }
   SEXPR getnext() const { return u.link.next; }

   void* id() { return this; }
};

extern SEXPR null;

// debugging support
void show( const SEXPR n );

// accessors
FIXNUM fixnum( const SEXPR n );
FLONUM flonum( const SEXPR n );

// list 
SEXPR car( const SEXPR n );
SEXPR cdr( const SEXPR n );
void rplaca( SEXPR n, SEXPR car );
void rplacd( SEXPR n, SEXPR cdr );
SEXPR nthcar( const SEXPR s, UINT32 n );
SEXPR nthcdr( const SEXPR s, UINT32 n );
UINT32 list_length( const SEXPR x );

void vset( SEXPR vector, UINT32 index, SEXPR value );
SEXPR vref( SEXPR vector, UINT32 index );

// symbol
char* name( const SEXPR n );
SEXPR value( SEXPR n );
SEXPR set( SEXPR symbol, SEXPR value );

// frame
void fset( SEXPR frame, UINT32 index, SEXPR value );
SEXPR fref( SEXPR frame, UINT32 index );


/////////////////////////////////////////////////////////////////
//
// Predicates
//
/////////////////////////////////////////////////////////////////

#define nullp(n) ((n) == null)
#define anyp(n)  ((n) != null)

bool symbolp( const SEXPR n );
bool fixnump( const SEXPR n );
bool flonump( const SEXPR n );
bool numberp( const SEXPR n );
bool booleanp( const SEXPR n );
bool stringp( const SEXPR n );
bool charp( const SEXPR n );
bool vectorp( const SEXPR n );
bool consp( const SEXPR n );
bool funcp( const SEXPR n );
bool specialp( const SEXPR n );
bool portp( const SEXPR n );
bool stringportp( const SEXPR n );
bool anyportp( const SEXPR n );
bool closurep( const SEXPR n );
bool contp( const SEXPR n );
bool envp( const SEXPR n );
bool bvecp( const SEXPR n );
bool listp( const SEXPR n );
bool atomp( const SEXPR n );
bool inportp( const SEXPR n );
bool outportp( const SEXPR n );
bool instringportp( const SEXPR n );
bool outstringportp( const SEXPR n );
bool anyinportp( const SEXPR n );
bool anyoutportp( const SEXPR n );
bool lastp( const SEXPR n );
bool promisep( const SEXPR n );
bool codep( const SEXPR n );
bool vcp( const SEXPR n );
bool primp( const SEXPR n );

#define _symbolp(n) ((n)->kind == n_symbol)
#define _fixnump(n) ((n)->kind == n_fixnum)
#define _flonump(n) ((n)->kind == n_flonum)
#define _stringp(n) ((n)->kind == n_string)
#define _charp(n) ((n)->kind == n_char)
#define _consp(n) ((n)->kind == n_cons)
#define _envp(n) ((n)->kind == n_environment)
#define _lastp(n) nullp(cdr(n))

#define _funcp(n) ((n)->kind == n_func)
#define _closurep(n) ((n)->kind == n_closure)
#define _codep(n) ((n)->kind == n_code)
#define _compiledp(n) _codep(getclosurecode(n))
#define _compiled_closurep(n) (_closurep(n) && _compiledp(n))

SEXPR guard( SEXPR s, PREDICATE predicate );

/////////////////////////////////////////////////////////////////
//
// Primitive accessors
//
/////////////////////////////////////////////////////////////////

// all
#define nodekind(n) ((n)->kind)
#define setnodekind(n,k) nodekind(n) = (k)
#define getform(n) ((n)->form)
#define setform(n,x) getform(n) = (x)

// cons
#define getcar(n) ((n)->u.cons.car)
#define getcdr(n) ((n)->u.cons.cdr)
#define setcar(n,x) getcar(n) = (x)
#define setcdr(n,x) getcdr(n) = (x)

// vector
#define getvectorlength(n) ((n)->u.vector.length)
#define getvectordata(n) ((n)->u.vector.data)
#define vectorref(n,i) ((n)->u.vector.data[(i)])
#define setvectorlength(n,x) getvectorlength(n) = (x)
#define setvectordata(n,x) getvectordata(n) = (x)
#define vectorset(n,i,x) vectorref(n,i) = (x)

// frame
#define getframenslots(fr)  (getvectorlength(fr)-2)
#define getframevars(fr)    (vectorref(fr,0))
#define getframeclosure(fr) (vectorref(fr,1))
#define frameref(fr,i)      (vectorref(fr,2+(i)))
#define setframevars(fr,x)    getframevars(fr) = (x)
#define setframeclosure(fr,x) getframeclosure(fr) = (x)
#define frameset(fr,i,x)      frameref(fr,i) = (x)

// continuation
#define cont_getstate(n) ((n)->u.cons.car)
#define cont_setstate(n,x) cont_getstate(n) = (x)

// string
#define getstringlength(n) ((n)->u.string.length)
#define getstringdata(n) ((n)->u.string.data)
#define setstringlength(n,x) getstringlength(n) = (x)
#define setstringdata(n,x) getstringdata(n) = (x)

// byte vector
#define getbveclength(n) ((n)->u.bvec.length)
#define getbvecdata(n) ((n)->u.bvec.data)
#define setbveclength(n,x) getbveclength(n) = (x)
#define setbvecdata(n,x) getbvecdata(n) = (x)
#define bvecref(n,i) ((n)->u.bvec.data[(i)])
#define bvecset(n,i,x) bvecref(n,i) = (x)

// character
#define getcharacter(n) ((n)->u.ch)
#define setcharacter(n,ch) getcharacter(n) = (ch)

// symbol
#define getname(n) ((n)->u.symbol.name)
#define getdata(n) ((n)->u.symbol.data)
#define getvalue(n) (getdata(n)[0])
#define getplist(n) (getdata(n)[1])
#define setname(n,x) getname(n) = (x)
#define setdata(n,x) getdata(n) = (x)
#define setvalue(n,x) getvalue(n) = (x)
#define setplist(n,x) getplist(n) = (x)

// number
#define getfixnum(n) ((n)->u.fixnum)
#define getflonum(n) ((n)->u.flonum)
#define setfixnum(n,x) getfixnum(n) = (x)
#define setflonum(n,x) getflonum(n) = (x)

// primitive function
#define getfunc(n) ((n)->u.prim.func)
#define setfunc(n,x) getfunc(n) = (x)
#define getprimname(n) ((n)->u.prim.name)
#define setprimname(n,x) getprimname(n) = (x)

// closure
// get
#define getclosuredata(n) ((n)->u.closure.data)
#define getclosurecode(n) (getclosuredata(n)[0])
#define getclosurebenv(n) (getclosuredata(n)[1])
#define getclosurevars(n) (getclosuredata(n)[2])
#define getclosurenumv(n) ((n)->u.closure.numv)
#define getclosurerargs(n) ((n)->u.closure.rargs)
// set
#define setclosuredata(n,x) getclosuredata(n) = (x)
#define setclosurecode(n,x) getclosurecode(n) = (x)
#define setclosurebenv(n,x) getclosurebenv(n) = (x)
#define setclosurevars(n,x) getclosurevars(n) = (x)
#define setclosurenumv(n,x) getclosurenumv(n) = (x)
#define setclosurerargs(n,x) getclosurerargs(n) = (x)

// environment
#define getenvframe(n) ((n)->u.environ.frame)
#define getenvbase(n) ((n)->u.environ.baseenv)
#define setenvframe(n,x) getenvframe(n) = (x)
#define setenvbase(n,x) getenvbase(n) = (x)

// port
#define getfile(n) ((n)->u.port.p.file)
#define getmode(n) ((n)->u.port.mode)
#define setfile(n,x) getfile(n) = (x)
#define setmode(n,x) getmode(n) = (x)

// string port
#define getstringportstring(n) ((n)->u.port.p.string)
#define setstringportstring(n,x) getstringportstring(n) = (x)
#define getstringportindex(n) ((n)->u.port.index)
#define setstringportindex(n,x) getstringportindex(n) = (x)

// code
#define code_getbcodes(n) ((n)->u.code.bcodes)
#define code_getsexprs(n) ((n)->u.code.sexprs)
#define code_setbcodes(n,x) code_getbcodes(n) = (x)
#define code_setsexprs(n,x) code_getsexprs(n) = (x)

// promise
#define promise_getexp(n) ((n)->u.promise.exp)
#define promise_getval(n) ((n)->u.promise.val)
#define promise_setexp(n,x) promise_getexp(n) = (x)
#define promise_setval(n,x) promise_getval(n) = (x)

#endif
