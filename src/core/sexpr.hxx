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
   SEXPR code;
   SEXPR pair;            // ( code . ( benv . vars ) )
};

struct SYMBOL
{
   char* name;
   SEXPR pair;            // ( name . ( value . plist ) )
};

struct PORT
{
   BYTE mode;
   union 
   {
      FILE* file;         // file port
      SEXPR string;       // string port
   } p;
};

struct STRING
{
   UINT32 length;         // the allocated length
   UINT32 index;          // the working index
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
//   aux1   # used for other rep
//   aux2   # used for other rep
//

struct Node
{
   BYTE kind;
   BYTE mark;
   BYTE form;
   BYTE recu;
   BYTE aux1;
   BYTE aux2;
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
#ifdef CHECKED_ACCESS
SEXPR& getcar(SEXPR n);
SEXPR& getcdr(SEXPR n);
void setcar(SEXPR n, SEXPR x);
void setcdr(SEXPR n, SEXPR x);
#else
#define getcar(n) ((n)->u.cons.car)
#define getcdr(n) ((n)->u.cons.cdr)
#define setcar(n,x) getcar(n) = (x)
#define setcdr(n,x) getcdr(n) = (x)
#endif

// vector
#ifdef CHECKED_ACCESS
UINT32& getvectorlength(SEXPR n);
SEXPR*& getvectordata(SEXPR n);
SEXPR& vectorref(SEXPR n, UINT32 i);
void setvectorlength(SEXPR n, UINT32 x);
void setvectordata(SEXPR n, SEXPR* x);
void vectorset(SEXPR n, UINT32 i, SEXPR x);
#else
#define getvectorlength(n) ((n)->u.vector.length)
#define getvectordata(n) ((n)->u.vector.data)
#define vectorref(n,i) ((n)->u.vector.data[(i)])
#define setvectorlength(n,x) getvectorlength(n) = (x)
#define setvectordata(n,x) getvectordata(n) = (x)
#define vectorset(n,i,x) vectorref(n,i) = (x)
#endif

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
#ifdef CHECKED_ACCESS
UINT32& getstringlength(SEXPR n);
UINT32& getstringindex(SEXPR n);
char*& getstringdata(SEXPR n);
void setstringlength(SEXPR n, UINT32 x);
void setstringindex(SEXPR n, UINT32 x);
void setstringdata(SEXPR n, char* x);
#else
#define getstringlength(n) ((n)->u.string.length)
#define getstringindex(n) ((n)->u.string.index)
#define getstringdata(n) ((n)->u.string.data)
#define setstringlength(n,x) getstringlength(n) = (x)
#define setstringindex(n,x) getstringindex(n) = (x)
#define setstringdata(n,x) getstringdata(n) = (x)
#endif

// byte vector
#ifdef CHECKED_ACCESS
UINT32& getbveclength(SEXPR n);
BYTE*& getbvecdata(SEXPR n);
void setbveclength(SEXPR n, UINT32 x);
void setbvecdata(SEXPR n, BYTE* x);
BYTE& bvecref(SEXPR n, UINT32 i);
void bvecset(SEXPR n, UINT32 i, BYTE x);
#else
#define getbveclength(n) ((n)->u.bvec.length)
#define getbvecdata(n) ((n)->u.bvec.data)
#define setbveclength(n,x) getbveclength(n) = (x)
#define setbvecdata(n,x) getbvecdata(n) = (x)
#define bvecref(n,i) ((n)->u.bvec.data[(i)])
#define bvecset(n,i,x) bvecref(n,i) = (x)
#endif

// character
#ifdef CHECKED_ACCESS
char& getcharacter(SEXPR n);
void setcharacter(SEXPR n, char ch);
#else
#define getcharacter(n) ((n)->u.ch)
#define setcharacter(n,ch) getcharacter(n) = (ch)
#endif

// symbol
#ifdef CHECKED_ACCESS
char*& getname(SEXPR n);
SEXPR& getsymbolpair(SEXPR n);
SEXPR& getvalue(SEXPR n);
SEXPR& getplist(SEXPR n);
void setname(SEXPR n, char* x);
void setsymbolpair(SEXPR n, SEXPR x);
void setvalue(SEXPR n, SEXPR x);
void setplist(SEXPR n, SEXPR x);
#else
#define getname(n) ((n)->u.symbol.name)
#define getsymbolpair(n) ((n)->u.symbol.pair)
#define getvalue(n) (getsymbolpair(n)->u.cons.car)
#define getplist(n) (getsymbolpair(n)->u.cons.cdr)
#define setname(n,x) getname(n) = (x)
#define setsymbolpair(n,x) getsymbolpair(n) = (x)
#define setvalue(n,x) getvalue(n) = (x)
#define setplist(n,x) getplist(n) = (x)
#endif

// number
#ifdef CHECKED_ACCESS
FIXNUM& getfixnum(SEXPR n);
FLONUM& getflonum(SEXPR n);
void setfixnum(SEXPR n, FIXNUM x);
void setflonum(SEXPR n, FLONUM x);
#else
#define getfixnum(n) ((n)->u.fixnum)
#define getflonum(n) ((n)->u.flonum)
#define setfixnum(n,x) getfixnum(n) = (x)
#define setflonum(n,x) getflonum(n) = (x)
#endif

// primitive function
#ifdef CHECKED_ACCESS
FUNCTION& getfunc(SEXPR n);
void setfunc(SEXPR n, FUNCTION x);
const char*& getprimname(SEXPR n);
void setprimname(SEXPR n, const char* x);
#else
#define getfunc(n) ((n)->u.prim.func)
#define setfunc(n,x) getfunc(n) = (x)
#define getprimname(n) ((n)->u.prim.name)
#define setprimname(n,x) getprimname(n) = (x)
#endif

// closure
#ifdef CHECKED_ACCESS
SEXPR& getclosurecode(SEXPR n);
SEXPR& getclosurepair(SEXPR n);
SEXPR& getclosurebenv(SEXPR n);
SEXPR& getclosurevars(SEXPR n);
BYTE& getclosurenumv(SEXPR n);
BYTE& getclosurerargs(SEXPR n);
#else
#define getclosurecode(n) ((n)->u.closure.code)
#define getclosurepair(n) ((n)->u.closure.pair)
#define getclosurebenv(n) (getclosurepair(n)->u.cons.car)
#define getclosurevars(n) (getclosurepair(n)->u.cons.cdr)
#define getclosurenumv(n) ((n)->aux1)
#define getclosurerargs(n) ((n)->aux2)
#endif

#ifdef CHECKED_ACCESS
void setclosurecode(SEXPR n, SEXPR x);
void setclosurepair(SEXPR n, SEXPR x);
void setclosurebenv(SEXPR n, SEXPR x);
void setclosurevars(SEXPR n, SEXPR x);
void setclosurenumv(SEXPR n, BYTE x);
void setclosurerargs(SEXPR n, BYTE x);
#else
#define setclosurecode(n,x) getclosurecode(n) = (x)
#define setclosurepair(n,x) getclosurepair(n) = (x)
#define setclosurebenv(n,x) getclosurebenv(n) = (x)
#define setclosurevars(n,x) getclosurevars(n) = (x)
#define setclosurenumv(n,x) getclosurenumv(n) = (x)
#define setclosurerargs(n,x) getclosurerargs(n) = (x)
#endif

// environment
#ifdef CHECKED_ACCESS
SEXPR& getenvframe(SEXPR n);
SEXPR& getenvbase(SEXPR n);
void setenvframe(SEXPR n, SEXPR x);
void setenvbase(SEXPR n, SEXPR x);
#else
#define getenvframe(n) ((n)->u.environ.frame)
#define getenvbase(n) ((n)->u.environ.baseenv)
#define setenvframe(n,x) getenvframe(n) = (x)
#define setenvbase(n,x) getenvbase(n) = (x)
#endif

// port
#ifdef CHECKED_ACCESS
FILE*& getfile(SEXPR n);
BYTE& getmode(SEXPR n);
void setfile(SEXPR n, FILE* x);
void setmode(SEXPR n, BYTE x);
#else
#define getfile(n) ((n)->u.port.p.file)
#define getmode(n) ((n)->u.port.mode)
#define setfile(n,x) getfile(n) = (x)
#define setmode(n,x) getmode(n) = (x)
#endif

// string port
#ifdef CHECKED_ACCESS
SEXPR& getstringportstring(SEXPR n);
void setstringportstring(SEXPR n, SEXPR x);
#else
#define getstringportstring(n) ((n)->u.port.p.string)
#define setstringportstring(n,x) getstringportstring(n) = (x)
#endif

// code
#ifdef CHECKED_ACCESS
SEXPR& code_getbcodes(SEXPR n);
SEXPR& code_getsexprs(SEXPR n);
void code_setbcodes(SEXPR n, SEXPR x);
void code_setsexprs(SEXPR n, SEXPR x);
#else
#define code_getbcodes(n) ((n)->u.code.bcodes)
#define code_getsexprs(n) ((n)->u.code.sexprs)
#define code_setbcodes(n,x) code_getbcodes(n) = (x)
#define code_setsexprs(n,x) code_getsexprs(n) = (x)
#endif

// promise
#ifdef CHECKED_ACCESS
SEXPR& promise_getexp(SEXPR n);
SEXPR& promise_getval(SEXPR n);
void promise_setexp(SEXPR n, SEXPR x);
void promise_setval(SEXPR n, SEXPR x);
#else
#define promise_getexp(n) ((n)->u.promise.exp)
#define promise_getval(n) ((n)->u.promise.val)
#define promise_setexp(n,x) promise_getexp(n) = (x)
#define promise_setval(n,x) promise_getval(n) = (x)
#endif

#endif
