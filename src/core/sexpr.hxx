#ifndef sexpr_hxx
#define sexpr_hxx

#include <cstdio>

// Opitimization Definitions
// 
#define GC_STATISTICS_DETAILED

#define DO_REGSTACK_CHECK
#undef DO_REGSTACK_CHECK

//
// escheme configration
//

enum ConfigurationConstants
{
   MAX_SYMBOL_LENGTH  = 80,     // reader's limitation
   MAX_STRING_LENGTH  = 80,     // reader's limitation
   MAX_INTEGER_LENGTH = 80,
   ECE_HISTORY_LENGTH = 100,
   NODE_BLOCK_SIZE    = 5000,
   ARGSTACK_SIZE      = 500,
   REGSTACK_SIZE      = 1000,
   INTSTACK_SIZE      = 1000,
   MAX_STRING_SIZE    = 0xFFFFFFFE,
};

enum NodeKind
{
   n_free,         // 0   // unassigned

   n_null,         // 1
   n_symbol,       // 2
   n_fixnum,       // 3
   n_flonum,       // 4
   n_char,         // 5

   n_string,       // 6
   n_cons,         // 7
   n_vector,       // 8
   n_bvec,         // 9
   n_environment,  // 10

   n_promise,      // 11
   n_closure,      // 12
   n_continuation, // 13
   n_port,         // 14
   n_string_port,  // 15

   n_func,         // 16
   n_eval,         // 17
   n_apply,        // 18
   n_callcc,       // 19
   n_map,          // 20

   n_foreach,      // 21
   n_force,        // 22
   n_gref,         // 23
   n_fref,         // 24
   n_code,         // 25

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

struct Frame;
using FRAME = Frame*;

using PRIMITIVE = SEXPR (*)();
using PREDICATE = bool (*)( const SEXPR );

struct Frame
{
   FRAME next;
   SEXPR vars;
   UINT32 nslots;
   SEXPR slot[1];        // varying numbers
};

struct ENVIRON
{
   FRAME frame;
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

// a 3-cell
struct CLOSURE
{
   SEXPR code;
   SEXPR benv;
   SEXPR vars;
};

// a 3-cell
struct SYMBOL
{
   char* name;
   SEXPR value;
   SEXPR plist;
};

struct PORT
{
   INT16 ungot;
   INT16 mode;
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

struct GREF
{
   SEXPR symbol;
};

struct FREF
{
   UINT32 depth;
   UINT32 index;
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
      PRIMITIVE func;
      ENVIRON environ;
      PORT port;
      BVECTOR bvec;
      SYMBOL symbol;
      CLOSURE closure;
      GREF gref;
      FREF fref;
      CODE code;
      PROMISE promise;
   } u;

   Node()
   {}

   Node( NodeKind k ) :
   kind(k), mark(0), form(0) {}

   Node( NodeKind k, SEXPR next ) :
      kind(k), mark(0), form(0) { u.link.next = next; }

   void setnext( SEXPR next ) { u.link.next = next; }
   SEXPR getnext() { return u.link.next; }

   void* id() { return this; }
};

extern SEXPR null;

// debugging support
void show( const SEXPR n );

// accessors
FIXNUM fixnum( const SEXPR n );
FLONUM flonum( const SEXPR n );
char*  string( const SEXPR n );

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
void fset( FRAME frame, UINT32 index, SEXPR value );
SEXPR fref( FRAME frame, UINT32 index );


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
bool grefp( const SEXPR n );
bool frefp( const SEXPR n );

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

// continuation
//   use vector

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
SEXPR& getvalue(SEXPR n);
SEXPR& getplist(SEXPR n);
void setname(SEXPR n, char* x);
void setvalue(SEXPR n, SEXPR x);
void setplist(SEXPR n, SEXPR x);
#else
#define getname(n) ((n)->u.symbol.name)
#define getvalue(n) ((n)->u.symbol.value)
#define getplist(n) ((n)->u.symbol.plist)
#define setname(n,x) getname(n) = (x)
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

// function
#ifdef CHECKED_ACCESS
PRIMITIVE& getfunc(SEXPR n);
void setfunc(SEXPR n, PRIMITIVE x);
#else
#define getfunc(n) ((n)->u.func)
#define setfunc(n,x) getfunc(n) = (x)
#endif

// closure
#ifdef CHECKED_ACCESS
SEXPR& getclosurecode(SEXPR n);
SEXPR& getclosurebenv(SEXPR n);
SEXPR& getclosurevars(SEXPR n);
BYTE& getclosurenumv(SEXPR n);
BYTE& getclosurerargs(SEXPR n);
#else
#define getclosurecode(n) ((n)->u.closure.code)
#define getclosurebenv(n) ((n)->u.closure.benv)
#define getclosurevars(n) ((n)->u.closure.vars)
#define getclosurenumv(n) ((n)->aux1)
#define getclosurerargs(n) ((n)->aux2)
#endif

#ifdef CHECKED_ACCESS
void setclosurecode(SEXPR n, SEXPR x);
void setclosurebenv(SEXPR n, SEXPR x);
void setclosurevars(SEXPR n, SEXPR x);
void setclosurenumv(SEXPR n, BYTE x);
void setclosurerargs(SEXPR n, BYTE x);
#else
#define setclosurecode(n,x) getclosurecode(n) = (x)
#define setclosurebenv(n,x) getclosurebenv(n) = (x)
#define setclosurevars(n,x) getclosurevars(n) = (x)
#define setclosurenumv(n,x) getclosurenumv(n) = (x)
#define setclosurerargs(n,x) getclosurerargs(n) = (x)
#endif

// environment
#ifdef CHECKED_ACCESS
FRAME& getenvframe(SEXPR n);
SEXPR& getenvbase(SEXPR n);
void setenvframe(SEXPR n, FRAME x);
void setenvbase(SEXPR n, SEXPR x);
#else
#define getenvframe(n) ((n)->u.environ.frame)
#define getenvbase(n) ((n)->u.environ.baseenv)
#define setenvframe(n,x) getenvframe(n) = (x)
#define setenvbase(n,x) getenvbase(n) = (x)
#endif

// frame
#define getframenslots(fr) ((fr)->nslots)
#define getframevars(fr) ((fr)->vars)
#define frameref(fr,i) ((fr)->slot[(i)])
#define setframenslots(fr,n) getframenslots(fr) = (n)
#define setframevars(fr,x) getframevars(fr) = (x)
#define frameset(fr,i,x) frameref(fr,i) = (x)

// port
#ifdef CHECKED_ACCESS
FILE*& getfile(SEXPR n);
INT16& getmode(SEXPR n);
void setfile(SEXPR n, FILE* x);
void setmode(SEXPR n, INT16 x);
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

// gref
#ifdef CHECKED_ACCESS
SEXPR& gref_getsymbol(SEXPR n);
void gref_setsymbol(SEXPR n, SEXPR s);
#else
#define gref_getsymbol(n) ((n)->u.gref.symbol)
#define gref_setsymbol(n,s) (gref_getsymbol(n) = (s))
#endif

// fref
#ifdef CHECKED_ACCESS
UINT32& fref_getdepth(SEXPR n);
UINT32& fref_getindex(SEXPR n);
void fref_setdepth(SEXPR n, UINT32 d);
void fref_setindex(SEXPR n, UINT32 i);
#else
#define fref_getdepth(n) ((n)->u.fref.depth)
#define fref_getindex(n) ((n)->u.fref.index)
#define fref_setdepth(n,d) (fref_getdepth(n) = (d))
#define fref_setindex(n,i) (fref_getindex(n) = (i))
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
