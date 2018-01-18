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
   FILE* file;
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

// string
#define getstringlength(n) ((n)->u.string.length)
#define getstringindex(n) ((n)->u.string.index)
#define getstringdata(n) ((n)->u.string.data)
#define setstringlength(n,x) getstringlength(n) = (x)
#define setstringindex(n,x) getstringindex(n) = (x)
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
#define getvalue(n) ((n)->u.symbol.value)
#define getplist(n) ((n)->u.symbol.plist)
#define setname(n,x) getname(n) = (x)
#define setvalue(n,x) getvalue(n) = (x)
#define setplist(n,x) getplist(n) = (x)

// number
#define getfixnum(n) ((n)->u.fixnum)
#define getflonum(n) ((n)->u.flonum)
#define setfixnum(n,x) getfixnum(n) = (x)
#define setflonum(n,x) getflonum(n) = (x)

// function
#define getfunc(n) ((n)->u.func)
#define setfunc(n,x) getfunc(n) = (x)

// closure
#define getclosurecode(n) ((n)->u.closure.code)
#define getclosurebenv(n) ((n)->u.closure.benv)
#define getclosurevars(n) ((n)->u.closure.vars)
#define getclosurenumv(n) ((n)->aux1)
#define getclosurerargs(n) ((n)->aux2)

#define setclosurecode(n,x) getclosurecode(n) = (x)
#define setclosurebenv(n,x) getclosurebenv(n) = (x)
#define setclosurevars(n,x) getclosurevars(n) = (x)
#define setclosurenumv(n,x) getclosurenumv(n) = (x)
#define setclosurerargs(n,x) getclosurerargs(n) = (x)

// continuation
//   use vector operations

// environment
#define getenvframe(n) ((n)->u.environ.frame)
#define getenvbase(n) ((n)->u.environ.baseenv)
#define setenvframe(n,x) getenvframe(n) = (x)
#define setenvbase(n,x) getenvbase(n) = (x)

#define getframenslots(fr) ((fr)->nslots)
#define setframenslots(fr,n) getframenslots(fr) = (n)

#define frameref(fr,i) ((fr)->slot[(i)])
#define frameset(fr,i,x) frameref(fr,i) = (x)

#define getframevars(fr) ((fr)->vars)
#define setframevars(fr,x) getframevars(fr) = (x)

// port
#define getfile(n) ((n)->u.port.file)
#define getmode(n) ((n)->u.port.mode)
#define setfile(n,x) getfile(n) = (x)
#define setmode(n,x) getmode(n) = (x)

// string port
#define getstringportstring(n) getcdr(n)
#define setstringportstring(n,x) getstringportstring(n) = (x)

// gref
#define gref_getsymbol(n) ((n)->u.gref.symbol)
#define gref_setsymbol(n,s) (gref_getsymbol(n) = (s))

// fref
#define fref_getdepth(n) ((n)->u.fref.depth)
#define fref_getindex(n) ((n)->u.fref.index)
#define fref_setdepth(n,d) (fref_getdepth(n) = (d))
#define fref_setindex(n,i) (fref_getindex(n) = (i))

#endif
