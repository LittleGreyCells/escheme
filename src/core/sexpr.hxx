#ifndef sexpr_hxx
#define sexpr_hxx

#include <string>
#include <cstdio>

namespace escheme
{

enum ConfigurationConstants
{
   NODE_BLOCK_SIZE    = 5000,
   ARGSTACK_SIZE      = 500,
   REGSTACK_SIZE      = 1000,
   INTSTACK_SIZE      = 1000,
   ECE_HISTORY_LENGTH = 100,
   MAX_IMAGE_LENGTH   = 256,
   MAX_STRING_SIZE    = 0xFFFE,
   CACHE_BLOCK_SIZE   = 10000,
   CACHE_TENURE       = 4,
   CACHE_MAXAGE       = 127,
};

enum NodeKind
{
   n_free,         // 0
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
   n_promise,
   n_closure,
   n_continuation,
   n_port,
   n_string_port,  // 15
   n_func,
   n_eval,
   n_apply,
   n_callcc,
   n_map,          // 20
   n_foreach,
   n_force,
   n_code,         // 23
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
// Node Representational Scheme
//

struct Node;
using SEXPR = Node*;

struct Frame;
using FRAME = Frame*;

using FUNCTION = SEXPR (*)();
using PREDICATE = bool (*)( const SEXPR );

using DWORD = void*;

inline auto NBYTES( int n )  { return n*sizeof(DWORD); }
inline auto NDWORDS( int n ) { return (n+sizeof(DWORD)-1)/sizeof(DWORD); }

struct Frame
{
   FRAME next;
   SEXPR vars;
   SEXPR closure;
   UINT32 size;
   UINT32 nslots;
   SEXPR slot[1];        // varying numbers
};

inline auto FRAMESIZE( int nslots ) { return ((sizeof(Frame)-sizeof(SEXPR))/sizeof(SEXPR))+nslots; }

struct PRIMITIVE
{
   FUNCTION func;
   const char* name;
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

struct CLOSURE
{
   BYTE numv;
   BYTE rargs;
   SEXPR* data;
};

struct SYMBOL
{
   char* name;
   SEXPR pair;
};

struct PORT
{
   BYTE mode;
   UINT32 index;
   union 
   {
      FILE* file;          // file port
      std::string* string; // string port
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
//   nage   # used by memory manager for aging
//

struct Node
{
   BYTE kind;
   BYTE mark;
   BYTE form;
   BYTE recu;
   BYTE nage;
   union
   {
      SEXPR next;
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

   Node() {}

   explicit Node( NodeKind k ) :
      kind(k), mark(0), form(0), nage(0) {}

   Node( NodeKind k, SEXPR next ) :
      kind(k), mark(0), form(0), nage(0) { u.next = next; }

   void setnext( SEXPR next ) { u.next = next; }
   SEXPR getnext() const { return u.next; }

   void* id() { return this; }
};

extern SEXPR null;

// accessors
FIXNUM fixnum( const SEXPR n );
FLONUM flonum( const SEXPR n );

// list 
SEXPR car( const SEXPR n );
SEXPR cdr( const SEXPR n );
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

// predicates (see also func.hxx)
bool symbolp( const SEXPR n );
bool fixnump( const SEXPR n );
bool flonump( const SEXPR n );
bool numberp( const SEXPR n );
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

SEXPR guard( SEXPR s, PREDICATE predicate );

// accessors

// all
inline auto& nodekind( SEXPR n ) { return n->kind; }
inline void setnodekind( SEXPR n, NodeKind k ) { n->kind = k; }
inline auto& getform( SEXPR n ) { return n->form; }
inline void setform( SEXPR n, BYTE x ) { n->form = x; }

// cons
inline auto& getcar( SEXPR n ) { return n->u.cons.car; }
inline auto& getcdr( SEXPR n ) { return n->u.cons.cdr; }
inline void setcar( SEXPR n, SEXPR x ) { getcar(n) = x; }
inline void setcdr( SEXPR n, SEXPR x ) { getcdr(n) = x; }

// vector
inline auto& getvectorlength( SEXPR n ) { return n->u.vector.length; }
inline auto& getvectordata( SEXPR n ) { return n->u.vector.data; }
inline auto& vectorref( SEXPR n, int i ) { return n->u.vector.data[i]; }
inline void setvectorlength( SEXPR n, int x ) { getvectorlength(n) = x; }
inline void setvectordata( SEXPR n, SEXPR* x ) { getvectordata(n) = x; }
inline void vectorset( SEXPR n, int i, SEXPR x ) { vectorref(n,i) = x; }

// continuation
inline auto& cont_getstate( SEXPR n ) { return n->u.cons.car; }
inline void cont_setstate( SEXPR n, SEXPR x ) { cont_getstate(n) = x; }

// string
inline auto& getstringlength( SEXPR n ) { return n->u.string.length; } 
inline auto& getstringdata( SEXPR n ) { return n->u.string.data; }
inline void setstringlength( SEXPR n, int x ) { getstringlength(n) = x; }
inline void setstringdata( SEXPR n, char* x ) { getstringdata(n) = x; }

// byte vector
inline auto& getbveclength( SEXPR n ) { return n->u.bvec.length; }
inline auto& getbvecdata( SEXPR n ) { return n->u.bvec.data; }
inline void setbveclength( SEXPR n, int x ) { getbveclength(n) = x; }
inline void setbvecdata( SEXPR n, BYTE* x ) { getbvecdata(n) = x; }
inline auto& bvecref( SEXPR n, int i ) { return n->u.bvec.data[i]; }
inline void bvecset( SEXPR n, int i, BYTE x ) { bvecref(n,i) = x; }

// character
inline char& getcharacter( SEXPR n ) { return n->u.ch; }
inline void setcharacter( SEXPR n, char ch ) { getcharacter(n) = ch; }

// symbol
inline auto& getname( SEXPR n ) { return n->u.symbol.name; }
inline auto& getpair( SEXPR n ) { return n->u.symbol.pair; }
inline auto& getvalue( SEXPR n ) { return getcar(getpair(n)); }
inline auto& getplist( SEXPR n ) { return getcdr(getpair(n)); }
inline void setname( SEXPR n, char* x ) { getname(n) = x; }
inline void setpair( SEXPR n, SEXPR x ) { getpair(n) = x; }
inline void setvalue( SEXPR n, SEXPR x ) { getvalue(n) = x; }
inline void setplist( SEXPR n, SEXPR x ) { getplist(n) = x; }

// number
inline auto& getfixnum( SEXPR n ) { return n->u.fixnum; }
inline auto& getflonum( SEXPR n ) { return n->u.flonum; }
inline void setfixnum( SEXPR n, FIXNUM x ) { getfixnum(n) = x; }
inline void setflonum( SEXPR n, FLONUM x ) { getflonum(n) = x; }

// primitive function
inline auto& getfunc( SEXPR n ) { return n->u.prim.func; }
inline void setfunc( SEXPR n, FUNCTION x ) { getfunc(n) = x; }
inline auto& getprimname( SEXPR n ) { return n->u.prim.name; }
inline void setprimname( SEXPR n, const char* x ) { getprimname(n) = x; }

// closure
inline auto& getclosuredata( SEXPR n ) { return n->u.closure.data; }
inline auto& getclosurecode( SEXPR n ) { return getclosuredata(n)[0]; }
inline auto& getclosurebenv( SEXPR n ) { return getclosuredata(n)[1]; }
inline auto& getclosurevars( SEXPR n ) { return getclosuredata(n)[2]; }
inline auto& getclosurenumv( SEXPR n ) { return n->u.closure.numv; }
inline auto& getclosurerargs( SEXPR n ) { return n->u.closure.rargs; }
inline void setclosuredata( SEXPR n, SEXPR* x ) { getclosuredata(n) = x; }
inline void setclosurecode( SEXPR n, SEXPR x ) { getclosurecode(n) = x; }
inline void setclosurebenv( SEXPR n, SEXPR x ) { getclosurebenv(n) = x; }
inline void setclosurevars( SEXPR n, SEXPR x ) { getclosurevars(n) = x; }
inline void setclosurenumv( SEXPR n, BYTE x ) { getclosurenumv(n) = x; }
inline void setclosurerargs( SEXPR n, BYTE x ) { getclosurerargs(n) = x; }

// environment
inline auto& getenvframe( SEXPR n ) { return n->u.environ.frame; }
inline auto& getenvbase( SEXPR n ) { return n->u.environ.baseenv; }
inline void setenvframe( SEXPR n, FRAME x ) { getenvframe(n) = x; }
inline void setenvbase( SEXPR n, SEXPR x ) { getenvbase(n) = x; }

// frame
inline auto& getframenslots( FRAME fr ) { return fr->nslots; }
inline auto& getframevars( FRAME fr ) { return fr->vars; }
inline auto& getframeclosure( FRAME fr ) { return fr->closure; }
inline auto& frameref( FRAME fr, int i ) { return fr->slot[i]; }
inline void setframenslots( FRAME fr, int n ) { getframenslots(fr) = n; }
inline void setframevars( FRAME fr, SEXPR x ) { getframevars(fr) = x; }
inline void setframeclosure( FRAME fr, SEXPR x ) { getframeclosure(fr) = x; }
inline void frameset( FRAME fr, int i, SEXPR x ) { frameref(fr,i) = x; }
inline auto& getframesize( FRAME fr ) { return fr->size; }
inline void setframesize( FRAME fr, int x ) { getframesize(fr) = x; }

// port
inline auto& getfile( SEXPR n ) { return n->u.port.p.file; }
inline auto& getmode( SEXPR n ) { return n->u.port.mode; }
inline void setfile( SEXPR n, FILE* x ) { getfile(n) = (x); }
inline void setmode( SEXPR n, int x ) { getmode(n) = (x); }

// string port
inline auto& getstringportstring( SEXPR n ) { return n->u.port.p.string; }
inline auto& getstringportindex( SEXPR n ) { return n->u.port.index; }
inline void setstringportstring( SEXPR n, std::string* x ) { getstringportstring(n) = (x); }
inline void setstringportindex( SEXPR n, int x ) { getstringportindex(n) = (x); }

// code
inline auto& code_getbcodes( SEXPR n ) { return n->u.code.bcodes; }
inline auto& code_getsexprs( SEXPR n ) { return n->u.code.sexprs; }
inline void code_setbcodes( SEXPR n, SEXPR x ) { code_getbcodes(n) = x; }
inline void code_setsexprs( SEXPR n, SEXPR x) { code_getsexprs(n) = x; }

// promise
inline auto& promise_getexp( SEXPR n ) { return n->u.promise.exp; }
inline auto& promise_getval( SEXPR n ) { return n->u.promise.val; }
inline void promise_setexp( SEXPR n, SEXPR x) { promise_getexp(n) = x; }
inline void promise_setval( SEXPR n, SEXPR x) { promise_getval(n) = x; }

// inline predicates
inline bool nullp( SEXPR n ) { return n == null; }
inline bool anyp( SEXPR n ) { return n != null; }
inline bool _symbolp( SEXPR n ) { return n->kind == n_symbol; }
inline bool _fixnump( SEXPR n ) { return n->kind == n_fixnum; }
inline bool _flonump( SEXPR n ) { return n->kind == n_flonum; }
inline bool _stringp( SEXPR n ) { return n->kind == n_string; }
inline bool _charp( SEXPR n ) { return n->kind == n_char; }
inline bool _consp( SEXPR n ) { return n->kind == n_cons; }
inline bool _envp( SEXPR n ) { return n->kind == n_environment; }
inline bool _funcp( SEXPR n ) { return n->kind == n_func; }
inline bool _closurep( SEXPR n ) { return n->kind == n_closure; }
inline bool _codep( SEXPR n ) { return n->kind == n_code; }
inline bool _compiledp( SEXPR n ) { return _codep(getclosurecode(n)); }
inline bool _compiled_closurep( SEXPR n ) { return _closurep(n) && _compiledp(n); }

inline bool _lastp( SEXPR n ) { return nullp(cdr(n)); }

}

#endif
