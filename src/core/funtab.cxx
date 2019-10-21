#include "funtab.hxx"

#include "sexpr.hxx"
#include "symtab.hxx"
#include "func.hxx"
#include "math.hxx"
#include "memory.hxx"
#include "regstack.hxx"
#include "error.hxx"
#include "ipcsoc.hxx"

#include "eval/eval.hxx"
#ifdef BYTE_CODE_EVALUATOR
#include "eval/assem.hxx"
#endif

struct Function
{
   const char* name;
   FUNCTION func;
   NodeKind kind;
};

static SEXPR unimplemented() 
{ 
   ERROR::severe("unimplemented"); 
   return null; 
}

static const Function funtab[] =
{
   { "exit",		     FUNC::exit 	, n_func },
   { "eval-state",	     EVAL::get_evaluator_state , n_func },

   { "cons",		     FUNC::cons		, n_func },
   { "list",		     FUNC::list		, n_func },
   { "list*",		     FUNC::liststar     , n_func },
   { "length",		     FUNC::length	, n_func },
   { "set-car!",             FUNC::set_car      , n_func },
   { "set-cdr!",             FUNC::set_cdr      , n_func },

   { "car",		     FUNC::car 		, n_func },
   { "cdr",		     FUNC::cdr 		, n_func },
   
   { "caar",                 []{ return FUNC::cxr("aa"); } , n_func },
   { "cadr",                 []{ return FUNC::cxr("ad"); } , n_func },
   { "cdar",                 []{ return FUNC::cxr("da"); } , n_func },
   { "cddr",                 []{ return FUNC::cxr("dd"); } , n_func },
   { "caaar",                []{ return FUNC::cxr("aaa"); } , n_func },
   { "caadr",                []{ return FUNC::cxr("aad"); } , n_func },
   { "cadar",                []{ return FUNC::cxr("ada"); } , n_func },
   { "caddr",                []{ return FUNC::cxr("add"); } , n_func },
   { "cdaar",                []{ return FUNC::cxr("daa"); } , n_func },
   { "cdadr",                []{ return FUNC::cxr("dad"); } , n_func },
   { "cddar",                []{ return FUNC::cxr("dda"); } , n_func },
   { "cdddr",                []{ return FUNC::cxr("ddd"); } , n_func },
   { "caaaar",               []{ return FUNC::cxr("aaaa"); } , n_func },
   { "caaadr",               []{ return FUNC::cxr("aaad"); } , n_func },
   { "caadar",               []{ return FUNC::cxr("aada"); } , n_func },
   { "caaddr",               []{ return FUNC::cxr("aadd"); } , n_func },
   { "cadaar",               []{ return FUNC::cxr("adaa"); } , n_func },
   { "cadadr",               []{ return FUNC::cxr("adad"); } , n_func },
   { "caddar",               []{ return FUNC::cxr("adda"); } , n_func },
   { "cadddr",               []{ return FUNC::cxr("addd"); } , n_func },
   { "cdaaar",               []{ return FUNC::cxr("daaa"); } , n_func },
   { "cdaadr",               []{ return FUNC::cxr("daad"); } , n_func },
   { "cdadar",               []{ return FUNC::cxr("dada"); } , n_func },
   { "cdaddr",               []{ return FUNC::cxr("dadd"); } , n_func },
   { "cddaar",               []{ return FUNC::cxr("ddaa"); } , n_func },
   { "cddadr",               []{ return FUNC::cxr("ddad"); } , n_func },
   { "cdddar",               []{ return FUNC::cxr("ddda"); } , n_func },
   { "cddddr",               []{ return FUNC::cxr("dddd"); } , n_func },

   { "append",		     FUNC::append 	, n_func },
   { "reverse",		     FUNC::reverse 	, n_func },
   { "last-pair",	     FUNC::last_pair 	, n_func },
   { "list-tail",	     FUNC::list_tail 	, n_func },

   { "vector",		     FUNC::vector	 , n_func },
   { "make-vector",	     FUNC::make_vector	 , n_func },
   { "vector-ref",	     FUNC::vector_ref	 , n_func },
   { "vector-set!",	     FUNC::vector_set    , n_func },
   { "vector-length",	     FUNC::vector_length , n_func },
   { "vector-fill!",	     FUNC::vector_fill   , n_func },
   { "vector-copy!",	     FUNC::vector_copy   , n_func },

   { "byte-vector",	     FUNC::bvector	  , n_func },
   { "make-byte-vector",     FUNC::make_bvector	  , n_func },
   { "byte-vector-ref",	     FUNC::bvector_ref	  , n_func },
   { "byte-vector-set!",     FUNC::bvector_set    , n_func },
   { "byte-vector-length",   FUNC::bvector_length , n_func },

   { "find-index",           FUNC::find_index     , n_func },
   { "rank",                 FUNC::rank           , n_func },

   { "list->vector",         FUNC::list_to_vector , n_func },
   { "vector->list",         FUNC::vector_to_list , n_func },
   { "list->string",         FUNC::list_to_string , n_func },
   { "string->list",         FUNC::string_to_list , n_func },

   { "eq?",                  FUNC::eq           , n_func },
   { "eqv?",                 FUNC::eqv          , n_func },
   { "equal?",               FUNC::equal        , n_func },

   { "string->symbol",       FUNC::string_to_symbol , n_func },
   { "symbol->string",       FUNC::symbol_to_string , n_func },
   { "gensym",               FUNC::gensym           , n_func },
   { "symbol-value",         FUNC::symbol_value     , n_func },
   { "set-symbol-value!",    FUNC::set_symbol_value , n_func },
   { "symbol-plist",         FUNC::symbol_plist     , n_func },
   { "set-symbol-plist!",    FUNC::set_symbol_plist , n_func },
   { "get",                  FUNC::get_property     , n_func },
   { "put",                  FUNC::put_property     , n_func },
   { "*symbols*",            FUNC::symbols          , n_func },

   { "read",		     FUNC::read		, n_func },
   { "print",		     FUNC::print	, n_func },
   { "write",		     FUNC::write	, n_func },
   { "display",		     FUNC::display	, n_func },
   { "newline",		     FUNC::newline	, n_func },
   { "read-char",            FUNC::read_char    , n_func },
   { "write-char",           FUNC::write_char   , n_func },

   { "gc",		     FUNC::gc		, n_func },
#ifdef OBJECT_CACHE
   { "gc-copy",		     FUNC::gc_copy	, n_func },
#endif
   { "fs",		     FUNC::fs		, n_func },

   { "eval",		     FUNCTION(0)        , n_eval    },
   { "apply",                FUNCTION(0)        , n_apply   },
   { "call/cc",              FUNCTION(0)        , n_callcc  },
   { "map",                  FUNCTION(0)        , n_map     },
   { "for-each",             FUNCTION(0)        , n_foreach },
   { "force",                FUNCTION(0)        , n_force   },

   { "+",		     MATH::add		, n_func },
   { "-",		     MATH::sub		, n_func },
   { "*",		     MATH::mul		, n_func },
   { "/",		     MATH::div		, n_func },

   { "=",		     MATH::eq		, n_func },
   { "<",		     MATH::lt		, n_func },
   { "<=",		     MATH::le		, n_func },
   { ">",		     MATH::gt		, n_func },
   { ">=",		     MATH::ge		, n_func },

   { "truncate",             MATH::truncate     , n_func },
   { "floor",                MATH::floor        , n_func },
   { "ceiling",              MATH::ceiling      , n_func },
   { "round",                MATH::round        , n_func },
   { "1+",                   MATH::inc          , n_func },
   { "1-",                   MATH::dec          , n_func },
   { "-1+",                  MATH::dec          , n_func },
   { "inc",                  MATH::inc          , n_func },
   { "dec",                  MATH::dec          , n_func },
   { "abs",                  MATH::abs          , n_func },
   { "gcd",                  MATH::gcd          , n_func },
   { "random",               MATH::random       , n_func },
   { "quotient",             MATH::quotient     , n_func },
   { "remainder",            MATH::remainder    , n_func },
   { "min",                  MATH::min          , n_func },
   { "max",                  MATH::max          , n_func },
   { "logand",               MATH::logand       , n_func },
   { "logior",               MATH::logior       , n_func },
   { "logxor",               MATH::logxor       , n_func },
   { "lognot",               MATH::lognot       , n_func },
   { "shift-right",          MATH::rsh          , n_func },
   { "shift-left",           MATH::lsh          , n_func },
   { "shift-right-arithmetic", MATH::ars          , n_func },

   { "the-environment",       FUNC::the_environment   , n_func },
   { "procedure-environment", FUNC::proc_environment  , n_func },
   { "environment-bindings",  FUNC::env_bindings      , n_func },
   { "environment-parent",    FUNC::env_parent	      , n_func },
   { "%make-environment",     FUNC::make_environment  , n_func },

   { "%make-closure",         FUNC::make_closure      , n_func },
   { "%parse-formals",        FUNC::parse_formals     , n_func },
#ifdef BYTE_CODE_EVALUATOR
   { "%make-code",            FUNC::make_code         , n_func },
   { "%get-bcodes",           FUNC::get_bcodes        , n_func },
   { "%get-sexprs",           FUNC::get_sexprs        , n_func },
#endif

   { "system",                FUNC::unix_system       , n_func },
   { "getargs",               FUNC::unix_getargs      , n_func },
   { "getenv",                FUNC::unix_getenv       , n_func },
   { "setenv",                FUNC::unix_setenv       , n_func },
   { "unsetenv",              FUNC::unix_unsetenv     , n_func },
   { "gettime",               FUNC::unix_gettime      , n_func },
   { "chdir",                 FUNC::unix_change_dir   , n_func },
   { "getcwd",                FUNC::unix_current_dir  , n_func },
   { "getenv",                FUNC::unix_getenv       , n_func },

   { "open-input-file",	      FUNC::open_input_file   , n_func },
   { "open-output-file",      FUNC::open_output_file  , n_func },
   { "open-append-file",      FUNC::open_append_file  , n_func },
   { "open-update-file",      FUNC::open_update_file  , n_func },
   { "get-file-position",     FUNC::get_file_position , n_func },
   { "set-file-position",     FUNC::set_file_position , n_func },
   { "close-port",	      FUNC::close_port        , n_func },
   { "close-input-port",      FUNC::close_input_port  , n_func },
   { "close-output-port",     FUNC::close_output_port , n_func },
   { "flush-output",	      FUNC::flush_output_port , n_func },

   { "open-input-string",     FUNC::open_input_string  , n_func },
   { "open-output-string",    FUNC::open_output_string , n_func },
   { "get-output-string",     FUNC::get_output_string  , n_func },

   { "not",                   []{ return FUNC::predicate(notp); } , n_func },
   { "bound?",                []{ return FUNC::predicate(boundp); } , n_func },
   { "null?",                 []{ return FUNC::predicate(nullp); } , n_func },
   { "atom?",                 []{ return FUNC::predicate(atomp); } , n_func },
   { "list?",                 []{ return FUNC::predicate(listp); } , n_func },
   { "number?",               []{ return FUNC::predicate(numberp); } , n_func },
   { "boolean?",              []{ return FUNC::predicate(booleanp); } , n_func },
   { "pair?",                 []{ return FUNC::predicate(consp); } , n_func },
   { "symbol?",               []{ return FUNC::predicate(symbolp); } , n_func },
   { "complex?",              unimplemented , n_func },
   { "real?",                 []{ return FUNC::predicate(flonump); } , n_func },
   { "rational?",             unimplemented , n_func },
   { "integer?",              []{ return FUNC::predicate(fixnump); } , n_func },
   { "char?",                 []{ return FUNC::predicate(charp); } , n_func },
   { "string?",               []{ return FUNC::predicate(stringp); } , n_func },
   { "vector?",               []{ return FUNC::predicate(vectorp); } , n_func },
   { "byte-vector?",          []{ return FUNC::predicate(bvecp); } , n_func },
   { "closure?"  ,            []{ return FUNC::predicate(closurep); } , n_func },
   { "procedure?",            FUNC::procedurep , n_func },
   { "environment?",	      []{ return FUNC::predicate(envp); } , n_func },
   { "continuation?",	      []{ return FUNC::predicate(contp); } , n_func },
   { "port?",                 []{ return FUNC::predicate(portp); } , n_func },
   { "input-port?",           []{ return FUNC::predicate(inportp); } , n_func },
   { "output-port?",          []{ return FUNC::predicate(outportp); } , n_func },
   { "string-port?",          []{ return FUNC::predicate(stringportp); } , n_func },
   { "input-string-port?",    []{ return FUNC::predicate(instringportp); } , n_func },
   { "output-string-port?",   []{ return FUNC::predicate(outstringportp); } , n_func },
   { "object?",               unimplemented , n_func },
   { "eof-object?",           []{ return FUNC::predicate(eof_objectp); } , n_func },
   { "default-object?",       []{ return FUNC::predicate(default_objectp); } , n_func },
   { "zero?",                 []{ return FUNC::predicate(zerop); } , n_func },
   { "positive?",             []{ return FUNC::predicate(positivep); } , n_func },
   { "negative?",             []{ return FUNC::predicate(negativep); } , n_func },
   { "odd?",                  []{ return FUNC::predicate(oddp); } , n_func },
   { "even?",                 []{ return FUNC::predicate(evenp); } , n_func },
   { "exact?",                []{ return FUNC::predicate(exactp); } , n_func },
   { "inexact?",              []{ return FUNC::predicate(inexactp); } , n_func },
   { "promise?",              []{ return FUNC::predicate(promisep); } , n_func },
#ifdef BYTE_CODE_EVALUATOR
   { "code?",                 []{ return FUNC::predicate(codep); } , n_func },
#endif
   { "string-null?",          []{ return FUNC::predicate(string_nullp); } , n_func },
   
   { "string-length",         FUNC::string_length , n_func },
   { "string-append",         FUNC::string_append , n_func },
   { "string-ref",            FUNC::string_ref , n_func },
   { "string-set!",           FUNC::string_set , n_func },
   { "string-fill!",          FUNC::string_fill , n_func },
   { "string-copy!",          FUNC::string_copy , n_func },
   { "substring",             FUNC::substring , n_func },

   { "string=?",              FUNC::string_EQ , n_func },
   { "string<?",              FUNC::string_LT , n_func },
   { "string<=?",             FUNC::string_LE , n_func },
   { "string>?",              FUNC::string_GT , n_func },
   { "string>=?",             FUNC::string_GE , n_func },
   { "string-ci=?",           FUNC::string_EQci , n_func },
   { "string-ci<?",           FUNC::string_LTci , n_func },
   { "string-ci<=?",          FUNC::string_LEci , n_func },
   { "string-ci>?",           FUNC::string_GTci , n_func },
   { "string-ci>=?",          FUNC::string_GEci , n_func },

   { "char=?",                FUNC::char_EQ , n_func },
   { "char<?",                FUNC::char_LT , n_func },
   { "char<=?",               FUNC::char_LE , n_func },
   { "char>?",                FUNC::char_GT , n_func },
   { "char>=?",               FUNC::char_GE , n_func },
   { "char-ci=?",             FUNC::char_EQci , n_func },
   { "char-ci<?",             FUNC::char_LTci , n_func },
   { "char-ci<=?",            FUNC::char_LEci , n_func },
   { "char-ci>?",             FUNC::char_GTci , n_func },
   { "char-ci>=?",            FUNC::char_GEci , n_func },

   { "char-alphabetic?",      FUNC::char_alphabeticp , n_func },
   { "char-numeric?",         FUNC::char_numericp , n_func },
   { "char-whitespace?",      FUNC::char_whitespacep , n_func },
   { "char-upper-case?",      FUNC::char_upper_casep , n_func },
   { "char-lower-case?",      FUNC::char_lower_casep , n_func },
   { "char-upcase",           FUNC::char_upcase , n_func },
   { "char-downcase",         FUNC::char_downcase , n_func },

   { "integer->char",         FUNC::integer_to_char , n_func },
   { "char->integer",         FUNC::char_to_integer , n_func },

   { "integer->string",       FUNC::integer_to_string , n_func },
   { "string->integer",       FUNC::string_to_integer , n_func },

   { "member",                FUNC::member , n_func },
   { "memv",                  FUNC::memv , n_func },
   { "memq",                  FUNC::memq , n_func },
   { "assoc",                 FUNC::assoc , n_func },
   { "assv",                  FUNC::assv , n_func },
   { "assq",                  FUNC::assq , n_func },

   { "transcript-on",         FUNC::transcript_on  , n_func },
   { "transcript-off",        FUNC::transcript_off , n_func },

   { "add-history",	      FUNC::history_add   , n_func },
   { "show-history",	      FUNC::history_show  , n_func },
   { "clear-history",	      FUNC::history_clear , n_func },
   { "set-prompt",	      FUNC::set_prompt    , n_func },

   { "%closure-code",         FUNC::closure_code , n_func },
   { "%closure-benv",         FUNC::closure_benv , n_func },
   { "%closure-vars",         FUNC::closure_vars , n_func },
   { "%closure-numv",         FUNC::closure_numv , n_func },
   { "%closure-rest",         FUNC::closure_rest , n_func },
#ifdef BYTE_CODE_EVALUATOR
   { "%closure-code-set!",    FUNC::closure_code_set , n_func },
#endif

   { "socket-read",           IPCSOC::read           , n_func },
   { "socket-write",          IPCSOC::write          , n_func },
   { "socket-recvfrom",       IPCSOC::recvfrom       , n_func },
   { "socket-recv",           IPCSOC::recv           , n_func },
   { "socket-sendto",         IPCSOC::sendto         , n_func },
   { "socket-create-tcp",     IPCSOC::create_tcp     , n_func },
   { "socket-create-udp",     IPCSOC::create_udp     , n_func },
   { "socket-bind",           IPCSOC::bind           , n_func },
   { "socket-bind-address",   IPCSOC::bind_address   , n_func },
   { "socket-create-address", IPCSOC::create_address , n_func },
   { "socket-listen",         IPCSOC::listen         , n_func },
   { "socket-accept",         IPCSOC::accept         , n_func },
   { "socket-connect",        IPCSOC::connect        , n_func },
   { "socket-disconnect",     IPCSOC::disconnect     , n_func },
   { "socket-close",          IPCSOC::close          , n_func },
   { "read-select",           IPCSOC::read_select    , n_func },

#ifdef BYTE_CODE_EVALUATOR
   { "assemble",              ASSEM::encode , n_func },
   { "disassemble",           ASSEM::decode , n_func },
#endif

   { "%object-address",       FUNC::objaddr          , n_func },
};


void FUNTAB::initialize()
{
   regstack.push(null);

   constexpr int NFUNCS = sizeof(funtab) / sizeof(funtab[0]);
   
   for ( int i = 0; i < NFUNCS; ++i )
   {
      const auto& fn = funtab[i];
      
      regstack.top() = MEMORY::prim( fn.func, fn.kind );
      setprimname( regstack.top(), fn.name );
      SYMTAB::enter( fn.name, regstack.top() );
   }

   regstack.pop();
}

