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

namespace escheme
{

struct Function
{
   const char* name;
   FUNCTION func;
};

static SEXPR unimplemented() 
{ 
   ERROR::severe("unimplemented"); 
   return null; 
}

static const Function funtab[] =
{
   { "exit",		       FUNC::exit 	  },
   { "gc",		       FUNC::gc		  },
   { "mm",		       FUNC::mm		  },
   { "fs",		       FUNC::fs		  },
   { "eval-state",	       EVAL::get_evaluator_state },

   { "cons",		       FUNC::cons	  },
   { "list",		       FUNC::list	  },
   { "list*",		       FUNC::liststar     },
   { "length",		       FUNC::length	  },
   { "set-car!",               FUNC::set_car      },
   { "set-cdr!",               FUNC::set_cdr      },

   { "car",		       FUNC::car 	  },
   { "cdr",		       FUNC::cdr 	  },
   { "caar",                   []{ return FUNC::cxr("aa"); } },
   { "cadr",                   []{ return FUNC::cxr("da"); } },
   { "cdar",                   []{ return FUNC::cxr("ad"); } },
   { "cddr",                   []{ return FUNC::cxr("dd"); } },
   { "caaar",                  []{ return FUNC::cxr("aaa"); } },
   { "caadr",                  []{ return FUNC::cxr("daa"); } },
   { "cadar",                  []{ return FUNC::cxr("ada"); } },
   { "caddr",                  []{ return FUNC::cxr("dda"); } },
   { "cdaar",                  []{ return FUNC::cxr("aad"); } },
   { "cdadr",                  []{ return FUNC::cxr("dad"); } },
   { "cddar",                  []{ return FUNC::cxr("add"); } },
   { "cdddr",                  []{ return FUNC::cxr("ddd"); } },
   { "caaaar",                 []{ return FUNC::cxr("aaaa"); } },
   { "caaadr",                 []{ return FUNC::cxr("daaa"); } },
   { "caadar",                 []{ return FUNC::cxr("adaa"); } },
   { "caaddr",                 []{ return FUNC::cxr("ddaa"); } },
   { "cadaar",                 []{ return FUNC::cxr("aada"); } },
   { "cadadr",                 []{ return FUNC::cxr("dada"); } },
   { "caddar",                 []{ return FUNC::cxr("adda"); } },
   { "cadddr",                 []{ return FUNC::cxr("ddda"); } },
   { "cdaaar",                 []{ return FUNC::cxr("aaad"); } },
   { "cdaadr",                 []{ return FUNC::cxr("daad"); } },
   { "cdadar",                 []{ return FUNC::cxr("adad"); } },
   { "cdaddr",                 []{ return FUNC::cxr("ddad"); } },
   { "cddaar",                 []{ return FUNC::cxr("aadd"); } },
   { "cddadr",                 []{ return FUNC::cxr("dadd"); } },
   { "cdddar",                 []{ return FUNC::cxr("addd"); } },
   { "cddddr",                 []{ return FUNC::cxr("dddd"); } },

   { "append",		       FUNC::append 	    },
   { "reverse",		       FUNC::reverse 	    },
   { "last-pair",	       FUNC::last_pair 	    },
   { "list-tail",	       FUNC::list_tail 	    }, 

   { "vector",		       FUNC::vector	    },
   { "make-vector",	       FUNC::make_vector    },
   { "vector-ref",	       FUNC::vector_ref	    },
   { "vector-set!",	       FUNC::vector_set     },
   { "vector-length",	       FUNC::vector_length  },
   { "vector-fill!",	       FUNC::vector_fill    },
   { "vector-copy!",	       FUNC::vector_copy    },

   { "byte-vector",	       FUNC::bvector	    },
   { "make-byte-vector",       FUNC::make_bvector   },
   { "byte-vector-ref",	       FUNC::bvector_ref    },
   { "byte-vector-set!",       FUNC::bvector_set    },
   { "byte-vector-length",     FUNC::bvector_length },

   { "list->vector",           FUNC::list_to_vector },
   { "vector->list",           FUNC::vector_to_list },
   { "list->string",           FUNC::list_to_string },
   { "string->list",           FUNC::string_to_list },

   { "eq?",                    FUNC::eq             },
   { "eqv?",                   FUNC::eqv            },
   { "equal?",                 FUNC::equal          },

   { "string->symbol",         FUNC::string_to_symbol },
   { "symbol->string",         FUNC::symbol_to_string },
   { "gensym",                 FUNC::gensym           },
   { "symbol-value",           FUNC::symbol_value     },
   { "set-symbol-value!",      FUNC::set_symbol_value },
   { "symbol-plist",           FUNC::symbol_plist     },
   { "set-symbol-plist!",      FUNC::set_symbol_plist },
   { "get",                    FUNC::get_property     },
   { "put",                    FUNC::put_property     },
   { "remprop",                FUNC::rem_property     },
   { "bound?",                 FUNC::boundp           },
   { "all-symbols",            FUNC::all_symbols      },

   { "read",		       FUNC::read	},
   { "print",		       FUNC::print	},
   { "write",		       FUNC::write	},
   { "display",		       FUNC::display	},
   { "newline",		       FUNC::newline	},
   { "read-char",              FUNC::read_char  },
   { "write-char",             FUNC::write_char },

   { "+",		       MATH::add	},
   { "-",		       MATH::sub	},
   { "*",		       MATH::mul	},
   { "/",		       MATH::div	},

   { "=",		       MATH::eq		},
   { "<",		       MATH::lt		},
   { "<=",		       MATH::le		},
   { ">",		       MATH::gt		},
   { ">=",		       MATH::ge		},

   { "truncate",               MATH::truncate   },
   { "floor",                  MATH::floor      },
   { "ceiling",                MATH::ceiling    },
   { "round",                  MATH::round      },
   { "1+",                     MATH::inc        },
   { "1-",                     MATH::dec        },
   { "-1+",                    MATH::dec        },
   { "inc",                    MATH::inc        },
   { "dec",                    MATH::dec        },
   { "abs",                    MATH::abs        },
   { "gcd",                    MATH::gcd        },
   { "random",                 MATH::random     },
   { "quotient",               MATH::quotient   },
   { "remainder",              MATH::remainder  },
   { "min",                    MATH::min        },
   { "max",                    MATH::max        },
   { "logand",                 MATH::logand     },
   { "logior",                 MATH::logior     },
   { "logxor",                 MATH::logxor     },
   { "lognot",                 MATH::lognot     },
   { "shift-right",            MATH::rsh        },
   { "shift-left",             MATH::lsh        },
   { "shift-right-arithmetic", MATH::ars        },

   { "the-environment",        FUNC::the_environment   },
   { "procedure-environment",  FUNC::proc_environment  },
   { "environment-bindings",   FUNC::env_bindings      },
   { "environment-parent",     FUNC::env_parent	       },
   { "%make-environment",      FUNC::make_environment  },
   { "the-global-environment", FUNC::the_global_environment   },

#ifdef BYTE_CODE_EVALUATOR
   { "%make-code",             FUNC::make_code         },
   { "%get-bcodes",            FUNC::get_bcodes        },
   { "%get-sexprs",            FUNC::get_sexprs        },
#endif

   { "system",                 FUNC::unix_system       },
   { "getargs",                FUNC::unix_getargs      },
   { "getenv",                 FUNC::unix_getenv       },
   { "setenv",                 FUNC::unix_setenv       },
   { "unsetenv",               FUNC::unix_unsetenv     },
   { "gettime",                FUNC::unix_gettime      },
   { "chdir",                  FUNC::unix_change_dir   },
   { "getcwd",                 FUNC::unix_current_dir  },
   { "getenv",                 FUNC::unix_getenv       },

   { "open-input-file",	       FUNC::open_input_file   },
   { "open-output-file",       FUNC::open_output_file  },
   { "open-append-file",       FUNC::open_append_file  },
   { "open-update-file",       FUNC::open_update_file  },
   { "get-file-position",      FUNC::get_file_position },
   { "set-file-position",      FUNC::set_file_position },
   { "close-port",	       FUNC::close_port        },
   { "close-input-port",       FUNC::close_input_port  },
   { "close-output-port",      FUNC::close_output_port },
   { "flush-output",	       FUNC::flush_output_port },

   { "open-input-string",      FUNC::open_input_string  },
   { "open-output-string",     FUNC::open_output_string },
   { "get-output-string",      FUNC::get_output_string  },

   { "not",                    []{ return FUNC::predicate(notp); } },
   { "null?",                  []{ return FUNC::predicate(nullp); } },
   { "atom?",                  []{ return FUNC::predicate(atomp); } },
   { "list?",                  []{ return FUNC::predicate(listp); } },
   { "number?",                []{ return FUNC::predicate(numberp); } },
   { "boolean?",               []{ return FUNC::predicate(booleanp); } },
   { "pair?",                  []{ return FUNC::predicate(consp); } },
   { "symbol?",                []{ return FUNC::predicate(symbolp); } },
   { "complex?",               unimplemented },
   { "real?",                  []{ return FUNC::predicate(flonump); } },
   { "rational?",              unimplemented },
   { "integer?",               []{ return FUNC::predicate(fixnump); } },
   { "char?",                  []{ return FUNC::predicate(charp); } },
   { "string?",                []{ return FUNC::predicate(stringp); } },
   { "vector?",                []{ return FUNC::predicate(vectorp); } },
   { "byte-vector?",           []{ return FUNC::predicate(bvecp); } },
   { "closure?"  ,             []{ return FUNC::predicate(closurep); } },
   { "procedure?",             []{ return FUNC::predicate(procedurep); } },
   { "environment?",	       []{ return FUNC::predicate(envp); } },
   { "continuation?",	       []{ return FUNC::predicate(contp); } },
   { "port?",                  []{ return FUNC::predicate(portp); } },
   { "input-port?",            []{ return FUNC::predicate(inportp); } },
   { "output-port?",           []{ return FUNC::predicate(outportp); } },
   { "string-port?",           []{ return FUNC::predicate(stringportp); } },
   { "input-string-port?",     []{ return FUNC::predicate(instringportp); } },
   { "output-string-port?",    []{ return FUNC::predicate(outstringportp); } },
   { "object?",                unimplemented },
   { "eof-object?",            []{ return FUNC::predicate(eof_objectp); } },
   { "zero?",                  []{ return FUNC::predicate(zerop); } },
   { "positive?",              []{ return FUNC::predicate(positivep); } },
   { "negative?",              []{ return FUNC::predicate(negativep); } },
   { "odd?",                   []{ return FUNC::predicate(oddp); } },
   { "even?",                  []{ return FUNC::predicate(evenp); } },
   { "exact?",                 []{ return FUNC::predicate(exactp); } },
   { "inexact?",               []{ return FUNC::predicate(inexactp); } },
   { "promise?",               []{ return FUNC::predicate(promisep); } },
#ifdef BYTE_CODE_EVALUATOR
   { "code?",                  []{ return FUNC::predicate(codep); } },
#endif
   { "string-null?",           []{ return FUNC::predicate(string_nullp); } },
   { "dict?",                  []{ return FUNC::predicate(dictp); } },
   { "assoc-env?",             []{ return FUNC::predicate(assocenvp); } },
   
   { "make-string",            FUNC::string_make },
   { "string-length",          FUNC::string_length },
   { "string-append",          FUNC::string_append },
   { "string-ref",             FUNC::string_ref },
   { "string-set!",            FUNC::string_set },
   { "string-fill!",           FUNC::string_fill },
   { "string-copy!",           FUNC::string_copy },
   { "substring",              FUNC::string_substring },
   { "string-find",            FUNC::string_find },
   { "string-dup",             FUNC::string_dup },
   { "string-trim",            FUNC::string_trim },
   { "string-trim-left",       FUNC::string_trim_left },
   { "string-trim-right",      FUNC::string_trim_right },
   { "string-downcase!",       FUNC::string_downcase },
   { "string-upcase!",         FUNC::string_upcase },
   { "string-pad-left",        FUNC::string_pad_left },
   { "string-pad-right",       FUNC::string_pad_right },

   { "string=?",               FUNC::string_EQ },
   { "string<?",               FUNC::string_LT },
   { "string<=?",              FUNC::string_LE },
   { "string>?",               FUNC::string_GT },
   { "string>=?",              FUNC::string_GE },
   { "string-ci=?",            FUNC::string_EQci },
   { "string-ci<?",            FUNC::string_LTci },
   { "string-ci<=?",           FUNC::string_LEci },
   { "string-ci>?",            FUNC::string_GTci },
   { "string-ci>=?",           FUNC::string_GEci },

   { "char=?",                 FUNC::char_EQ },
   { "char<?",                 FUNC::char_LT },
   { "char<=?",                FUNC::char_LE },
   { "char>?",                 FUNC::char_GT },
   { "char>=?",                FUNC::char_GE },
   { "char-ci=?",              FUNC::char_EQci },
   { "char-ci<?",              FUNC::char_LTci },
   { "char-ci<=?",             FUNC::char_LEci },
   { "char-ci>?",              FUNC::char_GTci },
   { "char-ci>=?",             FUNC::char_GEci },

   { "char-alphabetic?",       FUNC::char_alphabeticp },
   { "char-numeric?",          FUNC::char_numericp },
   { "char-whitespace?",       FUNC::char_whitespacep },
   { "char-upper-case?",       FUNC::char_upper_casep },
   { "char-lower-case?",       FUNC::char_lower_casep },
   { "char-upcase",            FUNC::char_upcase },
   { "char-downcase",          FUNC::char_downcase },

   { "integer->char",          FUNC::integer_to_char },
   { "char->integer",          FUNC::char_to_integer },

   { "integer->string",        FUNC::integer_to_string },
   { "string->integer",        FUNC::string_to_integer },

   { "member",                 FUNC::member },
   { "memv",                   FUNC::memv },
   { "memq",                   FUNC::memq },
   { "assoc",                  FUNC::assoc },
   { "assv",                   FUNC::assv },
   { "assq",                   FUNC::assq },

   { "transcript-on",          FUNC::transcript_on  },
   { "transcript-off",         FUNC::transcript_off },

   { "add-history",	       FUNC::history_add   },
   { "show-history",	       FUNC::history_show  },
   { "clear-history",	       FUNC::history_clear },
   { "set-prompt",	       FUNC::set_prompt    },

   { "%closure-code",          FUNC::closure_code },
   { "%closure-benv",          FUNC::closure_benv },
   { "%closure-vars",          FUNC::closure_vars },
   { "%closure-numv",          FUNC::closure_numv },
   { "%closure-rest",          FUNC::closure_rest },
#ifdef BYTE_CODE_EVALUATOR
   { "%closure-code-set!",     FUNC::closure_code_set },
#endif

   { "socket-read",            IPCSOC::read           },
   { "socket-write",           IPCSOC::write          },
   { "socket-recvfrom",        IPCSOC::recvfrom       },
   { "socket-recv",            IPCSOC::recv           },
   { "socket-sendto",          IPCSOC::sendto         },
   { "socket-create-tcp",      IPCSOC::create_tcp     },
   { "socket-create-udp",      IPCSOC::create_udp     },
   { "socket-bind",            IPCSOC::bind           },
   { "socket-bind-address",    IPCSOC::bind_address   },
   { "socket-create-address",  IPCSOC::create_address },
   { "socket-listen",          IPCSOC::listen         },
   { "socket-accept",          IPCSOC::accept         },
   { "socket-connect",         IPCSOC::connect        },
   { "socket-disconnect",      IPCSOC::disconnect     },
   { "socket-close",           IPCSOC::close          },
   { "read-select",            IPCSOC::read_select    },

#ifdef BYTE_CODE_EVALUATOR
   { "assemble",               ASSEM::encode },
   { "disassemble",            ASSEM::decode },
#endif

   { "make-dict",              FUNC::make_dict },
   { "has-key?",               FUNC::has_key   },
   { "dict-ref",               FUNC::dict_ref  },
   { "dict-set!",              FUNC::dict_set  },
   { "dict-items",             FUNC::dict_items  },
   { "dict-rem!",              FUNC::dict_rem  },
   { "dict-empty!",            FUNC::dict_empty  },

   { "%make-assoc-env",        FUNC::make_assocenv },
   { "%assoc-env-has?",        FUNC::assocenv_has },
   { "%assoc-env-ref",         FUNC::assocenv_ref },
   { "%assoc-env-set!",        FUNC::assocenv_set },

   { "%object-address",        FUNC::objaddr },
};

void FUNTAB::initialize()
{
   for ( auto& fn : funtab )
   {
      SYMTAB::enter( fn.name, MEMORY::prim( fn.name, fn.func ) );
   }

   SYMTAB::enter( "eval",     MEMORY::prim( "eval",     FUNCTION(0), n_eval    ) );
   SYMTAB::enter( "apply",    MEMORY::prim( "apply",    FUNCTION(0), n_apply   ) );
   SYMTAB::enter( "call/cc",  MEMORY::prim( "call/cc",  FUNCTION(0), n_callcc  ) );
   SYMTAB::enter( "map",      MEMORY::prim( "map",      FUNCTION(0), n_map     ) );
   SYMTAB::enter( "for-each", MEMORY::prim( "for-each", FUNCTION(0), n_foreach ) );
   SYMTAB::enter( "force",    MEMORY::prim( "force",    FUNCTION(0), n_force   ) );
}

}
