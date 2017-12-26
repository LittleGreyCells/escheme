#include <vector>

#include "funtab.hxx"
#include "sexpr.hxx"
#include "symtab.hxx"
#include "func.hxx"
#include "math.hxx"
#include "memory.hxx"
#include "regstack.hxx"
#include "eval.hxx"
#include "error.hxx"
#include "ipcsoc.hxx"

struct Function
{
   const char* name;
   PRIMITIVE func;
   NodeKind kind;
};

static SEXPR unimplemented() 
{ 
   ERROR::severe("unimplemented"); 
   return null; 
}

static std::vector<Function> funtab =
{
   { "exit",		     FUNC::exit 	},
   { "eval-state",	     EVAL::get_evaluator_state },

   { "cons",		     FUNC::cons		},
   { "list",		     FUNC::list		},
   { "list*",		     FUNC::liststar	},
   { "length",		     FUNC::length	},
   { "set-car!",             FUNC::set_car      },
   { "set-cdr!",             FUNC::set_cdr      },

   { "car",		     FUNC::car 		},
   { "cdr",		     FUNC::cdr 		},
   { "caar",		     FUNC::caar 	},
   { "cadr",		     FUNC::cadr 	},
   { "cdar",		     FUNC::cdar 	},
   { "cddr",		     FUNC::cddr 	},
   { "caaar",		     FUNC::caaar 	},
   { "caadr",		     FUNC::caadr 	},
   { "cadar",		     FUNC::cadar 	},
   { "caddr",		     FUNC::caddr 	},
   { "cdaar",		     FUNC::cdaar 	},
   { "cdadr",		     FUNC::cdadr 	},
   { "cddar",		     FUNC::cddar 	},
   { "cdddr",		     FUNC::cdddr 	},
   { "caaaar",		     FUNC::caaaar 	},
   { "caaadr",		     FUNC::caaadr 	},
   { "caadar",		     FUNC::caadar 	},
   { "caaddr",		     FUNC::caaddr 	},
   { "cadaar",		     FUNC::cadaar 	},
   { "cadadr",		     FUNC::cadadr 	},
   { "caddar",		     FUNC::caddar 	},
   { "cadddr",		     FUNC::cadddr 	},
   { "cdaaar",		     FUNC::cdaaar 	},
   { "cdaadr",		     FUNC::cdaadr 	},
   { "cdadar",		     FUNC::cdadar 	},
   { "cdaddr",		     FUNC::cdaddr 	},
   { "cddaar",		     FUNC::cddaar 	},
   { "cddadr",		     FUNC::cddadr 	},
   { "cdddar",		     FUNC::cdddar 	},
   { "cddddr",		     FUNC::cddddr 	},

   { "append",		     FUNC::append 	},
   { "reverse",		     FUNC::reverse 	},
   { "last-pair",	     FUNC::last_pair 	},
   { "list-tail",	     FUNC::list_tail 	},

   { "vector",		     FUNC::vector	 },
   { "make-vector",	     FUNC::make_vector	 },
   { "vector-ref",	     FUNC::vector_ref	 },
   { "vector-set!",	     FUNC::vector_set    },
   { "vector-length",	     FUNC::vector_length },
   { "vector-fill!",	     FUNC::vector_fill   },
   { "vector-copy!",	     FUNC::vector_copy   },

   { "byte-vector",	     FUNC::bvector	  },
   { "make-byte-vector",     FUNC::make_bvector	  },
   { "byte-vector-ref",	     FUNC::bvector_ref	  },
   { "byte-vector-set!",     FUNC::bvector_set    },
   { "byte-vector-length",   FUNC::bvector_length },

   { "find-index",           FUNC::find_index     },

   { "list->vector",         FUNC::list_to_vector },
   { "vector->list",         FUNC::vector_to_list },
   { "list->string",         FUNC::list_to_string },
   { "string->list",         FUNC::string_to_list },

   { "eq?",                  FUNC::eq           },
   { "eqv?",                 FUNC::eqv          },
   { "equal?",               FUNC::equal        },

   { "%transform-let",       FUNC::transform_let     },
   { "%transform-let*",      FUNC::transform_letstar },
   { "%transform-letrec",    FUNC::transform_letrec  },

   { "string->symbol",       FUNC::string_to_symbol },
   { "symbol->string",       FUNC::symbol_to_string },
   { "gensym",               FUNC::gensym           },
   { "symbol-value",         FUNC::symbol_value     },
   { "set-symbol-value!",    FUNC::set_symbol_value },
   { "symbol-plist",         FUNC::symbol_plist     },
   { "set-symbol-plist!",    FUNC::set_symbol_plist },
   { "get",                  FUNC::get_property     },
   { "put",                  FUNC::put_property     },

   { "read",		     FUNC::read		},
   { "print",		     FUNC::print	},
   { "write",		     FUNC::write	},
   { "display",		     FUNC::display	},
   { "newline",		     FUNC::newline	},
   { "read-char",            FUNC::read_char    },
   { "write-char",           FUNC::write_char   },

   { "%car",		     FUNC::unsafe_car	},
   { "%cdr",		     FUNC::unsafe_cdr	},
#if 0
   { "%vector-ref",	     FUNC::unsafe_vref	},
   { "%vector-set!",	     FUNC::unsafe_vset	},
   { "%vector-length",	     FUNC::unsafe_vlen	},
#endif

   { "gc",		     FUNC::gc		},

   { "eval",		     EVAL::eval,           n_eval     },
   { "%prim-eval",	     EVAL::eval,           n_eval     },
   { "apply",                EVAL::apply,          n_apply    },
   { "call/cc",              EVAL::callcc,         n_callcc   },
   { "map",                  EVAL::map,            n_map      },
   { "for-each",             EVAL::foreach,        n_foreach  },
   { "force",                EVAL::force,          n_force    },

   { "call-with-current-continuation", EVAL::callcc,   n_callcc   },

   { "+",		     MATH::add		},
   { "-",		     MATH::sub		},
   { "*",		     MATH::mul		},
   { "/",		     MATH::div		},

   { "=",		     MATH::eq		},
   { "<",		     MATH::lt		},
   { "<=",		     MATH::le		},
   { ">",		     MATH::gt		},
   { ">=",		     MATH::ge		},

   { "truncate",             MATH::truncate     },
   { "floor",                MATH::floor        },
   { "ceiling",              MATH::ceiling      },
   { "round",                MATH::round        },
   { "1+",                   MATH::inc          },
   { "-1+",                  MATH::dec          },
   { "inc",                  MATH::inc          },
   { "dec",                  MATH::dec          },
   { "abs",                  MATH::abs          },
   { "gcd",                  MATH::gcd          },
   { "random",               MATH::random       },
   { "quotient",             MATH::quotient     },
   { "remainder",            MATH::remainder    },
   { "min",                  MATH::min          },
   { "max",                  MATH::max          },
   { "logand",               MATH::logand       },
   { "logior",               MATH::logior       },
   { "logxor",               MATH::logxor       },
   { "lognot",               MATH::lognot       },
   { "shift-right",          MATH::rsh          },
   { "shift-left",           MATH::lsh          },
   { "shift-right-arithmetic", MATH::ars          },

   { "the-environment",       FUNC::the_environment   },
   { "procedure-environment", FUNC::proc_environment  },
   { "environment-bindings",  FUNC::env_bindings      },
   { "environment-parent",    FUNC::env_parent	      },
   { "%make-environment",     FUNC::make_environment  },

   { "%make-closure",         FUNC::make_closure      },
   { "%parse-formals",        FUNC::parse_formals     },

   { "system",                FUNC::unix_system       },
   { "getargs",               FUNC::unix_getargs      },
   { "gettime",               FUNC::unix_gettime      },

   { "open-input-file",	      FUNC::open_input_file   },
   { "open-output-file",      FUNC::open_output_file  },
   { "open-append-file",      FUNC::open_append_file  },
   { "open-update-file",      FUNC::open_update_file  },
   { "get-file-position",     FUNC::get_file_position },
   { "set-file-position",     FUNC::set_file_position },
   { "close-port",	      FUNC::close_port        },
   { "close-input-port",      FUNC::close_input_port  },
   { "close-output-port",     FUNC::close_output_port },
   { "flush-output",	      FUNC::flush_output_port },

   { "open-input-string",     FUNC::open_input_string  },
   { "open-output-string",    FUNC::open_output_string },
   { "get-output-string",     FUNC::get_output_string  },

   { "not",                   PRED_FUN(notp) },
   { "bound?",                PRED_FUN(boundp) },
   { "null?",                 PRED_FUN(nullp) },
   { "atom?",                 PRED_FUN(atomp) },
   { "list?",                 PRED_FUN(listp) },
   { "number?",               PRED_FUN(numberp) },
   { "boolean?",              PRED_FUN(booleanp) },
   { "pair?",                 PRED_FUN(consp) },
   { "symbol?",               PRED_FUN(symbolp) },
   { "complex?",              unimplemented },
   { "real?",                 PRED_FUN(flonump) },
   { "rational?",             unimplemented },
   { "integer?",              PRED_FUN(fixnump) },
   { "char?",                 PRED_FUN(charp) },
   { "string?",               PRED_FUN(stringp) },
   { "vector?",               PRED_FUN(vectorp) },
   { "byte-vector?",          PRED_FUN(bvecp) },
   { "closure?"  ,            PRED_FUN(closurep) },
   { "procedure?",            FUNC::procedurep },
   { "environment?",	      PRED_FUN(envp) },
   { "port?",                 PRED_FUN(portp) },
   { "input-port?",           PRED_FUN(inportp) },
   { "output-port?",          PRED_FUN(outportp) },
   { "string-port?",          PRED_FUN(stringportp) },
   { "input-string-port?",    PRED_FUN(instringportp) },
   { "output-string-port?",   PRED_FUN(outstringportp) },
   { "object?",               unimplemented },
   { "eof-object?",           PRED_FUN(eof_objectp) },
   { "default-object?",       PRED_FUN(default_objectp)  },
   { "zero?",                 PRED_FUN(zerop) },
   { "positive?",             PRED_FUN(positivep) },
   { "negative?",             PRED_FUN(negativep) },
   { "odd?",                  PRED_FUN(oddp) },
   { "even?",                 PRED_FUN(evenp) },
   { "exact?",                PRED_FUN(exactp) },
   { "inexact?",              PRED_FUN(inexactp) },
   { "promise?",              PRED_FUN(promisep) },

   { "string-null?",          PRED_FUN(string_nullp) },
   { "string-length",         FUNC::string_length },
   { "string-append",         FUNC::string_append },
   { "string-ref",            FUNC::string_ref },
   { "string-set!",           FUNC::string_set },
   { "string-fill!",          FUNC::string_fill },
   { "string-copy!",          FUNC::string_copy },
   { "substring",             FUNC::substring },

   { "string=?",              FUNC::string_EQ },
   { "string<?",              FUNC::string_LT },
   { "string<=?",             FUNC::string_LE },
   { "string>?",              FUNC::string_GT },
   { "string>=?",             FUNC::string_GE },
   { "string-ci=?",           FUNC::string_EQci },
   { "string-ci<?",           FUNC::string_LTci },
   { "string-ci<=?",          FUNC::string_LEci },
   { "string-ci>?",           FUNC::string_GTci },
   { "string-ci>=?",          FUNC::string_GEci },

   { "char=?",                FUNC::char_EQ },
   { "char<?",                FUNC::char_LT },
   { "char<=?",               FUNC::char_LE },
   { "char>?",                FUNC::char_GT },
   { "char>=?",               FUNC::char_GE },
   { "char-ci=?",             FUNC::char_EQci },
   { "char-ci<?",             FUNC::char_LTci },
   { "char-ci<=?",            FUNC::char_LEci },
   { "char-ci>?",             FUNC::char_GTci },
   { "char-ci>=?",            FUNC::char_GEci },

   { "char-alphabetic?",      FUNC::char_alphabeticp },
   { "char-numeric?",         FUNC::char_numericp },
   { "char-whitespace?",      FUNC::char_whitespacep },
   { "char-upper-case?",      FUNC::char_upper_casep },
   { "char-lower-case?",      FUNC::char_lower_casep },
   { "char-upcase",           FUNC::char_upcase },
   { "char-downcase",         FUNC::char_downcase },

   { "integer->char",         FUNC::integer_to_char },
   { "char->integer",         FUNC::char_to_integer },

   { "integer->string",       FUNC::integer_to_string },
   { "string->integer",       FUNC::string_to_integer },

   { "member",                FUNC::member },
   { "memv",                  FUNC::memv },
   { "memq",                  FUNC::memq },
   { "assoc",                 FUNC::assoc },
   { "assv",                  FUNC::assv },
   { "assq",                  FUNC::assq },

   { "%gref",                 FUNC::gref },
   { "%fref",                 FUNC::fref },

   { "transcript-on",         FUNC::transcript_on },
   { "transcript-off",        FUNC::transcript_off },

   { "add-history",	      FUNC::history_add },
   { "show-history",	      FUNC::history_show },
   { "history",	              FUNC::history_show },
   { "clear-history",	      FUNC::history_clear },
   { "set-prompt",	      FUNC::set_prompt },

   { "%closure-code",         FUNC::closure_code },
   { "%closure-benv",         FUNC::closure_benv },
   { "%closure-vars",         FUNC::closure_vars },
   { "%closure-numv",         FUNC::closure_numv },
   { "%closure-rest",         FUNC::closure_rest },

   { "socket-read",           IPCSOC::read },
   { "socket-write",          IPCSOC::write },
   { "socket-recvfrom",       IPCSOC::recvfrom },
   { "socket-recv",           IPCSOC::recv },
   { "socket-sendto",         IPCSOC::sendto },
   { "socket-create-tcp",     IPCSOC::create_tcp },
   { "socket-create-udp",     IPCSOC::create_udp },
   { "socket-bind",           IPCSOC::bind },
   { "socket-bind-address",   IPCSOC::bind_address },
   { "socket-create-address", IPCSOC::create_address },
   { "socket-listen",         IPCSOC::listen },
   { "socket-accept",         IPCSOC::accept },
   { "socket-connect",        IPCSOC::connect },
   { "socket-disconnect",     IPCSOC::disconnect },
   { "socket-close",          IPCSOC::close },
   { "read-select",           IPCSOC::read_select }
};


void FUNTAB::initialize()
{
   regstack.push(null);

   for ( auto& fn : funtab )
   {
      const NodeKind nk = fn.kind;
      regstack.top() = MEMORY::func(fn.func, nk ? nk : n_func );
      SYMTAB::enter(fn.name, regstack.top());
   }

   regstack.pop();
}

const char* FUNTAB::funname( PRIMITIVE fun )
{
   for ( auto& fn : funtab )
   {
      if (fun == fn.func)
	 return fn.name;
   }
  
   return "<unknown-primitive>";
}

