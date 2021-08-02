#ifndef func_hxx
#define func_hxx

#include "sexpr.hxx"

namespace escheme
{

bool booleanp( const SEXPR n );
bool notp( const SEXPR n );
bool boundp( const SEXPR n );
bool eof_objectp( const SEXPR n );
bool zerop( const SEXPR n );
bool positivep( const SEXPR n );
bool negativep( const SEXPR n );
bool oddp( const SEXPR n );
bool evenp( const SEXPR n );
bool exactp( const SEXPR );
bool inexactp( const SEXPR );
bool string_nullp( SEXPR n );
bool procedurep( const SEXPR );

namespace FUNC
{
   // general
   SEXPR exit();

   // list
   SEXPR cons();
   SEXPR car();
   SEXPR cdr();
   SEXPR length();
   SEXPR list();
   SEXPR liststar();
   SEXPR set_car();
   SEXPR set_cdr();

   // vector
   SEXPR vector();
   SEXPR make_vector();
   SEXPR vector_ref();
   SEXPR vector_set();
   SEXPR vector_length();
   SEXPR vector_fill();
   SEXPR vector_copy();

   // list/vector conversion
   SEXPR vector_to_list();
   SEXPR list_to_vector();

   // byte vector
   SEXPR bvector();
   SEXPR make_bvector();
   SEXPR bvector_ref();
   SEXPR bvector_set();
   SEXPR bvector_length();

   // equality
   SEXPR eq();
   SEXPR eqv();
   SEXPR equal();

   // symbol
   SEXPR string_to_symbol();
   SEXPR symbol_to_string();
   SEXPR gensym();
   SEXPR symbol_value();
   SEXPR set_symbol_value();
   SEXPR symbol_plist();
   SEXPR set_symbol_plist();
   SEXPR get_property();
   SEXPR put_property();
   SEXPR rem_property();
   SEXPR all_symbols();

   // other conversion
   SEXPR integer_to_string();
   SEXPR string_to_integer();
   SEXPR list_to_string();
   SEXPR string_to_list();

   // input/output
   SEXPR read();
   SEXPR print();
   SEXPR newline();
   SEXPR display();
   SEXPR read_char();
   SEXPR write_char();
   SEXPR write();

   // gc
   SEXPR gc();
   SEXPR mm();
   SEXPR fs();

   // environment
   SEXPR the_environment();
   SEXPR the_global_environment();
   SEXPR proc_environment();
   SEXPR env_bindings();
   SEXPR env_parent();
   SEXPR make_environment();

#ifdef BYTE_CODE_EVALUATOR
   // code
   SEXPR make_code();
   SEXPR get_bcodes();
   SEXPR get_sexprs();
#endif

   // unix
   SEXPR unix_system();
   SEXPR unix_getargs();
   SEXPR unix_getenv();
   SEXPR unix_setenv();
   SEXPR unix_unsetenv();
   SEXPR unix_gettime();
   SEXPR unix_change_dir();
   SEXPR unix_current_dir();

   // port
   SEXPR open_input_file();
   SEXPR open_output_file();
   SEXPR open_append_file();
   SEXPR open_update_file();
   SEXPR get_file_position();
   SEXPR set_file_position();
   SEXPR close_port();
   SEXPR close_input_port();
   SEXPR close_output_port();
   SEXPR flush_output_port();

   // string port
   SEXPR open_input_string();
   SEXPR open_output_string();
   SEXPR get_output_string();

   // predicates
   SEXPR procedurep();

   SEXPR string_make();
   SEXPR string_length();
   SEXPR string_append();
   SEXPR string_ref();
   SEXPR string_set();
   SEXPR string_substring();
   SEXPR string_fill();
   SEXPR string_copy();
   SEXPR string_dup();
   SEXPR string_find();
   SEXPR string_trim();
   SEXPR string_trim_left();
   SEXPR string_trim_right();
   SEXPR string_downcase();
   SEXPR string_upcase();
   SEXPR string_pad_left();
   SEXPR string_pad_right();


   SEXPR string_EQ();
   SEXPR string_LT();
   SEXPR string_LE();
   SEXPR string_GT();
   SEXPR string_GE();
   SEXPR string_EQci();
   SEXPR string_LTci();
   SEXPR string_LEci();
   SEXPR string_GTci();
   SEXPR string_GEci();

   SEXPR char_EQ();
   SEXPR char_LT();
   SEXPR char_LE();
   SEXPR char_GT();
   SEXPR char_GE();
   SEXPR char_EQci();
   SEXPR char_LTci();
   SEXPR char_LEci();
   SEXPR char_GTci();
   SEXPR char_GEci();

   SEXPR char_alphabeticp();
   SEXPR char_numericp();
   SEXPR char_whitespacep();
   SEXPR char_upper_casep();
   SEXPR char_lower_casep();
   SEXPR char_to_integer();
   SEXPR integer_to_char();
   SEXPR char_upcase();
   SEXPR char_downcase();

   // list membership
   SEXPR member();
   SEXPR memv();
   SEXPR memq();
   SEXPR assoc();
   SEXPR assv();
   SEXPR assq();

   // other list
   SEXPR append();
   SEXPR reverse();
   SEXPR last_pair();
   SEXPR list_tail();

   // closure
   SEXPR closure_code();
   SEXPR closure_benv();
   SEXPR closure_vars();
   SEXPR closure_numv();
   SEXPR closure_rest();
#ifdef BYTE_CODE_EVALUATOR
   SEXPR closure_code_set();
#endif

   // dict
   SEXPR make_dict();
   SEXPR has_key();
   SEXPR dict_ref();
   SEXPR dict_set();
   SEXPR dict_items();

   // module
   SEXPR make_module();
   SEXPR module_dict();

   // transcript
   SEXPR transcript_on();
   SEXPR transcript_off();

   // terminal
   SEXPR history_add();
   SEXPR history_clear();
   SEXPR history_show();
   SEXPR set_prompt();

   // object address
   SEXPR objaddr();

   // lambda helpers
   SEXPR predicate( PREDICATE p );
   SEXPR cxr( const char* s );
}

}

#endif

