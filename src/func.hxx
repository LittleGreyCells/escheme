#ifndef func_hxx
#define func_hxx

#include "sexpr.hxx"

#define PRED_DCL(name) SEXPR es##name()
#define PRED_FUN(name) FUNC::es##name

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

   SEXPR caar();
   SEXPR cadr();
   SEXPR cdar();
   SEXPR cddr();

   SEXPR caaar();
   SEXPR caadr();
   SEXPR cadar();
   SEXPR caddr();
   SEXPR cdaar();
   SEXPR cdadr();
   SEXPR cddar();
   SEXPR cdddr();

   SEXPR caaaar();
   SEXPR caaadr();
   SEXPR caadar();
   SEXPR caaddr();
   SEXPR cadaar();
   SEXPR cadadr();
   SEXPR caddar();
   SEXPR cadddr();
   SEXPR cdaaar();
   SEXPR cdaadr();
   SEXPR cdadar();
   SEXPR cdaddr();
   SEXPR cddaar();
   SEXPR cddadr();
   SEXPR cdddar();
   SEXPR cddddr();

   // vector
   SEXPR vector();
   SEXPR make_vector();
   SEXPR vector_ref();
   SEXPR vector_set();
   SEXPR vector_length();
   SEXPR vector_fill();
   SEXPR vector_copy();

   // other vector
   SEXPR find_index();

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

   // environment
   SEXPR the_environment();
   SEXPR proc_environment();
   SEXPR env_bindings();
   SEXPR env_parent();
   SEXPR make_environment();

   // closure
   SEXPR make_closure();
   SEXPR parse_formals();

   // unix
   SEXPR unix_system();
   SEXPR unix_getargs();
   SEXPR unix_gettime();

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

   // syntax
   SEXPR transform_letstar();

   // predicates
   PRED_DCL(nullp);
   PRED_DCL(symbolp);
   PRED_DCL(fixnump);
   PRED_DCL(flonump);
   PRED_DCL(numberp);
   PRED_DCL(charp);
   PRED_DCL(stringp);
   PRED_DCL(vectorp);
   PRED_DCL(bvecp);
   PRED_DCL(consp);
   PRED_DCL(funcp);
   PRED_DCL(portp);
   PRED_DCL(inportp);
   PRED_DCL(outportp);
   PRED_DCL(stringportp);
   PRED_DCL(instringportp);
   PRED_DCL(outstringportp);
   PRED_DCL(closurep);
   PRED_DCL(contp);
   PRED_DCL(envp);
   PRED_DCL(listp);
   PRED_DCL(atomp);
   PRED_DCL(promisep);

   PRED_DCL(boundp);
   PRED_DCL(notp);
   PRED_DCL(eof_objectp);
   PRED_DCL(default_objectp);
   PRED_DCL(booleanp);

   PRED_DCL(zerop);
   PRED_DCL(positivep);
   PRED_DCL(negativep);
   PRED_DCL(oddp);
   PRED_DCL(evenp);
   PRED_DCL(exactp);
   PRED_DCL(inexactp);

   SEXPR procedurep();

   PRED_DCL(string_nullp);

   SEXPR string_length();
   SEXPR string_append();
   SEXPR string_ref();
   SEXPR string_set();
   SEXPR substring();
   SEXPR string_fill();
   SEXPR string_copy();

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

   // reference
   SEXPR gref();
   SEXPR fref();

   // closure
   SEXPR closure_code();
   SEXPR closure_benv();
   SEXPR closure_vars();
   SEXPR closure_numv();
   SEXPR closure_rest();

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

   // unistd
   SEXPR change_dir();
   SEXPR current_dir();
}

#endif

