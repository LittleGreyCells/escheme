#ifndef SYMDICT_HXX
#define SYMDICT_HXX

#include <unordered_map>

#include "sexpr.hxx"

namespace escheme
{
   struct SymbolHasher { size_t operator()( const SEXPR& x ) const; };
   struct SymbolEqual { bool operator()( const SEXPR& x, const SEXPR& y ) const; };
   
   struct SymDict
   {
      SymDict();
      ~SymDict();
      bool has( SEXPR symbol );
      SEXPR get( SEXPR symbol );
      void set( SEXPR symbol, SEXPR value );
      std::unordered_map<SEXPR, SEXPR, SymbolHasher, SymbolEqual> umap;
   };
}

#endif
