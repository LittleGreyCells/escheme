#include "symdict.hxx"
#include "error.hxx"

namespace escheme
{ 
   size_t SymbolHasher::operator()( const SEXPR& x ) const
   {
      return reinterpret_cast<size_t>( x );
   }
   
   bool SymbolEqual::operator()( const SEXPR& x, const SEXPR& y ) const
   {
      return x == y;
   }  
   
   SymDict::SymDict() {}
   SymDict::~SymDict() {}
   
   bool SymDict::has( SEXPR symbol )
   {
      if ( !symbolp(symbol) )
	 ERROR::severe( "SymDict::has -- not a symbol", symbol );
      
      return umap.find(symbol) != umap.end();
   }
   
   SEXPR SymDict::get( SEXPR symbol )
   {
      if ( !symbolp(symbol) )
	 ERROR::severe( "SymDict::get -- not a symbol", symbol );
      
      auto it = umap.find( symbol );
      
      return ( it == umap.end() ) ? null : it->second;
   }
   
   void SymDict::set( SEXPR symbol, SEXPR value )
   {
      if ( !symbolp(symbol) )
	 ERROR::severe( "SymDict::set -- not a symbol", symbol );
      
      umap[symbol] = value;
   }
}
