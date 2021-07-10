#include "dict.hxx"
#include "hash.hxx"
#include "equality.hxx"
#include "regstack.hxx"
#include "memory.hxx"

namespace escheme
{
   bool has_key( SEXPR dict, SEXPR key )
   {
      auto vlen = getvectorlength(dict);
      auto data = getvectordata(dict);
      auto index = hash( key ) % vlen;
      
      for ( auto items = data[index]; anyp(items); items = getcdr(items) )
      {
	 auto item = getcar(items);
	 
	 if ( equal( getcar(item), key ) )
	    return true;
      }
      
      return false;
   }
   
   SEXPR dict_ref( SEXPR dict, SEXPR key )
   {
      auto vlen = getvectorlength(dict);
      auto data = getvectordata(dict);
      auto index = hash( key ) % vlen;
      
      for ( auto items = data[index]; anyp(items); items = getcdr(items) )
      {
	 auto item = getcar(items);
	 
	 if ( equal( getcar(item), key ) )
	    return getcdr(item);      
      }
      
      return null;
   }

   SEXPR dict_set( SEXPR dict, SEXPR key, SEXPR val )
   {
      auto vlen = getvectorlength(dict);
      auto data = getvectordata(dict);
      auto index = hash( key ) % vlen;
      
      for ( auto items = data[index]; anyp(items); items = getcdr(items) )
      {
	 auto item = getcar(items);
	 
	 if ( equal( getcar(item), key ) )
	 {
	    setcdr(item, val);
	    return val;
	 }      
      }
      
      // key not found; add a new dict entry
      regstack.push( MEMORY::cons( key, val ) );
      data[index] = MEMORY::cons( regstack.top(), data[index] );
      regstack.pop();
      
      return val;
   }

   SEXPR dict_items( SEXPR dict )
   {
      auto vlen = getvectorlength(dict);
      auto data = getvectordata(dict);
      
      regstack.push( null );
      
      for ( int i = 0; i < vlen; ++i )
	 for ( auto items = data[i]; anyp(items); items = getcdr(items) )
	    regstack.top() = MEMORY::cons( getcar(items), regstack.top() );
      
      return regstack.pop();
   }
}

