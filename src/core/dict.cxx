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
}

