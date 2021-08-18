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
   
   bool has_key( SEXPR dict, SEXPR key, SEXPR& binding )
   {
      auto vlen = getvectorlength(dict);
      auto data = getvectordata(dict);
      auto index = hash( key ) % vlen;
      
      for ( auto items = data[index]; anyp(items); items = getcdr(items) )
      {
	 auto item = getcar(items);
	 
	 if ( equal( getcar(item), key ) )
	 {
	    binding = item;
	    return true;
	 }
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
   
   SEXPR dict_rem( SEXPR dict, SEXPR key )
   {
      auto vlen = getvectorlength(dict);
      auto data = getvectordata(dict);
      auto index = hash( key ) % vlen;
      
      auto items = data[index];
      auto prev = null;

      while ( anyp(items) )
      {
	 auto item = getcar(items);
	 
	 if ( equal( getcar(item), key ) )
	 {
	    if ( nullp(prev) )
	       data[index] = getcdr(items);
	    else
	       setcdr( prev, getcdr(items) );
	    
	    return key;
	 }
	 
	 prev = items;
	 items = getcdr(items);
      }
      
      return null;
   }
   
   SEXPR dict_empty( SEXPR dict )
   {
      auto vlen = getvectorlength(dict);
      auto data = getvectordata(dict);

      for ( int i = 0; i < vlen; ++i )
	 data[i] = null;
      
      return dict;
   }
}

