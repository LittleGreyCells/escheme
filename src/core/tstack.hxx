#ifndef stack_hxx
#define stack_hxx

namespace escheme
{

template <typename T>
class TSTACK
{
public:
   explicit TSTACK<T>( int stacksize )
   {
      data = new T[size = stacksize];
      #ifdef MAX_DEPTH
      max_depth = 0;
      #endif
      flush();
   }
  
   void push( T s )
   { 
      if ( fullp() )
	 overflow();
      #ifdef MAX_DEPTH
      ++top_index;
      if ( top_index > max_depth )
	 max_depth = top_index;
      data[top_index] = s;
      #else
      data[++top_index] = s; 
      #endif
   }
   
   T pop()
   { 
      if ( emptyp() )
	 underflow();
      return data[top_index--]; 
   }

   T& top()
   { 
      if ( emptyp() )
	 underflow();
      return data[top_index];
   }
   
   T upop() { return data[top_index--]; }
   
   int emptyp() { return top_index == -1; }
   int fullp()  { return top_index == size-1; }
  
   int roomp( int amount ) { return top_index+amount < size; }

   void flush() { top_index = -1; }

   int gettop() { return top_index; }
   void settop( int top ) { top_index = top; }
   T& operator[]( int i ) { return data[i]; }

   int getdepth() { return top_index+1; }
   void newtop( int depth ) { top_index = depth-1; }

   virtual void underflow() = 0;
   virtual void overflow() = 0;

   T* data;
   int size;
   int top_index;
   #ifdef MAX_DEPTH
   int max_depth;
   #endif
};

}

#endif

