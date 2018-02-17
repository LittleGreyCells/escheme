#ifndef stack_hxx
#define stack_hxx

//
// Stack Template
//

template <typename T>
class TSTACK
{
public:
   TSTACK<T>( int stacksize );
  
   void push( T s )
   { 
      if (fullp())
	 overflow();
      data[++top_index] = s; 
   }
   
   T pop()
   { 
      if (emptyp())
	 underflow();
      return data[top_index--]; 
   }
   
   T& top()
   { 
      if (emptyp())
	 underflow();
      return data[top_index];
   }
   
   int emptyp() { return top_index == -1; }
   int fullp()  { return top_index == size-1; }
  
   int roomp( int amount ) { return top_index+amount < size; }

   void flush() { top_index = -1; }

   int gettop() { return top_index; }
   void settop( int top ) { top_index = top; }
   T& operator[]( int i ) { return data[i]; }

   int getdepth() { return top_index+1; }
   void newtop( int depth ) { top_index = depth-1; }

   virtual void underflow();
   virtual void overflow();

   T* data;
   int size;
   int top_index;
};

#endif

