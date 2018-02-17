#include "tstack.hxx"
#include "memory.hxx"
#include "error.hxx"

template <typename T>
TSTACK<T>::TSTACK( int stacksize )
{
   data = new T[size = stacksize];
   flush();
}

template <typename T>
void TSTACK<T>::overflow()
{
   ERROR::severe("stack overflow");
}

template <typename T>
void TSTACK<T>::underflow()
{
   ERROR::severe("stack underflow"); 
}

//
// template instantiation(s)
//

template class TSTACK<int>;
template class TSTACK<SEXPR>;
