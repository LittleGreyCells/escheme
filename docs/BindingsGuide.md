escheme "bindings" -- A Deveoper's Guide
========================================

What follows are guidelines for extending escheme with ones own primitive
functions written in C++.

## 1 General Strategy

The general strategy for this non-trivial exercise consists of several 
coordinated tasks:

  * Writing the extension function
  * Accessing the function arguments
  * Constructing an escheme object, if necessary
  * Returning the object or null
  * Extending the function table with extension function name and address.

Let's tackle each of these in turn.

### 1.1 Writing the extension function

Extension functions have a single signature:
```
  (#include "sexpr.hxx")
  SEXPR funcion_name();
```

It's a function which takes no arguments and returns an object of type SEXPR.

### 1.2 Accessing function arguments

Extension function arguments are passed to the function using the ARGSTACK.
```
  (#include "argstack.hxx")
  ARGSTACK argstack;
```

The arguments to user defined functions are escheme objects, not C++ native 
types. The embedded values must be accessed indirectly through access functions.
But let's not get ahead of ourselves. Argument values will be addressed later.

#### 1.2.1 Determine whether the function should have arguments

If the function should not have arguments, assert the following;
```
  argstack.noargs();
```

If arguments have been pushed on the stack, an error will be raised.
If there are arguments, continue to 2b.

#### 1.2.2 Create an ArgstackIterator
```
  ArgstackIterator iter;
```

Use the iterator to visit the argument objects and assert the type and number
of the arguments.

If the number of arguments are known, say N of them, the following approach 
can be used:
```
  SEXPR arg1 = iter.getarg();
  SEXPR arg2 = iter.getarg();
  ...
  SEXPR argN = iter.getlast();
```

If getarg() fails, it will generate "too few arguments" error.
If getlast() fails, it will genenate either "too few..." if one was expected,
or "too many..." if the argument fetched was not the last.

If the arguments are variable in number, the following approach can be used:
```
  while iter.more()
  {
    SEXPR arg = getarg();
    // process arg
    ...
  }
```

#### 1.2.3 Argument types

If the argument is expected to be a certain type, then the "guard" function
can be used:
```
  SEXPR arg = guard( iter.getarg(), <type-predicate> )
```

There are many predefined type guards:
```
  bool symbolp( const SEXPR n );
  bool fixnump( const SEXPR n );
  bool flonump( const SEXPR n );
  bool numberp( const SEXPR n );
  bool booleanp( const SEXPR n );
  bool stringp( const SEXPR n );
  bool charp( const SEXPR n );
  bool vectorp( const SEXPR n );
  bool consp( const SEXPR n );
  bool funcp( const SEXPR n );
  bool specialp( const SEXPR n );
  bool portp( const SEXPR n );
  bool stringportp( const SEXPR n );
  bool closurep( const SEXPR n );
  bool contp( const SEXPR n );
  bool envp( const SEXPR n );
  bool bvecp( const SEXPR n );
  bool listp( const SEXPR n );
  bool atomp( const SEXPR n );
  bool inportp( const SEXPR n );
  bool outportp( const SEXPR n );
  bool instringportp( const SEXPR n );
  bool outstringportp( const SEXPR n );
  bool anyinportp( const SEXPR n );
  bool anyoutportp( const SEXPR n );
  bool promisep( const SEXPR n );
```

#### 1.2.4 Argument values

To extract the value and store it in the correctly typed C++ object, use one
of the accessor functions designed to penetrate the escheme object.
```
  SEXPR car = getcar( guard( iter.getarg(), consp ) );
  SEXPR cdr = getcdr( guard( iter.getarg(), consp ) );
  SEXPR item = vectorref( guard( iter.getarg(), vectorp ), <index> );
  uchar byte = bvecref( guard( iter.getarg(), bvecp ), <index> );
  char ch = getcharacter( guard( iter.getarg(), charp ) );
  SEXPR val = getvalue( guard( iter.getarg(), symbolp ) );
  FIXNUM num = getfixnum( guard( iter.getarg(), fixnump ) );
  FLONUM num = getflonum( guard( iter.getarg(), flonump ) );
  etc.
```

### 1.3 Constructing an escheme object

It is imperative that an object, which is constructed and intended to be
returned by the function, be protected from garbage collection.
This can be achieved by pushing an intermidate value, if needed, onto the 
register stack and being careful to pop that value thus restoring the stack
before leaving the function.

Example: We write a function that returns the pair (1 . 2).
```
  #incude "regstack.hxx"

  SEXPR make_pair()
  {
    // create intermediate values
    SEXPR num1 = MEMORY::fixnum(1);	

    // protect from gc
    regstack.push( num1 );

    // num2 allocation might initiate gc
    SEXPR num2 = MEMORY::fixnum(2);

    // protect from gc
    regstack.push( num2 );

    // this cons allocation might initiate gc
    SEXPR pair = MEMORY::cons(num1, num2);

    // restore the regstack
    regstack.pop();
    regstack.pop();

    return pair;
  }
```

Alternatively one might contruct the pair as follows.
```
  SEXPR make_pair()
  {
    // protect the cons cell
    regstack.push( MEMORY::cons(nil, nil) );

    setcar( regstack.top(), MEMORY::fixnum(1) );
    setcdr( regstack.top(), MEMORY::fixnum(2) );

    // restore the regstack and return the object
    return regstack.pop();
  }
```

### 1.4 Returning the object or null

See the previous example in 3.

If the function doesn't return a meaningful value, it must return something.
Return the object nil.

### 1.5. Extending the function table


See file funtab.cxx and the function table funtab.
```
  extern SEXPR make_pair();

  static std::vector<SEXPR (*)()> funtab =
  {
    ...
    { "make-pair", make_pair, n_func },
    ...
  };
```

## 2 Advanced Example

The set of socket bindings for escheme is an advanced example of writing a 
non-trivial extension to escheme. See

* src/core/ipcsoc.hxx
* src/core/ipcsoc.cxx

