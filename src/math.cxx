#include <algorithm>
#include <cstdlib>

#include "math.hxx"
#include "error.hxx"
#include "argstack.hxx"
#include "memory.hxx"
#include "symtab.hxx"

enum BinOp { ADDop, SUBop, MULop, DIVop, 
             QUOop, REMop, 
             MINop, MAXop,
             ANDop, IORop, XORop };

enum RelOp { EQop, LTop, LEop, GTop, GEop };

static void badoperand() { ERROR::severe("bad operand type"); }
static void badoperator() { ERROR::severe("bad operator"); }

static void ZeroTest(FIXNUM a) { if (a == 0) ERROR::severe("division by zero"); }
static void ZeroTest(FLONUM a) { if (a == 0.0) ERROR::severe("division by zero"); }

static SEXPR binary( BinOp op )
{
   ArgstackIterator iter;
   SEXPR a = guard(iter.getarg(), numberp);

   FIXNUM x=0, y=0;
   FLONUM fx=0, fy=0;
   bool flop;

   if (fixnump(a))
   {
      x = getfixnum(a);
      flop = false;
   }
   else
   {
      fx = getflonum(a);
      flop = true;
   }

   // single operand?

   if (!iter.more())
   {
      switch (op)
      {
	 case SUBop: 
	    if (flop) 
	       fx = -fx; 
	    else 
	       x = -x; 
	    break;

	 case DIVop: 
	    if (flop)
	    {
	       ZeroTest(fx);
	       fx = 1.0 / fx;
	    }
	    else
	    {
	       ZeroTest(x);
	       fx = 1.0 / static_cast<FLONUM>(x);
	       flop = true;
	    }
	    break;

	 default:
	    break;
      }
   }

   // many operands

   while (iter.more())
   {
      a = guard(iter.getarg(), numberp);

      if (flop)
      {
	 if (fixnump(a))
	    fy = static_cast<FLONUM>(getfixnum(a));
	 else
	    fy = getflonum(a);
      }
      else
      {
	 if (fixnump(a))
	 {
	    y = getfixnum(a);
	 }
	 else
	 {
	    fx = static_cast<FLONUM>(x);
	    fy = getflonum(a);
	    flop = true;
	 }
      }

      switch (op)
      {
	 case MINop: if (flop) fx = std::min(fx, fy); else x = std::min(x, y); break;
	 case MAXop: if (flop) fx = std::max(fx, fy); else x = std::max(x, y); break;
	 case ANDop: if (flop) badoperand(); else x &= y; break;
	 case IORop: if (flop) badoperand(); else x |= y; break;
	 case XORop: if (flop) badoperand(); else x ^= y; break;
	 case ADDop: if (flop) fx += fy; else x += y; break;
	 case SUBop: if (flop) fx -= fy; else x -= y; break;
	 case MULop: if (flop) fx *= fy; else x *= y; break;
	 case QUOop: if (flop) { badoperand(); } else { ZeroTest(y); x /= y; } break;
	 case REMop: if (flop) { badoperand(); } else { ZeroTest(y); x %= y; } break;
	 case DIVop: if (flop) 
	 { 
	    ZeroTest(fy); 
	    fx /= fy; 
	 } 
	 else
	 { 
	    ZeroTest(y);
	    if (x % y == 0)
	    {
	       x /= y;
	    }
	    else
	    {
	       fx = static_cast<FLONUM>(x);
	       fy = static_cast<FLONUM>(y);
	       fx /= fy;
	       flop = true;
	    }
	 } 
	    break;
	 default:
	    badoperator();
      }
   }

   return flop ? MEMORY::flonum(fx) : MEMORY::fixnum(x);
}


static SEXPR relation( RelOp op )
{
   ArgstackIterator iter;
   SEXPR a = guard(iter.getarg(), numberp);

   FIXNUM x=0, y=0;
   FLONUM fx=0, fy=0;
   int r = 1;
   bool flop;

   if (fixnump(a))
   {
      x = getfixnum(a);
      flop = false;
   }
   else
   {
      fx = getflonum(a);
      flop = true;
   }

   while (iter.more())
   {
      a = guard(iter.getarg(), numberp);

      if (flop)
      {
	 if (fixnump(a))
	    fy = static_cast<FLONUM>(getfixnum(a));
	 else
	    fy = getflonum(a);
      }
      else
      {
	 if (fixnump(a))
	 {
	    y = getfixnum(a);
	 }
	 else
	 {
	    fx = static_cast<FLONUM>(x);
	    fy = getflonum(a);
	    flop = true;
	 }
      }

      switch (op)
      {
	 case EQop: r = flop ? (fx == fy) : (x == y); break;
	 case LTop: r = flop ? (fx <  fy) : (x <  y); break;
	 case LEop: r = flop ? (fx <= fy) : (x <= y); break;
	 case GTop: r = flop ? (fx >  fy) : (x >  y); break;
	 case GEop: r = flop ? (fx >= fy) : (x >= y); break;
	 default:
	    badoperator();
      }

      if (r == 0)
	 return symbol_false;

      if (flop)
	 fx = fy;
      else 
	 x = y;
   }

   return r ? symbol_true : symbol_false;
}



SEXPR MATH::add() 
{ 
   ArgstackIterator iter;
   return (!iter.more()) ? MEMORY::fixnum(0) : binary(ADDop);
}

SEXPR MATH::mul()
{ 
   ArgstackIterator iter;
   return (!iter.more()) ? MEMORY::fixnum(1) : binary(MULop);
}

SEXPR MATH::sub() { return binary(SUBop); }
SEXPR MATH::div() { return binary(DIVop); }

SEXPR MATH::min() { return binary(MINop); }
SEXPR MATH::max() { return binary(MAXop); }

SEXPR MATH::logand() { return binary(ANDop); }
SEXPR MATH::logior() { return binary(IORop); }
SEXPR MATH::logxor() { return binary(XORop); }

SEXPR MATH::quotient() { return binary(QUOop); }
SEXPR MATH::remainder() { return binary(REMop); }

SEXPR MATH::eq() { return relation(EQop); }
SEXPR MATH::lt() { return relation(LTop); }
SEXPR MATH::le() { return relation(LEop); }
SEXPR MATH::gt() { return relation(GTop); }
SEXPR MATH::ge() { return relation(GEop); }

SEXPR MATH::truncate()
{
   ArgstackIterator iter;
   SEXPR a = guard(iter.getlast(), numberp);
   return fixnump(a) ? a : MEMORY::fixnum(static_cast<FIXNUM>(getflonum(a)));
}

SEXPR MATH::floor()
{
   ArgstackIterator iter;
   SEXPR a = guard(iter.getlast(), numberp);
   if (fixnump(a))
   {
      return a;
   }
   else
   {
      const FLONUM n = getflonum(a);
      const FIXNUM fixn = static_cast<FIXNUM>(n);
      return MEMORY::fixnum((n > 0.0) ? fixn : fixn - 1);
   }
}

SEXPR MATH::ceiling()
{
   ArgstackIterator iter;
   SEXPR a = guard(iter.getlast(), numberp);
   if (fixnump(a))
   {
      return a;
   }
   else
   {
      const FLONUM n = getflonum(a);
      const FIXNUM fixn = static_cast<FIXNUM>(n);
      return MEMORY::fixnum((n > 0.0) ? fixn + 1 : fixn);
   }
}

SEXPR MATH::round()
{
   ArgstackIterator iter;
   SEXPR a = guard(iter.getlast(), numberp);
   if (fixnump(a))
      return a;
   else
   {
      const FLONUM n = getflonum(a);
      return MEMORY::fixnum(static_cast<FIXNUM>(n + ((n < 0.0) ? -0.5 : +0.5)));
   }
}

SEXPR MATH::inc()
{
   ArgstackIterator iter;
   SEXPR a = guard(iter.getlast(), numberp);
   if (fixnump(a))
      return MEMORY::fixnum(getfixnum(a) + 1);
   else
      return MEMORY::flonum(getflonum(a) + 1.0);
}

SEXPR MATH::dec()
{
   ArgstackIterator iter;
   SEXPR a = guard(iter.getlast(), numberp);
   if (fixnump(a))
      return MEMORY::fixnum(getfixnum(a) - 1);
   else
      return MEMORY::flonum(getflonum(a) - 1.0);
}

SEXPR MATH::abs()
{
   ArgstackIterator iter;
   SEXPR a = guard(iter.getlast(), numberp);
   if (fixnump(a))
      return MEMORY::fixnum(std::abs(getfixnum(a)));
   else
      return MEMORY::flonum(std::abs(getflonum(a)));
}

SEXPR MATH::gcd()
{
   ArgstackIterator iter;

   if (!iter.more())
      return MEMORY::fixnum(0);

   FIXNUM n = std::abs(getfixnum(guard(iter.getarg(), fixnump)));

   while (iter.more()) 
   {
      FIXNUM m = std::abs(getfixnum(guard(iter.getarg(), fixnump)));

      while (true) 
      {                      /* euclid's algorithm */
	 FIXNUM r = m % n;
	 if (r == 0)
	    break;
	 m = n;
	 n = r;
      }
   }
   return MEMORY::fixnum(n);
}

static FIXNUM rseed = 1L;

static FIXNUM rand( FIXNUM n )
{
   // make sure pseudo sequence does not get stuck at zero
   if (rseed == 0L) 
      rseed = 1L;
 
   // algorithm taken from Dr. Dobbs Journal, November 1985, page 91
   const FIXNUM k1 = rseed / 127773L;
   if ((rseed = 16807L * (rseed - k1 * 127773L) - k1 * 2836L) < 0L)
      rseed += 2147483647L;
 
   // return a random number between 0 and n-1
   return static_cast<FIXNUM>(rseed % n);
}

SEXPR MATH::random()
{
   ArgstackIterator iter;
   return MEMORY::fixnum(rand(getfixnum(guard(iter.getlast(), fixnump))));
}


SEXPR MATH::lognot()
{
   ArgstackIterator iter;
   const FIXNUM a = getfixnum(guard(iter.getlast(), fixnump));
   return MEMORY::fixnum(static_cast<FIXNUM>(~static_cast<UFIXNUM>(a)));
}

SEXPR MATH::rsh()
{
   ArgstackIterator iter;
   const FIXNUM a = getfixnum(guard(iter.getarg(), fixnump));
   const FIXNUM b = getfixnum(guard(iter.getlast(), fixnump));
   return MEMORY::fixnum(static_cast<FIXNUM>(static_cast<UFIXNUM>(a) >> b));
}

SEXPR MATH::lsh()
{
   ArgstackIterator iter;
   const FIXNUM a = getfixnum(guard(iter.getarg(), fixnump));
   const FIXNUM b = getfixnum(guard(iter.getlast(), fixnump));
   return MEMORY::fixnum(static_cast<FIXNUM>(static_cast<UFIXNUM>(a) << b));
}

SEXPR MATH::ars()
{
   ArgstackIterator iter;
   const FIXNUM a = getfixnum(guard(iter.getarg(), fixnump));
   const FIXNUM b = getfixnum(guard(iter.getlast(), fixnump));
   return MEMORY::fixnum(a >> b);
}


