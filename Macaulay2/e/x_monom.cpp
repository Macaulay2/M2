// (c) 1994-2002 Michael E. Stillman

#include "monomial.hpp"
#include "engine.h"

Monomial_pair Monomial_pair_ret;

const MonomialOrNull *rawVarMonomial(int v, int e)
{
  return Monomial::make(v,e);
}

const MonomialOrNull *rawMakeMonomial(const M2_arrayint m)
{
  return Monomial::make(m);
}

M2_bool IM2_Monomial_is_equal(const Monomial *a, const Monomial *b)
{
  return a->is_equal(*b);
}

M2_bool rawMonomialIsOne(const Monomial *a)
{
  return a->is_one();
}

int rawCompareMonomial(const Monoid *M, const Monomial *a, const Monomial *b)
{
  return a->compare(M,*b);
}

M2_bool IM2_Monomial_divides(const Monoid *M, const Monomial *a, const Monomial *b)
{
  return a->divides(M,*b);
}

MonomialOrNull *IM2_Monomial_divide(const Monoid *M, const Monomial *a, const Monomial *b)
{
  if (a->divides(M,*b))
    return (*a)/(*b);
  else
    return 0;
}

int IM2_Monomial_degree(const Monomial *a)
{
  return a->simple_degree();
}

const Monomial *IM2_Monomial_mult(const Monomial *a, const Monomial *b)
{
  return (*a) * (*b);
}

const Monomial *rawColonMonomial(const Monomial *a, const Monomial *b)
{
  return (*a) / (*b);
}

const Monomial *IM2_Monomial_power(const Monomial *a, int n)
{
  return a->power(n);
}

const Monomial *rawLCM(const Monomial *a, const Monomial *b)
{
  return a->lcm(*b);
}

const Monomial *rawGCD(const Monomial *a, const Monomial *b)
{
  return a->gcd(*b);
}

const Monomial *rawSaturateMonomial(const Monomial *a, const Monomial *b)
{
  return a->erase(*b);
}

const Monomial *rawRadicalMonomial(const Monomial *a)
{
  return a->radical();
}

const Monomial_pair *rawSyzygy(const Monomial *a, const Monomial *b)
{
  a->monsyz(*b, Monomial_pair_ret.a, Monomial_pair_ret.b);
  return &Monomial_pair_ret;
}

const M2_string IM2_Monomial_to_string(const Monomial *a)
{
  buffer o;
  a->text_out(o);
  return o.to_string();
}

const M2_arrayint rawSparseListFormMonomial(const Monomial *a)
{
  return a->to_arrayint();
}

unsigned long IM2_Monomial_hash(const Monomial *a)
{
  return a->get_hash_value();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
