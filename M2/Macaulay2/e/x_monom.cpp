// (c) 1994-2002 Michael E. Stillman

#include "monomial.hpp"
#include "engine.h"

Monomial_pair Monomial_pair_ret;

const MonomialOrNull *IM2_Monomial_var(int v, int e)
{
  return Monomial::make(v,e);
}

const MonomialOrNull *IM2_Monomial_make(const M2_arrayint m)
{
  return Monomial::make(m);
}

M2_bool IM2_Monomial_is_equal(const Monomial *a, const Monomial *b)
{
  return a->is_equal(*b);
}

M2_bool IM2_Monomial_is_one(const Monomial *a)
{
  return a->is_one();
}

int IM2_Monomial_compare(const Monomial *a, const Monomial *b)
{
  return a->compare(*b);
}

M2_bool IM2_Monomial_divides(const Monomial *a, const Monomial *b)
{
  return a->divides(*b);
}

int IM2_Monomial_degree(const Monomial *a)
{
  return a->simple_degree();
}

const Monomial *IM2_Monomial_mult(const Monomial *a, const Monomial *b)
{
  return (*a) * (*b);
}

const Monomial *IM2_Monomial_quotient(const Monomial *a, const Monomial *b)
{
  return (*a) / (*b);
}

const Monomial *IM2_Monomial_power(const Monomial *a, int n)
{
  return a->power(n);
}

const Monomial *IM2_Monomial_lcm(const Monomial *a, const Monomial *b)
{
  return a->lcm(*b);
}

const Monomial *IM2_Monomial_gcd(const Monomial *a, const Monomial *b)
{
  return a->gcd(*b);
}

const Monomial *IM2_Monomial_sat(const Monomial *a, const Monomial *b)
{
  return a->erase(*b);
}

const Monomial *IM2_Monomial_radical(const Monomial *a)
{
  return a->radical();
}

const Monomial_pair *IM2_Monomial_syz(const Monomial *a, const Monomial *b)
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

const M2_arrayint IM2_Monomial_to_arrayint(const Monomial *a)
{
  return a->to_arrayint();
}

unsigned long IM2_Monomial_hash(const Monomial *a)
{
  return a->get_hash_value();
}
