// (c) 1994-2002 Michael E. Stillman

#include "monomial.hpp"
#include "engine.h"
#include "exceptions.hpp"

Monomial_pair Monomial_pair_ret;

const MonomialOrNull *rawVarMonomial(int v, int e)
{
     try {
	  return Monomial::make(v,e);
     }
     catch (exc::engine_error m) {
	  ERROR(m.what());
	  return NULL;
     }
}

const MonomialOrNull *rawMakeMonomial(M2_arrayint m)
{
     try {
	  return Monomial::make(m);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
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
  return a->compare(M,*b);	// check for exceptions here?
}

M2_bool rawMonomialDivides(const Monoid *M, const Monomial *a, const Monomial *b)
{
  return a->divides(M,*b);
}

MonomialOrNull *rawMonomialDivide(const Monoid *M, const Monomial *a, const Monomial *b)
{
     try {
	  if (a->divides(M,*b))
	    return (*a)/(*b);
	  else
	    return 0;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

int IM2_Monomial_degree(const Monomial *a)
{
     try {
	  return a->simple_degree();
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
# warning : how do we signal an error to the front end here?
	  return 0;
     }
}

const MonomialOrNull *IM2_Monomial_mult(const Monomial *a, const Monomial *b)
{
     try {
	  return (*a) * (*b);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MonomialOrNull *rawColonMonomial(const Monomial *a, const Monomial *b)
{
     try {
	  return (*a) / (*b);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MonomialOrNull *IM2_Monomial_power(const Monomial *a, int n)
{
     try {
	  return a->power(n);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const Monomial *rawLCM(const Monomial *a, const Monomial *b)
{
  return a->lcm(*b);
}

const Monomial *rawGCD(const Monomial *a, const Monomial *b)
{
  return a->gcd(*b);
}

const MonomialOrNull *rawSaturateMonomial(const Monomial *a, const Monomial *b)
{
     try {
	  return a->erase(*b);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const Monomial *rawRadicalMonomial(const Monomial *a)
{
  return a->radical();
}

const MonomialPairOrNull *rawSyzygy(const Monomial *a, const Monomial *b)
{
     try {
	  a->monsyz(*b, Monomial_pair_ret.a, Monomial_pair_ret.b);
	  return &Monomial_pair_ret;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

M2_string IM2_Monomial_to_string(const Monomial *a)
{
     buffer o;
     try {
	  a->text_out(o);
	  return o.to_string();
     }
     catch (exc::engine_error e) {
	  o << "[unprintable monomial]";
	  return o.to_string();
     }
}

M2_arrayint rawSparseListFormMonomial(const Monomial *a)
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
