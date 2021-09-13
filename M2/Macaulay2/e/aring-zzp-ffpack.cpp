// Copyright 2011 Michael E. Stillman

#include "aring-zzp-ffpack.hpp"
#include "error.h"

#include "ringmap.hpp"

namespace M2 {

ARingZZpFFPACK::ARingZZpFFPACK(UTT charact)
    : mFfpackField(FieldType(static_cast<double>(charact))),
      mFfpackRandomIterator(mFfpackField),
      mCharac(charact),
      mDimension(1),
      mGeneratorComputed(false)
{
  assert(FieldType::maxCardinality() >= mCharac);
}

void ARingZZpFFPACK::elem_text_out(buffer &o,
                                   const ElementType elem,
                                   bool print_one,
                                   bool print_plus,
                                   bool print_parens) const
{
  STT a = static_cast<STT>(elem);
  if (a == 1 and not print_one) return;
  if (a > 0 and print_plus) o << "+";
  o << a;
}

/**  @todo Remove this function?  IE: rewrite it?
 //  http://www.johnkerl.org/doc/ffcomp.pdf
 */

ARingZZpFFPACK::ElementType ARingZZpFFPACK::computeGenerator() const
{
  for (UTT currIntElem = 2; currIntElem < mCharac; currIntElem++)
    {
      ElementType currElem;
      set_from_long(currElem, currIntElem);
      bool found = true;
      ElementType tmpElem = currElem;
      for (UTT count = 0; count < mCharac - 2; count++)
        {
          mult(tmpElem, tmpElem, currElem);
          if (is_equal(currElem, tmpElem)) found = false;
        }
      if (found)
        {
          std::cerr << "generator = " << currElem << std::endl;
          return currElem;
        }
    }
  assert(false);  // we should not get here, if the program logic is OK
  return ElementType(1);
}

bool ARingZZpFFPACK::is_unit(const ElementType f) const
{
  return not mFfpackField.isZero(f);
}

bool ARingZZpFFPACK::is_zero(const ElementType f) const
{
  return mFfpackField.isZero(f);
}

bool ARingZZpFFPACK::is_equal(const ElementType f, const ElementType g) const
{
  return mFfpackField.areEqual(f, g);
}

/// compare exponents of the used generator
/// @return -1: f < g, 1: f>g; 0: f==g;
int ARingZZpFFPACK::compare_elems(const ElementType f,
                                  const ElementType g) const
{
  if (f < g) return -1;
  if (f > g) return 1;

  return 0;
}

// 'init', 'init_set' functions

void ARingZZpFFPACK::init(ElementType &result) const
{
  //    assert(0 == mFfpackField.zero);
  result = 0;
}

void ARingZZpFFPACK::clear(ElementType &result) const { /* nothing */}
void ARingZZpFFPACK::set_zero(ElementType &result) const
{
  // assert(0 == mFfpackField.zero);
  result = 0;
}

void ARingZZpFFPACK::copy(ElementType &result, const ElementType a) const
{
  result = a;
}

/// @todo possible problem if type UTT is smaller than an int?
void ARingZZpFFPACK::set_from_long(ElementType &result, long a) const
{
  mFfpackField.init(result, a);
}

void ARingZZpFFPACK::set_from_mpz(ElementType &result, mpz_srcptr a) const
{
  unsigned long b = static_cast<UTT>(mpz_fdiv_ui(a, mCharac));
  mFfpackField.init(result, b);
}

bool ARingZZpFFPACK::set_from_mpq(ElementType &result, mpq_srcptr a) const
{
  ElementType n, d;
  set_from_mpz(n, mpq_numref(a));
  set_from_mpz(d, mpq_denref(a));
  if (is_zero(d)) return false;
  divide(result, n, d);
  return true;
}

// arithmetic
void ARingZZpFFPACK::negate(ElementType &result, const ElementType a) const
{
  mFfpackField.neg(result, a);
}

/// if a is zero, the result is 1 , but is that what we expect?
/// I vote for two invert functions, one with this check and one without.(Jakob)
void ARingZZpFFPACK::invert(ElementType &result, const ElementType a) const
{
  if (mFfpackField.isZero(a)) ERROR(" division by zero");
  mFfpackField.inv(result, a);
}

void ARingZZpFFPACK::unsafeInvert(ElementType &result,
                                  const ElementType a) const
{
  assert(not is_zero(a));
  mFfpackField.inv(result, a);
}

void ARingZZpFFPACK::add(ElementType &result,
                         const ElementType a,
                         const ElementType b) const
{
  mFfpackField.add(result, a, b);
}

void ARingZZpFFPACK::subtract(ElementType &result,
                              const ElementType a,
                              const ElementType b) const
{
  mFfpackField.sub(result, a, b);
}

/// @param c[in][out] c = c - a*b
void ARingZZpFFPACK::subtract_multiple(ElementType &c,
                                       const ElementType a,
                                       const ElementType b) const
{
  ElementType nega = a;
  mFfpackField.negin(nega);
  mFfpackField.axpyin(c, nega, b);
}

void ARingZZpFFPACK::mult(ElementType &result,
                          const ElementType a,
                          const ElementType b) const
{
  mFfpackField.mul(result, a, b);
}

void ARingZZpFFPACK::divide(ElementType &result,
                            const ElementType a,
                            const ElementType b) const
{
  if (mFfpackField.isZero(b)) ERROR(" division by zero");
  mFfpackField.div(result, a, b);
}

void ARingZZpFFPACK::power(ElementType &result,
                           const ElementType a,
                           STT n) const
{
  if (is_zero(a))
    {
      if (n < 0) ERROR("division by zero");
      set(result, a);
      return;
    }
  ElementType base;
  set(base, a);
  if (n < 0)
    {
      invert(base, base);
      n = -n;
    }
  n = n % (mCharac - 1);
  set_from_long(result, 1);
  if (n == 0) return;

  // Now use doubling algorithm
  assert(n > 0);
  for (;;)
    {
      if ((n % 2) != 0) mFfpackField.mulin(result, base);  // result *= base
      n >>= 1;
      if (n == 0)
        return;
      else
        mFfpackField.mulin(base, base);  // base = base^2
    }
}

/// @pre ensure that  mFfpackField.cardinality() fits in a unsigned long,
/// otherwise instead of mpz_fdiv_ui a different function has to be called)
void ARingZZpFFPACK::power_mpz(ElementType &result,
                               const ElementType a,
                               mpz_srcptr n) const
{
  if (is_zero(a) && mpz_sgn(n)<0) ERROR("division by zero");
  STT n1 = static_cast<STT>(mpz_fdiv_ui(n, mFfpackField.cardinality() - 1));
  power(result, a, n1);
}

///@note duplicate code
void ARingZZpFFPACK::swap(ElementType &a, ElementType &b) const
{
  ElementType tmp = a;
  a = b;
  b = tmp;
}

/** @brief returns x,y  s.y.  x*a + y*b == 0.
    if possible, x is set to 1.
    no need to consider the case a==0 or b==0.
*/

void ARingZZpFFPACK::syzygy(const ElementType a,
                            const ElementType b,
                            ElementType &x,
                            ElementType &y) const
{
  // x = mFfpackField.one;
  x = 1.;
  divide(y, a, b);
  negate(y, y);
}

/// @jakob document possible overflow and other nasty things
void ARingZZpFFPACK::random(ElementType &result) const
{
  mFfpackRandomIterator.random(result);
}

void ARingZZpFFPACK::eval(const RingMap *map,
                          const ElementType f,
                          int first_var,  // not used here. See ringmap.cpp
                          ring_elem &result) const
{
  // translate f to map->target()
  long a = static_cast<long>(f);
  result = map->get_ring()->from_long(a);
}
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
