// Copyright 2012 Michael E. Stillman

#include <vector>
#include <iostream>

#include "interface/random.h"
#include "relem.hpp"
#include "polyring.hpp"
#include "aring-m2-gf.hpp"
#include "ringmap.hpp"
#include "monoid.hpp"
#include "interrupted.hpp"

extern "C" void dringelem(const Ring *R, const ring_elem f);

namespace M2 {

GaloisFieldTable::GaloisFieldTable(const PolynomialRing &R,
                                   const ring_elem prim)
    : mCharac(static_cast<int>(R.characteristic())),
      mOriginalRing(R),
      mPrimitiveElement(prim)
{
  assert(mOriginalRing.n_quotients() == 1);

  mGenerator = RingElement::make_raw(&R, R.copy(prim));
  ring_elem f = mOriginalRing.quotient_element(0);
  Nterm *t = f;
  mDimension = mOriginalRing.getMonoid()->primary_degree(t->monom);
  mOrder = mCharac;
  for (int i = 1; i < mDimension; i++) mOrder *= mCharac;
  mOne = mOrder - 1;      // representation for the number 1: p^n - 1.
  mOrderMinusOne = mOne;  // p^n - 1
  mMinusOne = (mCharac == 2 ? mOne : mOne / 2);

  // Get ready to create mOneTable.
  VECTOR(ring_elem) polys;
  polys.push_back(mOriginalRing.from_long(0));
  polys.push_back(mOriginalRing.copy(mPrimitiveElement));

  ring_elem oneR = mOriginalRing.from_long(1);

  mGeneratorExponent = static_cast<GFElement>(-1);
  ring_elem x = mOriginalRing.var(0);
  if (mOriginalRing.is_equal(mPrimitiveElement, x)) mGeneratorExponent = 1;
  for (GFElement i = 2; i <= mOne; i++)
    {
      ring_elem g = mOriginalRing.mult(polys[i - 1], mPrimitiveElement);
      polys.push_back(g);
      if (mOriginalRing.is_equal(g, oneR)) break;
      if (mOriginalRing.is_equal(g, x)) mGeneratorExponent = i;
    }

#if 0
    for (size_t i = 0; i < polys.size(); i++)
      {
        std::cerr << i << "  ";
        dringelem(&R, polys[i]);
        std::cerr << "\n";
      }
#endif
  assert(polys.size() == mOrder);
  assert(mGeneratorExponent != static_cast<GFElement>(-1));

  // Set 'one_table'.
  mOneTable = newarray_atomic(GFElement, mOrder);
  mOneTable[0] = mOrderMinusOne;

  for (GFElement i = 1; i <= mOrderMinusOne; i++)
    {
      if (system_interrupted())
        {
          // first clean up?
          return;
        }
      ring_elem f1 = mOriginalRing.add(polys[i], oneR);
      GFElement j;
      for (j = 0; j <= mOrderMinusOne; j++)
        if (mOriginalRing.is_equal(f1, polys[j])) break;
      if (j > mOrderMinusOne)
        {
          std::cout << "oops: didn't find element " << i << " !!" << std::endl;
        }
      mOneTable[i] = j;
    }

  // Create the ZZ/P ---> GF(Q) inclusion map
  mFromIntTable = newarray_atomic(GFElement, mCharac);
  GFElement a = mOne;
  ;
  mFromIntTable[0] = 0;
  for (GFElement i = 1; i < mCharac; i++)
    {
      mFromIntTable[i] = a;
      a = mOneTable[a];
    }
}

void GaloisFieldTable::display(std::ostream &o) const
{
  o << "GF(" << mCharac << "^" << mDimension << ")" << std::endl;
  o << " order = " << mOrder << std::endl;
  o << " 1     = " << mOne << std::endl;
  o << " -1    = " << mMinusOne << std::endl;
  o << " fromZZ: " << std::endl << "    ";
  for (GFElement i = 0; i < mCharac; i++)
    {
      if ((i + 1) % 10 == 0) o << std::endl << "    ";
      o << mFromIntTable[i] << " ";
    }
  o << std::endl << " oneTable: " << std::endl << "    ";
  for (GFElement i = 0; i < mOrder; i++)
    {
      if ((i + 1) % 10 == 0) o << std::endl << "    ";
      o << mOneTable[i] << " ";
    }
  o << std::endl;
}

ARingGFM2::ARingGFM2(const PolynomialRing &R, const ring_elem a) : mGF(R, a)
{
  // Nothing to do here.
}

void ARingGFM2::fromSmallIntegerCoefficients(
    ElementType &result,
    const std::vector<long> &poly) const
{
  result = 0;
  ElementType a, b;
  for (long i = 0; i < poly.size(); i++)
    if (poly[i] != 0)
      {
        set_from_long(a, poly[i]);
        power(b, mGF.generatorExponent(), i);
        mult(a, a, b);
        add(result, result, a);
      }
}

bool ARingGFM2::promote(const Ring *Rf, const ring_elem f, elem &result) const
{
  if (&mGF.ring() != Rf) return false;

  std::vector<long> poly;
  RingElement F(Rf, f);
  F.getSmallIntegerCoefficients(poly);
  fromSmallIntegerCoefficients(result, poly);
  return true;
}

void ARingGFM2::lift_to_original_ring(ring_elem &result,
                                      const ElementType &f) const
{
  if (f == 0)
    result = mGF.ring().from_long(0);
  else if (f == mGF.one())
    result = mGF.ring().from_long(1);
  else
    result = mGF.ring().power(mGF.primitiveElement(), f);
}

bool ARingGFM2::lift(const Ring *Rg, const elem f, ring_elem &result) const
{
  // Rg = Z/p[x]/F(x) ---> GF(p,n)
  // promotion: need to be able to know the value of 'x'.
  // lift: need to compute (primite_element)^e

  if (&mGF.ring() != Rg) return false;

  lift_to_original_ring(result, f);
  return true;
}

void ARingGFM2::eval(const RingMap *map,
                     const elem f,
                     int first_var,
                     ring_elem &result) const
{
  result = map->get_ring()->power(map->elem(first_var), f);
}

void ARingGFM2::text_out(buffer &o) const
{
  o << "GF(" << mGF.characteristic() << "^" << mGF.dimension() << ",M2)";
}

void ARingGFM2::elem_text_out(buffer &o,
                              elem a,
                              bool p_one,
                              bool p_plus,
                              bool p_parens) const
{
  if (a == 0)
    {
      o << "0";
      return;
    }
  ring_elem h = mGF.ring().power(mGF.primitiveElement(), a);
  mGF.ring().elem_text_out(o, h, p_one, p_plus, p_parens);
  mGF.ring().remove(h);
}
}; // namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
