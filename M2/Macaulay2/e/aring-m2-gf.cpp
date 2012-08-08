// Copyright 2012 Michael E. Stillman

#include <vector>
#include <iostream>

#include "relem.hpp"
#include "polyring.hpp"
#include "aring-m2-gf.hpp"
#include "ringmap.hpp"

#include "../system/supervisorinterface.h"
#define SYSTEM_INTERRUPTED test_Field(THREADLOCAL(interrupts_interruptedFlag,struct atomic_field))

extern "C" void dringelem(const Ring *R, const ring_elem f);

namespace M2 {

  GaloisFieldTable::GaloisFieldTable(const PolynomialRing& R,
                                     const ring_elem prim):
    mCharac(R.charac()),
    mOriginalRing(R),
    mPrimitiveElement(prim)
  {
    ASSERT(mOriginalRing.n_quotients() == 1);

    ring_elem f = mOriginalRing.quotient_element(0);
    Nterm *t = f;
    mDimension = mOriginalRing.getMonoid()->primary_degree(t->monom);
    mOrder = mCharac;
    for (int i=1; i<mDimension; i++) mOrder *= mCharac;
    mOne = mOrder - 1; // representation for the number 1: p^n - 1.
    mOrderMinusOne = mOne; // p^n - 1
    mMinusOne = (mCharac == 2 ? mOne : mOne/2);
    
    // Get ready to create mOneTable.
    std::vector<ring_elem> polys;
    polys.push_back(mOriginalRing.from_int(0));
    polys.push_back(mOriginalRing.copy(mPrimitiveElement));
    
    ring_elem oneR = mOriginalRing.from_int(1);
    
    mGeneratorExponent = static_cast<size_t>(-1);
    ring_elem x = mOriginalRing.var(0);
    if (mOriginalRing.is_equal(mPrimitiveElement, x))
      mGeneratorExponent = 1;
    for (size_t i=2; i<=mOne; i++)
      {
	ring_elem g = mOriginalRing.mult(polys[i-1], mPrimitiveElement);
	polys.push_back(g);
	if (mOriginalRing.is_equal(g, oneR)) break;
	if (mOriginalRing.is_equal(g, x))
	  mGeneratorExponent = i;
      }

#if 0
    for (size_t i = 0; i < polys.size(); i++)
      {
        std::cerr << i << "  ";
        dringelem(&R, polys[i]);
        std::cerr << "\n";
      }
#endif
    ASSERT(polys.size() == mOrder);
    ASSERT(mGeneratorExponent != static_cast<size_t>(-1));
    
    // Set 'one_table'.
    mOneTable = newarray_atomic(size_t,mOrder);
    mOneTable[0] = mOrderMinusOne;

    for (size_t i=1; i<=mOrderMinusOne; i++)
      {
        if (SYSTEM_INTERRUPTED) 
          {
            // first clean up?
            return;
          }
	ring_elem f1 = mOriginalRing.add(polys[i], oneR);
        size_t j;
	for (j=0; j<=mOrderMinusOne; j++)
          if (mOriginalRing.is_equal(f1, polys[j]))
            break;
        if (j > mOrderMinusOne)
          {
            std::cout << "oops: didn't find element " << i << " !!" << std::endl;
          }
        mOneTable[i] = j;
      }
    
    // Create the ZZ/P ---> GF(Q) inclusion map
    mFromIntTable = newarray_atomic(size_t,mCharac);
    size_t a = mOne;;
    mFromIntTable[0] = 0;
    for (size_t i=1; i<mCharac; i++)
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
    for (size_t i = 0; i<mCharac; i++)
      {
        if ((i+1) % 10 == 0) o << std::endl << "    ";
        o << mFromIntTable[i] << " ";
      }
    o << std::endl << " oneTable: " << std::endl << "    ";
    for (size_t i = 0; i<mOrder; i++)
      {
        if ((i+1) % 10 == 0) o << std::endl << "    ";
        o << mOneTable[i] << " ";
      }
    o << std::endl;
  }

  ARingGFM2::ARingGFM2(const PolynomialRing &R,
                         const ring_elem a):
    mGF(R,a)
  {
  }

  bool ARingGFM2::promote(const Ring *Rf, const ring_elem f, elem &result) const
  {
    if (&mGF.ring() != Rf) return false;

    result = 0;
    int exp[1];
    for (Nterm *t = f; t != NULL; t = t->next)
      {
        elem a, b;
        set_from_int(a, mGF.ring().getCoefficientRing()->coerce_to_int(t->coeff));
        mGF.ring().getMonoid()->to_expvector(t->monom, exp);
        // exp[0] is the variable we want.  Notice that since the ring is a quotient,
        // this degree is < n (where Q_ = P^n).
        power(b, mGF.generatorExponent(), exp[0]);
        mult(a, a, b);
        add(result, result, a);
      }
    return true;
    
  }

  bool ARingGFM2::lift(const Ring *Rg, const elem f, ring_elem &result) const
  {
    // Rg = Z/p[x]/F(x) ---> GF(p,n)
    // promotion: need to be able to know the value of 'x'.
    // lift: need to compute (primite_element)^e

    if (&mGF.ring() != Rg) return false;
    
    
    if (f == 0)
      result = mGF.ring().from_int(0);
    else if (f == mGF.one())
      result = mGF.ring().from_int(1);
    else
      result = mGF.ring().power(mGF.primitiveElement(), f);
    
    return true;
  }

  void ARingGFM2::eval(const RingMap *map, const elem f, int first_var, ring_elem &result) const
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
  
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
