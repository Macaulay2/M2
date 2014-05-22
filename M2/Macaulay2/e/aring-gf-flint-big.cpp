// Copyright 2014 Michael E. Stillman

#include "aring-gf-flint-big.hpp"
#include "relem.hpp"
#include "poly.hpp"
#include "ringmap.hpp"

namespace M2 {

  ARingGFFlintBig::ARingGFFlintBig(const PolynomialRing &R,
                                   const ring_elem a):
    mOriginalRing(R),
    mCharacteristic(R.characteristic())
  {
    ring_elem b = R.copy(a);
    mPrimitiveElement = RingElement::make_raw(&R,b);
    ring_elem minpoly = mOriginalRing.quotient_element(0);

    std::vector<long> poly;
    RingElement F(&R,minpoly);
    F.getSmallIntegerCoefficients(poly);

    mDimension = poly.size() - 1;
#if 0
    printf("minpoly: ");
    for (long i=0; i<poly.size(); i++)
      printf(" %ld", poly[i]);
    printf("\n");
#endif
    nmod_poly_t mMinPoly;
    nmod_poly_init(mMinPoly, R.characteristic());

    for (long i=poly.size()-1; i>=0; i--)
      if (poly[i] != 0)
        nmod_poly_set_coeff_ui(mMinPoly, i, poly[i]);

    fq_nmod_ctx_init_modulus(mContext,mMinPoly,"a");
#if 0
    fq_nmod_ctx_print(mContext);
#endif
    nmod_poly_clear(mMinPoly);

    // powers of p, as ulongs's
    mPPowers = newarray_atomic(unsigned long, mDimension+1);
    mPPowers[0] = 1;
    for (long i=1; i<=mDimension; i++)
      mPPowers[i] = mPPowers[i-1] * static_cast<ulong>(mCharacteristic);
  }

  ARingGFFlintBig::~ARingGFFlintBig()
  {
    fq_nmod_ctx_clear(mContext);
    mPrimitiveElement = 0;
    deletearray(mPPowers);
  }

  void ARingGFFlintBig::getSmallIntegerCoefficients(const ElementType& a, std::vector<long>& poly) const
  {
    long deg = nmod_poly_degree(&a);
    poly.resize(deg+1);
    for (long i=deg; i>=0; i--)
      poly[i] = nmod_poly_get_coeff_ui(&a,i);
  }
  
  void ARingGFFlintBig::fromSmallIntegerCoefficients(ElementType& result, const std::vector<long>& poly) const
  {
#if 0
    printf("input = ");
    for (long i=0; i<poly.size(); i++)
      printf("%ld ", poly[i]);
    printf("\n");
#endif
    for (long i=poly.size()-1; i>=0; i--)
      {
        long a = poly[i];
        if (a == 0) continue;
        if (a < 0) a += characteristic();
        nmod_poly_set_coeff_ui(&result, i, a);
      }
#if 0
    printf("  result before reduction = ");
    fq_nmod_print_pretty(&result, mContext);
    printf("\n");
#endif
    fq_nmod_reduce(&result, mContext);
#if 0
    printf("  result = ");
    fq_nmod_print_pretty(&result, mContext);
    printf("\n");
#endif
  }

  bool ARingGFFlintBig::promote(const Ring *Rf, const ring_elem f, ElementType& result) const
  {
    if (&originalRing() != Rf) return false;
    std::vector<long> poly;
    RingElement F(Rf,f);
    F.getSmallIntegerCoefficients(poly);
    fromSmallIntegerCoefficients(result, poly);
    return true;
  }

  bool ARingGFFlintBig::lift(const Ring *Rg, const ElementType& f, ring_elem &result) const
  {
    // Rg = ZZ/p[x]/F(x) ---> GF(p,n)
    if (&originalRing() != Rg) return false;
    std::vector<long> poly;
    getSmallIntegerCoefficients(f, poly);
    result = originalRing().getNumeratorRing()->fromSmallIntegerCoefficients(poly, 0);
    return true;
  }

  void ARingGFFlintBig::eval(const RingMap *map, const ElementType& f, int first_var, ring_elem &result) const
  {
    // TODO
    // result = map->get_ring()->power(map->elem(first_var), f);
  }
  
  
  void ARingGFFlintBig::text_out(buffer &o) const
  {
    o << "GF(" << characteristic() << "^" << dimension() << ",FlintBig)";
  }

  void ARingGFFlintBig::elem_text_out(buffer &o,
                                const ElementType& a,
                                bool p_one,
                                bool p_plus,
                                bool p_parens) const
  {
    printf("entering ARingGFFlintBig::elem_text_out\n");
    if (is_zero(a))
      {
        o << "0";
        return;
      }
    ring_elem b;
    lift(& originalRing(), a, b);
    originalRing().elem_text_out(o,b,p_one,p_plus,p_parens);
  }

  int ARingGFFlintBig::compare_elems(const ElementType& f, const ElementType& g) const
  {
    // TODO
    // compare polynomial coeff by coeff
    return 0;
  }
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
