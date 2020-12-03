// Copyright 2014 Michael E. Stillman

#include "aring-gf-flint-big.hpp"
#include "relem.hpp"
#include "poly.hpp"
#include "ringmap.hpp"

namespace M2 {

ARingGFFlintBig::ARingGFFlintBig(const PolynomialRing& R, const ring_elem a)
    : mOriginalRing(R),
      mCharacteristic(R.characteristic()),
      mGeneratorComputed(false)
{

  // TODO: assert that the base ring of R is ZZ/p.
  // TODO: if mDimension <= 1 then give an error.
  ring_elem b = R.copy(a);
  mPrimitiveElement = RingElement::make_raw(&R, b);
  ring_elem minpoly = mOriginalRing.quotient_element(0);

  std::vector<long> poly;
  RingElement F(&R, minpoly);
  F.getSmallIntegerCoefficients(poly);

  // warning: flint expects unsigned longs, so we must make
  // all of these coeffs non-negative.
  for (long i = poly.size() - 1; i >= 0; i--)
    {
      long a = poly[i];
      if (a == 0) continue;
      if (a < 0) poly[i] += characteristic();
    }

  mDimension = poly.size() - 1;

#if 0
    printf("minpoly: ");
    for (long i=0; i<poly.size(); i++)
      printf(" %ld", poly[i]);
    printf("\n");
#endif
  nmod_poly_t mMinPoly;
  nmod_poly_init(mMinPoly, R.characteristic());

  for (long i = poly.size() - 1; i >= 0; i--)
    if (poly[i] != 0) nmod_poly_set_coeff_ui(mMinPoly, i, poly[i]);

  fq_nmod_ctx_init_modulus(mContext, mMinPoly, "a");
#if 0
    fq_nmod_ctx_print(mContext);
#endif
  nmod_poly_clear(mMinPoly);

  // powers of p, as ulongs's
  mPPowers = newarray_atomic(unsigned int, mDimension + 1);
  mPPowers[0] = 1;
  for (long i = 1; i <= mDimension; i++)
    mPPowers[i] = mPPowers[i - 1] * static_cast<unsigned int>(mCharacteristic);

  flint_randinit(mRandomState);
}

ARingGFFlintBig::~ARingGFFlintBig()
{
  fq_nmod_ctx_clear(mContext);
  mPrimitiveElement = 0;
  deletearray(mPPowers);
  flint_randclear(mRandomState);

  if (mGeneratorComputed) fq_nmod_clear(&mCachedGenerator, mContext);
}

void ARingGFFlintBig::getSmallIntegerCoefficients(const ElementType& a,
                                                  std::vector<long>& poly) const
{
  long deg = nmod_poly_degree(&a);
  poly.resize(deg + 1);
  for (long i = deg; i >= 0; i--) poly[i] = nmod_poly_get_coeff_ui(&a, i);
}

void ARingGFFlintBig::fromSmallIntegerCoefficients(
    ElementType& result,
    const std::vector<long>& poly) const
{
#if 0
    printf("input = ");
    for (long i=0; i<poly.size(); i++)
      printf("%ld ", poly[i]);
    printf("\n");
#endif
  for (long i = poly.size() - 1; i >= 0; i--)
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

void ARingGFFlintBig::getGenerator(ElementType& result_gen) const
{
  if (not mGeneratorComputed)
    {
      fq_nmod_init(&mCachedGenerator, mContext);
      fq_nmod_gen(&mCachedGenerator, mContext);
      mGeneratorComputed = true;
    }
  copy(result_gen, mCachedGenerator);
}

bool ARingGFFlintBig::promote(const Ring* Rf,
                              const ring_elem f,
                              ElementType& result) const
{
  if (&originalRing() != Rf) return false;
  std::vector<long> poly;
  RingElement F(Rf, f);
  F.getSmallIntegerCoefficients(poly);
  fromSmallIntegerCoefficients(result, poly);
  return true;
}

void ARingGFFlintBig::lift_to_original_ring(ring_elem& result,
                                            const ElementType& f) const
{
  std::vector<long> poly;
  getSmallIntegerCoefficients(f, poly);
  result =
      originalRing().getNumeratorRing()->fromSmallIntegerCoefficients(poly, 0);
}

bool ARingGFFlintBig::lift(const Ring* Rg,
                           const ElementType& f,
                           ring_elem& result) const
{
  // Rg = ZZ/p[x]/F(x) ---> GF(p,n)
  if (&originalRing() != Rg) return false;
  lift_to_original_ring(result, f);
  return true;
}

void ARingGFFlintBig::eval(const RingMap* map,
                           const ElementType& f,
                           int first_var,
                           ring_elem& result) const
{
  const Ring* R = map->get_ring();  // the target ring
  result = R->from_long(0);
  if (is_zero(f)) return;

  ring_elem a = map->elem(first_var);
  std::vector<long> poly;
  getSmallIntegerCoefficients(f, poly);
  for (long i = poly.size() - 1; i >= 0; i--)
    {
      if (!R->is_zero(result)) result = R->mult(a, result);
      if (poly[i] != 0)
        {
          ring_elem c = R->from_long(poly[i]);
          result = R->add(result, c);
        }
    }
}

void ARingGFFlintBig::text_out(buffer& o) const
{
  o << "GF(" << characteristic() << "^" << dimension() << ",FlintBig)";
}

void ARingGFFlintBig::elem_text_out(buffer& o,
                                    const ElementType& a,
                                    bool p_one,
                                    bool p_plus,
                                    bool p_parens) const
{
  if (is_zero(a))
    {
      o << "0";
      return;
    }
  ring_elem b;
  lift(&originalRing(), a, b);
  originalRing().elem_text_out(o, b, p_one, p_plus, p_parens);
}

int ARingGFFlintBig::compare_elems(const ElementType& f,
                                   const ElementType& g) const
{
  long degF = nmod_poly_degree(&f);
  long degG = nmod_poly_degree(&g);
  if (degF > degG)
    return GT;
  else if (degF < degG)
    return LT;
  // now degF == degG
  for (long i = degF; i >= 0; i--)
    {
      long coeffF = nmod_poly_get_coeff_ui(&f, i);
      long coeffG = nmod_poly_get_coeff_ui(&g, i);
      if (coeffF > coeffG) return GT;
      if (coeffG > coeffF) return LT;
    }
  return EQ;
}
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
