// Copyright 2011 Michael E. Stillman

#include "aring-gf-givaro.hpp"

#include "interface/random.h"
#include "error.h"
#include "ringmap.hpp"
#include "monoid.hpp"

// Uncomment the following line to see debugging output
//#define DEBUG_GF

namespace M2 {

// std::vector<GFqDom<long>::Residu_t> irreducible_11_2;
// GFqDom<long> gfqField(11,2,irreducible_11_2);

ARingGFGivaro::ARingGFGivaro(UTT charact_, UTT extensionDegree_)
    : mCharac(charact_),
      mDimension(extensionDegree_),
      mOriginalRing(0),
      givaroField(FieldType(charact_, extensionDegree_)),
      givaroRandomIterator(GivaroRandIter(givaroField))
{
  mCardinality = mCharac;
  for (int j = 1; j < mDimension; j++) mCardinality *= mCharac;

  /// @todo remove debug code
  /// debug code:
  getModPolynomialCoeffs();
  getGeneratorCoeffs();

  /*

   ARingGFGivaro *testGF = new ARingGFGivaro(charact_, getModPolynomialCoeffs()
   );

   std::cerr <<"random"<< std::endl;
   ElementType rnd ;
   this->random(rnd);
   std::cerr << " rnd = "<< rnd << std::endl;
   fieldElementToM2Array(rnd);
   fieldElementToM2Array(givaroField.one);
   fieldElementToM2Array(givaroField.zero);
   */

  /// end debug
}

ARingGFGivaro::ARingGFGivaro(UTT charact_,
                             const M2_arrayint &modPolynomial,
                             const PolynomialRing &originalRing)
    : mCharac(charact_),
      mDimension(M2arrayGetDegree(modPolynomial)),
      mOriginalRing(&originalRing),
      mPrimitiveElement(originalRing.var(0)),
      givaroField(
          FieldType(charact_,
                    mDimension,
                    ARingGFGivaro::M2arrayToStdVec(charact_, modPolynomial))),
      givaroRandomIterator(GivaroRandIter(givaroField))
{
  mCardinality = mCharac;
  for (int j = 1; j < mDimension; j++) mCardinality *= mCharac;

  /// @jakob find out if the irreducible polynomial is checked in givaro.
  UTT localdegree = M2arrayGetDegree(modPolynomial);

  if (!(modPolynomial->len > 1 && modPolynomial->array[localdegree] > 0))
    {
      std::cout << "assertion would have failed" << std::endl;
      assert(modPolynomial->len > 1);
      assert(modPolynomial->array[localdegree] > 0);
    }
  /// debug code:
  getModPolynomialCoeffs();
}

ARingGFGivaro::ARingGFGivaro(UTT charact_,
                             const M2_arrayint &modPolynomial,
                             const M2_arrayint &generatorPoly,
                             const PolynomialRing &originalRing)
    : mCharac(charact_),
      mDimension(M2arrayGetDegree(modPolynomial)),
      mOriginalRing(&originalRing),
      mPrimitiveElement(originalRing.var(0)),
      givaroField(
          FieldType(charact_,
                    mDimension,
                    ARingGFGivaro::M2arrayToStdVec(charact_, modPolynomial),
                    ARingGFGivaro::M2arrayToStdVec(charact_, generatorPoly))),
      givaroRandomIterator(GivaroRandIter(givaroField))
{
  mCardinality = mCharac;
  for (int j = 1; j < mDimension; j++) mCardinality *= mCharac;

#ifdef DEBUG_GF
  std::vector<UTT> debugGenPoly = M2arrayToStdVec(charact_, generatorPoly);
  std::cerr << "generatorPoly: ";
  for (int i = 0; i < debugGenPoly.size(); i++)
    {
      std::cerr << debugGenPoly[i] << " ";
    }
  std::cerr << std::endl;
#endif
  /// @jakob find out if the irreducible polynomial is checked in givaro.
  UTT localdegree = M2arrayGetDegree(modPolynomial);

  if (!(modPolynomial->len > 1 && modPolynomial->array[localdegree] > 0))
    {
      std::cout << "assertion would have failed" << std::endl;
      assert(modPolynomial->len > 1);
      assert(modPolynomial->array[localdegree] > 0);
    }
  /// debug code:
  getModPolynomialCoeffs();
}

M2_arrayint ARingGFGivaro::findMinimalPolynomial(UTT charac, UTT dim)
{
  // ARingGFGivaro tmp(charac,dim);
  // return tmp.getModPolynomialCoeffs();

  FieldType Zp(charac, 1);
  //         typedef CyclotomicTable<  GFqDom<TT>, Dense > PolDom;
  //         PolDom Pdom( Zp, e );
  typedef Givaro::Poly1FactorDom<FieldType::Self_t, Givaro::Dense> PolDom;
  PolDom Pdom(Zp);
  PolDom::Element F, G, H;

// F is irreducible of degree e over Zp
// G is a primitive polynomial for F
//         Pdom.random_prim_root(F,G, Degree(e));

// F is an irreducible factor of the
// (p^e-1) th cyclotomic polynomial
// G is a primitive polynomial for F : X
//         Pdom.getcyclo(F);
//         Pdom.init(G, Degree(1), Zp.one);

// F is irreducible of degree e over Zp
// with X as a primitive polynomial
#ifndef GIVARO_RANDOM_IRREDUCTIBLE_PRIMITIVE_ROOT
  Pdom.ixe_irreducible(F, Givaro::Degree((long)dim));
  //         Pdom.init(G, Degree(1), Zp.one);
  //         Pdom.assign(G, Degree(1), Zp.one);
  Pdom.init(G, Givaro::Degree(1));
#else
  Pdom.random_irreducible(F, Givaro::Degree((long)dim));
  Pdom.give_random_prim_root(G, F);
#endif

  Pdom.assign(H, G);

  typedef Givaro::Poly1PadicDom<FieldType, Givaro::Dense> PadicDom;
  PadicDom PAD(Pdom);

  UTT generator, irreducible;

  PAD.eval(generator, H);
  PAD.eval(irreducible, F);

  return (representationToM2Array(irreducible, dim + 1, charac));
}

ARingGFGivaro::UTT ARingGFGivaro::M2arrayToGFRepresentation(
    ARingGFGivaro::UTT pCharac,
    const M2_arrayint &m2array)
{
#ifdef DEBUG_GF
  std::cerr << "M2arrayToGFRepresentation" << std::endl;
#endif
  ARingGFGivaro::UTT rep = 0;
  assert(m2array->len > 1);
  assert(sizeof(m2array->array[0]) < sizeof(ARingGFGivaro::UTT));

  for (ARingGFGivaro::STT pos = m2array->len - 1; pos >= 0; pos--)
    {
#ifdef DEBUG_GF
      std::cerr << " m2array->array[" << pos << "]" << m2array->array[pos]
                << std::endl;
#endif
      if (m2array->array[pos] >= 0)
        {
          assert((ARingGFGivaro::UTT)(m2array->array[pos]) < pCharac);
          rep = rep * pCharac + (m2array->array[pos]);
        }
      if (m2array->array[pos] < 0)
        {
          assert((ARingGFGivaro::UTT)(-(m2array->array[pos])) < pCharac);
          rep = rep * pCharac + (m2array->array[pos] + pCharac);
        }
    }
  return rep;
  std::cerr << "rep" << rep << std::endl;
}

M2_arrayint ARingGFGivaro::fieldElementToM2Array(ElementType el) const
{
  UTT packedPolynomial;
  packedPolynomial = this->givaroField.convert(packedPolynomial, el);
#ifdef DEBUG_GF
  std::cerr << "packedPolynomial = " << packedPolynomial << std::endl;
#endif
  return elementRepresentationToM2Array(packedPolynomial);
}

ARingGFGivaro::UTT ARingGFGivaro::M2arrayGetDegree(const M2_arrayint &m2array)
{
  ARingGFGivaro::UTT degree = 0;
  ///@jakob find out the type of m2array->len
  for (UTT pos = 0; pos < m2array->len; pos++)
    {
      if (m2array->array[pos] != 0) degree = pos;
    }
  return degree;
}

std::vector<ARingGFGivaro::UTT> ARingGFGivaro::M2arrayToStdVec(
    ARingGFGivaro::UTT pCharac,
    const M2_arrayint &m2array)
{
// std::vector< UTT > stdvec;
#ifdef DEBUG_GF
  std::cerr << "M2arrayToStdVec" << std::endl;
#endif
  assert(m2array->len > 0);

  std::vector<ARingGFGivaro::UTT> vec;

  vec.resize(M2arrayGetDegree(m2array) + 1);

  for (UTT pos = 0; pos < m2array->len; pos++)
    {
      vec[pos] = m2array->array[pos];
      if (m2array->array[pos] >= 0)
        {
          assert((ARingGFGivaro::UTT)(m2array->array[pos]) < pCharac);
          vec[pos] = m2array->array[pos];
        }
      if (m2array->array[pos] < 0)
        {
          assert((ARingGFGivaro::UTT)(-(m2array->array[pos])) < pCharac);
          vec[pos] = (m2array->array[pos] + pCharac);
        }
    }
  return vec;
}

/// @mike correct output : print generator variable of the ring instead of 'X',
/// whatever generator variable will be
void ARingGFGivaro::elem_text_out(buffer &o,
                                  const ElementType a,
                                  bool p_one,
                                  bool p_plus,
                                  bool p_parens) const
{
  UTT rep;
  rep = givaroField.convert(rep, a);
  long exp = 0;
  if (rep == 0) o << "0";
  while (rep != 0)
    {
      UTT remainder = rep % mCharac;
      rep = rep / mCharac;
      if (exp > 0) o << " + ";
      o << remainder << "*"
        << "X^" << exp;
      exp++;
    }
}

M2_arrayint ARingGFGivaro::representationToM2Array(UTT representation,
                                                   long coeffNum,
                                                   UTT charac)
{
#ifdef DEBUG_GF
  std::cerr << "representationToM2Array:\n";
#endif
  M2_arrayint polynomialCoeffs = M2_makearrayint(static_cast<int>(coeffNum));
#ifdef DEBUG_GF
  std::cerr << "coeffNum" << coeffNum << std::endl;
  std::cerr << "charac" << charac << std::endl;
  std::cerr << "representation" << representation << std::endl;
#endif
  long exp = 0;

  while (representation != 0)
    {
      assert(exp < coeffNum);
      UTT remainder = representation % charac;
      representation = representation / charac;
      polynomialCoeffs->array[exp] = static_cast<int>(remainder);

#ifdef DEBUG_GF
      // debug:
      if (exp > 0) std::cerr << " + ";
      std::cerr << remainder << "*"
                << "X^" << exp;
// end debug
#endif
      exp++;
    }
  assert(representation == 0);
  for (; exp < coeffNum; exp++)
    {
      assert(exp < coeffNum);
      polynomialCoeffs->array[exp] = 0;
    }
#ifdef DEBUG_GF
  std::cerr << "\n";
#endif
  return polynomialCoeffs;
}

M2_arrayint ARingGFGivaro::representationToM2Array(UTT representation,
                                                   long coeffNum) const
{
  return (representationToM2Array(representation, coeffNum, mCharac));
}

M2_arrayint ARingGFGivaro::elementRepresentationToM2Array(
    UTT polynomialRep) const
{
#ifdef DEBUG_GF
  std::cerr << "representationToM2Array:\n";
#endif
  long coeffNum;
  return representationToM2Array(polynomialRep, coeffNum = this->mDimension);
}

M2_arrayint ARingGFGivaro::modPolynomialRepresentationToM2Array(
    UTT polynomialRep) const
{
#ifdef DEBUG_GF
  std::cerr << "modPolynomialRepresentationToM2Array:\n";
#endif
  long coeffNum;
  return representationToM2Array(polynomialRep,
                                 coeffNum = this->mDimension + 1);
}

/// returns mod polynomial coefficients as array of integers.
/// @todo problems, if characteristic does not fit in a int.
M2_arrayint ARingGFGivaro::getModPolynomialCoeffs() const
{
#ifdef DEBUG_GF
  std::cerr << "getModPolynomialCoeffs\n";
#endif
  UTT modPolynomialRepresentation = this->givaroField.irreducible();
  return modPolynomialRepresentationToM2Array(modPolynomialRepresentation);
}

M2_arrayint ARingGFGivaro::getGeneratorCoeffs() const
{
#ifdef DEBUG_GF
  std::cerr << "getGeneratorCoeffs\n";
#endif
  ElementType genRep, packedGenPolynomial;  /// todo: typ (gen) eigentlich UTT?
  givaroField.generator(genRep);
  packedGenPolynomial = givaroField.generator();

#ifdef DEBUG_GF
  std::cerr << "packedGenPolynomial " << packedGenPolynomial << std::endl;
  std::cerr << "genRep " << genRep << std::endl;
#endif
  // assert(gen==genRep);
  // UTT  generatorRepresentation;
  // generatorRepresentation =
  // this->givaroField.convert(generatorRepresentation,gen);
  // return elementRepresentationToM2Array( generatorRepresentation ) ;
  return elementRepresentationToM2Array(packedGenPolynomial);
}

#if 0
  // Commented out, MES 1 June 2014, as interface to this function has changed substantially.
ring_elem  ARingGFGivaro::getGenerator() const
{
#ifdef DEBUG_GF
   std::cerr << "  ARingGFGivaro::getGenerator()" << std::endl;
#endif
    ElementType packedGenPolynomial = givaroField.generator();  
    ElementType genRep;
    givaroField.generator(genRep);

#ifdef DEBUG_GF
     std::cerr << "packedGenPolynomial " << packedGenPolynomial << std::endl;
    std::cerr << "genRep " << genRep << std::endl;
#endif

     elementRepresentationToM2Array( packedGenPolynomial ) ;

#ifdef DEBUG_GF
    std::cerr << "end elementRepresentationToM2Array " << genRep << std::endl;
#endif

    ring_elem result;
    //to_ring_elem(result,packedGenPolynomial);
    to_ring_elem(result, genRep);
    //std::cerr << " result " << *result << std::endl;
    return result;
}
#endif

void ARingGFGivaro::getGenerator(ElementType &result_gen) const
{
  // ElementType packedGenPolynomial = givaroField.generator();
  givaroField.generator(result_gen);
}

bool ARingGFGivaro::is_unit(const ElementType f) const
{
#if HAVE_GIVARO_isunit
  return givaroField.isunit(f);
#else
  return givaroField.isUnit(f);
#endif
}

bool ARingGFGivaro::is_zero(const ElementType f) const
{
  return givaroField.isZero(f);
}

bool ARingGFGivaro::is_equal(const ElementType f, const ElementType g) const
{
  return givaroField.areEqual(f, g);
}

/// compare exponents of the used generator
int ARingGFGivaro::compare_elems(const ElementType f, const ElementType g) const
{
#ifdef DEBUG_GF
  std::cerr << "ARingGFGivaro::compare_elems" << std::endl;
#endif
  if (f < g) return -1;
  if (f > g) return 1;

  return 0;
}

// 'init', 'init_set' functions

void ARingGFGivaro::init(ElementType &result) const
{
  result = givaroField.zero;
}

void ARingGFGivaro::clear(ElementType &result) const { /* nothing */}

void ARingGFGivaro::set_zero(ElementType &result) const
{
  result = givaroField.zero;
}

void ARingGFGivaro::copy(ElementType &result, const ElementType a) const
{
  result = a;
}

/// @todo possible problem if type UTT is smaller than an int?
void ARingGFGivaro::set_from_long(ElementType &result, int64_t a) const
{
  // givaroField.init(result, a): returns an element in GF, where
  // 0 <= a < p^n.  It injects via lexicographic order of p-adic rep:
  // 0 --> [0,0,0]
  // 1 --> [1,0,0]
  // p --> [0,1,0], etc.
  ElementType p = static_cast<ElementType>(mCharac);
  ElementType a1 = a;
  if (a1 < 0 or a1 >= p)
    {
      a1 = a1 % p;
      if (a1 < 0) a1 = a1 + p;
    }
  // strange: if mCharac isn't cast away from unsigned,
  // then in "a % mCharac" a is coerced to unsigned, and get the wrong answer!
  // e.g:
  //  (-5) % (unsigned long)(5) == 1
  //  (-5) % (long)(5) == 0.  Wow!
  givaroField.init(result, a1);
}

void ARingGFGivaro::set_from_mpz(ElementType &result, mpz_srcptr a) const
{
  UTT b = static_cast<UTT>(mpz_fdiv_ui(a, mCharac));
  givaroField.init(result, b);
}

bool ARingGFGivaro::set_from_mpq(ElementType &result, mpq_srcptr a) const
{
  ElementType n, d;
  set_from_mpz(n, mpq_numref(a));
  set_from_mpz(d, mpq_denref(a));
  if (is_zero(d)) return false;
  divide(result, n, d);
  return true;
}

// arithmetic
void ARingGFGivaro::negate(ElementType &result, const ElementType a) const
{
  givaroField.neg(result, a);
}

/// if a is zero, the result is 1 , but is that what we expect?
/// I vote for two invert functions, one with this check and one without.(Jakob)
void ARingGFGivaro::invert(ElementType &result, const ElementType a) const
{
  if (givaroField.isZero(a)) ERROR(" division by zero");
  givaroField.inv(result, a);
}

void ARingGFGivaro::add(ElementType &result,
                        const ElementType a,
                        const ElementType b) const
{
  givaroField.add(result, a, b);
}

void ARingGFGivaro::subtract(ElementType &result,
                             const ElementType a,
                             const ElementType b) const
{
  givaroField.sub(result, a, b);
}

/// @param c[in][out] c = c- a*b
void ARingGFGivaro::subtract_multiple(ElementType &c,
                                      const ElementType a,
                                      const ElementType b) const
{
  givaroField.maxpyin(c, a, b);
}

void ARingGFGivaro::mult(ElementType &result,
                         const ElementType a,
                         const ElementType b) const
{
  givaroField.mul(result, a, b);
}

void ARingGFGivaro::divide(ElementType &result,
                           const ElementType a,
                           const ElementType b) const
{
  if (givaroField.isZero(b)) ERROR(" division by zero");
  givaroField.div(result, a, b);
}

/// @jakob overflow can occur due to multiplication. use exact mpz for
/// multiply and modulo operation instead!
void ARingGFGivaro::power(ElementType &result,
                          const ElementType a,
                          const STT n) const
{
  if (givaroField.isnzero(a))
    {
      mpz_t mpz_a;
      mpz_t mpz_n;
      mpz_t mpz_tmp;
      mpz_init(mpz_a);
      mpz_init(mpz_n);
      mpz_init(mpz_tmp);
      mpz_set_si(mpz_n, n);
      mpz_set_ui(mpz_a, a);
      // std::cerr << "a = " << a << std::endl;
      // std::cerr << "mpz_a = " << mpz_a << std::endl;
      // std::cerr << "n = " << n << std::endl;
      // std::cerr << "mpz_n = " << mpz_n << std::endl;
      mpz_fdiv_r_ui(mpz_tmp, mpz_n, givaroField.cardinality() - 1);
      mpz_mul(mpz_n, mpz_a, mpz_tmp);
      STT tmp =
          static_cast<STT>(mpz_fdiv_ui(mpz_n, givaroField.cardinality() - 1));
      if (tmp == 0)
        {
          tmp += givaroField.cardinality() - 1;
          // result=givaroField.one;
        }

      // std::cerr << "tmp = " << tmp << std::endl;
      assert(tmp >= 0);  // tmp<0 should never occur
      if (tmp < 0) tmp += givaroField.cardinality() - 1;
      result = tmp;
      mpz_clear(mpz_a);
      mpz_clear(mpz_n);
      mpz_clear(mpz_tmp);
    }
  else
    {
      if (n < 0) ERROR(" division by zero");
      result = 0;
    }
}

///@todo ensure that  givaroField.cardinality() fits in a unsigned long,
/// otherwise instead of mpz_fdiv_ui a different function has to be called)
void ARingGFGivaro::power_mpz(ElementType &result,
                              const ElementType a,
                              mpz_srcptr n) const
{
  STT n1 = static_cast<STT>(mpz_fdiv_ui(n, givaroField.cardinality() - 1));

  // std::cerr << "exponent = " << n << std::endl;
  // std::cerr << "n1 = " << n1 << std::endl;
  power(result, a, n1);
}

///@note duplicate code
void ARingGFGivaro::swap(ElementType &a, ElementType &b) const
{
  ElementType tmp = a;
  a = b;
  b = tmp;
}

/** @brief returns x,y  s.y.  x*a + y*b == 0.
   if possible, x is set to 1.
   no need to consider the case a==0 or b==0.
*/

void ARingGFGivaro::syzygy(const ElementType a,
                           const ElementType b,
                           ElementType &x,
                           ElementType &y) const

{
  x = givaroField.one;
  divide(y, a, b);
  negate(y, y);
}

/// @jakob document possible overflow and other nasty things
void ARingGFGivaro::random(GivaroRandIter &it, ElementType &result) const
{
  givaroField.random(it, result);
  //   std::cerr << " givaroField.cardinality()" << givaroField.cardinality();
  //   std::cerr << " givaroRandomIterator()" << it();
}

void ARingGFGivaro::random(ElementType &result) const
{
  return random(givaroRandomIterator, result);
  // result = rawRandomInt((int32_t) givaroField.cardinality());
  // result = givaroRandomIterator() %   givaroField.cardinality();
}

bool ARingGFGivaro::promote(const Ring *Rf,
                            const ring_elem f,
                            ElementType &result) const
{
  if (mOriginalRing != Rf) return false;

  result = givaroField.zero;
  int exp[1];
  ElementType genRep;
  givaroField.generator(genRep);
#ifdef DEBUG_GF
  std::cerr << "genRep " << genRep << std::endl;
#endif
  for (Nterm *t = f; t != NULL; t = t->next)
    {
      elem a, b;

      std::pair<bool, long> res =
          mOriginalRing->getCoefficientRing()->coerceToLongInteger(t->coeff);
      assert(res.first);
      set_from_long(a, res.second);

      mOriginalRing->getMonoid()->to_expvector(t->monom, exp);
      // exp[0] is the variable we want.  Notice that since the ring is a
      // quotient,
      // this degree is < n (where Q_ = P^n).
      power(b, genRep, exp[0]);
      mult(a, a, b);
      add(result, result, a);
    }
  return true;
}

void ARingGFGivaro::lift_to_original_ring(ring_elem &result,
                                          const ElementType &f) const
{
  // This code needs review, and tests.  See git issue #612
  if (f == givaroField.zero)
    result = mOriginalRing->from_long(0);
  else if (f == givaroField.one)
    result = mOriginalRing->from_long(1);
  else
    {
      result = mOriginalRing->power(mPrimitiveElement, static_cast<int>(f));
    }
}

bool ARingGFGivaro::lift(const Ring *Rg,
                         const ElementType f,
                         ring_elem &result) const
{
  // Rg = Z/p[x]/F(x) ---> GF(p,n)
  // promotion: need to be able to know the value of 'x'.
  // lift: need to compute (primite_element)^e

  if (mOriginalRing != Rg) return false;
  lift_to_original_ring(result, f);
  return true;
}

void ARingGFGivaro::eval(const RingMap *map,
                         const elem f,
                         int first_var,
                         ring_elem &result) const
{
  result = map->get_ring()->power(map->elem(first_var), f);
}
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
