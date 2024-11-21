// Copyright 2011 Michael E. Stillman

#ifndef _ring_glue_hh_
#define _ring_glue_hh_

#include "aring.hpp"
#include "aring-translate.hpp"
#include "ring.hpp"

#include "mutablemat.hpp"

static const bool displayArithmeticCalls = false;

#define COERCE_RING(RingType, R) dynamic_cast<const RingType *>(R)

namespace M2 {
/**
    @ingroup rings
*/
template <class RingType>
class ConcreteRing : public Ring
{
  std::unique_ptr<RingType> R;

 protected:
  explicit ConcreteRing(std::unique_ptr<RingType> R0) : R(std::move(R0)) {}
  virtual ~ConcreteRing() = default;

 public:
  // explicitly delete the copy constructor
  ConcreteRing(const ConcreteRing &ring) = delete;
  typedef typename RingType::ElementType ElementType;
  typedef typename RingType::Element Element;

  static ConcreteRing<RingType> *create(std::unique_ptr<RingType> R);

  template <class... Args>
  static ConcreteRing<RingType> *create(Args &&...args);

  virtual M2::RingID ringID() const { return RingType::ringID; }
  const RingType &ring() const { return *R; }
  /// Create either a dense or sparse MutableMatrix of the given size
  virtual MutableMatrix *makeMutableMatrix(size_t nrows,
                                           size_t ncols,
                                           bool dense) const
  {
    // JY: The use of get here on R is safe because MutableMat will always keep
    // a reference to this so long as it has a reference to R, and this is in
    // the gc heap, so that reference keeps R alive. Really, the MutableMat
    // constructor should probably be calling ring(), but ring() is not virtual
    // and not every Ring is an instance of ConcreteRing.
    if (dense)
      return new MutableMat<DMat<RingType> >(this, R.get(), nrows, ncols);
    else
      return new MutableMat<SMat<RingType> >(this, R.get(), nrows, ncols);
  }

  bool isFinitePrimeField() const
  {
    return ringID() == ring_ZZp or ringID() == ring_ZZpFfpack or
           ringID() == ring_ZZpFlint;
  }

  bool isGaloisField() const
  {
    return ringID() == ring_GFM2  or
           ringID() == ring_GFFlintBig or ringID() == ring_GFFlintZech;
  }

  /////////////////////////////////////////
  // Special functions for Galois Fields //
  /////////////////////////////////////////
  // These are declared below for Galois fields, and the definitions appear
  // where?

  const RingElement *getMinimalPolynomial() const { return nullptr; }
  const RingElement *getGenerator() const { return nullptr; }
  const RingElement *getRepresentation(const ring_elem &a) const { return nullptr; }
  virtual long discreteLog(const ring_elem &a) const
  {
    throw exc::engine_error("cannot compute discrete logarithm in this ring");
  }

  ////////////////////////////
  // Functions on elements ///
  ////////////////////////////
  virtual unsigned int computeHashValue(ring_elem a) const
  {
    const ElementType &b = ring().from_ring_elem_const(a);
    return ring().computeHashValue(b);
  }

  virtual std::pair<bool, long> coerceToLongInteger(ring_elem a) const
  {
    return std::pair<bool, long>(false, 0);
  }

  // The following are all the routines required by 'ring'
  virtual void text_out(buffer &o) const { return R->text_out(o); }
  virtual ring_elem from_long(long n) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling from_long\n");
    ring_elem result;
    Element a(*R);
    R->set_from_long(a, n);
    R->to_ring_elem(result, a);
    return result;
  }

  virtual ring_elem from_int(mpz_srcptr n) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling from_int(mpz)\n");
    ring_elem result;
    Element a(*R);
    R->set_from_mpz(a, n);
    R->to_ring_elem(result, a);
    return result;
  }
  virtual bool from_rational(mpq_srcptr q, ring_elem &result) const
  {
    Element a(*R);
    bool ret = R->set_from_mpq(a, q);
    if (ret) R->to_ring_elem(result, a);
    return ret;
  }
  virtual bool from_BigReal(gmp_RR q, ring_elem &result) const
  {
    Element a(*R);
    bool ret = get_from_BigReal(*R, a, q);
    if (ret) R->to_ring_elem(result, a);
    return ret;
  }

  virtual bool from_Interval(gmp_RRi q, ring_elem &result) const
  {
    Element a(*R);
    bool ret = get_from_Interval(*R, a, q);
    if (ret) R->to_ring_elem(result, a);
    return ret;
  }

  virtual bool from_BigComplex(gmp_CC q, ring_elem &result) const
  {
    Element a(*R);
    //      bool ret = R->set_from_BigComplex(a,q);
    bool ret = get_from_BigComplex(*R, a, q);
    if (ret) R->to_ring_elem(result, a);
    return ret;
  }
  virtual bool from_double(double q, ring_elem &result) const
  {
    Element a(*R);
    bool ret = get_from_double(*R, a, q);
    if (ret) R->to_ring_elem(result, a);
    return ret;
  }
  virtual bool from_complex_double(double re,
                                   double im,
                                   ring_elem &result) const
  {
    Element a(*R);
    bool ret = get_from_complex_double(*R, a, re, im);
    if (ret) R->to_ring_elem(result, a);
    return ret;
  }

  virtual ring_elem var(int v) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling var\n");
    ring_elem result;
    Element a(*R);
    R->set_var(a, v);
    R->to_ring_elem(result, a);
    return result;
  }

  /** If there is a "natural" map S --> R=this, where f is an element of
  // S , then result is set to the image of f, and true is returned.
  // Otherwise, false is returned.
  //
  // The map must one-step, e.g. for k --> k[x] --> k[x][y], promotion
  // must be done with two consecutive calls to promote(with different
  arguments).
  // Examples of natural maps:
  //  ZZ --> R, for any R
  //  QQ --> RR --> CC
  //  ZZ --> ZZ/p
  //  ZZ/p --> GF(p^n)
  //  GF(p^m) --> GF(p^n), where m|n
  //  A --> A[vars]/I
  //  A[vars]/J --> A[vars]/I  (assumption: I contains J).
  */

  virtual bool promote(const Ring *S,
                       const ring_elem f,
                       ring_elem &result) const;

  //* If there is a "natural" map S --> R=this, where f is an element of
  // R, then result is set to an element f in S which maps to f, and true is
  // returned.
  // Otherwise, false is returned.
  //
  // For examples of maps, see 'promote'.
  //
  // Lifting elements of ZZ/p to QQ does not count, as there is no actual
  // homomorphism
  // from QQ --> ZZ/p

  virtual bool lift(const Ring *S, const ring_elem f, ring_elem &result) const;

  virtual bool is_unit(const ring_elem f) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling is_unit\n");
    const ElementType &a = R->from_ring_elem_const(f);
    bool ret = R->is_unit(a);
    return ret;
  }

  virtual bool is_zero(const ring_elem f) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling is_zero\n");
    const ElementType &a = R->from_ring_elem_const(f);
    bool ret = R->is_zero(a);
    return ret;
  }

  virtual bool is_equal(const ring_elem f, const ring_elem g) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling is_equal\n");
    const ElementType &a = R->from_ring_elem_const(f);
    const ElementType &b = R->from_ring_elem_const(g);
    bool ret = R->is_equal(a, b);
    return ret;
  }

  virtual int compare_elems(const ring_elem f, const ring_elem g) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling compare_elems\n");
    const ElementType &a = R->from_ring_elem_const(f);
    const ElementType &b = R->from_ring_elem_const(g);
    int ret = R->compare_elems(a, b);
    return ret;
  }

  virtual ring_elem copy(const ring_elem f) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling copy\n");
    const ElementType &a = R->from_ring_elem_const(f);
    Element b(*R);
    ring_elem result;
    R->set(b, a);
    R->to_ring_elem(result, b);
    return result;
  }

  virtual void remove(ring_elem &f) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling remove\n");
    /* currently, do nothing... */
  }

  virtual ring_elem negate(const ring_elem f) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling negate\n");
    const ElementType &a = R->from_ring_elem_const(f);
    Element b(*R);
    ring_elem result;
    R->negate(b, a);
    R->to_ring_elem(result, b);
    return result;
  }

  virtual ring_elem add(const ring_elem f, const ring_elem g) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling add\n");
    const ElementType &a = R->from_ring_elem_const(f);
    const ElementType &b = R->from_ring_elem_const(g);
    Element c(*R);
    ring_elem result;
    R->add(c, a, b);
    R->to_ring_elem(result, c);
    return result;
  }

  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling subtract\n");
    const ElementType &a = R->from_ring_elem_const(f);
    const ElementType &b = R->from_ring_elem_const(g);
    Element c(*R);
    ring_elem result;
    R->subtract(c, a, b);
    R->to_ring_elem(result, c);
    return result;
  }

  virtual ring_elem mult(const ring_elem f, const ring_elem g) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling mult\n");
    const ElementType &a = R->from_ring_elem_const(f);
    const ElementType &b = R->from_ring_elem_const(g);
    Element c(*R);
    ring_elem result;
    R->mult(c, a, b);
    R->to_ring_elem(result, c);
    return result;
  }

  virtual ring_elem power(const ring_elem f, mpz_srcptr n) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling power mpz\n");
    const ElementType &a = R->from_ring_elem_const(f);
    Element b(*R);
    ring_elem result;
    R->power_mpz(b, a, n);
    R->to_ring_elem(result, b);
    return result;
  }

  virtual ring_elem power(const ring_elem f, int n) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling power int\n");
    const ElementType &a = R->from_ring_elem_const(f);
    Element b(*R);
    ring_elem result;
    R->power(b, a, n);
    R->to_ring_elem(result, b);
    return result;
  }

  virtual ring_elem invert(const ring_elem f) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling invert\n");
    const ElementType &a = R->from_ring_elem_const(f);
    Element b(*R);
    ring_elem result;
    R->invert(b, a);
    R->to_ring_elem(result, b);
    return result;
  }

  virtual ring_elem divide(const ring_elem f, const ring_elem g) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling divide\n");
    const ElementType &a = R->from_ring_elem_const(f);
    const ElementType &b = R->from_ring_elem_const(g);
    Element c(*R);
    ring_elem result;
    R->divide(c, a, b);
    R->to_ring_elem(result, c);
    return result;
  }

  virtual void syzygy(const ring_elem f,
                      const ring_elem g,
                      ring_elem &x,
                      ring_elem &y) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling syzygy\n");
    const ElementType &a = R->from_ring_elem_const(f);
    const ElementType &b = R->from_ring_elem_const(g);
    Element xe(*R), ye(*R);
    R->syzygy(a, b, xe, ye);
    R->to_ring_elem(x, xe);
    R->to_ring_elem(y, ye);
  }

  virtual ring_elem random() const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling random\n");
    ring_elem result;
    Element a(*R);
    R->random(a);
    R->to_ring_elem(result, a);
    return result;
  }

  virtual void elem_text_out(buffer &o,
                             const ring_elem f,
                             bool p_one = true,
                             bool p_plus = false,
                             bool p_parens = false) const
  {
    if (displayArithmeticCalls) fprintf(stderr, "calling elem_text_out\n");
    const ElementType &a = R->from_ring_elem_const(f);
    R->elem_text_out(o, a, p_one, p_plus, p_parens);
  }

  // map : this = R --> S, f in R
  // sending primelem --> map->elem(firstvar)
  // return map(f) as a ring_elem in S
  virtual ring_elem eval(const RingMap *map,
                         const ring_elem f,
                         int first_var) const
  {
    // TODO we shouldn't have to use a non-const element type,
    // but lots of ARing instances use a non-const reference for the input
    // this should be fixable, but will require checking
    Element a(*R);
    ring_elem result;
    R->from_ring_elem(a, f);
    R->eval(map, a, first_var, result);
    return result;
  }

  // TODO: look again at the next two when ring_elem is phased out
  virtual ring_elem zeroize_tiny(gmp_RR epsilon, const ring_elem f) const;
  virtual void increase_maxnorm(gmp_RRmutable norm, const ring_elem f) const;

  virtual unsigned long get_precision()
      const;  // if the ring is not over RRR/CCC returns 0

};  // class ConcreteRing<RingType>

class RingQQ : public ConcreteRing<ARingQQ>
{
 public:
  explicit RingQQ(std::unique_ptr<ARingQQ> R0)
      : ConcreteRing<ARingQQ>(std::move(R0))
  {
  }
  bool is_QQ() const { return true; }
  CoefficientType coefficient_type() const { return COEFF_QQ; }
  static RingQQ *create()
  {
    auto R0 = std::make_unique<ARingQQ>();
    auto characteristic = R0->characteristic();
    RingQQ *result = new RingQQ(std::move(R0));
    result->initialize_ring(characteristic);
    result->declare_field();

    result->zeroV = result->from_long(0);
    result->oneV = result->from_long(1);
    result->minus_oneV = result->from_long(-1);

    return result;
  }

  ring_elem fraction(ring_elem top, ring_elem bottom) const
  {
    mpz_srcptr numer = top.get_mpz();
    mpz_srcptr denom = bottom.get_mpz();
    mpq_ptr b = getmemstructtype(mpq_ptr);
    mpq_init(b);
    mpz_set(mpq_numref(b), numer);
    mpz_set(mpq_denref(b), denom);
    mpq_canonicalize(b);
    mpz_reallocate_limbs(mpq_numref(b));
    mpz_reallocate_limbs(mpq_denref(b));
    return ring_elem(b);
  }

  ring_elem numerator(ring_elem q) const
  {
    return globalZZ->from_int(mpq_numref(MPQ_VAL(q)));
  }

  ring_elem denominator(ring_elem q) const
  {
    return globalZZ->from_int(mpq_denref(MPQ_VAL(q)));
  }

  ring_elem preferred_associate(ring_elem f) const
  {
    mpq_srcptr a = MPQ_VAL(f);
    if (mpq_sgn(a) >= 0) return from_long(1);
    return from_long(-1);
  }

  bool lower_associate_divisor(ring_elem &f, const ring_elem g) const
  {
    mpq_srcptr a = MPQ_VAL(f);
    mpq_srcptr b = MPQ_VAL(g);
    int sa = mpq_sgn(a);
    int sb = mpq_sgn(b);
    int s = (sa == 0 ? sb : sa);
    mpq_ptr result = getmemstructtype(mpq_ptr);
    mpq_init(result);

    mpz_gcd(mpq_numref(result), mpq_numref(a), mpq_numref(b));
    mpz_lcm(mpq_denref(result), mpq_denref(a), mpq_denref(b));
    if (s != mpq_sgn(result)) mpq_neg(result, result);
    mpz_reallocate_limbs(mpq_numref(result));
    mpz_reallocate_limbs(mpq_denref(result));
    f = ring_elem(result);
    return true;  // the answer could become lower, if a newer g has a larger
                  // denom
  }

  void lower_content(ring_elem &c, const ring_elem g) const
  {
    if (is_zero(c))
      {
        c = g;
        return;
      }
    mpq_srcptr a = MPQ_VAL(c);
    mpq_srcptr b = MPQ_VAL(g);
    int sa = mpq_sgn(a);
    mpq_ptr result = getmemstructtype(mpq_ptr);
    mpq_init(result);

    mpz_gcd(mpq_numref(result), mpq_numref(a), mpq_numref(b));
    mpz_lcm(mpq_denref(result), mpq_denref(a), mpq_denref(b));
    if (sa != mpq_sgn(result)) mpq_neg(result, result);
    mpz_reallocate_limbs(mpq_numref(result));
    mpz_reallocate_limbs(mpq_denref(result));
    c = ring_elem(result);
  }
};

template <class RingType>
ConcreteRing<RingType> *ConcreteRing<RingType>::create(
    std::unique_ptr<RingType> R)
{
  auto characteristic = R->characteristic();
  ConcreteRing<RingType> *result = new ConcreteRing<RingType>(std::move(R));
  result->initialize_ring(characteristic);
  result->declare_field();

  result->zeroV = result->from_long(0);
  result->oneV = result->from_long(1);
  result->minus_oneV = result->from_long(-1);

  return result;
}

template <class RingType>
template <class... Args>
ConcreteRing<RingType> *ConcreteRing<RingType>::create(Args &&...args)
{
  return ConcreteRing<RingType>::create(
      std::make_unique<RingType>(std::forward<Args>(args)...));
}

//////////////////////
// promote and lift //
//////////////////////
// This is the second level of dispatch, sending the request
// to the routines in the namespace ARingTranslate

namespace RingPromoter {
  template <typename SourceRing, typename TargetRing>
  bool promoter(const Ring *R,
                const Ring *S,
                const ring_elem fR,
                ring_elem &resultS)
  {
    assert(dynamic_cast<const ConcreteRing<SourceRing> *>(R) != 0);
    assert(dynamic_cast<const ConcreteRing<TargetRing> *>(S) != 0);
    const SourceRing &R1 =
        dynamic_cast<const ConcreteRing<SourceRing> *>(R)->ring();
    const TargetRing &S1 =
        dynamic_cast<const ConcreteRing<TargetRing> *>(S)->ring();

    typename SourceRing::Element fR1(R1);
    typename TargetRing::Element gS1(S1);

    R1.from_ring_elem(fR1, fR);
    bool retval = mypromote(R1, S1, fR1, gS1);
    if (retval) S1.to_ring_elem(resultS, gS1);
    return retval;
  }

  template <typename SourceRing, typename TargetRing>
  bool lifter(const Ring *R,
              const Ring *S,
              ring_elem &result_gR,
              const ring_elem gS)
  {
    assert(dynamic_cast<const ConcreteRing<SourceRing> *>(R) != 0);
    assert(dynamic_cast<const ConcreteRing<TargetRing> *>(S) != 0);
    const SourceRing &R1 =
        dynamic_cast<const ConcreteRing<SourceRing> *>(R)->ring();
    const TargetRing &S1 =
        dynamic_cast<const ConcreteRing<TargetRing> *>(S)->ring();

    typename SourceRing::Element fR1(R1);
    typename TargetRing::Element gS1(S1);

    S1.from_ring_elem(gS1, gS);
    bool retval = mylift(R1, S1, fR1, gS1);  // sets fR1.
    if (retval) R1.to_ring_elem(result_gR, fR1);
    return retval;
  }
};

template <typename RingType>
bool ConcreteRing<RingType>::promote(const Ring *R,
                                     const ring_elem fR,
                                     ring_elem &resultS) const
{
  const Ring *S = this;
  //    fprintf(stderr, "calling promote\n");
  namespace RP = RingPromoter;
  if (R == globalZZ)
    {
      resultS = S->from_int(fR.get_mpz());
      return true;
    }
  if (R == S)
    {
      resultS = copy(fR);
      return true;
    }
  switch (R->ringID())
    {
      case M2::ring_ZZp:
        switch (S->ringID())
          {
            case M2::ring_ZZp:
              return false;
            case M2::ring_ZZpFfpack:
              return RP::promoter<ARingZZp, ARingZZpFFPACK>(R, S, fR, resultS);
            default:
              return false;
          }
        break;
      case M2::ring_ZZpFfpack:
        switch (S->ringID())
          {
            case M2::ring_ZZp:
              return RP::promoter<ARingZZpFFPACK, ARingZZp>(R, S, fR, resultS);
            case M2::ring_ZZpFfpack:
              return RP::promoter<ARingZZpFFPACK, ARingZZpFFPACK>(
                  R, S, fR, resultS);
            default:
              return false;
          }
      case M2::ring_QQ:
        switch (S->ringID())
          {
            case M2::ring_RR:
              return RP::promoter<ARingQQ, ARingRR>(R, S, fR, resultS);
            case M2::ring_RRR:
              return RP::promoter<ARingQQ, ARingRRR>(R, S, fR, resultS);
            case M2::ring_RRi:
              return RP::promoter<ARingQQ, ARingRRi>(R, S, fR, resultS);
            case M2::ring_CC:
              return RP::promoter<ARingQQ, ARingCC>(R, S, fR, resultS);
            case M2::ring_CCC:
              return RP::promoter<ARingQQ, ARingCCC>(R, S, fR, resultS);
            default:
              return false;
          }
      case M2::ring_RR:
        switch (S->ringID())
          {
            case M2::ring_RR:
              return RP::promoter<ARingRR, ARingRR>(R, S, fR, resultS);
            case M2::ring_RRR:
              return RP::promoter<ARingRR, ARingRRR>(R, S, fR, resultS);
            case M2::ring_RRi:
              return RP::promoter<ARingRR, ARingRRi>(R, S, fR, resultS);
            case M2::ring_CC:
              return RP::promoter<ARingRR, ARingCC>(R, S, fR, resultS);
            case M2::ring_CCC:
              return RP::promoter<ARingRR, ARingCCC>(R, S, fR, resultS);
            default:
              return false;
          }
      case M2::ring_RRR:
        switch (S->ringID())
          {
            case M2::ring_RR:
              return RP::promoter<ARingRRR, ARingRR>(R, S, fR, resultS);
            case M2::ring_RRR:
              return RP::promoter<ARingRRR, ARingRRR>(R, S, fR, resultS);
            case M2::ring_RRi:
              return RP::promoter<ARingRRR, ARingRRi>(R, S, fR, resultS);
            case M2::ring_CC:
              return RP::promoter<ARingRRR, ARingCC>(R, S, fR, resultS);
            case M2::ring_CCC:
              return RP::promoter<ARingRRR, ARingCCC>(R, S, fR, resultS);
            default:
              return false;
          }
      case M2::ring_RRi:
        switch (S->ringID())
           {
              case M2::ring_RR:
                return RP::promoter<ARingRRi, ARingRR>(R, S, fR, resultS);
              case M2::ring_RRR:
                return RP::promoter<ARingRRi, ARingRRR>(R, S, fR, resultS);
              case M2::ring_RRi:
                return RP::promoter<ARingRRi, ARingRRi>(R, S, fR, resultS);
              default:
                 return false;
          }
      case M2::ring_CC:
        switch (S->ringID())
          {
            case M2::ring_CC:
              return RP::promoter<ARingCC, ARingCC>(R, S, fR, resultS);
            case M2::ring_CCC:
              return RP::promoter<ARingCC, ARingCCC>(R, S, fR, resultS);
            default:
              return false;
          }
      case M2::ring_CCC:
        switch (S->ringID())
          {
            case M2::ring_CCC:
              return RP::promoter<ARingCCC, ARingCCC>(R, S, fR, resultS);
            case M2::ring_CC:
              return RP::promoter<ARingCCC, ARingCC>(R, S, fR, resultS);
            default:
              return false;
          }
      default:
        break;
    };
  return false;
}

// given a natural map: R --> S = this,
// 'lift' sets result_gR with an element which maps to gS, if possible.
// true is returned iff this is possible.
template <typename RingType>
bool ConcreteRing<RingType>::lift(const Ring *R,
                                  const ring_elem gS,
                                  ring_elem &result_gR) const
{
  const Ring *S = this;

  namespace RP = RingPromoter;
  if (R == S)
    {
      result_gR = gS;
      return true;
    }
  if (R == globalZZ)
    {
      printf("error!! lift called with no ZZ lifting method\n");
      // MES:TODO!! WRITE ME
      return true;
    }
  switch (R->ringID())
    {
      case M2::ring_ZZp:
        switch (S->ringID())
          {
            case M2::ring_ZZp:
              return false;
            case M2::ring_ZZpFfpack:
              return RP::lifter<ARingZZp, ARingZZpFFPACK>(R, S, result_gR, gS);
            default:
              return false;
          }
        break;
      case M2::ring_ZZpFfpack:
        switch (S->ringID())
          {
            case M2::ring_ZZp:
              return RP::lifter<ARingZZpFFPACK, ARingZZp>(R, S, result_gR, gS);
            case M2::ring_ZZpFfpack:
              return RP::lifter<ARingZZpFFPACK, ARingZZpFFPACK>(
                  R, S, result_gR, gS);
            default:
              return false;
          }
      case M2::ring_RR:
        switch (S->ringID())
          {
            case M2::ring_RR:
              return RP::lifter<ARingRR, ARingRR>(R, S, result_gR, gS);
            case M2::ring_RRR:
              return RP::lifter<ARingRR, ARingRRR>(R, S, result_gR, gS);
            case M2::ring_RRi:
              return RP::lifter<ARingRR, ARingRRi>(R, S, result_gR, gS);
            case M2::ring_CC:
              return RP::lifter<ARingRR, ARingCC>(R, S, result_gR, gS);
            case M2::ring_CCC:
              return RP::lifter<ARingRR, ARingCCC>(R, S, result_gR, gS);
            default:
              return false;
          }
      case M2::ring_RRR:
        switch (S->ringID())
          {
            case M2::ring_RR:
              return RP::lifter<ARingRRR, ARingRR>(R, S, result_gR, gS);
            case M2::ring_RRR:
              return RP::lifter<ARingRRR, ARingRRR>(R, S, result_gR, gS);
            case M2::ring_RRi:
              return RP::lifter<ARingRRR, ARingRRi>(R, S, result_gR, gS);
            case M2::ring_CC:
              return RP::lifter<ARingRRR, ARingCC>(R, S, result_gR, gS);
            case M2::ring_CCC:
              return RP::lifter<ARingRRR, ARingCCC>(R, S, result_gR, gS);
            default:
              return false;
          }
      case M2::ring_CC:
        switch (S->ringID())
          {
            case M2::ring_CC:
              return RP::lifter<ARingCC, ARingCC>(R, S, result_gR, gS);
            case M2::ring_CCC:
              return RP::lifter<ARingCCC, ARingCC>(R, S, result_gR, gS);
            default:
              return false;
          }
      case M2::ring_CCC:
        switch (S->ringID())
          {
            case M2::ring_CC:
              return RP::lifter<ARingCC, ARingCCC>(R, S, result_gR, gS);
            case M2::ring_CCC:
              return RP::lifter<ARingCCC, ARingCCC>(R, S, result_gR, gS);
            default:
              return false;
          }
      default:
#ifndef NDEBUG
        fprintf(stderr,
                "oh no: rings not in list\n, R->ringID()=%d S->ringID()=%d\n",
                R->ringID(),
                S->ringID());
#endif
        break;
    };
  return false;
}

// Note: the only promotion to 'this' allowed is ZZ --> this, which is covered
// above this code.
// So there is no need to provide a separate 'promote' function

template <typename RingType>
bool liftToInt(const RingType &R,
               const Ring *Rg,
               const ring_elem f,
               ring_elem &result)
{
  if (Rg == globalZZ)
    {
      typename RingType::Element a(R);
      R.from_ring_elem(a, f);
      result = Rg->from_long(R.coerceToLongInteger(a));
      return true;
    }
  return false;
}

template <>
inline bool ConcreteRing<ARingZZpFlint>::lift(const Ring *Rg,
                                              const ring_elem f,
                                              ring_elem &result) const
{
  return liftToInt(ring(), Rg, f, result);
}
template <>
inline bool ConcreteRing<ARingZZp>::lift(const Ring *Rg,
                                         const ring_elem f,
                                         ring_elem &result) const
{
  return liftToInt(ring(), Rg, f, result);
}
template <>
inline bool ConcreteRing<ARingZZpFFPACK>::lift(const Ring *Rg,
                                               const ring_elem f,
                                               ring_elem &result) const
{
  return liftToInt(ring(), Rg, f, result);
}

template <>
inline bool ConcreteRing<ARingGFM2>::promote(const Ring *Rf,
                                             const ring_elem f,
                                             ring_elem &result) const
{
  // Rf = Z/p[x]/F(x) ---> GF(p,n)
  // promotion: need to be able to know the value of 'x'.
  // lift: need to compute (primite_element)^e

  ElementType a;
  bool retval = R->promote(Rf, f, a);
  R->to_ring_elem(result, a);
  return retval;
}

template <>
inline bool ConcreteRing<ARingGFM2>::lift(const Ring *Rg,
                                          const ring_elem f,
                                          ring_elem &result) const
{
  // Rf = Z/p[x]/F(x) ---> GF(p,n)
  // promotion: need to be able to know the value of 'x'.
  // lift: need to compute (primite_element)^e

  ElementType a;
  R->from_ring_elem(a, f);
  bool retval = R->lift(Rg, a, result);
  return retval;
}

template <>
inline bool ConcreteRing<ARingGFFlintBig>::promote(const Ring *Rf,
                                                   const ring_elem f,
                                                   ring_elem &result) const
{
  Element a(ring());
  bool retval = ring().promote(Rf, f, a);
  ring().to_ring_elem(result, a);
  return retval;
}

template <>
inline bool ConcreteRing<ARingGFFlintBig>::lift(const Ring *Rg,
                                                const ring_elem f,
                                                ring_elem &result) const
{
  Element a(ring());
  ring().from_ring_elem(a, f);
  bool retval = ring().lift(Rg, a, result);
  return retval;
}

template <>
inline bool ConcreteRing<ARingGFFlint>::promote(const Ring *Rf,
                                                const ring_elem f,
                                                ring_elem &result) const
{
  Element a(ring());
  bool retval = ring().promote(Rf, f, a);
  ring().to_ring_elem(result, a);
  return retval;
}

template <>
inline bool ConcreteRing<ARingGFFlint>::lift(const Ring *Rg,
                                             const ring_elem f,
                                             ring_elem &result) const
{
  Element a(ring());
  ring().from_ring_elem(a, f);
  bool retval = ring().lift(Rg, a, result);
  return retval;
}

template <>
inline bool ConcreteRing<ARingQQ>::lift(const Ring *Rg,
                                        const ring_elem f,
                                        ring_elem &result) const
{
  if (Rg == globalZZ)
    {
      mpz_t b;
      mpz_init(b);

      Element a(*R);
      R->from_ring_elem(a, f);

      bool retval = R->lift_to_mpz(b, a);
      if (retval)
        {
          result = globalZZ->from_int(b);
        }
      mpz_clear(b);
      return retval;
    }
  return false;
}

template <typename RingType>
ring_elem ConcreteRing<RingType>::zeroize_tiny(gmp_RR epsilon,
                                               const ring_elem f) const
{
  return f;
}

template <typename RingType>
void ConcreteRing<RingType>::increase_maxnorm(gmp_RRmutable norm,
                                              const ring_elem f) const
{
  // do nothing by default
}

template <>
inline ring_elem ConcreteRing<ARingRR>::zeroize_tiny(gmp_RR epsilon,
                                                     const ring_elem f) const
{
  Element a(*R);
  R->from_ring_elem(a, f);
  R->zeroize_tiny(epsilon, a);
  ring_elem result;
  R->to_ring_elem(result, a);
  return result;
}

template <>
inline ring_elem ConcreteRing<ARingCC>::zeroize_tiny(gmp_RR epsilon,
                                                     const ring_elem f) const
{
  Element a(*R);
  R->from_ring_elem(a, f);
  R->zeroize_tiny(epsilon, a);
  ring_elem result;
  R->to_ring_elem(result, a);
  return result;
}

template <>
inline ring_elem ConcreteRing<ARingRRR>::zeroize_tiny(gmp_RR epsilon,
                                                      const ring_elem f) const
{
  Element a(*R);
  R->from_ring_elem(a, f);
  R->zeroize_tiny(epsilon, a);
  ring_elem result;
  R->to_ring_elem(result, a);
  return result;
}

template <>
inline ring_elem ConcreteRing<ARingCCC>::zeroize_tiny(gmp_RR epsilon,
                                                      const ring_elem f) const
{
  Element a(*R);
  R->from_ring_elem(a, f);
  R->zeroize_tiny(epsilon, a);
  ring_elem result;
  R->to_ring_elem(result, a);
  return result;
}

template <>
inline void ConcreteRing<ARingRR>::increase_maxnorm(gmp_RRmutable norm,
                                                    const ring_elem f) const
{
  ARingRR::Element a(*R);  // will be the norm
  const ElementType &b = R->from_ring_elem_const(f);
  R->abs(a, b);
  if (mpfr_cmp_d(norm, a) < 0) mpfr_set_d(norm, a, MPFR_RNDN);
}

template <>
inline void ConcreteRing<ARingCC>::increase_maxnorm(gmp_RRmutable norm,
                                                    const ring_elem f) const
{
  const ARingRR &realR = R->real_ring();
  ARingRR::Element a(realR);
  const ElementType &b = R->from_ring_elem_const(f);
  R->abs(a, b);
  if (mpfr_cmp_d(norm, a) < 0) mpfr_set_d(norm, a, MPFR_RNDN);
}

template <>
inline void ConcreteRing<ARingRRR>::increase_maxnorm(gmp_RRmutable norm,
                                                     const ring_elem f) const
{
  ARingRRR::Element a(*R);  // will be the norm
  const ElementType &b = R->from_ring_elem_const(f);
  R->abs(a, b);
  if (mpfr_cmp(&a.value(), norm) > 0) mpfr_set(norm, &a.value(), MPFR_RNDN);
}

template <>
inline void ConcreteRing<ARingCCC>::increase_maxnorm(gmp_RRmutable norm,
                                                     const ring_elem f) const
{
  const ARingRRR &realR = R->real_ring();
  ARingRRR::Element a(realR);
  const ElementType &b = R->from_ring_elem_const(f);
  R->abs(a, b);
  if (mpfr_cmp(&a.value(), norm) > 0) mpfr_set(norm, &a.value(), MPFR_RNDN);
}

template <typename RingType>
inline unsigned long ConcreteRing<RingType>::get_precision() const
{
  return 0;
}

template <>
inline unsigned long ConcreteRing<ARingRR>::get_precision() const
{
  return R->get_precision();
}

template <>
inline unsigned long ConcreteRing<ARingCC>::get_precision() const
{
  return R->get_precision();
}

template <>
inline unsigned long ConcreteRing<ARingRRR>::get_precision() const
{
  return R->get_precision();
}
    
template <>
inline unsigned long ConcreteRing<ARingRRi>::get_precision() const
{
  return R->get_precision();
}

template <>
inline unsigned long ConcreteRing<ARingCCC>::get_precision() const
{
  return R->get_precision();
}

template <typename RT>
std::pair<bool, long> coerceToLongIntegerFcn(const RT &ring, ring_elem a)
{
  const typename RT::ElementType &b = ring.from_ring_elem_const(a);
  long result = ring.coerceToLongInteger(b);
  return std::pair<bool, long>(true, result);
}

template <>
inline std::pair<bool, long> ConcreteRing<ARingZZp>::coerceToLongInteger(
    ring_elem a) const
{
  return coerceToLongIntegerFcn(ring(), a);
}

template <>
inline std::pair<bool, long> ConcreteRing<ARingZZpFlint>::coerceToLongInteger(
    ring_elem a) const
{
  return coerceToLongIntegerFcn(ring(), a);
}

template <>
inline std::pair<bool, long> ConcreteRing<ARingZZpFFPACK>::coerceToLongInteger(
    ring_elem a) const
{
  return coerceToLongIntegerFcn(ring(), a);
}

template <>
inline std::pair<bool, long> ConcreteRing<ARingZZGMP>::coerceToLongInteger(
    ring_elem a) const
{
  long b;
  bool succeed = ring().coerceToLongInteger(b, *a.get_mpz());
  return std::pair<bool, long>(succeed, b);
}

template <>
inline std::pair<bool, long> ConcreteRing<ARingZZ>::coerceToLongInteger(
    ring_elem a) const
{
  long b;
  fmpz t = reinterpret_cast<fmpz>(const_cast<Nterm *>(a.poly_val));
  bool succeed = ring().coerceToLongInteger(b, t);
  return std::pair<bool, long>(succeed, b);
}

inline const RingElement *findMinimalPolynomial(const PolynomialRing &originalR)
{
  const PolynomialRing *R = originalR.getAmbientRing();
  ring_elem f = R->copy(originalR.quotient_element(0));
  return RingElement::make_raw(R, f);
}

template <>
inline const RingElement *ConcreteRing<ARingGFM2>::getMinimalPolynomial() const
{
  return findMinimalPolynomial(ring().originalRing());
}

template <>
inline const RingElement *ConcreteRing<ARingGFFlintBig>::getMinimalPolynomial()
    const
{
  return findMinimalPolynomial(ring().originalRing());
}

template <>
inline const RingElement *ConcreteRing<ARingGFFlint>::getMinimalPolynomial()
    const
{
  return findMinimalPolynomial(ring().originalRing());
}

template <typename RT>
inline const RingElement *getLiftedRepresentation(const RT &R,
                                                  const ring_elem &a)
// R should be a Galois Field
{
  ring_elem c;                // in originalRing()
  typename RT::Element b(R);  // in ring()
  R.from_ring_elem(b, a);
  R.lift_to_original_ring(c, b);
  return RingElement::make_raw(&R.originalRing(), c);
}

template <>
inline const RingElement *ConcreteRing<ARingGFM2>::getRepresentation(
    const ring_elem &a) const
{
  return getLiftedRepresentation<ARingGFM2>(ring(), a);
}

template <>
inline const RingElement *ConcreteRing<ARingGFFlintBig>::getRepresentation(
    const ring_elem &a) const
{
  return getLiftedRepresentation<ARingGFFlintBig>(ring(), a);
}
template <>
inline const RingElement *ConcreteRing<ARingGFFlint>::getRepresentation(
    const ring_elem &a) const
{
  return getLiftedRepresentation<ARingGFFlint>(ring(), a);
}

template <typename ConcreteRT>
inline const RingElement *getGen(const ConcreteRT &R)
// R should be a Galois Field
{
  typename ConcreteRT::Element a(R.ring());
  ring_elem b;
  R.ring().getGenerator(a);
  R.ring().to_ring_elem(b, a);
  return RingElement::make_raw(&R, b);
}

template <>
inline const RingElement *ConcreteRing<ARingZZpFlint>::getGenerator() const
{
  return getGen<ConcreteRing<ARingZZpFlint> >(*this);
}
template <>
inline const RingElement *ConcreteRing<ARingGFM2>::getGenerator() const
{
  return getGen<ConcreteRing<ARingGFM2> >(*this);
}
template <>
inline const RingElement *ConcreteRing<ARingGFFlint>::getGenerator() const
{
  return getGen<ConcreteRing<ARingGFFlint> >(*this);
}
template <>
inline const RingElement *ConcreteRing<ARingGFFlintBig>::getGenerator() const
{
  return getGen<ConcreteRing<ARingGFFlintBig> >(*this);
}

template <>
inline long ConcreteRing<ARingZZpFlint>::discreteLog(const ring_elem &a1) const
{
  const ElementType &a = ring().from_ring_elem_const(a1);
  long result = ring().discreteLog(a);
  return result;
}

};  // namespace M2

#include "aring-qq.hpp"
typedef M2::RingQQ RingQQ;
extern void initializeRationalRing();
extern const RingQQ *globalQQ;
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
