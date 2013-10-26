// Copyright 2011 Michael E. Stillman

#ifndef _ring_glue_hh_
#define _ring_glue_hh_

#include "aring.hpp"
#include "aring-translate.hpp"
#include "ring.hpp"

#include "mutablemat.hpp"

static const bool displayArithmeticCalls = false;

#define COERCE_RING(RingType,R) dynamic_cast<const RingType *>(R)

namespace M2 {
/**
    @ingroup rings
*/
  template <class RingType>
  class ConcreteRing : public Ring
  {
    const RingType *R;
    ConcreteRing(const RingType *R0) : R(R0) {}
    virtual ~ConcreteRing() {}
  public:
    typedef typename RingType::ElementType ElementType;

    static ConcreteRing<RingType> * create(const RingType *R);

    virtual M2::RingID ringID() const { return RingType::ringID; }

    const RingType & ring() const { return *R; }

    /// Create either a dense or sparse MutableMatrix of the given size
    virtual MutableMatrix* makeMutableMatrix(size_t nrows, size_t ncols, bool dense) const
    {
      if (dense)
        return new MutableMat< DMat<RingType> >(this, R, nrows, ncols);
      else
        return new MutableMat< SMat<RingType> >(this, R, nrows, ncols);
    }

    ////////////////////////////
    // Functions on elements ///
    ////////////////////////////
    virtual unsigned long compute_hash_value(ring_elem a) const
    {
      ElementType b;
      ring().init(b);
      ring().from_ring_elem(b, a);
      unsigned long result = ring().computeHashValue(b);
      ring().clear(b);
      return result;
    }

    virtual int coerce_to_int(ring_elem a) const
    {
      //TODO: implement or remove
      return 0;
    }

    // The following are all the routines required by 'ring'
    virtual void text_out(buffer &o) const { return R->text_out(o); }

    virtual ring_elem from_int(int n) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling from_int\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_from_int(a,n);
      R->to_ring_elem(result, a);
      R->clear(a);
      return result;
    }

    virtual ring_elem from_int(mpz_ptr n) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling from_int(mpz)\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_from_mpz(a,n);
      R->to_ring_elem(result, a);
      R->clear(a);
      return result;
    }
    virtual ring_elem from_rational(mpq_ptr q) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling from_rational\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_from_mpq(a,q);
      R->to_ring_elem(result, a);
      R->clear(a);
      return result;
    }
    virtual bool from_BigReal(gmp_RR q, ring_elem &result) const
    {
      ElementType a;
      R->init(a);
      bool ret = get_from_BigReal(*R,a,q);
      if (ret) R->to_ring_elem(result, a);
      R->clear(a);
      return ret;
    }
    virtual bool from_BigComplex(gmp_CC q, ring_elem &result) const
    {
      ElementType a;
      R->init(a);
      //      bool ret = R->set_from_BigComplex(a,q);
      bool ret = get_from_BigComplex(*R,a,q);
      if (ret) R->to_ring_elem(result, a);
      R->clear(a);
      return ret;
    }

    virtual ring_elem var(int v) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling var\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_var(a,v);
      R->to_ring_elem(result, a);
      R->clear(a);
      return result;
    }

    /** If there is a "natural" map S --> R=this, where f is an element of
    // S , then result is set to the image of f, and true is returned.
    // Otherwise, false is returned.
    //
    // The map must one-step, e.g. for k --> k[x] --> k[x][y], promotion
    // must be done with two consecutive calls to promote(with different arguments).
    // Examples of natural maps:
    //  ZZ --> R, for any R
    //  QQ --> RR --> CC
    //  ZZ --> ZZ/p
    //  ZZ/p --> GF(p^n)
    //  GF(p^m) --> GF(p^n), where m|n
    //  A --> A[vars]/I
    //  A[vars]/J --> A[vars]/I  (assumption: I contains J).
    */

    virtual bool promote(const Ring *S, const ring_elem f, ring_elem &result) const;

    //* If there is a "natural" map S --> R=this, where f is an element of
    // R, then result is set to an element f in S which maps to f, and true is returned.
    // Otherwise, false is returned.
    //
    // For examples of maps, see 'promote'.
    //
    // Lifting elements of ZZ/p to QQ does not count, as there is no actual homomorphism
    // from QQ --> ZZ/p

    virtual bool lift(const Ring *S, const ring_elem f, ring_elem &result) const;

    virtual bool is_unit(const ring_elem f) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling is_unit\n");
      ElementType a;
      R->init(a);
      R->from_ring_elem(a, f);
      bool ret = R->is_unit(a);
      R->clear(a);
      return ret;
    }

    virtual bool is_zero(const ring_elem f) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling is_zero\n");
      ElementType a;
      R->init(a);
      R->from_ring_elem(a, f);
      bool ret = R->is_zero(a);
      R->clear(a);
      return ret;
    }

    virtual bool is_equal(const ring_elem f, const ring_elem g) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling is_equal\n");
      ElementType a, b;
      R->init(a);
      R->init(b);
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      bool ret = R->is_equal(a,b);
      R->clear(a);
      R->clear(b);
      return ret;
    }

    virtual int compare_elems(const ring_elem f, const ring_elem g) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling compare_elems\n");
      ElementType a, b;
      R->init(a);
      R->init(b);
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      int ret = R->compare_elems(a,b);
      R->clear(a);
      R->clear(b);
      return ret;
    }

    virtual ring_elem copy(const ring_elem f) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling copy\n");
      ElementType a,b;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->from_ring_elem(a, f);
      R->set(b,a);
      R->to_ring_elem(result,b);
      R->clear(a);
      R->clear(b);
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
      ElementType a,b;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->from_ring_elem(a, f);
      R->negate(b,a);
      R->to_ring_elem(result,b);
      R->clear(a);
      R->clear(b);
      return result;
    }

    virtual ring_elem add(const ring_elem f, const ring_elem g) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling add\n");
      ElementType a, b, c;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->init(c);
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      R->add(c,a,b);
      R->to_ring_elem(result,c);
      R->clear(a);
      R->clear(b);
      R->clear(c);
      return result;
    }

    virtual ring_elem subtract(const ring_elem f, const ring_elem g) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling subtract\n");
      ElementType a, b, c;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->init(c);
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      R->subtract(c,a,b);
      R->to_ring_elem(result,c);
      R->clear(a);
      R->clear(b);
      R->clear(c);
      return result;
    }

    virtual ring_elem mult(const ring_elem f, const ring_elem g) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling mult\n");
      ElementType a, b, c;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->init(c);
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      R->mult(c,a,b);
      R->to_ring_elem(result,c);
      R->clear(a);
      R->clear(b);
      R->clear(c);
      return result;
    }

    virtual ring_elem power(const ring_elem f, mpz_t n) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling power mpz\n");
      ElementType a,b;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->from_ring_elem(a, f);
      R->power_mpz(b,a,n);
      R->to_ring_elem(result,b);
      R->clear(a);
      R->clear(b);
      return result;
    }

    virtual ring_elem power(const ring_elem f, int n) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling power int\n");
      ElementType a,b;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->from_ring_elem(a, f);
      R->power(b,a,n);
      R->to_ring_elem(result,b);
      R->clear(a);
      R->clear(b);
      return result;
    }

    virtual ring_elem invert(const ring_elem f) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling invert\n");
      ElementType a,b;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->from_ring_elem(a, f);
      R->invert(b,a);
      R->to_ring_elem(result,b);
      R->clear(a);
      R->clear(b);
      return result;
    }

    virtual ring_elem divide(const ring_elem f, const ring_elem g) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling divide\n");
      ElementType a, b, c;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->init(c);
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      R->divide(c,a,b);
      R->to_ring_elem(result,c);
      R->clear(a);
      R->clear(b);
      R->clear(c);
      return result;
    }

    virtual void syzygy(const ring_elem f, const ring_elem g,
                        ring_elem &x, ring_elem &y) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling syzygy\n");
      ElementType a, b, xe, ye;
      R->init(a);
      R->init(b);
      R->init(xe);
      R->init(ye);
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      R->syzygy(a,b, xe,ye);
      R->to_ring_elem(x,xe);
      R->to_ring_elem(y,ye);
      R->clear(a);
      R->clear(b);
      R->clear(xe);
      R->clear(ye);
    }

    virtual ring_elem random() const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling random\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->random(a);
      R->to_ring_elem(result,a);
      R->clear(a);
      return result;
    }

    virtual void elem_text_out(buffer &o,
                               const ring_elem f,
                               bool p_one=true,
                               bool p_plus=false,
                               bool p_parens=false) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling elem_text_out\n");
      ElementType a;
      R->init(a);
      R->from_ring_elem(a, f);
      R->elem_text_out(o,a,p_one,p_plus,p_parens);
      R->clear(a);
    }

    // map : this = R --> S, f in R
    // sending primelem --> map->elem(firstvar)
    // return map(f) as a ring_elem in S
    virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const
    {
      ElementType a;
      ring_elem result;
      R->init(a);
      R->from_ring_elem(a, f);
      R->eval(map, a, first_var, result);
      R->clear(a);
      return result;
    }

    //TODO: look again at the next two when ring_elem is phased out
    virtual ring_elem zeroize_tiny(gmp_RR epsilon, const ring_elem f) const;
    virtual void increase_maxnorm(gmp_RR norm, const ring_elem f) const; 

    virtual unsigned long get_precision() const;  // if the ring is not over RRR/CCC returns 0

  }; // class ConcreteRing<RingType>


  template<class RingType>
  ConcreteRing<RingType> * ConcreteRing<RingType>::create(const RingType *R)
  {
    ConcreteRing<RingType> *result = new ConcreteRing<RingType>(R);
    result->initialize_ring(static_cast<int>(R->characteristic()));
    result->declare_field();

    result->zeroV = result->from_int(0);
    result->oneV = result->from_int(1);
    result->minus_oneV = result->from_int(-1);

    return result;
  }

  //////////////////////
  // promote and lift //
  //////////////////////
  // This is the second level of dispatch, sending the request
  // to the routines in the namespace ARingTranslate
  //  namespace ARingTranslate;

  namespace RingPromoter {
    template<typename SourceRing, typename TargetRing>
    bool promoter(const Ring *R, const Ring *S, const ring_elem fR, ring_elem &resultS)
    {
      M2_ASSERT(dynamic_cast< const ConcreteRing<SourceRing> * >(R) != 0);
      M2_ASSERT(dynamic_cast< const ConcreteRing<TargetRing> * >(S) != 0);
      const SourceRing &R1 = dynamic_cast< const ConcreteRing<SourceRing> * >(R)->ring();
      const TargetRing &S1 = dynamic_cast< const ConcreteRing<TargetRing> * >(S)->ring();
    
      typename SourceRing::ElementType fR1;
      typename TargetRing::ElementType gS1;
    
      R1.init(fR1);
      S1.init(gS1);
      R1.from_ring_elem(fR1, fR);
      bool retval = mypromote(R1,S1,fR1,gS1);
      if (retval)
        S1.to_ring_elem(resultS, gS1);
      R1.clear(fR1);
      S1.clear(gS1);
      return retval;
    }

    template<typename SourceRing, typename TargetRing>
    bool lifter(const Ring *R, const Ring *S, ring_elem &result_gR, const ring_elem gS)
    {
      M2_ASSERT(dynamic_cast< const ConcreteRing<SourceRing> * >(R) != 0);
      M2_ASSERT(dynamic_cast< const ConcreteRing<TargetRing> * >(S) != 0);
      const SourceRing &R1 = dynamic_cast< const ConcreteRing<SourceRing> * >(R)->ring();
      const TargetRing &S1 = dynamic_cast< const ConcreteRing<TargetRing> * >(S)->ring();
    
      typename SourceRing::ElementType fR1;
      typename TargetRing::ElementType gS1;
    
      R1.init(fR1);
      S1.init(gS1);
      S1.from_ring_elem(gS1, gS);
      bool retval = mylift(R1,S1,fR1,gS1); // sets fR1.
      if (retval)
        R1.to_ring_elem(result_gR, fR1);
      R1.clear(fR1);
      S1.clear(gS1);
      return retval;
    }

  };

  template<typename RingType>
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
    switch (R->ringID()) {
    case M2::ring_ZZp:
      switch (S->ringID()) {
      case M2::ring_ZZp: return false;
      case M2::ring_GFGivaro: return RP::promoter<ARingZZp,ARingGFGivaro>(R,S,fR,resultS);
      case M2::ring_ZZpFfpack: return RP::promoter<ARingZZp,ARingZZpFFPACK>(R,S,fR,resultS);
      default: return false;
      }
      break;
    case M2::ring_GFGivaro:
      switch (S->ringID()) {
      case M2::ring_ZZp: return RP::promoter<ARingGFGivaro,ARingZZp>(R,S,fR,resultS);
      case M2::ring_GFGivaro: return RP::promoter<ARingGFGivaro,ARingGFGivaro>(R,S,fR,resultS);
      case M2::ring_ZZpFfpack: return RP::promoter<ARingGFGivaro,ARingZZpFFPACK>(R,S,fR,resultS);
      default: return false;
      }
    case M2::ring_ZZpFfpack:
      switch (S->ringID()) {
      case M2::ring_ZZp: return RP::promoter<ARingZZpFFPACK,ARingZZp>(R,S,fR,resultS);
      case M2::ring_GFGivaro: return RP::promoter<ARingZZpFFPACK,ARingGFGivaro>(R,S,fR,resultS);
      case M2::ring_ZZpFfpack: return RP::promoter<ARingZZpFFPACK,ARingZZpFFPACK>(R,S,fR,resultS);
      default: return false;
      }
    case M2::ring_RRR:
      switch (S->ringID()) {
      case M2::ring_RRR: return RP::promoter<ARingRRR,ARingRRR>(R,S,fR,resultS);
      case M2::ring_CCC: return RP::promoter<ARingRRR,ARingCCC>(R,S,fR,resultS);
      default: return false;
      }
    case M2::ring_CCC:
      switch (S->ringID()) {
      case M2::ring_CCC: return RP::promoter<ARingCCC,ARingCCC>(R,S,fR,resultS);
      default: return false;
      }
    default:
      break;
    };
    return false;
  }

  // given natural map : this = S --> R
  // this sets result_gR with an element which maps to gS, if possible.
  // true is returned iff this is possible.
  template<typename RingType>
  bool ConcreteRing<RingType>::lift(const Ring *R, 
                                    const ring_elem gS,
                                    ring_elem &result_gR) const
  {
    const Ring *S = this;
    fprintf(stderr, "calling lift\n");
    namespace RP = RingPromoter;
    if (R == globalZZ)
      {
        // Need a method in S for lifting to an int...
        //        resultS = S->from_int(fR.get_mpz());
        return true;
      }
    if (R == S)
      {
        result_gR = gS;
        return true;
      }
    switch (R->ringID()) {
    case M2::ring_ZZp:
      switch (S->ringID()) {
      case M2::ring_ZZp: return false;
      case M2::ring_GFGivaro: return RP::lifter<ARingZZp,ARingGFGivaro>(R,S,result_gR,gS);
      case M2::ring_ZZpFfpack: return RP::lifter<ARingZZp,ARingZZpFFPACK>(R,S,result_gR,gS);
      default: return false;
      }
      break;
    case M2::ring_GFGivaro:
      switch (S->ringID()) {
      case M2::ring_ZZp: return RP::lifter<ARingGFGivaro,ARingZZp>(R,S,result_gR,gS);
      case M2::ring_GFGivaro: return RP::lifter<ARingGFGivaro,ARingGFGivaro>(R,S,result_gR,gS);
      case M2::ring_ZZpFfpack: return RP::lifter<ARingGFGivaro,ARingZZpFFPACK>(R,S,result_gR,gS);
      default: return false;
      }
    case M2::ring_ZZpFfpack:
      switch (S->ringID()) {
      case M2::ring_ZZp: return RP::lifter<ARingZZpFFPACK,ARingZZp>(R,S,result_gR,gS);
      case M2::ring_GFGivaro: return RP::lifter<ARingZZpFFPACK,ARingGFGivaro>(R,S,result_gR,gS);
      case M2::ring_ZZpFfpack: return RP::lifter<ARingZZpFFPACK,ARingZZpFFPACK>(R,S,result_gR,gS);
      default: return false;
      }
    case M2::ring_RRR:
      switch (S->ringID()) {
      case M2::ring_RRR: return RP::lifter<ARingRRR,ARingRRR>(R,S,result_gR,gS);
      case M2::ring_CCC: return RP::lifter<ARingRRR,ARingCCC>(R,S,result_gR,gS);
      default: return false;
      }
    case M2::ring_CCC:
      switch (S->ringID()) {
      case M2::ring_CCC: return RP::lifter<ARingCCC,ARingCCC>(R,S,result_gR,gS);
      default: return false;
      }
    default:
      break;
    };
    return false;
  }

  template<>
  inline bool ConcreteRing<ARingGFM2>::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
  {
    // Rf = Z/p[x]/F(x) ---> GF(p,n)
    // promotion: need to be able to know the value of 'x'.
    // lift: need to compute (primite_element)^e

    ElementType a;
    bool retval = R->promote(Rf,f,a);
    R->to_ring_elem(result, a);
    return retval;
  }

  template<>
  inline bool ConcreteRing<ARingGFM2>::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
  {
    // Rf = Z/p[x]/F(x) ---> GF(p,n)
    // promotion: need to be able to know the value of 'x'.
    // lift: need to compute (primite_element)^e

    ElementType a;
    R->from_ring_elem(a, f);
    bool retval = R->lift(Rg,a,result);
    return retval;
  }

  template<>
  inline bool ConcreteRing<ARingGFGivaro>::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
  {
    // Rf = Z/p[x]/F(x) ---> GF(p,n)
    // promotion: need to be able to know the value of 'x'.
    // lift: need to compute (primite_element)^e

    ElementType a;
    bool retval = R->promote(Rf,f,a);
    R->to_ring_elem(result, a);
    return retval;
  }

  template<>
  inline bool ConcreteRing<ARingGFGivaro>::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
  {
    // Rf = Z/p[x]/F(x) ---> GF(p,n)
    // promotion: need to be able to know the value of 'x'.
    // lift: need to compute (primite_element)^e

    ElementType a;
    R->init(a);
    R->from_ring_elem(a, f);
    bool retval = R->lift(Rg,a,result);
    R->clear(a);
    return retval;
  }

  template<typename RingType>
  ring_elem ConcreteRing<RingType>::zeroize_tiny(gmp_RR epsilon, const ring_elem f) const
  {
    return f;
  }

  template<typename RingType>
  void ConcreteRing<RingType>::increase_maxnorm(gmp_RR norm, const ring_elem f) const
  {
    // do nothing by default
  }

  template<>
  inline ring_elem ConcreteRing<ARingRRR>::zeroize_tiny(gmp_RR epsilon, const ring_elem f) const
  {
    ElementType a;
    R->init(a);
    R->from_ring_elem(a, f);
    R->zeroize_tiny(epsilon,a);
    ring_elem result;
    R->to_ring_elem(result,a);
    R->clear(a);
    return result;
  }

  template<>
  inline ring_elem ConcreteRing<ARingCCC>::zeroize_tiny(gmp_RR epsilon, const ring_elem f) const
  {
    ElementType a;
    R->init(a);
    R->from_ring_elem(a, f);
    R->zeroize_tiny(epsilon,a);
    ring_elem result;
    R->to_ring_elem(result,a);
    R->clear(a);
    return result;
  }

  template<>
  inline void ConcreteRing<ARingRRR>::increase_maxnorm(gmp_RR norm, const ring_elem f) const
  {
    ARingRRR::ElementType a;
    ElementType b;
    R->init(a); // will be the norm
    R->init(b);
    R->from_ring_elem(b,f);
    R->abs(a,b);
    if (mpfr_cmp(&a, norm)>0)
      mpfr_set(norm, &a, GMP_RNDN);
    R->clear(b);
    R->clear(a);
  }

  template<>
  inline void ConcreteRing<ARingCCC>::increase_maxnorm(gmp_RR norm, const ring_elem f) const
  {
    const ARingRRR& realR = R->real_ring();
    ARingRRR::ElementType a;
    ElementType b;
    realR.init(a);
    R->init(b);
    R->from_ring_elem(b,f);
    R->abs(a,b);
    if (mpfr_cmp(&a, norm)>0)
      mpfr_set(norm, &a, GMP_RNDN);
    R->clear(b);
    realR.clear(a);
  }

  template<typename RingType>
  inline unsigned long ConcreteRing<RingType>::get_precision() const
  {
    return 0;
  }

  template<>
  inline unsigned long ConcreteRing<ARingRRR>::get_precision() const
  {
    return R->get_precision();
  }

  template<>
  inline unsigned long ConcreteRing<ARingCCC>::get_precision() const
  {
    return R->get_precision();
  }

}; // namespace M2

#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
