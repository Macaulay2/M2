// Copyright 2011 Michael E. Stillman

#include "aring-promoter.hpp"

namespace M2 {  
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

  template<typename RingType>
  bool ConcreteRing<RingType>::newpromote(const Ring *R, 
                                       const ring_elem fR, 
                                       ring_elem &resultS) const
  {
    const Ring *S = this;
    fprintf(stderr, "calling newpromote\n");
    typedef RingPromoter RP;
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
    //    Promote<RingType>::promote(const Ring* R, const ring_elem fR, ring(), ElementType& result);
    ElementType result;
    ring().init(result);
    bool retval = Promoter::NewPromoter::newpromote(R, fR, ring(), result);
    ring().to_ring_elem(resultS, result);
    ring().clear(result);
    return retval;
  }

  // Rings considered:
  // ZZ, ZZFLINT
  // QQ, QQFLINT
  // ZZp, ZZpFFPACK, ZZpFLINT
  // GF, GFGIVARO
  // RR, CC (implemented using doubles)
  // RRR, CCC (using mpfr)
  // ZZtFLINT
  // ZZptFLINT
  // Poly (base: ?? over a set of variables)
  // PolyQuotient
  // Frac(R), where R=Poly or PolyQuotient??
  // Tower(base)

  // Operations considered:
  // promote
  // lift
  // eval

  // promote: assuming there is a *natural* map (S = this)
  // R --> S, and an element fR of R, this function
  // returns in 'resultS' the image of the element fR.
  //
  // Cases:
  //  1. ZZ --> S.  Every class implements such a function: from_mpz
  //  2. R==S: returns (a copy of) fR.
  // Now for all of the cases implemented:
  // 

  //  3. ZZ/p --> ZZ/p (where the ZZ/p are all different types of mod
  //     p integers, but where the characteristic is the same
  //     the implementation lifts to an int (or mpz?) and does from_int.
  //
  //  4. ZZ/p --> GF(p^r)
  //     
  //  5. QQ --> RR, CC, RRR, CCC
  //  6. RR --> RRR,CC,CCC
  //  7. RRR --> CCC
  //  8. R --> R[x,...], R[vars]/I
  //  9. R[vars]/I --> R[vars]/J
  //  10. R --> frac R

  //  11. ZZtFLINT --> ZZ[t]
  //  12. ZZ[t] --> ZZtFLINT
  //
  //  13. ZZ <---> ZZFlint

  template<typename RingType>
  bool ConcreteRing<RingType>::promote(const Ring *R, 
                                       const ring_elem fR, 
                                       ring_elem &resultS) const
  {
    const Ring *S = this;
    fprintf(stderr, "calling promote\n");
    typedef RingPromoter RP;
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
      case M2::ring_GF: return RP::promoter<ARingZZp,ARingGFGivaro>(R,S,fR,resultS);
      case M2::ring_FFPACK: return RP::promoter<ARingZZp,ARingZZpFFPACK>(R,S,fR,resultS);
      default: return false;
      }
      break;
    case M2::ring_GF:
      switch (S->ringID()) {
      case M2::ring_ZZp: return RP::promoter<ARingGFGivaro,ARingZZp>(R,S,fR,resultS);
      case M2::ring_GF: return RP::promoter<ARingGFGivaro,ARingGFGivaro>(R,S,fR,resultS);
      case M2::ring_FFPACK: return RP::promoter<ARingGFGivaro,ARingZZpFFPACK>(R,S,fR,resultS);
      default: return false;
      }
    case M2::ring_FFPACK:
      switch (S->ringID()) {
      case M2::ring_ZZp: return RP::promoter<ARingZZpFFPACK,ARingZZp>(R,S,fR,resultS);
      case M2::ring_GF: return RP::promoter<ARingZZpFFPACK,ARingGFGivaro>(R,S,fR,resultS);
      case M2::ring_FFPACK: return RP::promoter<ARingZZpFFPACK,ARingZZpFFPACK>(R,S,fR,resultS);
      default: return false;
      }
    default:
      break;
    };
    return false;
  }

  template<>
  bool ConcreteRing<ARingGFM2>::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
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
  bool ConcreteRing<ARingGFM2>::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
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
  bool ConcreteRing<ARingGFGivaro>::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
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
  bool ConcreteRing<ARingGFGivaro>::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
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

  //explicit instantiation
  template class ConcreteRing< ARingZZp >;
  template class ConcreteRing< ARingZZpFFPACK >;

#ifdef HAVE_FLINT
  template class ConcreteRing<ARingZZ>;
  template class ConcreteRing< ARingZZpFlint >;
#endif
  
  template class ConcreteRing< ARingGFM2 >;
  template class ConcreteRing< ARingGFGivaro >;

  template class ConcreteRing<ARingRRR>;
  template class ConcreteRing<ARingTower>;

  template class ConcreteRing<ARingCCC>;

  ///////////////////////////
  // Matrix creation ////////
  ///////////////////////////

};


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
