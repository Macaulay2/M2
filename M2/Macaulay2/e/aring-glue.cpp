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
      case M2::ring_GF: return RP::promoter<ARingZZp,ARingGF>(R,S,fR,resultS);
      case M2::ring_FFPACK: return RP::promoter<ARingZZp,ARingZZpFFPACK>(R,S,fR,resultS);
      default: return false;
      }
      break;
    case M2::ring_GF:
      switch (S->ringID()) {
      case M2::ring_ZZp: return RP::promoter<ARingGF,ARingZZp>(R,S,fR,resultS);
      case M2::ring_GF: return RP::promoter<ARingGF,ARingGF>(R,S,fR,resultS);
      case M2::ring_FFPACK: return RP::promoter<ARingGF,ARingZZpFFPACK>(R,S,fR,resultS);
      default: return false;
      }
    case M2::ring_FFPACK:
      switch (S->ringID()) {
      case M2::ring_ZZp: return RP::promoter<ARingZZpFFPACK,ARingZZp>(R,S,fR,resultS);
      case M2::ring_GF: return RP::promoter<ARingZZpFFPACK,ARingGF>(R,S,fR,resultS);
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
  bool ConcreteRing<ARingGF>::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
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
  bool ConcreteRing<ARingGF>::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
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
  
  template class ConcreteRing< ARingGFM2 >;
  template class ConcreteRing< ARingGF >;

  template class ConcreteRing<ARingRRR>;
  template class ConcreteRing<ARingTower>;

  ///////////////////////////
  // Matrix creation ////////
  ///////////////////////////

};


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
