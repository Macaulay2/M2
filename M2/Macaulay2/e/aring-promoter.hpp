// Copyright 2012 Michael E. Stillman

#ifndef _aring_promoter_hpp_
#define _aring_promoter_hpp_

#include "aring-glue.hpp"
#include "aring-zzp.hpp"
#include "aring-RRR.hpp"
#include "aring-gf-givaro.hpp"
#include "aring-zzp-ffpack.hpp"
#include "aring-m2-gf.hpp"
#include "aring-tower.hpp"

#include "aring-zz-flint.hpp"

#if defined(HAVE_FLINT)
#include "aring-zzp-flint.hpp"
#endif

namespace M2 {
  namespace Promoter {
    ////////////////////////////////////////////////////////////////////
    // Now come all of the instances of promote that we wish to have ///
    ////////////////////////////////////////////////////////////////////
    static bool mypromote(const ARingZZp &R, 
                        const ARingZZp::ElementType &fR, 
                        const ARingZZpFFPACK &S, 
                        ARingZZpFFPACK::ElementType &fS)
    {
      return false;
    }

    static bool mypromote(const ARingGFGivaro &R, 
                        const ARingGFGivaro::ElementType &fR, 
                        const ARingZZp &S, 
                        ARingZZp::ElementType &fS)
    {
      return false;
    }

    static bool mypromote(const ARingZZpFFPACK &R, 
                        const ARingZZpFFPACK::ElementType &fR, 
                        const ARingZZp &S, 
                        ARingZZp::ElementType &fS)
    {
      return false;
    }

    //////////////////////////////////////////////////////////////
    // Boiler plate: dispatch to the correct newpromote routine //
    //////////////////////////////////////////////////////////////    
    template<typename SourceRing, typename TargetRing>
    static bool newpromoter(const Ring *R, const ring_elem fR, 
                            const TargetRing& S, typename TargetRing::ElementType  &resultS)
    {
      M2_ASSERT(dynamic_cast< const ConcreteRing<SourceRing> * >(R) != 0);
      const SourceRing &R1 = dynamic_cast< const ConcreteRing<SourceRing> * >(R)->ring();
      
      typename SourceRing::ElementType fR1;
      R1.init(fR1);
      R1.from_ring_elem(fR1, fR);
      bool retval = mypromote(R1,fR1,S,resultS);
      R1.clear(fR1);
      return retval;
    }

    ////////////////////////////////////////////////////////////////////////////////////
    // Each ring type needs one routine here saying what can be promoted to this ring //
    // These are called from aring-glue ////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////  

    class NewPromoter
    {
    public:
      static bool newpromote(const Ring* R, const ring_elem fR, 
                      const ARingZZp& S, ARingZZp::ElementType& resultS)
      {
        switch (R->ringID()) {
        case M2::ring_GF: return newpromoter<ARingGFGivaro,ARingZZp>(R,fR,S,resultS);
        case M2::ring_FFPACK: return newpromoter<ARingZZpFFPACK,ARingZZp>(R,fR,S,resultS);
        default: return false;
        }
      }
      
      static bool newpromote(const Ring* R, const ring_elem fR, 
                      const ARingGFGivaro& S, ARingGFGivaro::ElementType& resultS)
      {
        return false;
      }
      
      static bool newpromote(const Ring* R, const ring_elem fR, 
                      const ARingZZpFFPACK& S, ARingZZpFFPACK::ElementType& resultS)
      {
        return false;
      }
      
      static bool newpromote(const Ring* R, const ring_elem fR, 
                      const ARingZZ& S, ARingZZ::ElementType& resultS)
      {
        return false;
      }

#if defined(HAVE_FLINT)
      static bool newpromote(const Ring* R, const ring_elem fR, 
                      const ARingZZpFlint& S, ARingZZpFlint::ElementType& resultS)
      {
        return false;
      }
#endif
      
      static bool newpromote(const Ring* R, const ring_elem fR, 
                      const ARingGFM2& S, ARingGFM2::ElementType& resultS)
      {
        return false;
      }
      
      static bool newpromote(const Ring* R, const ring_elem fR, 
                      const ARingRRR& S, ARingRRR::ElementType& resultS)
      {
        return false;
      }
      
      static bool newpromote(const Ring* R, const ring_elem fR, 
                      const ARingTower& S, ARingTower::ElementType& resultS)
      {
        return false;
      }
    };
    
    
  }; // namespace Promoter
}; // namespace M2

namespace M2 {
  class RingPromoter
  {
  public:
    template<typename SourceRing, typename TargetRing>
    static bool promoter(const Ring *R, const Ring *S, const ring_elem fR, ring_elem &resultS)
    {
      ASSERT(dynamic_cast< const ConcreteRing<SourceRing> * >(R) != 0);
      ASSERT(dynamic_cast< const ConcreteRing<TargetRing> * >(S) != 0);
      const SourceRing &R1 = dynamic_cast< const ConcreteRing<SourceRing> * >(R)->ring();
      const TargetRing &S1 = dynamic_cast< const ConcreteRing<TargetRing> * >(S)->ring();
      
      typename SourceRing::ElementType fR1;
      typename TargetRing::ElementType gS1;
      
      R1.from_ring_elem(fR1, fR);
      bool retval = promote(R1,S1,fR1,gS1);
      if (retval)
        S1.to_ring_elem(resultS, gS1);
      return retval;
    }

  private:
    static bool promote(const ARingZZp &R, 
                 const ARingZZp &S, 
                 const ARingZZp::ElementType &fR, 
                 ARingZZp::ElementType &fS)
    {
      return false;
    }

//#if defined(HAVE_GIVARO)
    static bool promote(const ARingZZp &R, 
                 const ARingGFGivaro &S, 
                 const ARingZZp::ElementType &fR, 
                 ARingGFGivaro::ElementType &fS)
    {
      return false;
    }
    
    static bool promote(const ARingZZp &R, 
                 const ARingZZpFFPACK &S, 
                 const ARingZZp::ElementType &fR, 
                 ARingZZpFFPACK::ElementType &fS)
    {
      return false;
    }
    
    static bool promote(const ARingGFGivaro &R, 
                 const ARingGFGivaro &S, 
                 const ARingGFGivaro::ElementType &fR, 
                 ARingGFGivaro::ElementType &fS)
    {
      return false;
    }
    
    static bool promote(const ARingGFGivaro &R, 
                 const ARingZZp &S, 
                 const ARingGFGivaro::ElementType &fR, 
                 ARingZZp::ElementType &fS)
    {
      return false;
    }
    
    static bool promote(const ARingGFGivaro &R, 
                 const ARingZZpFFPACK &S, 
                 const ARingGFGivaro::ElementType &fR, 
                 ARingZZpFFPACK::ElementType &fS)
    {
      return false;
    }
    
    static bool promote(const ARingZZpFFPACK &R, 
                 const ARingGFGivaro &S, 
                 const ARingZZpFFPACK::ElementType &fR, 
                 ARingGFGivaro::ElementType &fS)
    {
      return false;
    }
    
    static bool promote(const ARingZZpFFPACK &R, 
                 const ARingZZp &S, 
                 const ARingZZpFFPACK::ElementType &fR, 
                 ARingZZp::ElementType &fS)
    {
      return false;
    }
    
    static bool promote(const ARingZZpFFPACK &R, 
                 const ARingZZpFFPACK &S, 
                 const ARingZZpFFPACK::ElementType &fR, 
                 ARingZZpFFPACK::ElementType &fS)
    {
      return false;
    }
//#endif
  };
};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
