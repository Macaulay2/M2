// Copyright 2012 Michael E. Stillman

#ifndef _aring_promoter_hpp_
#define _aring_promoter_hpp_

#include "aring-glue.hpp"
#include "aring-zzp.hpp"
#include "aring-RRR.hpp"
#include "aring-gf.hpp"
#include "aring-ffpack.hpp"
#include "aring-m2-gf.hpp"
#include "aring-tower.hpp"

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
                 const ARingGF &S, 
                 const ARingZZp::ElementType &fR, 
                 ARingGF::ElementType &fS)
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
    
    static bool promote(const ARingGF &R, 
                 const ARingGF &S, 
                 const ARingGF::ElementType &fR, 
                 ARingGF::ElementType &fS)
    {
      return false;
    }
    
    static bool promote(const ARingGF &R, 
                 const ARingZZp &S, 
                 const ARingGF::ElementType &fR, 
                 ARingZZp::ElementType &fS)
    {
      return false;
    }
    
    static bool promote(const ARingGF &R, 
                 const ARingZZpFFPACK &S, 
                 const ARingGF::ElementType &fR, 
                 ARingZZpFFPACK::ElementType &fS)
    {
      return false;
    }
    
    static bool promote(const ARingZZpFFPACK &R, 
                 const ARingGF &S, 
                 const ARingZZpFFPACK::ElementType &fR, 
                 ARingGF::ElementType &fS)
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
