// Copyright 2013 Michael E. Stillman

#ifndef _aring_translate_hpp_
#define _aring_translate_hpp_

///////////////////////////////////////////////////////
// Contains functions which are "ring translational" //
///////////////////////////////////////////////////////

#include "aring-RRR.hpp"
#include "aring-CCC.hpp"

namespace M2 {
  template<typename RT> 
  bool get_from_BigReal(const RT& R, typename RT::ElementType& a, gmp_RR b)
  {
    return false;
  }

  template<typename RT> 
  bool get_from_BigComplex(const RT& R, typename RT::ElementType& a, gmp_CC b)
  {
    return false;
  }

  inline bool get_from_BigReal(const ARingRRR& R, ARingRRR::ElementType& a, gmp_RR b)
  {return R.set_from_BigReal(a,b);}

  inline bool get_from_BigReal(const ARingCCC& R, ARingCCC::ElementType& a, gmp_RR b)
  {return R.set_from_BigReal(a,b);}

  inline bool get_from_BigComplex(const ARingCCC& R, ARingCCC::ElementType& a, gmp_CC b)
  {return R.set_from_BigComplex(a,b); }

  // Promote an element of one ring to another.
  // Given a "natural and canonical" map R --> S (depends on the context)
  // and an element fR of R, set result_fS to be the image of fR under this map.
  // Return true if this was done, else return false.
  template<typename RingR, typename RingS>
  bool mypromote(const RingR& R, 
               const typename RingR::ElementType& fR, 
               const RingS& S, 
               typename RingS::ElementType& result_fS)
  {
    return false;
  }


}; // namespace M2

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
