// Copyright 2013 Michael E. Stillman

#ifndef _aring_translate_hpp_
#define _aring_translate_hpp_

///////////////////////////////////////////////////////
// Contains functions which are "ring translational" //
///////////////////////////////////////////////////////

#include "aring-RRR.hpp"
#include "aring-CCC.hpp"
#include "aring-zzp.hpp"
#include "aring-zzp-flint.hpp"
#include "aring-zzp-ffpack.hpp"
#include "aring-qq.hpp"
#include "aring-m2-gf.hpp"
#include "aring-gf-givaro.hpp"

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
               const RingS& S, 
               const typename RingR::ElementType& fR, 
               typename RingS::ElementType& result_fS)
  {
    return false;
  }

  inline bool mypromote(const ARingRRR& R,
                        const ARingRRR& S,
                        const ARingRRR::ElementType& fR,
                        ARingRRR::ElementType& fS)
  {
    std::cout << "calling mypromote RRR RRR" << std::endl;
    S.set(fS, fR);
    return true;
  }

  inline bool mypromote(const ARingRRR& R,
                        const ARingCCC& S,
                        const ARingRRR::ElementType& fR,
                        ARingCCC::ElementType& fS)
  {
    std::cout << "calling mypromote RRR CCC" << std::endl;
    S.set_from_RRR(fS, fR);
    return true;
  }

  inline bool mypromote(const ARingCCC& R,
                        const ARingCCC& S,
                        const ARingCCC::ElementType& fR,
                        ARingCCC::ElementType& fS)
  {
    std::cout << "calling mypromote CCC CCC" << std::endl;
    S.set(fS, fR);
    return true;
  }

  // ZZ/p --> ZZ/p. 9 versions NONE OF THESE.
  // instead:
  //  (1) lift to ZZ (int version?)
  //  (2) promote to the version of ZZ/p.
  
  // ZZ/p --> GF(p^n)

  // GF(p^m) --> GF(p^n), where m|n, also switch GF type.

  // The following are all essentially from_BigRational
  // QQ --> RR
  // QQ --> RRR
  // QQ --> CC
  // QQ --> CCC

  // really, there are:
  // RR --> RRR (from double)
  // RR --> CC (imag part = 0)
  // CC --> CCC (from doubles)
  // RRR --> RRR  (change precision)
  // RRR --> CCC  (imag part = 0)
  // CCC --> CCC  (change precision)

  // RRR --> RR  Use lift.
  // CCC --> CC  Use lift.

  // RRR --> RR (truncate)
  // RR --> RRR (make new precison)
  // RRR --> RRR (change precision)
  // RR --> CC (imag part = 0)
  // RRR --> CC (RRR --> RR, and imag part = 0)
  // CCC --> CC (truncate)
  // RR --> CCC
  // RRR --> CCC
  // CC --> CCC
  // CCC --> CCC
  
}; // namespace M2

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
