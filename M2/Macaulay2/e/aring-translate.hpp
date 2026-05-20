// Copyright 2013 Michael E. Stillman

#ifndef _aring_translate_hpp_
#define _aring_translate_hpp_

/**
 * @file aring-translate.hpp
 * @brief Cross-ring coercion templates: `mypromote` / `mylift` between two `aring` rings, and `get_from_*` from an external M2 numeric type into an `aring`.
 *
 * Declares the templated cross-ring coercion routines:
 * `mypromote<RingR, RingS>(R, S, fR, result_fS)` for the
 * forward direction `R -> S` (e.g. `Q -> RR` via
 * `S.set_from_mpq`, `Z -> Z/p`), `mylift<RingR, RingS>` for the
 * reverse `S -> R` (e.g. `Z/p -> Z` lifting to a canonical
 * representative), and the `get_from_*` family
 * (`get_from_BigReal`, `get_from_BigComplex`,
 * `get_from_Interval`, `get_from_ComplexInterval`,
 * `get_from_double`, `get_from_complex_double`) that injects an
 * external M2 numeric type into an aring element. Each generic
 * template body returns `false` to signal "this pair is
 * unsupported"; the supported pairs (`ARingQQ -> ARingRR /
 * ARingRRR / ARingRRi / ARingCC`, the various
 * `BigReal/Complex/Interval -> ARing*`, ...) are added as
 * `inline` overload-resolution specialisations further down.
 * Because each aring's own header knows only about itself,
 * this file pulls in *every* `aring-*.hpp` so the
 * specialisations have access to both source and target types.
 *
 * Together with `aring-glue.hpp` this file completes the aring
 * integration story: `aring-glue.hpp` is the vertical bridge
 * (aring up to the legacy `Ring*` API) and `aring-translate.hpp`
 * is the horizontal bridge (aring across to another aring).
 * The `promote` / `lift` paths in the legacy `Ring` machinery
 * funnel through these templates whenever the source and target
 * rings are both aring-backed.
 *
 * @see aring.hpp
 * @see aring-glue.hpp
 * @see ringmap.hpp
 */

///////////////////////////////////////////////////////
// Contains functions which are "ring translational" //
///////////////////////////////////////////////////////

#include "aring-RR.hpp"
#include "aring-CC.hpp"
#include "aring-RRR.hpp"
#include "aring-CCC.hpp"
#include "aring-RRi.hpp"
#include "aring-CCi.hpp"
#include "aring-zz-gmp.hpp"
#include "aring-zzp.hpp"
#include "aring-zzp-ffpack.hpp"
#include "aring-qq.hpp"
#include "aring-m2-gf.hpp"
#include "aring-tower.hpp"

// include flint headers last to avoid #1674
#include "aring-zz-flint.hpp"
#include "aring-zzp-flint.hpp"
#include "aring-gf-flint-big.hpp"
#include "aring-gf-flint.hpp"

namespace M2 {
template <typename RT>
bool get_from_BigReal(const RT& R, typename RT::ElementType& a, gmp_RR b)
{
  (void) R;
  (void) a;
  (void) b;
  return false;
}

template <typename RT>
bool get_from_Interval(const RT& R, typename RT::ElementType& a, gmp_RRi b)
{
  (void) R;
  (void) a;
  (void) b;
  return false;
}

template <typename RT>
bool get_from_ComplexInterval(const RT& R, typename RT::ElementType & a, gmp_CCi b)
{
    return false;
}

template <typename RT>
bool get_from_BigComplex(const RT& R, typename RT::ElementType& a, gmp_CC b)
{
  (void) R;
  (void) a;
  (void) b;
  return false;
}
template <typename RT>
bool get_from_double(const RT& R, typename RT::ElementType& a, double b)
{
  (void) R;
  (void) a;
  (void) b;
  return false;
}
template <typename RT>
bool get_from_complex_double(const RT& R,
                             typename RT::ElementType& a,
                             double re,
                             double im)
{
  (void) R;
  (void) a;
  (void) re;
  (void) im;
  return false;
}

inline bool get_from_BigReal(const ARingRR& R,
                             ARingRR::ElementType& a,
                             gmp_RR b)
{
  return R.set_from_BigReal(a, b);
}

inline bool get_from_BigReal(const ARingRRR& R,
                             ARingRRR::ElementType& a,
                             gmp_RR b)
{
  return R.set_from_BigReal(a, b);
}
    
inline bool get_from_BigReal(const ARingRRi& R,
                             ARingRRi::ElementType& a,
                             gmp_RR b)
{
    return R.set_from_BigReal(a, b);
}

inline bool get_from_BigReal(const ARingCC& R,
                             ARingCC::ElementType& a,
                             gmp_RR b)
{
  return R.set_from_BigReal(a, b);
}

inline bool get_from_BigReal(const ARingCCC& R,
                             ARingCCC::ElementType& a,
                             gmp_RR b)
{
  return R.set_from_BigReal(a, b);
}

inline bool get_from_BigComplex(const ARingCCC& R,
                                ARingCCC::ElementType& a,
                                gmp_CC b)
{
  return R.set_from_BigComplex(a, b);
}

inline bool get_from_BigComplex(const ARingCC& R,
                                ARingCC::ElementType& a,
                                gmp_CC b)
{
  return R.set_from_BigComplex(a, b);
}

inline bool get_from_double(const ARingRRR& R,
                            ARingRRR::ElementType& a,
                            double b)
{
  return R.set_from_double(a, b);
}
  
inline bool get_from_double(const ARingRRi& R,
                            ARingRRi::ElementType& a,
                            double b)
{
   return R.set_from_double(a, b);
}  
    
inline bool get_from_Interval(const ARingRRi& R,
                              ARingRRi::ElementType& a,
                              gmp_RRi b)
{
    return R.set_from_Interval(a, b);
}

inline bool get_from_ComplexInterval(const ARingCCi& R,
                              ARingCCi::ElementType& a,
                              gmp_CCi b)
{
    R.set(a, b);
    return true;
}

inline bool get_from_double(const ARingCCi& R,
                            ARingCCi::ElementType& a,
                            double b)
{
   return R.set_from_double(a, b);
}

inline bool get_from_Interval(const ARingCCi& R,
                              ARingCCi::ElementType& a,
                              gmp_RRi b)
{
    return R.set_from_Interval(a, b);
}

inline bool get_from_BigComplex(const ARingCCi& R,
                                ARingCCi::ElementType& a,
                                gmp_CC b)
{
  return R.set_from_BigComplex(a, b);
}

inline bool get_from_BigReal(const ARingCCi& R,
                             ARingCCi::ElementType& a,
                             gmp_RR b)
{
  return R.set_from_BigReal(a, b);
}

inline bool get_from_double(const ARingRR& R, ARingRR::ElementType& a, double b)
{
  return R.set_from_double(a, b);
}

inline bool get_from_double(const ARingCCC& R,
                            ARingCCC::ElementType& a,
                            double b)
{
  return R.set_from_double(a, b);
}

inline bool get_from_double(const ARingCC& R, ARingCC::ElementType& a, double b)
{
  return R.set_from_double(a, b);
}

inline bool get_from_complex_double(const ARingCCC& R,
                                    ARingCCC::ElementType& a,
                                    double re,
                                    double im)
{
  return R.set_from_complex_double(a, re, im);
}

inline bool get_from_complex_double(const ARingCC& R,
                                    ARingCC::ElementType& a,
                                    double re,
                                    double im)
{
  return R.set_from_complex_double(a, re, im);
}

inline bool get_from_complex_double(const ARingCCi& R,
                                    ARingCCi::ElementType& a,
                                    double re,
                                    double im)
{
  return R.set_from_complex_double(a, re, im);
}

// Promote an element of one ring to another.
// Given a "natural and canonical" map R --> S (depends on the context)
// and an element fR of R, set result_fS to be the image of fR under this map.
// Return true if this was done, else return false.
template <typename RingR, typename RingS>
bool mypromote(const RingR& R,
               const RingS& S,
               const typename RingR::ElementType& fR,
               typename RingS::ElementType& result_fS)
{
  (void) R;
  (void) S;
  (void) fR;
  (void) result_fS;
  return false;
}
template <typename RingR, typename RingS>
bool mylift(const RingR& R,
            const RingS& S,
            typename RingR::ElementType& result_gR,
            const typename RingS::ElementType& gS)
{
  (void) R;
  (void) S;
  (void) result_gR;
  (void) gS;
  return false;
}

/////////////////////////////////////////////////////
inline bool mypromote(const ARingQQ& R,
                      const ARingRR& S,
                      const ARingQQ::ElementType& fR,
                      ARingRR::ElementType& fS)
{
  (void) R;
  return S.set_from_mpq(fS, &fR);
}
inline bool mypromote(const ARingQQ& R,
                      const ARingRRR& S,
                      const ARingQQ::ElementType& fR,
                      ARingRRR::ElementType& fS)
{
  (void) R;
  return S.set_from_mpq(fS, &fR);
}
inline bool mypromote(const ARingQQ& R,
                      const ARingRRi& S,
                      const ARingQQ::ElementType& fR,
                      ARingRRi::ElementType& fS)
{
  (void) R;
  return S.set_from_mpq(fS, &fR);
}
inline bool mypromote(const ARingQQ& R,
                      const ARingCC& S,
                      const ARingQQ::ElementType& fR,
                      ARingCC::ElementType& fS)
{
  (void) R;
  return S.set_from_mpq(fS, &fR);
}
inline bool mypromote(const ARingQQ& R,
                      const ARingCCC& S,
                      const ARingQQ::ElementType& fR,
                      ARingCCC::ElementType& fS)
{
  (void) R;
  return S.set_from_mpq(fS, &fR);
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingRR& R,
                      const ARingRR& S,
                      const ARingRR::ElementType& fR,
                      ARingRR::ElementType& fS)
{
  (void) R;
  S.set_from_double(fS, fR);
  return true;
}
inline bool mypromote(const ARingRR& R,
                      const ARingRRR& S,
                      const ARingRR::ElementType& fR,
                      ARingRRR::ElementType& fS)
{
  (void) R;
  S.set_from_double(fS, fR);
  return true;
}
inline bool mypromote(const ARingRR& R,
                      const ARingCC& S,
                      const ARingRR::ElementType& fR,
                      ARingCC::ElementType& fS)
{
  (void) R;
  S.set_from_doubles(fS, fR, 0);
  return true;
}
inline bool mypromote(const ARingRR& R,
                      const ARingCCC& S,
                      const ARingRR::ElementType& fR,
                      ARingCCC::ElementType& fS)
{
  (void) R;
  S.set_from_doubles(fS, fR, 0);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingRRR& R,
                      const ARingRRR& S,
                      const ARingRRR::ElementType& fR,
                      ARingRRR::ElementType& fS)
{
  (void) R;
  S.set(fS, fR);
  return true;
}
inline bool mypromote(const ARingRRR& R,
                      const ARingRR& S,
                      const ARingRRR::ElementType& fR,
                      ARingRR::ElementType& fS)
{
  (void) R;
  auto fR1 = const_cast<ARingRRR::ElementType&>(fR);
  S.set_from_BigReal(fS, &fR1);
  return true;
}

inline bool mypromote(const ARingRRR& R,
                      const ARingCCC& S,
                      const ARingRRR::ElementType& fR,
                      ARingCCC::ElementType& fS)
{
  (void) R;
  S.set_from_RRR(fS, fR);
  return true;
}
inline bool mypromote(const ARingRRR& R,
                      const ARingCC& S,
                      const ARingRRR::ElementType& fR,
                      ARingCC::ElementType& fS)
{
  (void) R;
  auto fR1 = const_cast<ARingRRR::ElementType&>(fR);
  S.set_from_BigReal(fS, &fR1);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingRRi& R,
                      const ARingRRi& S,
                      const ARingRRi::ElementType& fR,
                      ARingRRi::ElementType& fS)
{
  (void) R;
  S.set(fS, fR);
  return true;
}
inline bool mypromote(const ARingRR& R,
                      const ARingRRi& S,
                      const ARingRR::ElementType& fR,
                      ARingRRi::ElementType& fS)
{
  (void) R;
  S.set_from_double(fS, fR);
  return true;
}
inline bool mypromote(const ARingRRR& R,
                      const ARingRRi& S,
                      const ARingRRR::ElementType& fR,
                      ARingRRi::ElementType& fS)
{
  (void) R;
  S.set_from_BigReal(fS, &fR);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingCC& R,
                      const ARingCC& S,
                      const ARingCC::ElementType& fR,
                      ARingCC::ElementType& fS)
{
  (void) R;
  S.set(fS, fR);
  return true;
}
inline bool mypromote(const ARingCC& R,
                      const ARingCCC& S,
                      const ARingCC::ElementType& fR,
                      ARingCCC::ElementType& fS)
{
  (void) R;
  S.set_from_complex_double(fS, fR.re, fR.im);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingCCC& R,
                      const ARingCC& S,
                      const ARingCCC::ElementType& fR,
                      ARingCC::ElementType& fS)
{
  (void) R;
  auto fR1 = const_cast<ARingCCC::ElementType&>(fR);
  S.set_from_BigReals(fS, &fR1.re, &fR1.im);
  return true;
}
inline bool mypromote(const ARingCCC& R,
                      const ARingCCC& S,
                      const ARingCCC::ElementType& fR,
                      ARingCCC::ElementType& fS)
{
  (void) R;
  S.set(fS, fR);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingCCi& R,
                      const ARingCCi& S,
                      const ARingCCi::ElementType& fR,
                      ARingCCi::ElementType& fS)
{
  S.set(fS, fR);
  return true;
}
inline bool mypromote(const ARingRR& R,
                      const ARingCCi& S,
                      const ARingRR::ElementType& fR,
                      ARingCCi::ElementType& fS)
{
  S.set_from_double(fS, fR);
  return true;
}

inline bool mypromote(const ARingRRi& R,
                      const ARingCCi& S,
                      const ARingRRi::ElementType& fR,
                      ARingCCi::ElementType& fS)
{
  S.set_from_Interval(fS, &fR);
  return true;
}

inline bool mypromote(const ARingRRR& R,
                      const ARingCCi& S,
                      const ARingRRR::ElementType& fR,
                      ARingCCi::ElementType& fS)
{
  S.set_from_BigReal(fS, &fR);
  return true;
}
inline bool mypromote(const ARingCC& R,
                      const ARingCCi& S,
                      const ARingCC::ElementType& fR,
                      ARingCCi::ElementType& fS)
{
  S.set_from_complex_double(fS, fR.re, fR.im);
  return true;
}
inline bool mypromote(const ARingCCC& R,
                      const ARingCCi& S,
                      const ARingCCC::ElementType& fR,
                      ARingCCi::ElementType& fS)
{
  S.set_from_BigComplex(fS, &fR);
  return true;
}
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
inline bool mylift(const ARingRRR& R,
                   const ARingRR& S,
                   ARingRRR::ElementType& result_gR,
                   const ARingRR::ElementType& gS)
{
  (void) S;
  R.set_from_double(result_gR, gS);
  return true;
}
inline bool mylift(const ARingRRR& R,
                   const ARingRRR& S,
                   ARingRRR::ElementType& result_gR,
                   const ARingRRR::ElementType& gS)
{
  (void) S;
  R.set(result_gR, gS);
  return true;
}
inline bool mylift(const ARingRRR& R,
                   const ARingCCC& S,
                   ARingRRR::ElementType& result_gR,
                   const ARingCCC::ElementType& gS)
{
  R.set(result_gR, S.realPartReference(gS));
  return (R.is_zero(S.imaginaryPartReference(gS)));
}
inline bool mylift(const ARingRRR& R,
                   const ARingCC& S,
                   ARingRRR::ElementType& result_gR,
                   const ARingCC::ElementType& gS)
{
  (void) S;
  R.set_from_double(result_gR, gS.re);
  return gS.im == 0;
}
/////////////////////////////////////////////////////
inline bool mylift(const ARingRR& R,
                   const ARingRR& S,
                   ARingRR::ElementType& result_gR,
                   const ARingRR::ElementType& gS)
{
  (void) S;
  R.set_from_double(result_gR, gS);
  return true;
}
inline bool mylift(const ARingRR& R,
                   const ARingRRR& S,
                   ARingRR::ElementType& result_gR,
                   const ARingRRR::ElementType& gS)
{
  (void) S;
  auto gS1 = const_cast<ARingRRR::ElementType&>(gS);
  R.set_from_BigReal(result_gR, &gS1);
  return true;
}
inline bool mylift(const ARingRR& R,
                   const ARingCCC& S,
                   ARingRR::ElementType& result_gR,
                   const ARingCCC::ElementType& gS)
{
  auto gS1 = const_cast<ARingRRR::ElementType&>(S.realPartReference(gS));
  R.set_from_BigReal(result_gR, &gS1);
  return (S.real_ring().is_zero(S.imaginaryPartReference(gS)));
}
inline bool mylift(const ARingRR& R,
                   const ARingCC& S,
                   ARingRR::ElementType& result_gR,
                   const ARingCC::ElementType& gS)
{
  (void) S;
  R.set_from_double(result_gR, gS.re);
  return gS.im == 0;
}
/////////////////////////////////////////////////////
inline bool mylift(const ARingCCC& R,
                   const ARingCCC& S,
                   ARingCCC::ElementType& result_gR,
                   const ARingCCC::ElementType& gS)
{
  (void) S;
  R.set(result_gR, gS);
  return true;
}
inline bool mylift(const ARingCCC& R,
                   const ARingCC& S,
                   ARingCCC::ElementType& result_gR,
                   const ARingCC::ElementType& gS)
{
  (void) S;
  R.set_from_complex_double(result_gR, gS.re, gS.im);
  return true;
}
inline bool mylift(const ARingCC& R,
                   const ARingCCC& S,
                   ARingCC::ElementType& result_gR,
                   const ARingCCC::ElementType& gS)
{
  (void) S;
  auto gS1 = const_cast<ARingCCC::ElementType&>(gS);
  R.set_from_BigReals(result_gR, &gS1.re, &gS1.im);
  return true;
}
inline bool mylift(const ARingCC& R,
                   const ARingCC& S,
                   ARingCC::ElementType& result_gR,
                   const ARingCC::ElementType& gS)
{
  (void) S;
  R.set(result_gR, gS);
  return true;
}
    
/////////////////////////////////////////////////////
    
inline bool mylift(const ARingRR& R,
                    const ARingRRi& S,
                    ARingRR::ElementType& result_gR,
                    const ARingRRi::ElementType& gS)
{
    ARingRRR T(S.get_precision());
    ARingRRR::Element gT(T);
    auto gS1 = const_cast<ARingRRi::ElementType&>(gS);
    S.midpoint(gT,gS1);
    bool liftstep = mylift(R,T,result_gR,gT);
    S.diameter(gT,gS1);
    return liftstep && T.is_zero(gT);
}

inline bool mylift(const ARingRRR& R,
                    const ARingRRi& S,
                    ARingRRR::ElementType& result_gR,
                    const ARingRRi::ElementType& gS)
{
    ARingRRR T(S.get_precision());
    ARingRRR::Element gT(T);
    auto gS1 = const_cast<ARingRRi::ElementType&>(gS);
    S.midpoint(gT,gS1);
    bool liftstep = mylift(R,T,result_gR,gT);
    S.diameter(gT,gS1);
    return liftstep && T.is_zero(gT);
}

inline bool mylift(const ARingQQ& R,
                    const ARingRRi& S,
                    ARingQQ::ElementType& result_gR,
                    const ARingRRi::ElementType& gS)
{
    ARingRRR T(S.get_precision());
    ARingRRR::Element gT(T);
    auto gS1 = const_cast<ARingRRi::ElementType&>(gS);
    S.midpoint(gT,gS1);
    bool liftstep = mylift(R,T,result_gR,gT);
    S.diameter(gT,gS1);
    return liftstep && T.is_zero(gT);
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
// RR --> RRR (make new precision)
// RRR --> RRR (change precision)
// RR --> CC (imag part = 0)
// RRR --> CC (RRR --> RR, and imag part = 0)
// CCC --> CC (truncate)
// RR --> CCC
// RRR --> CCC
// CC --> CCC
// CCC --> CCC

};  // namespace M2

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
