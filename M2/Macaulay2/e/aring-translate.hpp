// Copyright 2013 Michael E. Stillman

#ifndef _aring_translate_hpp_
#define _aring_translate_hpp_

///////////////////////////////////////////////////////
// Contains functions which are "ring translational" //
///////////////////////////////////////////////////////

#include "aring-RR.hpp"
#include "aring-CC.hpp"
#include "aring-RRR.hpp"
#include "aring-CCC.hpp"
#include "aring-zz-gmp.hpp"
#include "aring-zz-flint.hpp"
#include "aring-zzp.hpp"
#include "aring-zzp-flint.hpp"
#include "aring-zzp-ffpack.hpp"
#include "aring-qq.hpp"
#include "aring-m2-gf.hpp"
#include "aring-gf-givaro.hpp"
#include "aring-gf-flint-big.hpp"
#include "aring-gf-flint.hpp"
#include "aring-tower.hpp"

namespace M2 {
template <typename RT>
bool get_from_BigReal(const RT& R, typename RT::ElementType& a, gmp_RR b)
{
  return false;
}

template <typename RT>
bool get_from_BigComplex(const RT& R, typename RT::ElementType& a, gmp_CC b)
{
  return false;
}
template <typename RT>
bool get_from_double(const RT& R, typename RT::ElementType& a, double b)
{
  return false;
}
template <typename RT>
bool get_from_complex_double(const RT& R,
                             typename RT::ElementType& a,
                             double re,
                             double im)
{
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
  return false;
}
template <typename RingR, typename RingS>
bool mylift(const RingR& R,
            const RingS& S,
            typename RingR::ElementType& result_gR,
            const typename RingS::ElementType& gS)
{
  return false;
}

/////////////////////////////////////////////////////
inline bool mypromote(const ARingQQ& R,
                      const ARingRR& S,
                      const ARingQQ::ElementType& fR,
                      ARingRR::ElementType& fS)
{
  return S.set_from_mpq(fS, &fR);
}
inline bool mypromote(const ARingQQ& R,
                      const ARingRRR& S,
                      const ARingQQ::ElementType& fR,
                      ARingRRR::ElementType& fS)
{
  return S.set_from_mpq(fS, &fR);
}
inline bool mypromote(const ARingQQ& R,
                      const ARingCC& S,
                      const ARingQQ::ElementType& fR,
                      ARingCC::ElementType& fS)
{
  return S.set_from_mpq(fS, &fR);
}
inline bool mypromote(const ARingQQ& R,
                      const ARingCCC& S,
                      const ARingQQ::ElementType& fR,
                      ARingCCC::ElementType& fS)
{
  return S.set_from_mpq(fS, &fR);
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingRR& R,
                      const ARingRR& S,
                      const ARingRR::ElementType& fR,
                      ARingRR::ElementType& fS)
{
  S.set_from_double(fS, fR);
  return true;
}
inline bool mypromote(const ARingRR& R,
                      const ARingRRR& S,
                      const ARingRR::ElementType& fR,
                      ARingRRR::ElementType& fS)
{
  S.set_from_double(fS, fR);
  return true;
}
inline bool mypromote(const ARingRR& R,
                      const ARingCC& S,
                      const ARingRR::ElementType& fR,
                      ARingCC::ElementType& fS)
{
  S.set_from_doubles(fS, fR, 0);
  return true;
}
inline bool mypromote(const ARingRR& R,
                      const ARingCCC& S,
                      const ARingRR::ElementType& fR,
                      ARingCCC::ElementType& fS)
{
  S.set_from_doubles(fS, fR, 0);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingRRR& R,
                      const ARingRRR& S,
                      const ARingRRR::ElementType& fR,
                      ARingRRR::ElementType& fS)
{
  S.set(fS, fR);
  return true;
}
inline bool mypromote(const ARingRRR& R,
                      const ARingRR& S,
                      const ARingRRR::ElementType& fR,
                      ARingRR::ElementType& fS)
{
  auto fR1 = const_cast<ARingRRR::ElementType&>(fR);
  S.set_from_BigReal(fS, &fR1);
  return true;
}

inline bool mypromote(const ARingRRR& R,
                      const ARingCCC& S,
                      const ARingRRR::ElementType& fR,
                      ARingCCC::ElementType& fS)
{
  S.set_from_RRR(fS, fR);
  return true;
}
inline bool mypromote(const ARingRRR& R,
                      const ARingCC& S,
                      const ARingRRR::ElementType& fR,
                      ARingCC::ElementType& fS)
{
  auto fR1 = const_cast<ARingRRR::ElementType&>(fR);
  S.set_from_BigReal(fS, &fR1);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingCC& R,
                      const ARingCC& S,
                      const ARingCC::ElementType& fR,
                      ARingCC::ElementType& fS)
{
  S.set(fS, fR);
  return true;
}
inline bool mypromote(const ARingCC& R,
                      const ARingCCC& S,
                      const ARingCC::ElementType& fR,
                      ARingCCC::ElementType& fS)
{
  S.set_from_complex_double(fS, fR.re, fR.im);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingCCC& R,
                      const ARingCC& S,
                      const ARingCCC::ElementType& fR,
                      ARingCC::ElementType& fS)
{
  auto fR1 = const_cast<ARingCCC::ElementType&>(fR);
  S.set_from_BigReals(fS, &fR1.re, &fR1.im);
  return true;
}
inline bool mypromote(const ARingCCC& R,
                      const ARingCCC& S,
                      const ARingCCC::ElementType& fR,
                      ARingCCC::ElementType& fS)
{
  S.set(fS, fR);
  return true;
}
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
inline bool mylift(const ARingRRR& R,
                   const ARingRR& S,
                   ARingRRR::ElementType& result_gR,
                   const ARingRR::ElementType& gS)
{
  R.set_from_double(result_gR, gS);
  return true;
}
inline bool mylift(const ARingRRR& R,
                   const ARingRRR& S,
                   ARingRRR::ElementType& result_gR,
                   const ARingRRR::ElementType& gS)
{
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
  R.set_from_double(result_gR, gS.re);
  return gS.im == 0;
}
/////////////////////////////////////////////////////
inline bool mylift(const ARingRR& R,
                   const ARingRR& S,
                   ARingRR::ElementType& result_gR,
                   const ARingRR::ElementType& gS)
{
  R.set_from_double(result_gR, gS);
  return true;
}
inline bool mylift(const ARingRR& R,
                   const ARingRRR& S,
                   ARingRR::ElementType& result_gR,
                   const ARingRRR::ElementType& gS)
{
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
  R.set_from_double(result_gR, gS.re);
  return gS.im == 0;
}
/////////////////////////////////////////////////////
inline bool mylift(const ARingCCC& R,
                   const ARingCCC& S,
                   ARingCCC::ElementType& result_gR,
                   const ARingCCC::ElementType& gS)
{
  R.set(result_gR, gS);
  return true;
}
inline bool mylift(const ARingCCC& R,
                   const ARingCC& S,
                   ARingCCC::ElementType& result_gR,
                   const ARingCC::ElementType& gS)
{
  R.set_from_complex_double(result_gR, gS.re, gS.im);
  return true;
}
inline bool mylift(const ARingCC& R,
                   const ARingCCC& S,
                   ARingCC::ElementType& result_gR,
                   const ARingCCC::ElementType& gS)
{
  auto gS1 = const_cast<ARingCCC::ElementType&>(gS);
  R.set_from_BigReals(result_gR, &gS1.re, &gS1.im);
  return true;
}
inline bool mylift(const ARingCC& R,
                   const ARingCC& S,
                   ARingCC::ElementType& result_gR,
                   const ARingCC::ElementType& gS)
{
  R.set(result_gR, gS);
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
