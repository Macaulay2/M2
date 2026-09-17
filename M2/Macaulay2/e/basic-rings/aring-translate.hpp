// Copyright 2013 Michael E. Stillman

#ifndef M2_BASIC_RINGS_ARING_TRANSLATE_HPP_
#define M2_BASIC_RINGS_ARING_TRANSLATE_HPP_

#include <type_traits>
#include <utility>

///////////////////////////////////////////////////////
// Contains functions which are "ring translational" //
///////////////////////////////////////////////////////

#include "basic-rings/aring-RR.hpp"
#include "basic-rings/aring-CC.hpp"
#include "basic-rings/aring-RRR.hpp"
#include "basic-rings/aring-CCC.hpp"
#include "basic-rings/aring-RRi.hpp"
#include "basic-rings/aring-CCi.hpp"
#include "basic-rings/aring-ZZ-gmp.hpp"
#include "basic-rings/aring-ZZp.hpp"
#include "basic-rings/aring-ZZp-ffpack.hpp"
#include "basic-rings/aring-QQ.hpp"
#include "basic-rings/aring-m2-GF.hpp"
#include "basic-rings/aring-tower.hpp"

// include flint headers last to avoid #1674
#include "basic-rings/aring-ZZ-flint.hpp"
#include "basic-rings/aring-ZZp-flint.hpp"
#include "basic-rings/aring-GF-flint-big.hpp"
#include "basic-rings/aring-GF-flint.hpp"

namespace M2 {

namespace detail {

template <typename RT, typename = void>
inline constexpr bool has_set_from_mpq = false;
template <typename RT>
inline constexpr bool has_set_from_mpq<
    RT,
    std::void_t<decltype(std::declval<const RT&>().set(
        std::declval<typename RT::ElementType&>(),
        std::declval<mpq_srcptr>()))>> = true;

template <typename RT, typename = void>
inline constexpr bool has_set_from_double = false;
template <typename RT>
inline constexpr bool has_set_from_double<
    RT,
    std::enable_if_t<std::is_same_v<
        decltype(std::declval<const RT&>().set(
            std::declval<typename RT::ElementType&>(), std::declval<double>())),
        bool>>> = true;

template <typename RT, typename = void>
inline constexpr bool has_set_from_BigReal = false;
template <typename RT>
inline constexpr bool has_set_from_BigReal<
    RT,
    std::void_t<decltype(std::declval<const RT&>().set(
        std::declval<typename RT::ElementType&>(), std::declval<gmp_RR>()))>> =
    true;

template <typename RT, typename = void>
inline constexpr bool has_set_from_Interval = false;
template <typename RT>
inline constexpr bool has_set_from_Interval<
    RT,
    std::void_t<decltype(std::declval<const RT&>().set(
        std::declval<typename RT::ElementType&>(), std::declval<gmp_RRi>()))>> =
    true;

template <typename RT, typename = void>
inline constexpr bool has_set_from_doubles = false;
template <typename RT>
inline constexpr bool has_set_from_doubles<
    RT,
    std::void_t<decltype(std::declval<const RT&>().set(
        std::declval<typename RT::ElementType&>(),
        std::declval<double>(),
        std::declval<double>()))>> = true;

template <typename RT, typename = void>
inline constexpr bool has_set_from_BigComplex = false;
template <typename RT>
inline constexpr bool has_set_from_BigComplex<
    RT,
    std::void_t<decltype(std::declval<const RT&>().set(
        std::declval<typename RT::ElementType&>(), std::declval<gmp_CC>()))>> =
    true;

template <typename RT, typename = void>
inline constexpr bool has_set_from_ComplexInterval = false;
template <typename RT>
inline constexpr bool has_set_from_ComplexInterval<
    RT,
    std::void_t<decltype(std::declval<const RT&>().set(
        std::declval<typename RT::ElementType&>(), std::declval<gmp_CCi>()))>> =
    true;
}  // namespace detail

template <typename RT>
bool try_set(const RT& R, typename RT::ElementType& a, double b)
{
  if constexpr (detail::has_set_from_double<RT>)
    return R.set(a, b);
  else
    return false;
}

template <typename RT>
bool try_set(const RT& R, typename RT::ElementType& a, gmp_RR b)
{
  if constexpr (detail::has_set_from_BigReal<RT>)
    return R.set(a, b);
  else
    return false;
}

template <typename RT>
bool try_set(const RT& R, typename RT::ElementType& a, gmp_RRi b)
{
  if constexpr (detail::has_set_from_Interval<RT>)
    return R.set(a, b);
  else
    return false;
}

template <typename RT>
bool try_set(const RT& R,
                             typename RT::ElementType& a,
                             double re,
                             double im)
{
  if constexpr (detail::has_set_from_doubles<RT>)
    { R.set(a, re, im); return true; }
  else
    return false;
}

template <typename RT>
bool try_set(const RT& R, typename RT::ElementType& a, gmp_CC b)
{
  if constexpr (detail::has_set_from_BigComplex<RT>)
    return R.set(a, b);
  else
    return false;
}

template <typename RT>
bool try_set(const RT& R, typename RT::ElementType & a, gmp_CCi b)
{
  if constexpr (detail::has_set_from_ComplexInterval<RT>)
    return R.set(a, b);
  else
    return false;
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
template <typename RingS>
bool mypromote(const ARingQQ& R,
               const RingS& S,
               const ARingQQ::ElementType& fR,
               typename RingS::ElementType& fS)
{
  (void) R;
  if constexpr (detail::has_set_from_mpq<RingS>)
    return S.set(fS, &fR);
  else
    return false;
}
/////////////////////////////////////////////////////
template <typename Ring>
bool mypromote(const Ring& R,
               const Ring& S,
               const typename Ring::ElementType& fR,
               typename Ring::ElementType& fS)
{
  (void) R;
  S.set(fS, fR);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingRR& R,
                      const ARingRRR& S,
                      const ARingRR::ElementType& fR,
                      ARingRRR::ElementType& fS)
{
  (void) R;
  S.set(fS, fR);
  return true;
}
inline bool mypromote(const ARingRR& R,
                      const ARingCC& S,
                      const ARingRR::ElementType& fR,
                      ARingCC::ElementType& fS)
{
  (void) R;
  S.set(fS, fR, 0);
  return true;
}
inline bool mypromote(const ARingRR& R,
                      const ARingCCC& S,
                      const ARingRR::ElementType& fR,
                      ARingCCC::ElementType& fS)
{
  (void) R;
  S.set(fS, fR, 0);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingRRR& R,
                      const ARingRR& S,
                      const ARingRRR::ElementType& fR,
                      ARingRR::ElementType& fS)
{
  (void) R;
  auto fR1 = const_cast<ARingRRR::ElementType&>(fR);
  S.set(fS, &fR1);
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
  S.set(fS, &fR1);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingRR& R,
                      const ARingRRi& S,
                      const ARingRR::ElementType& fR,
                      ARingRRi::ElementType& fS)
{
  (void) R;
  S.set(fS, fR);
  return true;
}
inline bool mypromote(const ARingRRR& R,
                      const ARingRRi& S,
                      const ARingRRR::ElementType& fR,
                      ARingRRi::ElementType& fS)
{
  (void) R;
  S.set(fS, &fR);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingCC& R,
                      const ARingCCC& S,
                      const ARingCC::ElementType& fR,
                      ARingCCC::ElementType& fS)
{
  (void) R;
  S.set(fS, fR.re, fR.im);
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
  S.set(fS, &fR1.re, &fR1.im);
  return true;
}
/////////////////////////////////////////////////////
inline bool mypromote(const ARingRR& R,
                      const ARingCCi& S,
                      const ARingRR::ElementType& fR,
                      ARingCCi::ElementType& fS)
{
  S.set(fS, fR);
  return true;
}

inline bool mypromote(const ARingRRi& R,
                      const ARingCCi& S,
                      const ARingRRi::ElementType& fR,
                      ARingCCi::ElementType& fS)
{
  S.set(fS, &fR);
  return true;
}

inline bool mypromote(const ARingRRR& R,
                      const ARingCCi& S,
                      const ARingRRR::ElementType& fR,
                      ARingCCi::ElementType& fS)
{
  S.set(fS, &fR);
  return true;
}
inline bool mypromote(const ARingCC& R,
                      const ARingCCi& S,
                      const ARingCC::ElementType& fR,
                      ARingCCi::ElementType& fS)
{
  S.set(fS, fR.re, fR.im);
  return true;
}
inline bool mypromote(const ARingCCC& R,
                      const ARingCCi& S,
                      const ARingCCC::ElementType& fR,
                      ARingCCi::ElementType& fS)
{
  S.set(fS, &fR);
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
  R.set(result_gR, gS);
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
  R.set(result_gR, gS.re);
  return gS.im == 0;
}
/////////////////////////////////////////////////////
inline bool mylift(const ARingRR& R,
                   const ARingRR& S,
                   ARingRR::ElementType& result_gR,
                   const ARingRR::ElementType& gS)
{
  (void) S;
  R.set(result_gR, gS);
  return true;
}
inline bool mylift(const ARingRR& R,
                   const ARingRRR& S,
                   ARingRR::ElementType& result_gR,
                   const ARingRRR::ElementType& gS)
{
  (void) S;
  auto gS1 = const_cast<ARingRRR::ElementType&>(gS);
  R.set(result_gR, &gS1);
  return true;
}
inline bool mylift(const ARingRR& R,
                   const ARingCCC& S,
                   ARingRR::ElementType& result_gR,
                   const ARingCCC::ElementType& gS)
{
  auto gS1 = const_cast<ARingRRR::ElementType&>(S.realPartReference(gS));
  R.set(result_gR, &gS1);
  return (S.real_ring().is_zero(S.imaginaryPartReference(gS)));
}
inline bool mylift(const ARingRR& R,
                   const ARingCC& S,
                   ARingRR::ElementType& result_gR,
                   const ARingCC::ElementType& gS)
{
  (void) S;
  R.set(result_gR, gS.re);
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
  R.set(result_gR, gS.re, gS.im);
  return true;
}
inline bool mylift(const ARingCC& R,
                   const ARingCCC& S,
                   ARingCC::ElementType& result_gR,
                   const ARingCCC::ElementType& gS)
{
  (void) S;
  auto gS1 = const_cast<ARingCCC::ElementType&>(gS);
  R.set(result_gR, &gS1.re, &gS1.im);
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
    
template <typename RingR>
bool mylift(const RingR& R,
            const ARingRRi& S,
            typename RingR::ElementType& result_gR,
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
                   const ARingRR& S,
                   ARingQQ::ElementType& fR,
                   const ARingRR::ElementType& fS)
{
  (void) S;
  return R.set(fR, fS);
}

inline bool mylift(const ARingQQ& R,
                   const ARingRRR& S,
                   ARingQQ::ElementType& fR,
                   const ARingRRR::ElementType& fS)
{
  (void) S;
  return R.set(fR, &fS);
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
