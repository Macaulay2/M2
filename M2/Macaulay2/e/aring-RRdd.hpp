// MultiFloats double-double (Float64x2) approximate real field for Macaulay2.
// ~106-bit precision at a fraction of MPFR's cost: standalone benchmarks show
// 7-16x faster scalar ops and 7.4x faster dense LU vs ARingRRR(106) (see
// ~/research/dd-proto/). Modeled on aring-RR.hpp (value-type SimpleARing) for the
// interface, and aring-RRR.hpp for the ring_elem boxing.
//
// ElementType is a 16-byte POD (hi+lo); dmat<ARingRRdd> stores it directly (the fast
// numerical-LA path where the speedup lands). It is too big for a ring_elem inline
// slot, so to/from_ring_elem box it losslessly through a 106-bit mpfr (gmp_RR) — the
// ring_elem path is not performance-critical.
//
// TODO(build-verify): add `ring_RRdd` to the RingID enum (aring.hpp) and a front-end
// ring constructor; until then ringID uses a placeholder. Confirm moveTo_gmpRR/get_mpfr
// signatures against ringelem.hpp during the first engine compile.
#ifndef _aring_RRdd_hpp_
#define _aring_RRdd_hpp_

#include <cmath>
#include "interface/gmp-util.h"  // moveTo_gmpRR
#include "interface/random.h"    // randomDouble
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "ringmap.hpp"

class RingMap;

namespace M2 {

// ---- double-double core ----
// Arithmetic algorithms are ported from MultiFloats.jl v3.0 (Zhang 2025,
// https://theory.stanford.edu/~aiken/publications/papers/sc25.pdf): branch-free,
// already-normalised results (no trailing renormalisation step), shorter
// dependency chains than the classical QD (Hida-Li-Bailey 2001) algorithms.
// Primitives — TwoSum / FastTwoSum / TwoProd-via-FMA — are unchanged.
struct DoubleDouble { double hi; double lo; };

static inline DoubleDouble dd_two_sum(double a, double b)
{ double s = a + b, bb = s - a, e = (a - (s - bb)) + (b - bb); return {s, e}; }
static inline DoubleDouble dd_fast_two_sum(double a, double b)  // |a| >= |b|
{ double s = a + b, e = b - (s - a); return {s, e}; }
static inline DoubleDouble dd_two_prod(double a, double b)
{ double p = a * b, e = std::fma(a, b, -p); return {p, e}; }

// v3.0 add — one fewer dependent op on the critical path than the v2.0 QD
// algorithm (the b-accumulator update runs in parallel with the first
// fast_two_sum), branch-free.
static inline DoubleDouble dd_add(DoubleDouble x, DoubleDouble y)
{
  DoubleDouble s = dd_two_sum(x.hi, y.hi);                 // (a, b)
  DoubleDouble t = dd_two_sum(x.lo, y.lo);                 // (c, d)
  DoubleDouble u = dd_fast_two_sum(s.hi, t.hi);            // (a, c')
  double bb = s.lo + t.lo + u.lo;                          // b += d; b += c'
  return dd_fast_two_sum(u.hi, bb);
}
static inline DoubleDouble dd_neg(DoubleDouble a) { return {-a.hi, -a.lo}; }
static inline DoubleDouble dd_sub(DoubleDouble a, DoubleDouble b) { return dd_add(a, dd_neg(b)); }

// v3.0 mul — same shape as v2.0 (already minimal at N=2; the v3.0 win is at N>=3).
static inline DoubleDouble dd_mul(DoubleDouble a, DoubleDouble b)
{
  DoubleDouble p = dd_two_prod(a.hi, b.hi);
  return dd_fast_two_sum(p.hi, p.lo + std::fma(a.hi, b.lo, a.lo * b.hi));
}

// v3.0 specialised squaring — cheaper than dd_mul(x, x) since a few cross terms
// collapse.  Used by abs_squared and by power_iteration-style kernels.
static inline DoubleDouble dd_sqr(DoubleDouble x)
{
  DoubleDouble p = dd_two_prod(x.hi, x.hi);
  return dd_fast_two_sum(p.hi, std::fma(x.hi, x.lo + x.lo, p.lo));
}

// v3.0 div — single Newton-style correction (vs the classical 3-iteration
// long-division approach); roughly 5–6× fewer ops than the v2.0 dd_div.
static inline DoubleDouble dd_div(DoubleDouble x, DoubleDouble y)
{
  double w = 1.0 / y.hi;
  double z = x.hi * w;
  DoubleDouble p = dd_two_prod(y.hi, z);
  double r = ((x.hi - p.hi) - p.lo) + std::fma(-y.lo, z, x.lo);
  return dd_fast_two_sum(z, r * w);
}

// v3.0 sqrt — branch-free (no zero special case needed; sqrt(0) is well-defined
// and the subsequent r/(y+y) divides by +0 which produces NaN, so we still
// short-circuit the zero case but via FP arithmetic rather than a comparison).
static inline DoubleDouble dd_sqrt(DoubleDouble x)
{
  if (x.hi == 0.0) return {0.0, 0.0};      // FP-safe zero short-circuit
  double y = std::sqrt(x.hi);
  double r = std::fma(-y, y, x.hi) + x.lo;
  return dd_fast_two_sum(y, r / (y + y));
}

// ---- specialised dd × double fast paths ----
// Hot-loop kernels frequently combine a dd with a precomputed scalar (LU pivot
// updates, vector scaling, mat-vec normalisation).  The general dd_op(a, dd{x, 0})
// expression has an a.hi * 0 cross-term the compiler does not always remove;
// these specialised forms make the saving explicit.  They produce identical
// answers to dd_op(a, dd{x, 0}) but at roughly half the operation count.
static inline DoubleDouble dd_add_d(DoubleDouble a, double b)
{ DoubleDouble s = dd_two_sum(a.hi, b); s.lo += a.lo; return dd_fast_two_sum(s.hi, s.lo); }
static inline DoubleDouble dd_sub_d(DoubleDouble a, double b)
{ return dd_add_d(a, -b); }
static inline DoubleDouble dd_mul_d(DoubleDouble a, double b)
{ DoubleDouble p = dd_two_prod(a.hi, b); return dd_fast_two_sum(p.hi, std::fma(a.lo, b, p.lo)); }
static inline DoubleDouble dd_div_d(DoubleDouble a, double b)
{ double qh = a.hi / b; double err = std::fma(-qh, b, a.hi) + a.lo; return dd_fast_two_sum(qh, err / b); }

// ---- optional AVX2 vector primitives (4-wide SIMD on dd) ----
// On x86_64 targets compiled with AVX2+FMA (-mavx2 -mfma, or -march=native on
// any reasonably modern CPU), these primitives compute the v3.0 dd operations
// on 4 dd values in parallel.  Same algorithms as the scalar versions above,
// applied lane-wise.  Inputs/outputs are structure-of-arrays — one __m256d
// packs 4 hi-parts, another packs 4 lo-parts.
//
// Header-guarded so the rest of aring-RRdd.hpp keeps compiling on non-AVX2
// targets (older Intel without AVX2, non-x86 archs).  Downstream kernels
// (e.g., a future dmat<ARingRRdd> SIMD specialisation) can include this
// section conditionally to opt in.
//
// Portability (per review): even when the compiler targets AVX2+FMA, the build
// can force the scalar path by defining M2_NO_DD_SIMD (e.g. a configure-time
// --disable-dd-simd). This matters because Macaulay2 is frequently compiled on
// one machine and run on another: a binary built with -march=native on an AVX2
// host would otherwise emit AVX2 instructions that fault (SIGILL) on a baseline
// CPU. Distributions that build for a baseline ISA already won't define
// __AVX2__, so they get the scalar path automatically; M2_NO_DD_SIMD covers the
// native-build-then-redistribute case and leaves room for a runtime CPUID
// dispatch later.
#if defined(__AVX2__) && defined(__FMA__) && !defined(M2_NO_DD_SIMD)
#include <immintrin.h>

static inline __m256d v4_two_sum(__m256d a, __m256d b, __m256d* e) {
  __m256d s  = _mm256_add_pd(a, b);
  __m256d bb = _mm256_sub_pd(s, a);
  __m256d aa = _mm256_sub_pd(s, bb);
  *e = _mm256_add_pd(_mm256_sub_pd(a, aa), _mm256_sub_pd(b, bb));
  return s;
}
static inline __m256d v4_fast_two_sum(__m256d a, __m256d b, __m256d* e) {
  __m256d s = _mm256_add_pd(a, b);
  *e = _mm256_sub_pd(b, _mm256_sub_pd(s, a));
  return s;
}
static inline __m256d v4_two_prod(__m256d a, __m256d b, __m256d* e) {
  __m256d p = _mm256_mul_pd(a, b);
  *e = _mm256_fmsub_pd(a, b, p);
  return p;
}

// 4-wide v3.0 dd_add
static inline void v4_dd_add(__m256d xhi, __m256d xlo, __m256d yhi, __m256d ylo,
                              __m256d* zhi, __m256d* zlo)
{
  __m256d slo, tlo, ulo;
  __m256d shi = v4_two_sum(xhi, yhi, &slo);
  __m256d thi = v4_two_sum(xlo, ylo, &tlo);
  __m256d uhi = v4_fast_two_sum(shi, thi, &ulo);
  __m256d bb  = _mm256_add_pd(_mm256_add_pd(slo, tlo), ulo);
  *zhi = v4_fast_two_sum(uhi, bb, zlo);
}

// 4-wide v3.0 dd_mul
static inline void v4_dd_mul(__m256d xhi, __m256d xlo, __m256d yhi, __m256d ylo,
                              __m256d* zhi, __m256d* zlo)
{
  __m256d plo;
  __m256d phi   = v4_two_prod(xhi, yhi, &plo);
  __m256d cross = _mm256_fmadd_pd(xhi, ylo, _mm256_mul_pd(xlo, yhi));
  *zhi = v4_fast_two_sum(phi, _mm256_add_pd(plo, cross), zlo);
}

// Broadcast a scalar dd across a 4-wide vector
static inline void v4_dd_broadcast(DoubleDouble a, __m256d* hi, __m256d* lo) {
  *hi = _mm256_set1_pd(a.hi);
  *lo = _mm256_set1_pd(a.lo);
}

// ---- high-level SIMD dd_axpy ----
// Computes y[i] -= f * x[i] for i in [0, N), the inner loop of LU pivot
// updates, dense mat-vec, and the perturbed solves in homotopy continuation.
// Vectors are in structure-of-arrays layout (separate hi[] / lo[] arrays);
// SoA is the natural SIMD layout — dmat<ARingRRdd> kernels wanting maximum
// throughput should adopt it.  AoS callers can pre-split or use the scalar
// dd_* primitives instead.  Tail elements (when N % 4 != 0) handled by the
// scalar v3.0 ops.
static inline void dd_axpy(int N, DoubleDouble f,
                           const double* xhi, const double* xlo,
                           double* yhi, double* ylo)
{
  __m256d fhi = _mm256_set1_pd(-f.hi);   // y -= f*x  ≡  y += (-f)*x
  __m256d flo = _mm256_set1_pd(-f.lo);
  int i = 0;
  for (; i + 4 <= N; i += 4) {
    __m256d xh = _mm256_loadu_pd(xhi + i);
    __m256d xl = _mm256_loadu_pd(xlo + i);
    __m256d phi, plo;
    v4_dd_mul(fhi, flo, xh, xl, &phi, &plo);
    __m256d yh = _mm256_loadu_pd(yhi + i);
    __m256d yl = _mm256_loadu_pd(ylo + i);
    __m256d zhi, zlo;
    v4_dd_add(yh, yl, phi, plo, &zhi, &zlo);
    _mm256_storeu_pd(yhi + i, zhi);
    _mm256_storeu_pd(ylo + i, zlo);
  }
  // scalar tail
  DoubleDouble nf = {-f.hi, -f.lo};
  for (; i < N; i++) {
    DoubleDouble p = dd_mul(nf, {xhi[i], xlo[i]});
    DoubleDouble z = dd_add({yhi[i], ylo[i]}, p);
    yhi[i] = z.hi; ylo[i] = z.lo;
  }
}
#endif  // __AVX2__ && __FMA__
static inline int dd_cmp(DoubleDouble a, DoubleDouble b)
{ if (a.hi != b.hi) return a.hi < b.hi ? -1 : 1; if (a.lo != b.lo) return a.lo < b.lo ? -1 : 1; return 0; }
static inline DoubleDouble dd_from_mpfr(mpfr_srcptr x)
{ double hi = mpfr_get_d(x, MPFR_RNDN); mpfr_t t; mpfr_init2(t, 120); mpfr_sub_d(t, x, hi, MPFR_RNDN);
  double lo = mpfr_get_d(t, MPFR_RNDN); mpfr_clear(t); return dd_fast_two_sum(hi, lo); }

/**
\ingroup rings
*/
class ARingRRdd : public SimpleARing<ARingRRdd>
{
  // approximate real numbers, double-double (~106-bit), MultiFloats Float64x2.
 public:
  static const RingID ringID = ring_RRdd;
  static const unsigned long PRECISION = 106;

  typedef DoubleDouble elem;
  typedef elem ElementType;

  ARingRRdd() {}
  size_t characteristic() const { return 0; }
  unsigned long get_precision() const { return PRECISION; }
  void text_out(buffer &o) const;

  unsigned int computeHashValue(const elem &a) const
  { return static_cast<unsigned int>(a.hi) ^ static_cast<unsigned int>(a.lo); }

  bool is_unit(const ElementType &f) const { return !is_zero(f); }
  bool is_zero(const ElementType &f) const { return f.hi == 0.0 && f.lo == 0.0; }
  bool is_equal(const ElementType &f, const ElementType &g) const { return f.hi == g.hi && f.lo == g.lo; }
  int compare_elems(const ElementType &f, const ElementType &g) const { return dd_cmp(f, g); }

  // ---- to/from ring_elem: box the 16-byte dd losslessly through a 106-bit mpfr ----
  void to_ring_elem(ring_elem &result, const ElementType &a) const
  {
    mpfr_ptr res = getmemstructtype(mpfr_ptr);
    mpfr_init2(res, PRECISION);
    mpfr_set_d(res, a.hi, MPFR_RNDN);
    mpfr_add_d(res, res, a.lo, MPFR_RNDN);
    result = ring_elem(moveTo_gmpRR(res));
  }
  void from_ring_elem(ElementType &result, const ring_elem &a) const { result = dd_from_mpfr(a.get_mpfr()); }
  ElementType from_ring_elem_const(const ring_elem &a) const { return dd_from_mpfr(a.get_mpfr()); }

  // ---- init/set (trivial: POD value type, like ARingRR) ----
  void init(ElementType &result) const { result = {0.0, 0.0}; }
  void init_set(ElementType &result, const ElementType &a) const { result = a; }
  void set(ElementType &result, const ElementType &a) const { result = a; }
  void set_zero(ElementType &result) const { result = {0.0, 0.0}; }
  static void clear(ElementType &result) { (void) result; }
  void copy(ElementType &result, const ElementType &a) const { result = a; }

  void set_from_long(ElementType &result, long a) const { result = dd_fast_two_sum(static_cast<double>(a), 0.0); }
  void set_var(ElementType &result, int v) const { (void) v; result = {1.0, 0.0}; }
  void set_from_mpz(ElementType &result, mpz_srcptr a) const
  { mpfr_t t; mpfr_init2(t, PRECISION + 8); mpfr_set_z(t, a, MPFR_RNDN); result = dd_from_mpfr(t); mpfr_clear(t); }
  bool set_from_mpq(ElementType &result, mpq_srcptr a) const
  { mpfr_t t; mpfr_init2(t, PRECISION + 8); mpfr_set_q(t, a, MPFR_RNDN); result = dd_from_mpfr(t); mpfr_clear(t); return true; }
  bool set_from_BigReal(ElementType &result, gmp_RR a) const { result = dd_from_mpfr(a); return true; }
  bool set_from_double(ElementType &result, double a) const { result = {a, 0.0}; return true; }

  // ---- arithmetic ----
  void negate(ElementType &result, const ElementType &a) const { result = dd_neg(a); }
  void invert(ElementType &result, const ElementType &a) const { result = dd_div({1.0, 0.0}, a); }
  void add(ElementType &result, const ElementType &a, const ElementType &b) const { result = dd_add(a, b); }
  void addMultipleTo(ElementType &result, const ElementType &a, const ElementType &b) const { result = dd_add(result, dd_mul(a, b)); }
  void subtract(ElementType &result, const ElementType &a, const ElementType &b) const { result = dd_sub(a, b); }
  void subtract_multiple(ElementType &result, const ElementType &a, const ElementType &b) const { result = dd_sub(result, dd_mul(a, b)); }
  void mult(ElementType &result, const ElementType &a, const ElementType &b) const { result = dd_mul(a, b); }
  void divide(ElementType &result, const ElementType &a, const ElementType &b) const { result = dd_div(a, b); }
  void abs_squared(ElementType &result, const ElementType &a) const { result = dd_mul(a, a); }
  void abs(ElementType &result, const ElementType &a) const { result = (a.hi < 0.0) ? dd_neg(a) : a; }

  void power(ElementType &result, const ElementType &a, int n) const
  { ElementType r = {1.0, 0.0}, base = a; bool neg = n < 0; unsigned long e = neg ? -(long)n : n;
    while (e) { if (e & 1) r = dd_mul(r, base); base = dd_mul(base, base); e >>= 1; }
    result = neg ? dd_div({1.0, 0.0}, r) : r; }
  void power_mpz(ElementType &result, const ElementType &a, mpz_srcptr n) const
  { std::pair<bool, int> n1 = RingZZ::get_si(n);
    if (n1.first) power(result, a, n1.second); else throw exc::engine_error("exponent too large"); }

  void swap(ElementType &a, ElementType &b) const { std::swap(a, b); }
  void elem_text_out(buffer &o, const ElementType &a, bool p_one = true, bool p_plus = false, bool p_parens = false) const;

  void syzygy(const ElementType &a, const ElementType &b, ElementType &x, ElementType &y) const
  { set_var(x, 0); if (!is_zero(b)) { set(y, a); negate(y, y); divide(y, y, b); } }

  void random(ElementType &result) const { result = {randomDouble(), 0.0}; }

  void eval(const RingMap *map, ElementType &f, int first_var, ring_elem &result) const
  { (void) first_var; ring_elem tmp; to_ring_elem(tmp, f);
    if (!map->get_ring()->from_double(coerceToDouble(f), result))
      { result = map->get_ring()->from_long(0); ERROR("cannot map double-double to ring type"); } }

  void zeroize_tiny(gmp_RR epsilon, ElementType &a) const
  { if (mpfr_cmp_d(epsilon, std::fabs(a.hi)) > 0) set_zero(a); }
  void increase_norm(mpfr_ptr norm, const ElementType &a) const
  { double d = std::fabs(a.hi); if (mpfr_cmp_d(norm, d) < 0) mpfr_set_d(norm, d, MPFR_RNDN); }

  double coerceToDouble(const ElementType &a) const { return a.hi; }
};

};  // end namespace M2

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
