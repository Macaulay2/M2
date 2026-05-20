// Copyright 2011 Michael E. Stillman

#ifndef _cra_hpp_
#define _cra_hpp_

/**
 * @file cra.hpp
 * @brief `ChineseRemainder` --- CRT lifting and rational reconstruction primitives.
 *
 * Declares `ChineseRemainder`, a static-only collection that
 * combines modular results back into `ZZ` and `QQ`. `CRA0` is
 * the core step: given `a mod m`, `b mod n` with `gcd(m, n) = 1`
 * and the precomputed values `um = u*m`, `vn = v*n`, `mn = m*n`
 * (where `1 = u*m + v*n`), it produces the unique value reducing
 * to `a mod m` and `b mod n`; the `.cpp` lifts the result into
 * balanced-residue form on `(-mn/2, mn/2]` so small integers
 * stay small. `computeMultipliers` produces those Bezout terms
 * up front and `CRA` overloads lift `ring_elem`, `vec`, `Matrix`,
 * and `RingElement` element-by-element with the same multipliers.
 *
 * `ratConversion` does rational reconstruction: given `x mod m`,
 * the half-extended-Euclidean run recovers a fraction `p / q`
 * with `p == x * q (mod m)` and returns `true` iff the numerator
 * and denominator can both be chosen smaller than `sqrt(m) / 2`,
 * which is the standard uniqueness threshold. `interface/cra.cpp`
 * exposes the whole class to M2 as the `rawRingElementCRA` /
 * `rawMatrixCRA` / `rawRingElementRatConversion` /
 * `rawMatrixRatConversion` raw entry points.
 *
 * @see interface/cra.h
 */

#include <M2/math-include.h>

#include "ringelem.hpp"  // for ring_elem, vec

class Matrix;
class PolyRing;
class RingElement;

// !!!! we need the balanced residue class in chinese remainder !!!

/**
   Routines for Chinese remaindering and rational reconstruction
*/
class ChineseRemainder
{
 public:
  static void CRA0(mpz_srcptr a,
                   mpz_srcptr b,
                   mpz_srcptr um,
                   mpz_srcptr vn,
                   mpz_srcptr mn,
                   mpz_t result);
  // computes using precomputed multipliers, the unique integer 'result' < m*n
  // s.t.
  // result == a mod m, and == b mod n.
  // 1 = u*m + v*n, and um = u*m, vn = v*n, mn = m*n

  static bool computeMultipliers(mpz_srcptr m,
                                 mpz_srcptr n,
                                 mpz_t result_um,
                                 mpz_t result_vn,
                                 mpz_t result_mn);
  // computes the multipliers um, vn, mn used in CRA0

  static ring_elem CRA(const PolyRing *R,
                       const ring_elem f,
                       const ring_elem g,
                       mpz_srcptr um,
                       mpz_srcptr vn,
                       mpz_srcptr mn);
  // does the vector combination without error checking and with precomputed
  // multipliers

  static vec CRA(const PolyRing *R, vec f, vec g, mpz_srcptr um, mpz_srcptr vn, mpz_srcptr mn);
  // does the vector combination without error checking and with precomputed
  // multipliers

  static Matrix *CRA(const Matrix *f,
                     const Matrix *g,
                     mpz_srcptr um,
                     mpz_srcptr vn,
                     mpz_srcptr mn);
  // does the matrix combination without error checking and with precomputed
  // multipliers

  static RingElement *CRA(const RingElement *f,
                          const RingElement *g,
                          mpz_srcptr um,
                          mpz_srcptr vn,
                          mpz_srcptr mn);
  // does the ring element combination without error checking and with
  // precomputed multipliers

  static bool ratConversion(mpz_srcptr a, mpz_srcptr m, mpq_t result);
  // computes a rational number "result" that reduces to a mod m
  // if the numerator and denominator can be chosen to smaller than
  // 1/2*sqrt(m) then "true" is returned.

  static ring_elem ratConversion(const ring_elem f,
                                 mpz_srcptr m,
                                 const PolyRing *RQ);
  // computes a polynomial with rational coefficients that reduces
  // to f mod m; f should be in a polynomial ring ZZ[M] and RQ=QQ[M]

  static vec ratConversion(vec f, mpz_srcptr m, const PolyRing *RQ);
  // computes a polynomial with rational coefficients that reduces
  // to f mod m; f should be in a polynomial ring ZZ[M] and RQ=QQ[M]

  static ring_elem CRA(const PolyRing *R,
                       ring_elem f,
                       ring_elem g,
                       mpz_srcptr m,
                       mpz_srcptr n);
  // Assumption: f and g are in a poly ring whose coeff ring is ZZ
};

#endif

// Local Variables:
// indent-tabs-mode: nil
// End:
