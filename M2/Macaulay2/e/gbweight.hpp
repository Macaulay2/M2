// Copyright 2004.  Michael E. Stillman

#ifndef _gbweight_hpp_
#define _gbweight_hpp_

/**
 * @file gbweight.hpp
 * @brief `GBWeight` --- packed-weight evaluator that drives S-pair selection.
 *
 * Declares `GBWeight`, the helper that GB code consults to assign
 * a heuristic weight to a `gbvector` or to a single
 * `(monomial, component)` pair. The single constructor takes a
 * `FreeModule` and an `M2_arrayint` weight vector; the `GBRing`
 * pointer is derived from `F->get_ring()->cast_to_PolynomialRing()`.
 * If the weight argument is `nullptr` or has the wrong length,
 * `GBWeight` falls back to the variables' primary degrees (subbed
 * with `1` where the primary degree is non-positive) and turns on
 * `use_component_degrees_` only when *every* primary degree is
 * positive --- a single non-positive primary degree turns it back
 * off mid-loop. An explicit weight vector always disables that
 * flag. `GBWeight` itself does not choose an S-pair selection
 * policy --- callers use the returned weight as the sort key.
 *
 * The implementation is a packed dot product of the exponent
 * vector with `wts_` (via `exponents::weight`), plus a
 * constant-time `Fdegs_[comp]` lookup when component degrees are
 * enabled and `comp > 0`. The two-argument
 * `gbvector_weight(f, lead_term_weight)` returns the maximum
 * weight over all terms of `f` and writes the lead-term weight
 * to its out-parameter, letting an S-pair queue snapshot both
 * numbers in one walk.
 *
 * @see gbring.hpp
 * @see spair.hpp
 * @see gb-default.hpp
 */

#include "engine-includes.hpp"  // for M2_arrayint

#include "ExponentVector.hpp"   // for const_exponents
#include "monoid.hpp"           // for const_monomial
#include "newdelete.hpp"        // for our_new_delete

class FreeModule;
class GBRing;
class gbvector;

/**
 * @brief Heuristic-weight evaluator for `gbvector`s, used during Groebner
 * basis computation to drive the S-pair sugar strategy.
 *
 * @details Caches a per-variable weight vector (`wts_`), an optional flag
 * `use_component_degrees_` that mixes in the target free module's
 * component degrees, and a `Fdegs_` table of per-component degree
 * shifts pulled from `F_`. The weight of a `gbvector` is then the
 * `wts_`-weighted sum of the leading monomial's exponents plus
 * `Fdegs_[component]`, computed without allocating a temporary
 * exponent vector longer than `exp_size`.
 */
class GBWeight : public our_new_delete
// A class to compute the "heuristic weight" of elements
// Mainly for use with Groebner basis computation
{
  M2_arrayint wts_;
  bool use_component_degrees_;
  const FreeModule *F_;

  int nvars_;
  GBRing *R_;

  size_t exp_size;  // length in bytes of temporary exponent vectors
  long *Fdegs_;

 public:
  GBWeight(const FreeModule *F, M2_arrayint wts);

  ~GBWeight() {}
  M2_arrayint get_weights() const { return wts_; }
  // All of these routines expect components in the range 1..N,
  // or, if the component is 0, then it is in the ring, not in the
  // free module F_

  int exponents_weight(const_exponents e, int comp) const;

  int gbvector_term_weight(const gbvector *f) const;

  int gbvector_weight(const gbvector *f) const;

  int gbvector_weight(const gbvector *f, int &lead_term_weight) const;

  int monomial_weight(const_monomial m, int comp) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
