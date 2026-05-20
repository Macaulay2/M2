/* Copyright 2010-2021, Michael E. Stillman */

#ifndef _hilb_fcn_h_
#define _hilb_fcn_h_

/**
 * @file f4/hilb-fcn.hpp
 * @brief `HilbertController` --- early-exit driver for F4 given a known Hilbert series.
 *
 * Declares `HilbertController`, the helper F4 consults when the
 * user supplies the expected Hilbert series of the input ideal.
 * `setDegree(d)` reads the expected number of new generators in
 * degree `d` off the supplied Hilbert series and stores it in
 * `hilb_n_in_degree`; `addMonomial(...)` decrements the counter
 * each time F4 commits a new leading monomial of that degree;
 * `nRemainingExpected()` lets F4 abandon any remaining
 * degree-`d` S-pairs once the counter hits zero. Skipping the
 * unproductive pairs avoids the matrix builds they would have
 * triggered and the cascading work at later degrees.
 *
 * Constructed from a target `FreeModule*` (so the Hilbert
 * function is interpreted in the right multi-grading) and a
 * `RingElement* hf` carrying the user-supplied series.
 * `hilb.hpp` is what produces the series in the first place.
 *
 * @see f4.hpp
 * @see hilb.hpp
 */

#include "newdelete.hpp"  // for VECTOR
#include "ringelem.hpp"   // for vec

class FreeModule;
class Matrix;
class MatrixConstructor;
class PolynomialRing;
class RingElement;

/**
 * @brief Hilbert-function-driven early termination helper used by `F4GB`
 * to skip degrees the user-supplied Hilbert series predicts hold no
 * new basis elements.
 *
 * @details Constructed with the source `FreeModule` and a `RingElement* hf`
 * encoding the expected Hilbert series. `setDegree(d)` advances
 * the controller to degree `d` and exposes `nRemainingExpected()`,
 * the number of basis-element insertions the series still
 * predicts at that degree. `addMonomial(a, comp)` is called for
 * each newly minimal monomial and decrements the count; when the
 * count hits zero the driver can skip the rest of the degree.
 */
class HilbertController : public our_new_delete
{
 public:
  HilbertController(const FreeModule *F0, const RingElement *hf);
  ~HilbertController();

  int nRemainingExpected() { return hilb_n_in_degree; }
  // This number is decremented each time addMonomial is called.

  bool setDegree(int this_degree);
  // recomputes Hilbert function, if necessary.
  // returns false if computation was interrupted
  //   After this call, nRemainingExpected() returns the number of elements
  //   expected in this degree

  bool addMonomial(int *a, int comp);
  // add in a new monomial
  // returns true if no more GB elements expected in this degree

 private:
  bool recomputeHilbertFunction();
  Matrix *make_lead_term_matrix();

 private:
  const PolynomialRing *R;
  const FreeModule *F;
  MatrixConstructor *leadterms;

  bool hilb_new_elems;   // True if any new elements since HF was last computed
  int hilb_n_in_degree;  // The number of new elements that we expect to find
                         // in the current degree.

  const RingElement
      *hf_orig;  // The Hilbert function that we are given at the beginning
  RingElement
      *hf_diff;  // The difference between hf_orig and the computed hilb fcn

  VECTOR(vec) elems;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
