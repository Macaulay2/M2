/* Copyright 2010-2021, Michael E. Stillman */

#ifndef _hilb_fcn_h_
#define _hilb_fcn_h_

#include "newdelete.hpp"  // for VECTOR
#include "ringelem.hpp"   // for vec

class FreeModule;
class Matrix;
class MatrixConstructor;
class PolynomialRing;
class RingElement;

class HilbertController
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
