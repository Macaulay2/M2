// Copyright 1998  Michael E. Stillman

#ifndef M2_COMPUTATIONS_LLL_HPP_
#define M2_COMPUTATIONS_LLL_HPP_

#include "basic-mutable-matrices/mat.hpp"
#include "ring-elements/ring-element.hpp"

class LLLoperations
{
  static bool checkThreshold(ring_elem num, ring_elem den);

  static bool Lovasz(MutableMatrix *lambda,
                     int k,
                     ring_elem alphaTop,
                     ring_elem alphaBottom);

  static void REDI(
      int k,
      int ell,
      MutableMatrix *A,
      MutableMatrix *Achange,  // if non-NULL, should have same ncols as A
      MutableMatrix *lambda);
  static void SWAPI(
      int k,
      int kmax,
      MutableMatrix *A,
      MutableMatrix *Achange,  // if non-NULL, should have same ncols as A
      MutableMatrix *lambda);

  static bool initializeLLL(const MutableMatrix *A,
                            gmp_QQ threshold,
                            MutableMatrix *&LLLstate);
  // Returns false if there is an error, and sets gError.

  static int doLLL(MutableMatrix *A,
                   MutableMatrix *Achange,
                   MutableMatrix *LLLstate,
                   int nsteps = -1);
  // Return values: COMP_DONE, COMP_DONE_STEPS, COMP_INTERRUPTED
 public:
  ///////////////////////////
  // packaged LLL routines //
  ///////////////////////////

  // This routine return false if the computation is interrupted
  static bool LLL(MutableMatrix *M, MutableMatrix *U, gmp_QQ threshold);
  // M is replaced with the LLL basis. false is returned if there was an
  // error or an interrupt.
  // U is either NULL, or an m by m matrix over ZZ (m = numrows M), which will
  // be made into the transform matrix.
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
