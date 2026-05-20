// Copyright 1998  Michael E. Stillman

#ifndef _LLL_hpp_
#define _LLL_hpp_

/**
 * @file LLL.hpp
 * @brief Lenstra-Lenstra-Lovász integer lattice basis reduction, in place on a `MutableMatrix`.
 *
 * Declares `LLLoperations`, a static-only collection that runs the
 * classical Lenstra-Lenstra-Lovász (1982) algorithm on the columns
 * of a `MutableMatrix` over `ZZ`. The output is an LLL-reduced
 * basis of the same lattice: each Gram-Schmidt projection
 * coefficient is bounded by `1/2` and consecutive squared GS
 * lengths satisfy the Lovász condition with parameter
 * `α = alphaTop/alphaBottom`. Closer to `1` means stronger
 * reduction and slower runtime.
 *
 * The class is intentionally objectless --- `REDI` is the
 * size-reduction step, `Lovasz` is the swap-condition check,
 * `SWAPI` does the column swap, and `initializeLLL` / `doLLL`
 * (with `doLLL`'s `nsteps` argument) coordinate them while a
 * companion `lambda` matrix carries the integer Gram-Schmidt
 * numerators. Reduction happens in place, which is why the
 * routines work on `MutableMatrix` rather than the immutable
 * `Matrix`. The packaged top-level `LLL(M, U, threshold)` is
 * surfaced to M2 by `rawLLL` in `interface/mutable-matrix.cpp`;
 * faster external backends are available through
 * `fplll-interface.hpp` and `ntl-interface.hpp::ntl_LLL`.
 *
 * @see mat.hpp
 * @see fplll-interface.hpp
 * @see ntl-interface.hpp
 */

#include "mat.hpp"
#include "relem.hpp"

/**
 * @brief Static-method namespace for the LLL lattice-basis reduction
 * algorithm operating on a `MutableMatrix`.
 *
 * @details Holds no state --- every method is `static` --- and implements
 * the standard Lovasz / REDI / SWAPI primitives that the engine's
 * LLL driver calls. Works over rings where the comparison
 * threshold `alphaTop / alphaBottom` can be evaluated (typically
 * `QQ` or `ZZ` with a rational alpha).
 */
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
