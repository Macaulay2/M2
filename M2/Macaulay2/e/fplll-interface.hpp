/**
 * @file fplll-interface.hpp
 * @brief Engine-side wrapper around the external fplll lattice-reduction library.
 *
 * Declares a single entry point, `fp_LLL(M, U, strategy)`: `M`
 * holds the lattice basis with one basis vector per column and
 * is reduced in place. The current implementation asserts
 * `U == NULL` and ignores `strategy`; internally it always
 * configures fplll with `delta = 0.99`, `eta = 0.51`, and
 * `LM_WRAPPER` --- the wrapper that picks an LLL variant
 * automatically based on the input size. Returns `true` on
 * success and reports the underlying `RED_*` error on Babai or
 * LLL failure.
 *
 * The whole body of `fplll-interface.cpp` is gated behind
 * `#ifdef HAVE_FPLLL`; when fplll is not configured in, the
 * function raises `ERROR("fplll is not available ...")` and
 * returns `0` so callers don't have to write their own
 * `#ifdef`. The other in-tree LLL backends are `LLL.hpp`
 * (`LLLoperations`, the pure-engine implementation) and
 * `ntl-interface.hpp::ntl_LLL` (NTL); the top-level M2 `LLL`
 * routine chooses between them.
 *
 * @see mat.hpp
 * @see LLL.hpp
 * @see ntl-interface.hpp
 */

class MutableMatrix;

bool fp_LLL(MutableMatrix *M, MutableMatrix *U, int strategy);

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
