// Copyright 2013 Michael E. Stillman.

#ifndef _aring_qq_hpp_
#define _aring_qq_hpp_

/**
 * @file aring-qq.hpp
 * @brief Tiny dispatcher header that picks the default `ARingQQ` from among the QQ aring implementations.
 *
 * Pulls in both `aring-qq-flint.hpp` and `aring-qq-gmp.hpp`,
 * forward-declares `ARingQQFlint` and `ARingQQGMP`, and aliases the
 * default `ARingQQ` to `ARingQQGMP`. The GMP default is historical:
 * FLINT-backed QQ is now faster in most cases but the typedef has not
 * been flipped to avoid disturbing existing engine code paths. Both
 * concrete classes remain fully functional and accessible by name;
 * `ARingQQ` exists so consumers like `DMat<ARingQQ>` and
 * `mat-linalg.hpp` template specialisations can say "the QQ aring"
 * without committing to a backend.
 *
 * Switching the engine-wide default is a one-line edit to this file.
 * The same pattern shows up in `schreyer-resolution/res-moninfo.hpp`
 * for dense vs. sparse `ResMonoid`.
 *
 * @see aring-qq-flint.hpp
 * @see aring-qq-gmp.hpp
 * @see aring.hpp
 */

#include "aring-qq-flint.hpp"
#include "aring-qq-gmp.hpp"

namespace M2 {
class ARingQQFlint;
class ARingQQGMP;

typedef ARingQQGMP ARingQQ;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
