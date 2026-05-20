#ifndef __dmat_test_hpp__
#define __dmat_test_hpp__

/**
 * @file unit-tests/DMatTest.hpp
 * @brief Convention placeholder header that pulls in `dmat.hpp` for the dense-matrix test suite.
 *
 * Two-line header that exists as a stable include point for the
 * dense-matrix tests: every `DMat*Test.cpp` (currently
 * `DMatZZpTest.cpp`, with more planned) reaches the engine's
 * `dmat.hpp` through this file rather than including it
 * directly, so future shared fixtures, helper templates, or
 * `almostEqual`-style epsilon checks can land here without
 * touching every test file. The pattern parallels
 * `ARingTest.hpp` and `RingTest.hpp` for the coefficient-ring
 * suites.
 *
 * @see dmat.hpp
 * @see DMatZZpTest.cpp
 * @see ARingTest.hpp
 * @see RingTest.hpp
 */

#include "dmat.hpp"

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
