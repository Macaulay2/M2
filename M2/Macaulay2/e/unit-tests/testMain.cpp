/**
 * @file unit-tests/testMain.cpp
 * @brief `main()` of the `M2-unit-tests` binary --- boots the engine, then runs gtest.
 *
 * Three calls in strict order: `IM2_initialize()` brings up the
 * engine's static state (global rings, monoid prefab tables,
 * the bdwgc collector, FLINT / MPFR singletons) so every
 * subsequent test sees a live engine; `InitGoogleTest` parses
 * gtest's `--gtest_filter` / `--gtest_repeat` and similar flags
 * out of `argv`; `RUN_ALL_TESTS` dispatches to gtest's actual
 * driver and returns its summary status as the process exit
 * code. The ordering is load-bearing --- engine calls during
 * test setup would crash on uninitialised globals if `IM2_initialize`
 * ran last.
 *
 * The companion harness files (`M2-cpp-replacement.cpp` stubs
 * out `system_interrupted`, `fromStream.cpp` carries per-ring
 * stream-parser specialisations, `util-polyring-creation.*`
 * gives tests one-line polynomial-ring builders) are all
 * covered by the same `file-test-harness` markdown.
 *
 * @see engine.h
 * @see ARingTest.hpp
 * @see RingTest.hpp
 * @see util-polyring-creation.hpp
 */

#include <gtest/gtest.h>
#include <M2/gc-include.h>
#include <engine.h>

int main(int argc, char **argv)
{
  IM2_initialize();
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
