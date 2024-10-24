#include <gtest/gtest.h>
#include <M2/gc-include.h>
#include <engine.h>

unsigned long gmp_defaultPrecision = 53;

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
