#include <gtest/gtest.h>

#include "stdinc.hpp"
#include "moninfo.hpp"

TEST(testingTest, first) {
  EXPECT_EQ(4, 4);
}

TEST(moninfo, create) {
  MonomialOrdering mo(4);
  MonomialInfo M(mo);
  M.show();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions check "
// indent-tabs-mode: nil
// End:
