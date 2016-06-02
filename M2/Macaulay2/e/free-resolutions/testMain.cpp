#include <gtest/gtest.h>

int M2_gbTrace = 0;

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions check  "
// indent-tabs-mode: nil
// End:
