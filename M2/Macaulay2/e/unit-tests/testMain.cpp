#include <gtest/gtest.h>
#include <gc/gc.h>
extern "C"  void IM2_initialize(void);

extern "C" int breakOnMe () { return 0; }

int break1 = breakOnMe ();
double pi = 3.1415;
int break2 = breakOnMe ();

#ifdef NDEBUG
#define GC_IGNORE_WARN
#endif
#define GC_FREE_SPACE_DIVISOR 12
#define GC_INITIAL_HEAP_SIZE 70000000

int main(int argc, char **argv) {
    GC_INIT();
    IM2_initialize();
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
