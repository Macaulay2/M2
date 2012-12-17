#include <gtest/gtest.h>
#include <gc/gc.h>

extern "C" int breakOnMe () { return 0; }

int break1 = breakOnMe ();
float pi = 3.1415;
int break2 = breakOnMe ();

#ifdef NDEBUG
#define GC_IGNORE_WARN
#endif
#define GC_FREE_SPACE_DIVISOR 12
#define GC_INITIAL_HEAP_SIZE 70000000

int main(int argc, char **argv) {
    GC_INIT();
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
