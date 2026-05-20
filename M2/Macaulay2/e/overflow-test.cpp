/**
 * @file overflow-test.cpp
 * @brief Standalone microbenchmark and overflow-trip harness for the `safe::` primitives in `overflow.hpp`.
 *
 * Not part of the engine library --- a separately-compiled
 * binary (`make DEPENDS=no overflow-test`) developers reach for
 * to confirm overflow detection and measure the per-call cost
 * of `safe::add`, `safe::mult`, `safe::sub`, `safe::div`,
 * `safe::minus`, and `safe::ov` (sub-nanosecond-per-iteration on
 * modern hardware). `main` has two modes: with a command-line
 * argument in `{sub, add, mult, div, minus, throw}` it deliberately
 * passes overflowing operands (`safe::add(INT_MAX, 1)`, etc.) and
 * exits with the expected fatal error, so the developer can
 * verify each detector individually; with no argument it runs a
 * hot loop of `outer * inner * roll = 20000 * 5000 * 100 = 10^10`
 * iterations of a chosen `stmt` macro --- the preserved
 * `#if 0` block holds the developer's previous timing
 * comparisons (`y = j + x` vs `y = safe::add(j, x)` at 10^10
 * reps, etc.) and the bare-`x = 1` loop-overhead measurements
 * (single assignment 4.488s for 5*10^9 reps ≈ 0.9ns each, pair
 * 8.973s ≈ 1.8ns per pair) that quantify how much of the
 * per-iteration cost is overflow checking versus loop overhead.
 *
 * Build with optimisation but no LTO so call sites stay
 * distinct; no engine state is involved.
 *
 * @see overflow.hpp
 */

#define outer 20000
#define inner 5000

#include <stdio.h>
#include "overflow.hpp"
#include "assert.h"
#include <stdlib.h>
#include <string.h>

#if 0
     return 0;
               x = safe::add(j,s); //  /2000000000 repetitions
// this pair of timings shows that there is virtually no loop overhead
#define stmt x = 1, x = 1  // 0m8.973s/5000000000 repetitions, no power
#define stmt x = 1         // 0m4.488s/5000000000 repetitions, no power

#define stmt y = j + x  // 0m 9.065s/10000000000
#define stmt \
  y = safe::add(j, x)  // 0m18.569s/10000000000, no long long, pretty good!
#define TRYLONGLONG
#define stmt y = safe::add(j, x)  // 1m46.455s/10000000000, long long, too slow

#define stmt y = j * x             //   0m2.408s/1000000000
#define stmt y = safe::mult(j, x)  // 0m15.361s/1000000000, no long long
#define TRYLONGLONG
#define stmt y = safe::mult(j, x)  // 0m15.369s/1000000000, long long

#define stmt y = -x              //  0m9.057s/10000000000
#define stmt y = safe::minus(x)  // 0m15.205s/10000000000, no long long

#endif

//=============================================================================

#define stmt x = 1

volatile int s0 = 100, x = 200;

int main(int argc, char **argv)
{
  int i, j;
  int s = s0;
  s0 = s;  // use s at least once
  //   use one of these to see if overflows are really detected.  Each one
  //   should generate an overflow and a program termination.
  if (argc > 1)
    {
      if (0 == strcmp(argv[1], "sub"))
        {
          printf("test failed, result %x\n",
                 (unsigned int)safe::sub(0x80000000, 1 - x + x));
          exit(1);
        }
      else if (0 == strcmp(argv[1], "add"))
        {
          printf("test failed, result %x\n",
                 (unsigned int)safe::add(0x7fffffff, 1 - x + x));
          exit(1);
        }
      else if (0 == strcmp(argv[1], "mult"))
        {
          printf("test failed, result %x\n",
                 (unsigned int)safe::mult(0x8000, 0x10000 - x + x));
          exit(1);
        }
      else if (0 == strcmp(argv[1], "div"))
        {
          printf("test failed, result %x\n",
                 (unsigned int)safe::div(0x80000000, -1 - x + x));
          exit(1);
        }
      else if (0 == strcmp(argv[1], "minus"))
        {
          printf("test failed, result %x\n",
                 (unsigned int)safe::minus(0x80000000 - x + x));
          exit(1);
        }
      else if (0 == strcmp(argv[1], "throw"))
        {
          safe::ov("throw overflow exception");
          printf("test failed\n");
          exit(1);
        }
      else
        {
          printf("unknown test: %s\n", argv[1]);
          exit(1);
        }
    }
#define roll 100
  printf("%lld repetitions\n", (long long)inner * outer * roll);
  for (i = outer; i > 0; i--)
    for (j = 0; j < inner; j++)
      {  // 100 times ( == roll, defined above )
        {
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt; /* 10 times */
        }
        {
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt; /* 10 times */
        }
        {
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt; /* 10 times */
        }
        {
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt; /* 10 times */
        }
        {
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt; /* 10 times */
        }
        {
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt; /* 10 times */
        }
        {
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt; /* 10 times */
        }
        {
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt; /* 10 times */
        }
        {
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt; /* 10 times */
        }
        {
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt;
          stmt; /* 10 times */
        }
      }
}

// Local Variables:
// compile-command: "cd $M2BUILDDIR/Macaulay2/e && make DEPENDS=no overflow-test
// && time ./overflow-test"
// indent-tabs-mode: nil
// End:
