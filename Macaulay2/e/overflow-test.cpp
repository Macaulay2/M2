#define outer 20000
#define inner 5000

#include <stdio.h>
#include "overflow.hpp"
#include "assert.h"

#if 0
     return 0;
	       x = safe::add(j,s); //  /2000000000 repetitions
// this pair of timings shows that there is virtually no loop overhead
#define stmt x = 1, x = 1	// 0m8.973s/5000000000 repetitions, no power
#define stmt x = 1		// 0m4.488s/5000000000 repetitions, no power

#define stmt y = j + x		// 0m 9.065s/10000000000
#define stmt y = safe::add(j,x) // 0m18.569s/10000000000, no long long, pretty good!
#define TRYLONGLONG
#define stmt y = safe::add(j,x) // 1m46.455s/10000000000, long long, too slow

#define stmt y = j * x		//   0m2.408s/1000000000
#define stmt y = safe::mult(j,x) // 0m15.361s/1000000000, no long long
#define TRYLONGLONG
#define stmt y = safe::mult(j,x) // 0m15.369s/1000000000, long long

#define stmt y = - x		//  0m9.057s/10000000000
#define stmt y = safe::minus(x) // 0m15.205s/10000000000, no long long

#endif

//=============================================================================

volatile int s0 = 100, x = 200, y;

int main () {
     int i,j;
     int s=s0;
     s0=s;			// use s at least once
#    define roll 100
     printf("%Ld repetitions\n",(long long)inner*outer*roll);
     for (i=outer; i>0; i--) for (j=0; j<inner; j++) { // 100 times ( == roll, defined above )
	  { stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; /* 10 times */ }
	  { stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; /* 10 times */ }
	  { stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; /* 10 times */ }
	  { stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; /* 10 times */ }
	  { stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; /* 10 times */ }
	  { stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; /* 10 times */ }
	  { stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; /* 10 times */ }
	  { stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; /* 10 times */ }
	  { stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; /* 10 times */ }
	  { stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; stmt; /* 10 times */ } } }

// Local Variables:
// compile-command: "cd $M2BUILDDIR/Macaulay2/e && make DEPENDS=no overflow-test-demangled.s overflow-test && time ./overflow-test"
// End:
