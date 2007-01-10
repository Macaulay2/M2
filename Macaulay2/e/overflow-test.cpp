#define outer 10000
#define inner 5000
#include <stdio.h>
#undef TRYLONGLONG
#include "overflow.hpp"
#include "assert.h"

#if 0
     return 0;
	       x = safe::add(j,s); //  /2000000000 repetitions
// this pair of timings shows that there is virtually no loop overhead
#define stmt x = 1, x = 1	// 0m8.973s/5000000000 repetitions, no power
#define stmt x = 1		// 0m4.488s/5000000000 repetitions, no power
#endif
#define stmt x = j+s		// 0m4.368s/5000000000

volatile int s0 = 100, x;

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
