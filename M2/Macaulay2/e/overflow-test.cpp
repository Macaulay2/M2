#include <stdio.h>
#undef TRYLONGLONG
#include "overflow.hpp"
#include "assert.h"

#define e8 10000000
#define outer (e8)
#define inner 300

int s;
volatile int x;

int main () {
     int i,j;
     printf("%Ld repetitions\n",(long long)inner*outer);
     // safe::add(0x7fffffff,0x7fffffff);
     for (i=outer; i>0; i--)
	  for (j=0; j<inner; j++)
	       // x = j+s;
	       x = safe::add(j,s);
}

// Local Variables:
// compile-command: "cd $M2BUILDDIR/Macaulay2/e && make DEPENDS=no overflow-test-demangled.s overflow-test && time ./overflow-test"
// End:
