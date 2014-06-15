#include <stdlib.h>

#include "config.h"
#include <M2/gc-include.h>

struct x { 
     struct x *p[20];
     int h[20];
} *a[20], *b;

main () {
     int i,j,k;
     GC_all_interior_pointers = 1;
     while (1) {
	  for (i=0; i<20 ;i++) {
	       for (j=0; j<20; j++) {
		    b = (struct x *)GC_malloc(sizeof(struct x));
		    for (k=0; k<20; k++) {
			 b->p[k] = a[k];
			 b->h[k] = rand();
		    }
		    a[j] = b;
	       }
	  }
	  for (k=0; k<20; k++) a[k] = NULL;
#ifdef COLLECTOFTEN
	  GC_gcollect();
#endif
     }
}

/* 
ulimit -t 10
export GC_PRINT_STATS=1
gcc foo.c -lgc && ulimit -v 400000 && time ./a.out
gcc -DCOLLECTOFTEN foo.c -lgc && ulimit -v 400000 && time ./a.out
*/
