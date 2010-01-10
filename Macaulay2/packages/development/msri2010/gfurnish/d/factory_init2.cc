#include "M2inits.h"
#include <stdio.h>
#include <stdlib.h>
extern "C" void factory_init2(){}
int factory_init2_run = 0;
static struct factory_init2_class {
     factory_init2_class () { 
	  if (!factory_init1_run) {
	       fprintf(stderr, "internal error: factory_init1 not initialized before factory_init2\n");
	       exit(1);
	  }
	  enterM2();
	  factory_init2_run = 1;
     }
     ~factory_init2_class () { enterFactory(); }
} x2;
