#include "M2inits.h"
#include <stdio.h>
#include <stdlib.h>
extern "C" void factory_init1(){}
int factory_init1_run = 0;
static struct factory_init1_class {
     factory_init1_class () { 
	  if (factory_init2_run) {
	       fprintf(stderr, "internal error: factory_init2 initialized before factory_init1\n");
	       exit(1);
	  }
	  M2inits();
	  enterFactory(); 
	  factory_init1_run = 1; 
     }
     ~factory_init1_class () { enterM2(); }
} x1;
