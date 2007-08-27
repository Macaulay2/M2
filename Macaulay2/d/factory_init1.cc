#include "M2inits.h"
#include <stdio.h>
#include <stdlib.h>
void factory_init1(){}
int factory_init1_run = 0;
static struct C {
     C () { 
	  if (factory_init2_run) {
	       fprintf(stderr, "internal error: factory_init2 initialized before factory_init1\n");
	       exit(1);
	  }
	  M2inits();
	  enterFactory(); 
	  factory_init1_run = 1; 
     }
     ~C () { enterM2(); }
} x;
