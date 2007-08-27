#include "M2inits.h"
#include <stdio.h>
#include <stdlib.h>

void factory_init1(){}

int factory_init1_run = 0;

static void f() __attribute__ ((constructor));
static void f() { 
     if (factory_init2_run) {
	  fprintf(stderr, "internal error: factory_init2 initialized before factory_init1\n");
	  exit(1);
     }
     enterFactory(); 
     factory_init1_run = 1; 
}

static void g() __attribute__ ((destructor));
static void g() { enterM2(); }

static struct C {
      C () { enterFactory(); factory_init1_run = 1; }
     ~C () { enterM2(); }
} x;
