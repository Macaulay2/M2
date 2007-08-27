#include "M2inits.h"
#include <stdio.h>
#include <stdlib.h>

void factory_init2(){}

int factory_init2_run = 0;

static void f() __attribute__ ((constructor));
static void f() { 
  if (!factory_init1_run) {
    fprintf(stderr, "internal error: factory_init1 not initialized before factory_init2\n");
    exit(1);
  }
  enterM2();
  factory_init2_run = 1;
}

static void g() __attribute__ ((destructor));
static void g() { enterFactory(); }

static struct C {
      C () { enterM2(); }
     ~C () { enterFactory(); }
} x;
