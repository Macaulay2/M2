#include "M2inits.h"

void factory_init1(){}

int factory_init1_run = 0;

static void f() __attribute__ ((constructor));
static void f() { enterFactory(); factory_init1_run = 1; }

static void g() __attribute__ ((destructor));
static void g() { enterM2(); }

static struct C {
      C () { enterFactory(); factory_init1_run = 1; }
     ~C () { enterM2(); }
} x;
