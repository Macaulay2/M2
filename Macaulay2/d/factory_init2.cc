#include "M2inits.h"

void factory_init2(){}

static void f() __attribute__ ((constructor));
static void f() { enterFactory(); }

static void g() __attribute__ ((destructor));
static void g() { enterFactory(); }

static struct C {
      C () { enterFactory(); }
     ~C () { enterFactory(); }
} x;
