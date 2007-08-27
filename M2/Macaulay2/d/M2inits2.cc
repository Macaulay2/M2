#include "M2inits.h"

static void f(void) __attribute__ ((constructor));
static void f(void) { M2inits(); enterM2(); }

static void g(void) __attribute__ ((destructor));
static void g(void) { enterM2(); }

static struct C2 {
      C2() { M2inits(); enterM2(); }
     ~C2() { enterM2(); }
} x;
