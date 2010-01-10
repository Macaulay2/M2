#include "M2inits.h"

static void f(void) __attribute__ ((constructor));
static void f(void) { M2inits(); enterM2(); }

static void g(void) __attribute__ ((destructor));
static void g(void) { enterM2(); }

static struct C3 {
      C3() { M2inits(); enterM2(); }
     ~C3() { enterM2(); }
} x;
