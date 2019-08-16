#include "M2inits.h"

static void f(void) __attribute__ ((constructor));
static void f(void) { M2inits(); }

static void g(void) __attribute__ ((destructor));
static void g(void) { }

static struct C3 {
      C3() { M2inits(); }
     ~C3() { }
} x;
