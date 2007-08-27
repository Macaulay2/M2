#include "M2inits.h"
void M2inits2(){}
static struct C2 {
      C2() { M2inits(); }
     ~C2() { enterM2(); }
} x;
