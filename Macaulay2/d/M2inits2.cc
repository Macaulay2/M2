#include "M2inits.h"
void M2inits2(){}
static struct M2inits2_class {
      M2inits2_class() { M2inits(); }
     ~M2inits2_class() { enterM2(); }
} x;
