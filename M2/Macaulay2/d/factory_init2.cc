#include "M2inits.h"

void factory_init2() __attribute__ ((constructor));
void factory_init2() { enterFactory(); }

void factory_final2() __attribute__ ((destructor));
void factory_final2() { enterFactory(); }

struct C {
      C () { enterFactory(); }
     ~C () { enterFactory(); }
} factory_init2_x;
