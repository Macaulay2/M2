#include <stdio.h>
#include "targettypes.h"

union {
     int32 x;
     struct { int8 a,b,c,d; } s;
} u;

int main () {
     u.s.a = 1;
     u.s.b = 2;
     u.s.c = 3;
     u.s.d = 4;
     printf("#define OVERLAY 0x%08x\n",u.x);
     return 0;
}
