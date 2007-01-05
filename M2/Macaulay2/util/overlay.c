#include "config.h"
#include <stdio.h>
#if HAVE_STDINT_H
#include <stdint.h>
#elif HAVE_INTTYPES_H
#include <inttypes.h>
#else
#error integer type definitions not available
#endif

union {
     int32_t x;
     struct { int8_t a,b,c,d; } s;
} u;

int main () {
     u.s.a = 1;
     u.s.b = 2;
     u.s.c = 3;
     u.s.d = 4;
     printf("#define OVERLAY 0x%08x\n",u.x);
     return 0;
}
