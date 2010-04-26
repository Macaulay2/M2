#include <atomic_ops.h>

extern void fff(), hhh();

__thread AO_t flag;

void ggg() {
     if (AO_load(&flag)) fff();
     else hhh();
     }

/*
   Local Variables:
   compile-command: "make -C $M2BUILDDIR/Macaulay2/c test.o test.s CFLAGS='-g0 -O3' "
   End:
   */
