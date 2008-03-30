#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#define STDERR 2

void *	__gmp_default_allocate (unsigned n) {
     char *msg = "__gmp_default_allocate called\n";
     write(STDERR,msg,strlen(msg));
     abort();
}

void *	__gmp_default_reallocate (void *p, unsigned n, unsigned m) {
     char *msg = "__gmp_default_reallocate called\n";
     write(STDERR,msg,strlen(msg));
     abort();
}

void	__gmp_default_free (void *p , unsigned n) {
     char *msg = "__gmp_default_free called\n";
     write(STDERR,msg,strlen(msg));
     abort();
}

extern void *	(*__gmp_allocate_func) (unsigned);
extern void *	(*__gmp_reallocate_func) (void *, unsigned, unsigned);
extern void *	(*__gmp_reallocate_atomic_func) (void *, unsigned, unsigned);
extern void	(*__gmp_free_func) (void *, unsigned);

static void gmp_memory_init_function(void) __attribute__ ((constructor));
static void gmp_memory_init_function(void) {
     __gmp_allocate_func = __gmp_default_allocate;
     __gmp_reallocate_func = __gmp_default_reallocate;
     __gmp_free_func = __gmp_default_free;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
