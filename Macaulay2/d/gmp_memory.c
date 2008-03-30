#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#define STDERR 2

static void *our_gmp_default_allocate (unsigned n) {
     char *msg = "our_gmp_default_allocate called\n";
     write(STDERR,msg,strlen(msg));
     abort();
}

static void *our_gmp_default_reallocate (void *p, unsigned n, unsigned m) {
     char *msg = "our_gmp_default_reallocate called\n";
     write(STDERR,msg,strlen(msg));
     abort();
}

static void our_gmp_default_free (void *p , unsigned n) {
     char *msg = "our_gmp_default_free called\n";
     write(STDERR,msg,strlen(msg));
     abort();
}

extern void *	(*__gmp_allocate_func) (unsigned);
extern void *	(*__gmp_reallocate_func) (void *, unsigned, unsigned);
extern void *	(*__gmp_reallocate_atomic_func) (void *, unsigned, unsigned);
extern void	(*__gmp_free_func) (void *, unsigned);

static void gmp_memory_init_function(void) __attribute__ ((constructor));
static void gmp_memory_init_function(void) {
#if 1
     __gmp_allocate_func = our_gmp_default_allocate;
     __gmp_reallocate_func = our_gmp_default_reallocate;
     __gmp_free_func = our_gmp_default_free;
#else
     __gmp_allocate_func = __gmp_default_allocate;
     __gmp_reallocate_func = __gmp_default_reallocate;
     __gmp_free_func = __gmp_default_free;
#endif
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
