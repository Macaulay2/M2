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

void *	(*__gmp_allocate_func) (unsigned) = __gmp_default_allocate;
void *	(*__gmp_reallocate_func) (void *, unsigned, unsigned) = __gmp_default_reallocate;
void	(*__gmp_free_func) (void *, unsigned) = __gmp_default_free;

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
