#include <stddef.h>

#if defined(__cplusplus)
extern "C" {
#endif
  extern void outofmem(void);
  extern void outofmem2(size_t);
  extern char *getmem(size_t);
  extern void freemem(void *);
  extern void freememlen(void *, size_t);
  extern char *getmem_clear(size_t);
  extern char *getmem_atomic(size_t);
  extern char *getmem_malloc(size_t);
  extern char *getmem_atomic_clear(size_t);
  extern char *getmoremem(char *, size_t oldsize, size_t newsize);
  extern char *getmoremem1(char *, size_t newsize);
  extern char *getmoremem_atomic(char *, size_t oldsize, size_t newsize);
#if defined(__cplusplus)
}
#endif

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
