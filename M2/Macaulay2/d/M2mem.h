#include <stddef.h>

#if defined(__cplusplus)
extern "C" {
#endif
  extern void outofmem(void);
  extern char *getmem(size_t);
  extern char *getmoremem(char *,size_t);
  extern char *getmem_clear(size_t);
  extern char *getmem_atomic(size_t);
  extern char *getmem_malloc(size_t);
  extern char *getmem_atomic_clear(size_t);
#if defined(__cplusplus)
}
#endif

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
