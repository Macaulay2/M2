/* this file gets included into each file created by scc1 */
#ifndef SCC_CORE_H
#define SCC_CORE_H
#include <M2/gc-include.h>
/* for size_t: */
 #include <stdlib.h>
/* for memcpy: */
 #include <string.h>
#if defined(__cplusplus)
  extern "C" {
#endif
    extern void scc_core_prepare() __attribute__ ((constructor));
    extern void fatal(const char *s,...);
    struct FUNCTION_CELL { void (*fun)(); struct FUNCTION_CELL *next; };
    extern struct FUNCTION_CELL *pre_final_list, *final_list, *thread_prepare_list;
    extern void invalidTypeTag(int,const char*,int,int);
    extern void invalidNullPointer(const char*,int,int);
    extern void fatalarraylen(int,const char*,int,int);
    extern void fatalarrayindex(int,int,const char*,int,int);
    extern void outofmem2(size_t);
    #ifndef NDEBUG
      #include <assert.h>
      void *GC_check_annotated_obj(void *);
      #define GC_CHECK_CLOBBER(p) assert((p) == 0 || 0 == GC_check_annotated_obj(GC_base((void *)(p))))
    #else
      #define GC_CHECK_CLOBBER(p)
    #endif
#if defined(__cplusplus)
  }
#endif
#endif

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
