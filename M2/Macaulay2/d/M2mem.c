#include <stdio.h>

#include "types.h"
#include "M2mem.h"
#include "debug.h"

#ifdef MEMDEBUG
#include <memdebug.h>
#endif

#ifndef NDEBUG
#include <M2/config.h>
#define __thread
static __thread bool in_getmem = FALSE;
static inline void enter_getmem() {
  #if 0
    /* this is not always an error, because we may call GC_malloc from a finalizer */
    if (in_getmem) fatal("internal error: getmem called while getmem active");
  #endif
  in_getmem = TRUE;
}
static inline void exit_getmem() {
  in_getmem = FALSE;
}
#else
static inline void enter_getmem() {}
static inline void exit_getmem() {}
#endif

void outofmem(void) {
     const char *msg = "\n\n *** out of memory, exiting ***\n";
     int r = write(STDERR,msg,strlen(msg));
     if (r == ERROR) exit(1);
     exit(1);
}

void outofmem2(size_t new) {
     const char *msg = "\n\n *** out of memory trying to allocate %ld bytes, exiting ***\n";
     static char buf[sizeof(msg) + 100];
     sprintf(buf,msg,(long)new);
     int r = write(STDERR,buf,strlen(buf));
     if (r == ERROR) exit(1);
     exit(1);
}

char *getmem(size_t n)
{
  char *p;
  TRAPCHK_SIZE(n);
  enter_getmem();
#ifdef MEMDEBUG
  p = M2_debug_malloc(n);
#else
  p = GC_MALLOC(n);		/* GC_MALLOC clears its memory; we preserve that */
#endif
  if (p == NULL) outofmem2(n);
#ifndef NDEBUG
  memset(p,0xbe,n);		/* fill with 0xbebebebe ... */
  trapchk(p);
#endif
  exit_getmem();
  return p;
}

void freememlen(void *s, size_t old) {
#    ifndef NDEBUG
     trapchk(s);
#    endif
#ifdef MEMDEBUG
     M2_debug_free(s);
#else     
     GC_FREE(s);
#endif
}

void freemem(void *s) {
#    ifndef NDEBUG
     trapchk(s);
#    endif
#ifdef MEMDEBUG
     M2_debug_free(s);
#else     
     GC_FREE(s);
#endif
}

char *getmem_clear(size_t n)
{
  char *p;
  enter_getmem();
#ifdef MEMDEBUG
  p = M2_debug_malloc(n);
#else
  p = GC_MALLOC(n);
#endif
  if (p == NULL) outofmem2(n);
#ifdef MEMDEBUG
  memset(p,0,n);
#else
  /* note: GC_MALLOC clears memory before returning.
     If you switch to another memory allocator, you must clear it explicitly here */
#endif
  #ifndef NDEBUG
  trapchk(p);
  #endif
  exit_getmem();
  return p;
}

char *getmem_atomic(size_t n)
{
  char *p;
  enter_getmem();
#ifdef MEMDEBUG
  p = M2_debug_malloc_atomic(n);
#else  
  p = GC_MALLOC_ATOMIC(n);
#endif
  if (p == NULL) outofmem2(n);
#ifndef NDEBUG
  memset(p,0xac,n);		/* fill with 0xacacacac ... */
  trapchk(p);
#endif
  exit_getmem();
  return p;
}

char *getmem_malloc(size_t n)
{
  char *p;
  enter_getmem();
  p = malloc(n);
  if (p == NULL) outofmem2(n);
#ifndef NDEBUG
  memset(p,0xca,n);		/* fill with 0xcacacaca */
  trapchk(p);
#endif
  exit_getmem();
  return p;
}

char *getmem_atomic_clear(size_t n)
{
  char *p;
  enter_getmem();
#ifdef MEMDEBUG
  p = M2_debug_malloc_atomic(n);
#else  
  p = GC_MALLOC_ATOMIC(n);
#endif
  if (p == NULL) outofmem2(n);
  memset(p,0,n);		/* GC_MALLOC_ATOMIC does not clear memory */
#ifndef NDEBUG
  trapchk(p);
#endif
  exit_getmem();
  return p;
}

char *getmoremem (char *s, size_t old, size_t new) {
     void *p;
     enter_getmem();
     p = GC_REALLOC(s,new);
     if (p == NULL) outofmem2(new);
#    ifndef NDEBUG
     trapchk(p);
#    endif
     exit_getmem();
     return p;
     }

char *getmoremem1 (char *s, size_t new) {
     void *p;
     enter_getmem();
     p = GC_REALLOC(s,new);
     if (p == NULL) outofmem2(new);
#    ifndef NDEBUG
     trapchk(p);
#    endif
     exit_getmem();
     return p;
     }

char *getmoremem_atomic (char *s, size_t old, size_t new) {
     void *p;
     enter_getmem();
#ifdef MEMDEBUG
     p = M2_debug_malloc_atomic(new);
#else  
     p = GC_MALLOC_ATOMIC(new);
#endif
     size_t min = old<new ? old : new;
     if (p == NULL) outofmem2(new);
     memcpy(p, s, min);
     GC_FREE(s);
#    ifndef NDEBUG
     {
       int excess = new - min;
       if (excess > 0) memset((char *)p+min,0xbe,excess); /* fill with 0xbebebebe */
     }
     trapchk(p);
#    endif
     exit_getmem();
     return p;
     }

/*
 Local Variables:
 compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
