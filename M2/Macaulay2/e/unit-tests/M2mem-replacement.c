#include <stdio.h>
#include <M2/gc-include.h>
#include "../d/types.h"

#include "M2mem-replacement.h"

/* trapchk: taken from d/debug.h *************************************/

void *trapaddr = (void *)1;
int trapcount = 0;
int trapset = 0;
size_t trapsize = (size_t)-1;

void trap(void) {}		/* I used to be concerned that this function would get optimized away, but it isn't static ... */

void *pointers[10];		/* during debugging we can put pointers here, visible to the garbage collector */
void trapchk(void *p) { 
     trapcount++;
     if (trapcount == trapset || p == trapaddr || p == (void *)~(intptr_t)trapaddr) trap();
}
void trapchk_size(size_t n) { 
     trapcount++;
     if (trapcount == trapset || trapsize == n) trap();
}

/*********************************************************************/

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
  p = GC_MALLOC(n);		/* GC_MALLOC clears its memory, but getmem doesn't guarantee to */
  if (p == NULL) outofmem2(n);
#ifndef NDEBUG
  memset(p,0xbe,n);		/* fill with 0xbebebebe ... */
  trapchk(p);
#endif
  return p;
}

void freememlen(void *s, size_t old) {
#    ifndef NDEBUG
     trapchk(s);
#    endif
     GC_FREE(s);
}

void freemem(void *s) {
#    ifndef NDEBUG
     trapchk(s);
#    endif
     GC_FREE(s);
}

char *getmem_clear(size_t n)
{
  char *p;
  p = GC_MALLOC(n);
  if (p == NULL) outofmem2(n);
  #if 0
  /* 
     note: GC_MALLOC clears memory before returning.
     If you switch to another memory allocator, you must clear it explicitly with this:
  */
  bzero(p,n);
  #endif
  #ifndef NDEBUG
  trapchk(p);
  #endif
  return p;
}

char *getmem_atomic(size_t n)
{
  char *p;
  p = GC_MALLOC_ATOMIC(n);
  if (p == NULL) outofmem2(n);
#ifndef NDEBUG
  memset(p,0xac,n);		/* fill with 0xacacacac ... */
  trapchk(p);
#endif
  return p;
}

char *getmem_malloc(size_t n)
{
  char *p;
  p = malloc(n);
  if (p == NULL) outofmem2(n);
#ifndef NDEBUG
  memset(p,0xca,n);		/* fill with 0xcacacaca */
  trapchk(p);
#endif
  return p;
}

char *getmem_atomic_clear(size_t n)
{
  char *p;
  p = GC_MALLOC_ATOMIC(n);
  if (p == NULL) outofmem2(n);
  bzero(p,n);
#ifndef NDEBUG
  trapchk(p);
#endif
  return p;
}

char *getmoremem (char *s, size_t old, size_t new) {
     void *p;
     p = GC_REALLOC(s,new);
     if (p == NULL) outofmem2(new);
#    ifndef NDEBUG
     trapchk(p);
#    endif
     return p;
     }

char *getmoremem1 (char *s, size_t new) {
     void *p;
     p = GC_REALLOC(s,new);
     if (p == NULL) outofmem2(new);
#    ifndef NDEBUG
     trapchk(p);
#    endif
     return p;
     }

char *getmoremem_atomic (char *s, size_t old, size_t new) {
     void *p;
     p = GC_MALLOC_ATOMIC(new);
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
     return p;
     }

/* Local Variables:
   compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check "
   indent-tabs-mode: nil
   End:
*/

