#include <stdio.h>

#include "types.h"
#include "M2inits.h"
#include "M2mem.h"
#include "debug.h"

#ifndef NDEBUG
  #include <M2/config.h>
  #ifndef USE_THREADS
    #define __thread
  #endif
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
  p = GC_MALLOC(n);		/* GC_MALLOC clears its memory, but getmem doesn't guarntee to */
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
  enter_getmem();
  p = GC_MALLOC(n);
  if (p == NULL) outofmem2(n);
  #if 0
  /* 
     note: GC_MALLOC clears memory before returning.
     If you switch to another memory allocator, you must clear it explicitly with this:
  */
  memset(p,0,n);
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
  p = GC_MALLOC_ATOMIC(n);
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
  p = GC_MALLOC_ATOMIC(n);
  if (p == NULL) outofmem2(n);
  memset(p,0,n);
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
     exit_getmem();
     return p;
     }

/* 
   memory allocation issues:

      libgc works best for us
      on some systems (used by us?) libgc uses malloc as its primary source of new memory, so we can't
           redefine malloc and friends
      remark: it is a mistake to call GC_FREE on an object for which a finalizer has been registered,
           because GC_FREE will reclaim the memory and the finalizer will get called later
      we use gmp and want its objects collected, too, so we tell gmp to use libgc (__gmp_set_memory_functions)
      we use pari, and it uses malloc() and free() to allocate memory in which it stores pointers to
           gmp integers, so we have to replace malloc and free by libgc functions
      we use ntl, and it uses operator new to allocate memory in which it stores pointers to
           gmp integers, so we have to replace operator new and delete by libgc functions (see
	   ../e/ournewdelete.cpp)
      ntl erroneously uses free() to delete objects allocated with operator new, so we have to replace
           malloc, calloc, realloc, and free by calls to libgc; or we have to fix ntl
      but if we replace malloc, we get into a problem, because of this circularity, at least in the debug version,
		#3  0x082b2c2f in GC_save_callers (info=0x86dff88) at os_dep.c:4025
		#4  0x082a68e7 in GC_debug_malloc (lb=28, s=0x84fa228 "/home/dan/src/M2-dev/BUILD/dan/../../Macaulay2/d/M2mem.c", i=53) at dbg_mlc.c:472
		#5  0x081172a7 in getmem (n=28) at /home/dan/src/M2-dev/BUILD/dan/../../Macaulay2/d/M2mem.c:53
		#6  0x4000c07f in _dl_map_object_deps (map=<value optimized out>, preloads=<value optimized out>, npreloads=<value optimized out>, trace_mode=0, open_mode=-2147483648) at dl-deps.c:506
		#7  0x4001184e in dl_open_worker (a=0xbfffee80) at dl-open.c:326
		#8  0x4000d4e6 in _dl_catch_error (objname=<value optimized out>, errstring=<value optimized out>, mallocedp=<value optimized out>, operate=0x400116b0 <dl_open_worker>, args=0xbfffee80) at dl-error.c:178
		#9  0x40011200 in _dl_open (file=0x40f6b85d "libgcc_s.so.1", mode=<value optimized out>, caller_dlopen=0x0, nsid=-2, argc=6, argv=0xbffff474, env=0xbffff490) at dl-open.c:615
		#10 0x40f4ef92 in do_dlopen (ptr=0xbffff010) at dl-libc.c:86
		#11 0x4000d4e6 in _dl_catch_error (objname=<value optimized out>, errstring=<value optimized out>, mallocedp=<value optimized out>, operate=0x40f4ef30 <do_dlopen>, args=0xbffff010) at dl-error.c:178
		#12 0x40f4f091 in dlerror_run (operate=<value optimized out>, args=0xbffff010) at dl-libc.c:47
		#13 0x40f4f1ab in *__GI___libc_dlopen_mode (name=0x40f6b85d "libgcc_s.so.1", mode=-2147483647) at dl-libc.c:160
		#14 0x40f2cfb8 in init () at ../sysdeps/i386/backtrace.c:44
		#15 0x4002b160 in pthread_once () at ../nptl/sysdeps/unix/sysv/linux/i386/pthread_once.S:122
		#16 0x40f2d1ad in *__GI___backtrace (array=0xbffff0e0, size=9) at ../sysdeps/i386/backtrace.c:121
		#17 0x082b2c2f in GC_save_callers (info=0x86dee68) at os_dep.c:4025
		#18 0x082a68e7 in GC_debug_malloc (lb=152, s=0x84fa228 "/home/dan/src/M2-dev/BUILD/dan/../../Macaulay2/d/M2mem.c", i=79) at dbg_mlc.c:472
		#19 0x0811771a in getmem_clear (n=19, k=8) at /home/dan/src/M2-dev/BUILD/dan/../../Macaulay2/d/M2mem.c:79
		#20 calloc (n=19, k=8) at /home/dan/src/M2-dev/BUILD/dan/../../Macaulay2/d/M2mem.c:188
		#21 0x400109ac in allocate_dtv (mem=0x412f4b70) at dl-tls.c:300
		#22 *__GI__dl_allocate_tls (mem=0x412f4b70) at dl-tls.c:466
		#23 0x40026103 in allocate_stack (newthread=0x865d960, attr=0xbffff228, start_routine=0x82b70c0 <GC_mark_thread>, arg=0x0) at allocatestack.c:561
		#24 __pthread_create_2_1 (newthread=0x865d960, attr=0xbffff228, start_routine=0x82b70c0 <GC_mark_thread>, arg=0x0) at pthread_create.c:441
		#25 0x082b6ec2 in start_mark_threads () at pthread_support.c:350
		#26 GC_thr_init () at pthread_support.c:837
		#27 0x082b1aba in GC_init_inner () at misc.c:761
		#28 0x0821cd86 in init_gc () at /home/dan/src/M2-dev/BUILD/dan/../../Macaulay2/d/M2inits.c:32
		#29 M2inits () at /home/dan/src/M2-dev/BUILD/dan/../../Macaulay2/d/M2inits.c:104
		#30 0x08117308 in malloc (n=232) at /home/dan/src/M2-dev/BUILD/dan/../../Macaulay2/d/M2mem.c:182
      so we replace backtrace() by a better version that does nothing!

*/

/*
 the next routines redefine malloc and friends
   */

#if 0

void *malloc (size_t n) {
  if (M2inits_firsttime) M2inits();
  return getmem(n);
}

void *calloc (size_t n,size_t k) {
  if (M2inits_firsttime) M2inits();
  return getmem_clear(n*k);
}

void free (void *p) {
  GC_FREE(p);
}

void *realloc (void *p, size_t n) {
  return getmoremem1(p,n);
}

#endif

/*
 Local Variables:
 compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
