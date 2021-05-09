/* We provide this include file, so each of our files that calls pthread functions
   can set the proper macro definitions for gc */

/* gc doc tells us to include pthread.h before gc.h */

#ifndef GC_INCLUDED
  #define GC_INCLUDED 1

  #ifdef GC_MALLOC
   #error "gc.h already included"
  #endif

  #if defined(__linux__) 
    #define GC_LINUX_THREADS
  #endif 

  #define _REENTRANT 1
  #include <pthread.h>

  /* to get sigset_t defined for gc.h: */
  #define __need_sigset_t 1
  #include <signal.h>

  /* for size_t: */
  #include <stdlib.h>
  /* for memcpy: */
  #include <string.h>

  #ifdef NDEBUG
   #define GC_IGNORE_WARN 1
  #endif

  #define GC_THREADS 1

  /*
   * these two macros affect the definition of GC_INIT, but have
   * to appear before we include gc.h, in order to take effect
   */
  #define GC_FREE_SPACE_DIVISOR 12
  #define GC_INITIAL_HEAP_SIZE 70000000

  #include <gc/gc.h>

  #if defined(__cplusplus)
    #define GC_NEW_ABORTS_ON_OOM
    #ifdef __CYGWIN__
      /* prevent gc from defining global new and delete methods */
      #undef __CYGWIN__
      #include <gc/gc_cpp.h>
      #define __CYGWIN__ 1
    #else
      #include <gc/gc_cpp.h>
    #endif
  #endif
#endif
