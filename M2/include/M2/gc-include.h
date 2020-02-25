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

  #ifdef NDEBUG
   #define GC_IGNORE_WARN 1
  #endif

  #define GC_THREADS 1
  #include <gc/gc.h>

  #if defined(__cplusplus)
    #define GC_NEW_ABORTS_ON_OOM
    #include <gc/gc_cpp.h>
  #endif
#endif
