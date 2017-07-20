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

  #ifdef HAVE_WINSOCK2_H
   #include <winsock2.h>
     /* Under mingw64, winsock2.h should be included before including windows.h,
	and pthread.h and gc.h include windows.h;
	therefore winsock2.h should be included before pthread.h and gc.h */
   #undef ERROR
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
    #include <gc/gc_cpp.h>
  #endif
#endif
