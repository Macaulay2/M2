#ifndef _DEBUG_H_
#define _DEBUG_H_ 1

#ifdef DEBUG

#warning : using debug.h debugging code

#if defined(__cplusplus)
extern "C" {
#endif

  extern void trap(void);
  extern void *trapaddr;
  extern void trapchk(void *);

#ifdef DEBUG
  static __attribute__ ((unused)) void debug_version() {}
#endif

#if defined(__cplusplus)
}
#endif

#endif

#endif
