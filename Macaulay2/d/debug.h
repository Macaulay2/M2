#ifndef _DEBUG_H_
#define _DEBUG_H_ 1

#ifdef DEBUG

#warning : using debug.h debugging code

#if defined(__cplusplus)
extern "C" {
#endif

  extern void trap(void);
  extern void *trapaddr;
  extern int trapcount;
  extern int trapset;
  extern void trapchk(void *);

#ifdef DEBUG
  static __attribute__ ((unused)) void debug_version() {}
#endif

#ifdef GC_DEBUG
  extern void GC_check(void *);
  extern void gmp_GC_check(void *);
# define GC_CHECK(p) GC_check((void *)p)
# define GMP_GC_CHECK(p) gmp_GC_check((void *)p)
#else
# define GC_CHECK(p) p
# define GMP_GC_CHECK(p) p
#endif

#if defined(__cplusplus)
}
#endif

#endif

#endif
