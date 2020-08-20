#ifndef _DEBUG_H_
#define _DEBUG_H_ 1

#ifndef NDEBUG

#if defined(__cplusplus)
extern "C" {
#endif

  extern void trap(void);
  extern void *trapaddr;
  extern int trapcount;
  extern int trapset;
  extern void trapchk(void *);
  extern void trapchk_size(size_t);
  extern int badBlock();

  static __attribute__ ((unused)) void debug_version() {}

#if defined(__cplusplus)
}
#endif

#define TRAPCHK(p) trapchk(p)
#define TRAPCHK_SIZE(n) trapchk_size(n)

#else

#define TRAPCHK(p)
#define TRAPCHK_SIZE(n)

#endif
#endif

