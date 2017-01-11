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

#ifndef NDEBUG
  static __attribute__ ((unused)) void debug_version() {}
#endif

#ifndef NDEBUG
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

#define TRAPCHK(p) trapchk(p)
#define TRAPCHK_SIZE(n) trapchk_size(n)

#else

#define TRAPCHK(p)
#define TRAPCHK_SIZE(n)

#endif

#endif
