/* 
  basic running time, all options below set to 0 : 7.56, 7.58, 7.57
*/

/* without -lcfmem (see Makefile.in) we use the native operator new/delete: 8.66, 8.67, 8.67 */
/* with -lcfmem we use the "old" memory manager of singular -- it gets the lowest times */

/* for factor.cc */
#define NEW_USES_GC 0		/* adds 3.77 sec : 8.87, 8.86, 8.85 (GC_MALLOC) ; 9.08, 9.06, 9.06 (GC_MALLOC_UNCOLLECTABLE) */
#define NEW_TESTS_RETURN_VALUE_FROM_GC 0

/* for factor-init.c */
#define INTERIOR_POINTERS 0	/* */
#define GMP_USES_GC 1		/* adds 0.05 : 7.12, 7.13, 7.14 */
#define GMP_TESTS_RETURN_VALUE_FROM_GC 0
#define FACTORY_USES_GC_DIRECTLY 0	/* adds 5.20 sec */
#define FACTORY_TESTS_RETURN_VALUE_FROM_GC 0
#define REPLACE_GETBLOCK 1	/* no change, our code is identical */
#define REPLACE_MALLOC_BY_GC_IN_GETBLOCK 1 /* 7.06, 7.11, 7.09, 7.08 -- speeds it up! */
#define REPLACE_GC_FREE_BY_NOP 0 /* 7.63, 7.64, 7.62 */

/* consequences */

#define USE_GC (NEW_USES_GC || GMP_USES_GC || FACTORY_USES_GC_DIRECTLY || REPLACE_MALLOC_BY_GC_IN_GETBLOCK && REPLACE_GETBLOCK)

#if REPLACE_GC_FREE_BY_NOP
#undef GC_FREE
#define GC_FREE(x)
#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/test/engine benchmark"
// End:
*/
