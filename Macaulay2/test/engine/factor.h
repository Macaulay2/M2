/* 
  basic running time, all options below set to 0 : 7.56, 7.58, 7.57
*/

/* for factor.cc */
#define NEW_USES_GC 0		/* */
#define NEW_TESTS_RETURN_VALUE_FROM_GC 0

/* for factor-init.c */
#define INTERIOR_POINTERS 0	/* */
#define GMP_USES_GC 0		/* */
#define GMP_TESTS_RETURN_VALUE_FROM_GC 0
#define FACTORY_USES_GC 0	/* */
#define FACTORY_TESTS_RETURN_VALUE_FROM_GC 0
#define REPLACE_GETBLOCK 1	/* no change, our code is identical */
#define REPLACE_MALLOC_BY_GC_IN_GETBLOCK 1 /* 7.06, 7.11, 7.09, 7.08 -- speeds it up! */
#define REPLACE_GC_FREE_BY_NOP 1

/* consequences */

#define USE_GC (NEW_USES_GC || GMP_USES_GC || FACTORY_USES_GC || REPLACE_MALLOC_BY_GC_IN_GETBLOCK && REPLACE_GETBLOCK)

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/test/engine benchmark"
// End:
*/
