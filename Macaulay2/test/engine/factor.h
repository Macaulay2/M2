/* 
  basic running time, all options below set to 0 : 7.68, 7.69, 7.67
*/

/* for factor.cc */
#define NEW_USES_GC 0		/* adds 1.4 seconds to 7.5 sec running time */

/* for factor-init.c */
#define INTERIOR_POINTERS 0	/* would add only 0.04 sec */
#define GMP_USES_GC 0		/* adds 0.1 sec */
#define GMP_TESTS_RETURN_VALUE_FROM_GC 0
#define FACTORY_USES_GC 0	/* adds 4.7 sec to 7.5 second running time */
#define FACTORY_TESTS_RETURN_VALUE_FROM_GC 0
#define REPLACE_GETBLOCK 0
#define REPLACE_MALLOC_BY_GC_IN_GETBLOCK 0

/* consequences */

#define USE_GC (NEW_USES_GC || GMP_USES_GC || FACTORY_USES_GC || REPLACE_MALLOC_BY_GC_IN_GETBLOCK)

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/test/engine benchmark"
// End:
*/
