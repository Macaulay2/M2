/* 
  basic running time, all options below set to 0 : 7.56, 7.58, 7.57
  now it's risen to 7.84, 7.84, 7.83 for no reason I know
  hmm, if I compile factory with -O3 instead of -O2 I get 8.17, 8.22, 8.24 -- that's counterintuitive
*/

/* without -lcfmem (see Makefile.in) we use the native operator new/delete: 8.66, 8.67, 8.67 */
/* with -lcfmem we use the "old" memory manager of singular -- it gets the lowest times */

/* with -pg profiling option on and linking with -lcf-pg -lcfmem-pg -lntl-pg -lgmp-pg -lgc-pg, the time jumps to 46 seconds */

/*
   21,886,823 calls to getBlock
   21,886,743 calls to freeBlock
*/

/* for factor.cc */
#define NEW_USES_GC 0		/* adds 1.77 sec : 8.87, 8.86, 8.85 (GC_MALLOC) ; 9.08, 9.06, 9.06 (GC_MALLOC_UNCOLLECTABLE) */
#define NEW_TESTS_RETURN_VALUE_FROM_GC 0

/* for factor-init.c */
#define INTERIOR_POINTERS 1	/* 7.63, 7.64, no change */
#define GMP_USES_GC 1		/* adds 0.05 : 7.12, 7.13, 7.14, 7.12, 7.10, 7.10, but now it's 7.52, 7.55 ??? */
#define GMP_TESTS_RETURN_VALUE_FROM_GC 1 /* subtracts 0.05, so leave it in, now 7.63, 7.64 */
#define FACTORY_USES_GC_DIRECTLY 0	/* adds 5.20 sec */
#define FACTORY_TESTS_RETURN_VALUE_FROM_GC 0
#define REPLACE_GETBLOCK 1	/* improved realloc algorithm, notices when we have enough space, but it slows down: 8.82, 8.83 ???? */
#define REPLACE_MALLOC_BY_GC_IN_GETBLOCK 1 /* 7.06, 7.11, 7.09, 7.08 -- speeds it up!  But now it's 7.73, 7.72 ??? */
#define REPLACE_GC_FREE_BY_NOP 0 /* 7.63, 7.64, 7.62 */

/* consequences */

#define USE_GC (NEW_USES_GC || GMP_USES_GC || FACTORY_USES_GC_DIRECTLY || REPLACE_MALLOC_BY_GC_IN_GETBLOCK && REPLACE_GETBLOCK)

#if REPLACE_GC_FREE_BY_NOP
#undef GC_FREE
#define GC_FREE(x)
#endif

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/test/engine benchmark"
 End:
*/
