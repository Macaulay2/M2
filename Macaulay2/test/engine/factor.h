/* for factor.cc */
#define NEWNEW 1		// adds 1.4 seconds to 7.5 sec running time

/* for factor-init.c */
#define INTERIOR_POINTERS 1	/* would add only 0.04 sec */
#define GMP_USES_GC 1		/* adds 0.1 sec */
#define GMP_TESTS_RETURN_VALUE_FROM_GC 0
#define FACTORY_USES_GC 1	/* adds 4.7 sec to 7.5 second running time */
#define FACTORY_TESTS_RETURN_VALUE_FROM_GC 0
#define REPLACE_GETBLOCK 1
#define REPLACE_MALLOC_BY_GC_IN_GETBLOCK 1
