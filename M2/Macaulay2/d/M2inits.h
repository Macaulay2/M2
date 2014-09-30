#ifdef __cplusplus
extern "C" {
#endif

extern void M2inits(void) __attribute__ ((constructor));
extern void M2inits1(), M2inits2();
extern void enterM2();
extern void check_M2init();
extern int M2inits_run, M2inits_firsttime;

/* get size_t */
#include <stddef.h>

/* this should be the same as in mpir-1.2.1/gmp-impl.h : */
extern void *	(*__gmp_allocate_func) (size_t);
extern void *	(*__gmp_reallocate_func) (void *, size_t, size_t);
extern void	(*__gmp_free_func) (void *, size_t);
extern void *__gmp_default_allocate(size_t);
extern void *__gmp_default_reallocate(void *, size_t, size_t);
extern void __gmp_default_free(void *, size_t);

#ifdef __cplusplus
}
#endif
