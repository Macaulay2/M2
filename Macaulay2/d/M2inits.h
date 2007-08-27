#ifdef __cplusplus
extern "C" {
#endif

extern void M2inits(), M2inits1(), M2inits2();
extern void enterFactory(), enterM2();
extern int M2inits_run;
     extern int factory_init1_run, factory_init2_run;

/* get size_t */
#include <stddef.h>

extern void *	(*__gmp_allocate_func) (size_t);
extern void *	(*__gmp_reallocate_func) (void *, size_t, size_t);
extern void	(*__gmp_free_func) (void *, size_t);
extern void *   (*__gmp_allocate_atomic_func  )(size_t);
extern void *   (*__gmp_reallocate_atomic_func)(void *, size_t, size_t);

#ifdef __cplusplus
}
#endif
