/* sometimes alloca.h is missing! */

#ifdef __GNUC__
#define alloca __builtin_alloca
#endif
