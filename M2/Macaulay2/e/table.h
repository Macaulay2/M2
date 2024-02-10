/* $Id$ */
/* This is from Hanson's code, from his book: C interfaces and implementations.
   I have added c++ ifdef's to be able to use it from C++, MES, July 2002 */

#ifndef TABLE_INCLUDED
#define TABLE_INCLUDED

/******************************************************/
/*these next lines added by MES, July 2002, to use our gc routines..*/
#include "engine-includes.hpp"
#define  NEW(p) ((p) = (void *) getmem((long)sizeof *(p)))
#define FREE(ptr) ((void)(freemem((ptr)), (ptr) = 0))
/******************************************************/

#define T Table_T
struct T;
typedef struct T T;

#if defined(__cplusplus)
extern "C" {
#endif
extern T *   Table_new (int hint,
        int cmp(const void *x, const void *y),
        unsigned hash(const void *key));
extern void Table_free(T **table);
extern int   Table_length(T * table);
extern void *Table_put   (T * table, const void *key,
        void *value);
extern void *Table_get   (T * table, const void *key);
extern void *Table_remove(T * table, const void *key);
extern void   Table_map    (T * table,
        void apply(const void *key, void **value, void *cl),
        void *cl);
extern const void **Table_toArray(T * table, void *end);
#if defined(__cplusplus)
}
#endif


#undef T
#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
