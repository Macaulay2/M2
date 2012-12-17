#include "engine-exports.h"
#include "M2mem-replacement.h"
#include <memory.h>
#include <gc/gc.h>
M2_string M2_tostringn(char *s, int n)
{
    M2_string p = (M2_string)getmem_atomic(sizeofarray(p,n));
    p->len = n;
    memcpy(p->array,s,n);
    //GC_CHECK_CLOBBER(p);
    return p;
}
