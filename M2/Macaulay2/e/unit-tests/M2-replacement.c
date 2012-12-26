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

typedef struct RingElementrec *RingElement;

  const RingElement /* or null */ *rawGCDRingElement(
                                             const RingElement *f, const RingElement *g,
                                             const RingElement *mipo, M2_bool inExtension
                                             )
{
 /* connect to rawGCD */
    return 0;
}
  const RingElement /* or null */ *rawExtendedGCDRingElement(
                                                     const RingElement *f, const RingElement *g,
                                                     const RingElement **A, const RingElement **B
                                                     )
{
    /* connected to rawExtendedGCD */
    return 0;
}

char * M2_tocharstar(M2_string s)
{
}

M2_arrayint M2_makearrayint(int len)
{
}

M2_string M2_join(M2_string s, M2_string t)
{
}
M2_string M2_tostring(M2_constcharstarOrNull s)
{
}

M2_string (*gmp_tonetCCparenpointer)(gmp_CC);
M2_string (*gmp_tonetCCpointer)(gmp_CC);
M2_string (*gmp_tostringRRpointer)(__mpfr_struct *);


char newline[] = "\n";

int M2_gbTrace = 0;
