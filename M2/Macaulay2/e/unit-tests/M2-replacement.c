#include "engine-exports.h"
#include "M2mem-replacement.h"
#include <memory.h>
#include <M2/gc-include.h>

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

M2_arrayint M2_makearrayint(int n)
{
  M2_arrayint z = (M2_arrayint)getmem_atomic(sizeofarray(z,n));
  z->len = n;
  //GC_CHECK_CLOBBER(z);
  return z; /* Note that getmem_atomic returns zeroed memory */
}
char * M2_tocharstar(M2_string s)
{
  char *p = getmem_atomic(s->len + 1);
  memcpy(p,s->array,s->len);
  p[s->len] = 0;
  //GC_CHECK_CLOBBER(p);
  return p;
}
M2_string M2_join(M2_string x, M2_string y)
{
  M2_string p;
  p = (M2_string) getmem_atomic(sizeofarray(p,x->len+y->len));
  p->len = x->len + y->len;
  memcpy(p->array,x->array,x->len);
  memcpy(p->array+x->len,y->array,y->len);
  //GC_CHECK_CLOBBER(p);
  return p;
}
M2_string M2_tostring(M2_constcharstarOrNull s)
{
  int n = s ? strlen(s) : 0;
  M2_string p = getmematomicarraytype(M2_string,n);
  p->len = n;
  memcpy(p->array,s,n);
  //GC_CHECK_CLOBBER(p);
  return p;
}
M2_string M2_tostringn(char *s, int n)
{
    M2_string p = (M2_string)getmem_atomic(sizeofarray(p,n));
    p->len = n;
    memcpy(p->array,s,n);
    //GC_CHECK_CLOBBER(p);
    return p;
}

M2_string (*gmp_tonetCCparenpointer)(gmp_CC);
M2_string (*gmp_tonetCCpointer)(gmp_CC);
M2_string (*gmp_tostringRRpointer)(__mpfr_struct *);


char newline[] = "\n";

int M2_gbTrace = 0;
int M2_numericalAlgebraicGeometryTrace = 0;

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
 indent-tabs-mode: nil
 End:
*/
