#include "src_test/Csuite.h"
#include "engine.h"

extern Test *monomial_test(void);
extern Test *monordering_test(void);
extern Test *monoid_test(void);
extern Test *ring_test(void);
extern Test *freemodule_test(void);
extern Test *matrix_test(void);
extern Test *sparsemat_test(void);
extern Test *lapack_test(void);
extern void M2inits();

int main()
{
  int nfailed = 0;
  M2inits(); /* calls IM2_initialize */
  Suite *s = cs_create("Engine C Interface");
  cs_addTest(s,monomial_test());
  cs_addTest(s,monordering_test());
  cs_addTest(s,monoid_test());
  cs_addTest(s,ring_test());
  cs_addTest(s,freemodule_test());
  cs_addTest(s,matrix_test());
#if 0
  cs_addTest(s,sparsemat_test());
  /*  cs_addTest(s,lapack_test());*/
#endif
  cs_run(s);
  nfailed = cs_report(s);
  cs_destroy(s,1);
  if (nfailed != 0) 
    {
      printf("## %d tests FAILED ##\n", nfailed);
      return 1;
    }
  return 0;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check "
// End:
*/

