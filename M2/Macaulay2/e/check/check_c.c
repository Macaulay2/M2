#include "src_test/Csuite.h"
#include "engine.h"

extern Test *monomial_test(void);
extern void M2inits();

int main()
{
  M2inits(); /* calls IM2_initialize */
  Suite *s = cs_create("Engine C Interface");
  cs_addTest(s,monomial_test());
  cs_run(s);
  cs_report(s);
  cs_destroy(s,1);
  return 0;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check"
// End:
*/

