#include "src_test/Csuite.h"
#include "engine.h"

extern Test *monomial_test(void);

int main()
{
  IM2_initialize();
  Suite *s = cs_create("Engine C Interface");
  cs_addTest(s,monomial_test());
  cs_run(s);
  cs_report(s);
  cs_destroy(s,1);
  return 0;
}
