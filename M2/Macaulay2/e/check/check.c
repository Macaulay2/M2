#include <stdlib.h>
#include <check.h>

extern void IM2_initialize(void);
extern TCase *monomial_tests(void);

Suite *engine_suite(void)
{
  Suite *s = suite_create("Engine");
  TCase *tc_monom = monomial_tests();
  suite_add_tcase(s, tc_monom);
  return s;
}

int main(int argc, char **argv)
{
  int nf;
  Suite *s = engine_suite();
  SRunner *sr = srunner_create(s);
  IM2_initialize();
  srunner_run_all(sr, CK_NORMAL);
  nf = srunner_ntests_failed(sr);
  srunner_free(sr);
  suite_free(s);
  return (nf==0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
