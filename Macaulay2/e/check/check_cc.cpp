#include "src_test/suite.h"
#include "engine.h"
#include <iostream>

extern Test *monomial_test();
extern Test *ring_test();
extern Test *gb_test();

int main()
{
  IM2_initialize();
  Suite s("Engine", &std::cout);

  s.addTest(monomial_test());
  s.addTest(ring_test());
  s.addTest(gb_test());

  s.run();
  s.report();
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check "
// End:
*/

