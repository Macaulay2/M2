#include "src_test/suite.h"
#include "engine.h"
#include <iostream>

extern Test *monomial_test();

int main()
{
  IM2_initialize();
  Suite s("Engine", &std::cout);
  s.addTest(monomial_test());
  s.run();
  s.report();
}
