#include "src_test/suite.h"
#include "monomial.hpp"

extern "C" M2_arrayint arrayint(int len, ...);

class MonomialTest : public Test
{
public:
  MonomialTest() {}

  void run()
  {
    Monomial *m = Monomial::make(1,3);
    Monomial *n = Monomial::make(1,4);
    Monomial *p = Monomial::make(1,7);
    Monomial *b1 = Monomial::make(arrayint(4,3,1,2,2));
    Monomial *c1 = Monomial::make(3,1);
    Monomial *c2 = Monomial::make(2,2);
    Monomial *c3 = (*c1) * (*c2);
    Monomial *c4 = (*c2) * (*c1);
    _test(p->is_equal(*((*m) * (*n))));
    _test(p->is_equal(*((*m) * (*n))));
    _test(b1->is_equal(*c3));
    _test(b1->is_equal(*c4));
  }
};

Test *monomial_test() { return new MonomialTest; }

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check"
// End:
*/

