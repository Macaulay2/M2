#include "src_test/suite.h"
#include "polyring.hpp"
#include "util.h"

extern "C" M2_arrayint arrayint(int len, ...);

class RingTest : public Test
{
public:
  RingTest() {}

  Monoid *makemonoid()
  {
    MonomialOrdering *mo;
    M2_arrayint degs;
    M2_stringarray names;
    char *s[] = {"a","b","c","d","e","f","g","h"};


    /* Now another simple one */
    mo = rawGRevLexMonomialOrdering(arrayint(8, 1,1,1,1,1,1,1,1),1);
    names = tostrings(8, s);
    degs = arrayint(0);
    return IM2_Monoid_make(mo, names, IM2_Monoid_trivial(), degs);
  }

  void run()
  {
    const Ring *A = make_poly_ring(0,4);
    const Ring *B = IM2_Ring_polyring(A, makemonoid());
    printf("%s\n", tocharstar(IM2_Ring_to_string(B)));
    const PolynomialRing *C = B->cast_to_PolynomialRing()->get_flattened_ring();
    printf("%s\n", tocharstar(IM2_Ring_to_string(C)));
    _test(B != 0);
  }
};

Test *ring_test() { return new RingTest; }

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check "
// End:
*/

