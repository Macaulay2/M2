#include "src_test/suite.h"
#include "polyring.hpp"
#include "freemod.hpp"
#include "gbring.hpp"

#include "util.h"

extern "C" M2_arrayint arrayint(int len, ...);

class GBVectorTest : public Test
{
public:
  GBVectorTest() {}

  void trans() {
    const Ring *R = make_poly_ring(0,5);
    const FreeModule *F = R->make_FreeModule(4);
    vec v = R->make_vec(3,R->var(2,4));
    vec v2 = R->make_vec(2,R->var(1,3));
    R->add_vec_to(v,v2);

    // Now translate to gbvector and back
    const PolynomialRing *P = R->cast_to_PolynomialRing();
    GBRing *Q = P->get_gb_ring();
    ring_elem a;
    gbvector *w = P->translate_gbvector_from_vec(F,v,a);

    // Translate back (with and w/o denominator)
    vec vresult = P->translate_gbvector_to_vec(F,w);

    _test(R->is_equal(v,vresult));

    buffer o;
    R->elem_text_out(o,v);
    printf("vec = %s\n",o.str());
    o.reset();
    Q->gbvector_text_out(o,F,w);
    printf("  gbvector = %s\n",o.str());
    o.reset();
    R->elem_text_out(o,vresult);
    printf("  vec result = %s\n",o.str());

  }

#if 0  
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
#endif
  void run()
  {
    trans();
    _test(1);
  }
};

Test *gb_test() { return new GBVectorTest; }

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check "
// End:
*/

