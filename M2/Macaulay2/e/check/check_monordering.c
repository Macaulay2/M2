/* A test of the engine interface: monomial orderings */
#include "engine.h"
#include "util.h"
#include "src_test/Ctest.h"
#include "src_test/Csuite.h"

void test_monorderings(Test *pTest)
{
  MonomialOrdering *mo1, *mo2, *mo3, *mo4, *mo5, *mo6,
    *mo7, *mo8, *mo9, *mo10, *mo11, *mo12, *mo13, *mo14, *moc,
    *n1;

  mo1 = IM2_MonomialOrdering_lex(8,1);
  ct_test(pTest,IM2_MonomialOrdering_nvars(mo1) == 8);
  ct_test(pTest,is_eq(IM2_MonomialOrdering_to_string(mo1),
	       "MonomialOrdering => {\n"
	       "    Lex => 8\n"
	       "    }"));

  mo2 = IM2_MonomialOrdering_weights(arrayint(4,1,4,7,13));
  ct_test(pTest,is_eq(IM2_MonomialOrdering_to_string(mo2),
	       "MonomialOrdering => {\n"
	       "    Weights => {1,4,7,13}\n"
	       "    }"));

  mo3 = monorder(2,mo2,mo1);
  ct_test(pTest,is_eq(IM2_MonomialOrdering_to_string(mo3),
	       "MonomialOrdering => {\n"
	       "    Weights => {1,4,7,13},\n"
	       "    Lex => 8\n"
	       "    }"));

  mo4 = IM2_MonomialOrdering_lex(4,2);
  mo5 = IM2_MonomialOrdering_lex(7,4);
  mo6 = IM2_MonomialOrdering_grevlex(arrayint(13, 1,1,1,1,1,1,1,1,1,1,1,1,1),1);
  mo7 = IM2_MonomialOrdering_grevlex(arrayint(13, 1,1,1,1,1,1,1,1,1,1,1,1,2),1);
  mo8 = IM2_MonomialOrdering_grevlex(arrayint(13, 1,1,1,1,1101,1,1,1,1,1,1,123,1),1);
  mo9 = IM2_MonomialOrdering_grevlex(arrayint(5, 1,1,2,2,2), 1);
  mo10 = IM2_MonomialOrdering_grevlex(arrayint(5, 1,1,1,1,1), 2);
  mo11 = IM2_MonomialOrdering_grevlex(arrayint(5, 1,3,4,5,6), 4);
  mo12 = IM2_MonomialOrdering_revlex(100);
  mo13 = IM2_MonomialOrdering_laurent(3);
  mo14 = IM2_MonomialOrdering_NClex(10);
  moc = IM2_MonomialOrdering_position(1);

  n1 = monorder(5,mo4,mo9,mo12,mo14,moc);

  ct_test(pTest,is_eq(IM2_MonomialOrdering_to_string(n1),
	       "MonomialOrdering => {\n"
	       "    LexSmall => 4,\n"
	       "    GRevLex => {1,1,2,2,2},\n"
	       "    RevLex => 100,\n"
	       "    NCLex => 10,\n"
	       "    Component\n"
	       "    }"));
}

Test * monordering_test(void)
{
  Test *result = ct_create("monordering tests", 0);
  ct_addTestFun(result, test_monorderings);
  return result;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check"
// End:
*/
