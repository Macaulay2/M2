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

  mo1 = rawLexMonomialOrdering(8,1);
  ct_test(pTest,rawNumberOfVariables(mo1) == 8);
  ct_test(pTest,is_eq(IM2_MonomialOrdering_to_string(mo1),
	       "MonomialOrdering => {\n"
	       "    Lex => 8\n"
	       "    }"));

  mo2 = rawWeightsMonomialOrdering(arrayint(4,1,4,7,13));
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

  mo4 = rawLexMonomialOrdering(4,2);
  mo5 = rawLexMonomialOrdering(7,4);
  mo6 = rawGRevLexMonomialOrdering(arrayint(13, 1,1,1,1,1,1,1,1,1,1,1,1,1),1);
  mo7 = rawGRevLexMonomialOrdering(arrayint(13, 1,1,1,1,1,1,1,1,1,1,1,1,2),1);
  mo8 = rawGRevLexMonomialOrdering(arrayint(13, 1,1,1,1,1101,1,1,1,1,1,1,123,1),1);
  mo9 = rawGRevLexMonomialOrdering(arrayint(5, 1,1,2,2,2), 1);
  mo10 = rawGRevLexMonomialOrdering(arrayint(5, 1,1,1,1,1), 2);
  mo11 = rawGRevLexMonomialOrdering(arrayint(5, 1,3,4,5,6), 4);
  mo12 = rawRevLexMonomialOrdering(100);
  mo13 = rawGroupLexMonomialOrdering(3);
  mo14 = rawNClexMonomialOrdering(10);
  moc = rawPositionMonomialOrdering(1);

  n1 = monorder(5,mo4,mo9,mo12,mo14,moc);

  ct_test(pTest,is_eq(IM2_MonomialOrdering_to_string(n1),
	       "MonomialOrdering => {\n"
	       "    LexSmall => 4,\n"
	       "    GRevLex => {1,1,2,2,2},\n"
	       "    RevLex => 100,\n"
	       "    NCLex => 10,\n"
	       "    Position => Up\n"
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
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check "
// End:
*/
