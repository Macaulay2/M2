/* A test of the engine interface: monomial orderings */
#include "engine.h"
#include "util.h"

void test_monorderings()
{
  MonomialOrdering *mo1, *mo2, *mo3, *mo4, *mo5, *mo6,
    *mo7, *mo8, *mo9, *mo10, *mo11, *mo12, *mo13, *mo14, *moc,
    *n1;

  mo1 = IM2_MonomialOrdering_lex(8,1);
  assert(IM2_MonomialOrdering_nvars(mo1) == 8);
  assert(is_eq(IM2_MonomialOrdering_to_string(mo1),
	       "MonomialOrdering => {\n"
	       "    Lex => 8\n"
	       "    }"));

  mo2 = IM2_MonomialOrdering_weights(arrayint(4,1,4,7,13));
  assert(is_eq(IM2_MonomialOrdering_to_string(mo2),
	       "MonomialOrdering => {\n"
	       "    Weights => {1,4,7,13}\n"
	       "    }"));

  mo3 = monorder(2,mo2,mo1);
  assert(is_eq(IM2_MonomialOrdering_to_string(mo3),
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
  moc = IM2_MonomialOrdering_component();

  n1 = monorder(5,mo4,mo9,mo12,mo14,moc);

  assert(is_eq(IM2_MonomialOrdering_to_string(n1),
	       "MonomialOrdering => {\n"
	       "    LexSmall => 4,\n"
	       "    GRevLex => {1,1,2,2,2},\n"
	       "    RevLex => 100,\n"
	       "    NCLex => 10,\n"
	       "    Component\n"
	       "    }"));
}

int main(int argc, char **argv)
{
  IM2_initialize();
  test_monorderings();
  return 0;
}
