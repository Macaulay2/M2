/* A test of the engine interface: monoids */
/* Note that monoid element arithmetic is NOT part of the engine interface.
   But we should check that here too */
#include "engine.h"
#include "util.h"

void test_monoids()
{
  MonomialOrdering *mo;
  Monoid *M0, *M, *M1, *M2;
  M2_arrayint degs;
  M2_stringarray names;
  char *s[] = {"a","b","c","d","e","f","g","h"};
  /* First make a simple monoid */
  M0 = IM2_Monoid_trivial();
  assert(is_eq(IM2_Monoid_to_string(M0), 
	       "[,\n"
	       "  Degrees => {}\n"
	       "  ]"));

  /* Now another simple one */
  mo = IM2_MonomialOrdering_lex(8,1);
  names = tostrings(8, s);
  degs = arrayint(8, 1,1,1,1,1,1,1,1);
  degs = arrayint(0);
  M = IM2_Monoid_make(mo, names, M0, degs);
  assert(is_eq(IM2_Monoid_to_string(M), 
	       "[a,b,c,d,e,f,g,h,\n"
	       "  Degrees => {},\n"
	       "  MonomialOrdering => {\n"
	       "    Lex => 8\n"
	       "    }\n"
	       "  ]"));

  /* Now make a "degrees 1" monoid */
  mo = IM2_MonomialOrdering_laurent(1);
  names = tostrings(1, (char *[]){"t"});
  degs = arrayint(0);
  M1 = IM2_Monoid_make(mo, names, M0, degs);
  assert(is_eq(IM2_Monoid_to_string(M1), 
	       "[t,\n"
	       "  Degrees => {},\n"
	       "  MonomialOrdering => {\n"
	       "    GroupLex => 1\n"
	       "    }\n"
	       "  ]"));

  mo = IM2_MonomialOrdering_grevlex(arrayint(4,1,3,7,9),1);
  names = tostrings(4, (char *[]){"a","b","c","d"});
  degs = arrayint(4, 1,6, 3, 168);
  M2 = IM2_Monoid_make(mo, names, M1, degs);
  assert(is_eq(IM2_Monoid_to_string(M2), 
	       "[a,b,c,d,\n"
	       "  Degrees => {1, 6, 3, 168},\n"
	       "  MonomialOrdering => {\n"
	       "    GRevLex => {1,3,7,9}\n"
	       "    }\n"
	       "  ]"));
}

int main(int argc, char **argv)
{
  IM2_initialize();
  test_monoids();
  return 0;
}
