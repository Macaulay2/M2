#include "engine.h"
#include "util.h"
#include "src_test/Ctest.h"
#include "src_test/Csuite.h"

/* Test all vector routines over ZZ, a polynomial ring, and a Schreyer order.
   Also over any other rings ... */

void test_vector_ops(Test *pTest,const RingElement *f, const RingElement *g, 
		     const Vector *v, const Vector *w)
{
  const FreeModule *F;
  const Vector *e1, *zero;
  const Vector *z1,*z2;
  int i;
  /* v and w should have the same ambient free module */
  F = IM2_Vector_freemodule(v);
  ct_test(pTest,F == IM2_Vector_freemodule(w));
  i = IM2_FreeModule_rank(F)-1;
  e1 = IM2_Vector_e_sub(F,i);
  z1 = IM2_Vector_scalar_right_mult(e1,f);
  z2 = IM2_Vector_term(F,f,i);
  ct_test(pTest,IM2_Vector_is_equal(z1,z2));
  zero = IM2_Vector_zero(F);
  ct_test(pTest,IM2_Vector_is_equal(
			    IM2_Vector_subtract(zero,w),
			    IM2_Vector_negate(w)));

  ct_test(pTest,IM2_Vector_is_zero(
			    IM2_Vector_add(w, IM2_Vector_negate(w))));

}
   
void test_vector_ZZ(Test *pTest)
{
  const Ring *R = IM2_Ring_ZZ();
  const FreeModule *F = IM2_FreeModule_make(R, 5);
  const Vector *V = make_vector(F, 
				IM2_RingElement_from_Integer(R, make_integer(13)),
				IM2_RingElement_from_Integer(R, make_integer(-123)),
				IM2_RingElement_from_Integer(R, make_integer(0)),
				IM2_RingElement_from_Integer(R, make_integer(0)),
				IM2_RingElement_from_Integer(R, make_integer(9999999)));
  ct_test(pTest,V != 0);
  ct_test(pTest,IM2_Vector_n_terms(V) == 3);
  ct_test(pTest,is_eq(IM2_Vector_to_string(V), "9999999<4>-123<1>+13<0>"));

  test_vector_ops(pTest,IM2_RingElement_from_Integer(R, make_integer(100)),
		  IM2_RingElement_from_Integer(R, make_integer(3)),
		  V,V);
}

void test_vectors(Test *pTest)
{
  test_vector_ZZ(pTest);
}

Test * vector_test(void)
{
  Test *result = ct_create("vector tests", 0);
  ct_addTestFun(result, test_vectors);
  return result;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check "
// End:
*/
