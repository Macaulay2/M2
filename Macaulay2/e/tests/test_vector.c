#include "engine.h"
#include "util.h"

/* Test all vector routines over ZZ, a polynomial ring, and a Schreyer order.
   Also over any other rings ... */

void test_vector_ops(const RingElement *f, const RingElement *g, 
		     const Vector *v, const Vector *w)
{
  const FreeModule *F;
  const Vector *e1, *zero;
  const Vector *z1,*z2;
  int i;
  /* v and w should have the same ambient free module */
  F = IM2_Vector_freemodule(v);
  assert(F == IM2_Vector_freemodule(w));
  i = IM2_FreeModule_rank(F)-1;
  e1 = IM2_Vector_e_sub(F,i);
  z1 = IM2_Vector_scalar_right_mult(e1,f);
  z2 = IM2_Vector_term(F,f,i);
  assert(IM2_Vector_is_equal(z1,z2));
  zero = IM2_Vector_zero(F);
  assert(IM2_Vector_is_equal(
			    IM2_Vector_subtract(zero,w),
			    IM2_Vector_negate(w)));

  assert(IM2_Vector_is_zero(
			    IM2_Vector_add(w, IM2_Vector_negate(w))));

}
   
void test_vector_ZZ()
{
  const Ring *R = IM2_Ring_ZZ();
  const FreeModule *F = IM2_FreeModule_make(R, 5);
  const Vector *V = make_vector(F, 
				IM2_RingElement_from_Integer(R, make_integer(13)),
				IM2_RingElement_from_Integer(R, make_integer(-123)),
				IM2_RingElement_from_Integer(R, make_integer(0)),
				IM2_RingElement_from_Integer(R, make_integer(0)),
				IM2_RingElement_from_Integer(R, make_integer(9999999)));
  assert(V != 0);
  assert(IM2_Vector_n_terms(V) == 3);
  assert(is_eq(IM2_Vector_to_string(V), "9999999<4>-123<1>+13<0>"));

  test_vector_ops(IM2_RingElement_from_Integer(R, make_integer(100)),
		  IM2_RingElement_from_Integer(R, make_integer(3)),
		  V,V);
}

int main(int argc, char **argv)
{
  IM2_initialize();
  test_vector_ZZ();
  return 0;
}
