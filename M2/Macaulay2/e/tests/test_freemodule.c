#include "engine.h"
#include "util.h"

/* TODO: test all of these routines using a multi-degree, no degree */

void test_freemodule_ZZ()
{
  const Ring *R = IM2_Ring_ZZ();
  const FreeModule *F = IM2_FreeModule_make(R, 5);
  assert(IM2_FreeModule_ring(F) == R);
  assert(IM2_FreeModule_rank(F) == 5);

  F = IM2_FreeModule_make(R, 0);
  assert(IM2_FreeModule_ring(F) == R);
  assert(IM2_FreeModule_rank(F) == 0);
}

void test_freemodule_polyring()
{
  const FreeModule *F1,*F2,*F12,*F3,*F4;
  M2_arrayint degs0, degs;
  const Ring *R = make_poly_ring(0,5);
  const FreeModule *F = IM2_FreeModule_make(R, 5);
  assert(IM2_FreeModule_ring(F) == R);
  assert(IM2_FreeModule_rank(F) == 5);

  F = IM2_FreeModule_make(R, 0);
  assert(IM2_FreeModule_ring(F) == R);
  assert(IM2_FreeModule_rank(F) == 0);

  degs0 = arrayint(5,-4,6,7,123,-12345);
  F = IM2_FreeModule_make_degs(R, degs0);
  assert(is_eq(IM2_FreeModule_to_string(F),
	       "free(rank 5 degrees = {t0^(-4), t0^6, t0^7, t0^123, t0^(-12345)})"));
  assert(IM2_FreeModule_ring(F) == R);
  assert(IM2_FreeModule_rank(F) == 5);

  degs = IM2_FreeModule_get_degrees(F);
  assert(arrayint_is_eq(degs0,degs));

  F1 = IM2_FreeModule_make_degs(R, arrayint(2,-4,6));
  F2 = IM2_FreeModule_make_degs(R, arrayint(3,7,123,-12345));
  F12 = IM2_FreeModule_sum(F1,F2);
  assert(IM2_FreeModule_rank(F12) == 5);
  assert(IM2_FreeModule_is_equal(F,F12));
  F12 = IM2_FreeModule_make(R,5);
  assert(!IM2_FreeModule_is_equal(F,F12));

  F1 = IM2_FreeModule_make_degs(R, arrayint(2, 100, 200));
  F2 = IM2_FreeModule_make_degs(R, arrayint(3, -1,0,1));
  F3 = IM2_FreeModule_tensor(F1,F2);

  assert(arrayint_is_eq(IM2_FreeModule_get_degrees(F3),
			arrayint(6, 99,100,101, 199,200,201)));

  F3 = IM2_FreeModule_dual(F1);
  assert(arrayint_is_eq(IM2_FreeModule_get_degrees(F3),
			arrayint(2,-100,-200)));

  F1 = IM2_FreeModule_make_degs(R, arrayint(4, 0,1,4,9));
  F2 = IM2_FreeModule_symm(2,F1);
  F3 = IM2_FreeModule_exterior(3,F1);
  
  assert(arrayint_is_eq(IM2_FreeModule_get_degrees(F2),
			arrayint(10, 0,1,4,9, 2,5,10, 8,13, 18)));

  assert(arrayint_is_eq(IM2_FreeModule_get_degrees(F3),
			arrayint(4, 5, 10, 13, 14)));

  F4 = IM2_FreeModule_submodule(F1, arrayint(5, 3,2,1,0,3));

  assert(arrayint_is_eq(IM2_FreeModule_get_degrees(F4),
			arrayint(5, 9,4,1,0,9)));
}

void test_schreyer_order()
{
  const FreeModule *F,*G,*H;
  int i;
  const Ring *R, *ZZ;
  const Matrix *M, *N;
  Vector_array *V;
  const RingElement *one;

  ZZ = IM2_Ring_ZZ();
  R = make_poly_ring(0,8);

  /* Now make a matrix over R, with one row */
  F = IM2_FreeModule_make(R, 1);
  V = make_vector_array(5);
  one = IM2_RingElement_from_Integer(ZZ, make_integer(1));
  for (i=0; i<5; i++)
    {
      const Vector *w = 
        make_vector(F, IM2_RingElement_term(R, one, monom(2,i,i+1)));
      
      V->array[i] = w;
    }
  M = IM2_Matrix_make1(F,V);
  display_matrix(M);

  /* Now use this to make a free module of rank 5: */
  G = IM2_FreeModule_make_schreyer(M);
  display_matrix(M);

  N = IM2_FreeModule_get_schreyer(G);
  H = IM2_Matrix_get_source(N);

  display_freemodule(H);
  assert(is_eq(IM2_FreeModule_to_string(H),
	       "free(rank 5 degrees = {t0, t0^2, t0^3, t0^4, t0^5}x0.0 x1^2.1 x2^3.2 x3^4.3 x4^5.4)"));

  assert(is_eq(IM2_FreeModule_to_string(G),
	       "free(rank 5 degrees = {t0, t0^2, t0^3, t0^4, t0^5}x0.0 x1^2.1 x2^3.2 x3^4.3 x4^5.4)"));
  
  assert(is_eq(IM2_Matrix_to_string(N),
	       "x0 0    0    0    0    \n"
	       "0  x1^2 0    0    0    \n"
	       "0  0    x2^3 0    0    \n"
	       "0  0    0    x3^4 0    \n"
	       "0  0    0    0    x4^5 \n"));
}

int main(int argc, char **argv)
{
  IM2_initialize();
  test_freemodule_ZZ();
  test_freemodule_polyring();
  test_schreyer_order();
  return 0;
}
