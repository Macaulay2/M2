#include "engine.h"
#include "util.h"
#include "src_test/Ctest.h"
#include "src_test/Csuite.h"

void test_coeffs(Test *pTest)
{
  const Ring *ZZ = IM2_Ring_ZZ();
  const Ring *R = make_poly_ring(0,10);
  /* Make a polynomial (x1+x2)^5, find monomials in x1, and in {x1,x2} */

  const RingElement *a1,*f1,*f2,*f;
  const Monomial *m1,*m2;
  const FreeModule *F;
  const Matrix *M, *N;
  RingElement_array *elems;

  a1 = IM2_RingElement_from_Integer(ZZ, make_integer(1));
  m1 = rawMakeMonomial(arrayint(4,1,1,0,1));
  f1 = IM2_RingElement_term(R,a1,m1);

  m2 = rawMakeMonomial(arrayint(4,3,1,0,2));
  f2 = IM2_RingElement_term(R,a1,m2);

  f = IM2_RingElement_add(f1,f2);
  f = IM2_RingElement_power(f,make_integer(5));
  display_relem(f);

  F = IM2_FreeModule_make(R,1);
  elems = make_ringelem_array(1,f);
  M = IM2_Matrix_make1(F,1,elems,0);

  display_matrix(M);

  N = IM2_Matrix_monomials(arrayint(2,0,1), M);
  display_matrix(N);

  N = IM2_Matrix_monomials(arrayint(1,1), M);
  display_matrix(N);

  N = IM2_Matrix_monomials(arrayint(3,1,2,3), M);
  display_matrix(N);

  N = IM2_Matrix_monomials(arrayint(10,0,1,2,3,4,5,6,7,8,9), M);
  display_matrix(N);
}

void test_matrix_ops(Test *pTest,const Matrix *M, const Matrix *N)
     /* M, N should be square matrices */
{
  const Matrix *Mdual, *Mdual2, *P, *P1, *P2;
  const Matrix *MN = IM2_Matrix_tensor(M,N);
  display_matrix(MN);

  P = IM2_Matrix_identity(IM2_Matrix_get_target(M));
  P1 = IM2_Matrix_add(
		      IM2_Matrix_tensor(M,N),
		      IM2_Matrix_tensor(P,N));
  display_matrix(P1);
  printf("--------------\n");
  P2 = IM2_Matrix_tensor(
			 IM2_Matrix_add(M,P),
			 N);
  display_matrix(P2);
  printf("--------------\n");

  ct_test(pTest,IM2_Matrix_is_equal(P1,P2));
  ct_test(pTest,IM2_Matrix_is_zero(IM2_Matrix_subtract(P1,P2)));

  Mdual = IM2_Matrix_transpose(M);
  Mdual2 = IM2_Matrix_transpose(Mdual);
  ct_test(pTest,IM2_Matrix_is_equal(M, Mdual2));
  ct_test(pTest,IM2_Matrix_is_equal(IM2_Matrix_add(M,Mdual),
			     IM2_Matrix_add(Mdual,M)));
  ct_test(pTest,IM2_Matrix_is_equal(IM2_Matrix_transpose(IM2_Matrix_mult(M,Mdual,0)),
			     IM2_Matrix_mult(IM2_Matrix_transpose(Mdual),
					     IM2_Matrix_transpose(M),
					     0)));
  ct_test(pTest,IM2_Matrix_is_equal(IM2_Matrix_negate(IM2_Matrix_subtract(M,Mdual)),
			     IM2_Matrix_subtract(Mdual,M)));
}

void test_matrix_ZZ(Test *pTest)
{
  const Ring *R = IM2_Ring_ZZ();
  const FreeModule *F = IM2_FreeModule_make(R, 5);
  int r,c,next=0;
  RingElement_array *elems = alloc_ringelem_array(25);
  const Matrix *M, *N;
  for (r=0; r<5; r++)
    for(c=0; c<5; c++)
      elems->array[next++] = IM2_RingElement_from_Integer(R, make_integer(10*r+c));
  M = IM2_Matrix_make1(F,5,elems,0);
  ct_test(pTest,is_eq(IM2_Matrix_to_string(M),
		      "0 6  12 18 24 \n"
		      "1 7  13 19 25 \n"
		      "2 8  14 20 26 \n"
		      "3 9  15 21 27 \n"
		      "4 10 16 22 28 \n"));
  
  next = 0;
  elems = alloc_ringelem_array(9);
  for (r=0; r<3; r++)
    for(c=0; c<3; c++)
      elems->array[next++] = IM2_RingElement_from_Integer(R, make_integer(-r+c));
  F = IM2_FreeModule_make(R, 3);
  N = IM2_Matrix_make1(F,3,elems,0);
  ct_test(pTest,is_eq(IM2_Matrix_to_string(M),
		      "0 -1 -2 \n"
		      "1 0  -1 \n"
		      "2 1  0  \n"));
  test_matrix_ops(pTest,M,N);
}
#if 0
void test_matrix_ZZ(Test *pTest)
{
  int i;
  const Ring *R = IM2_Ring_ZZ();
  const FreeModule *F = IM2_FreeModule_make(R, 5);
  Vector_array *V = make_vector_array(5);
  const Matrix *M, *N;
  for (i=0; i<5; i++)
    {
      const Vector *w = 
        make_vector(F, 
		    IM2_RingElement_from_Integer(R, make_integer(6*i)),
		    IM2_RingElement_from_Integer(R, make_integer(6*i+1)),
		    IM2_RingElement_from_Integer(R, make_integer(6*i+2)),
		    IM2_RingElement_from_Integer(R, make_integer(6*i+3)),
		    IM2_RingElement_from_Integer(R, make_integer(6*i+4)));
      
      V->array[i] = w;
    }
  M = IM2_Matrix_make1(F,V);
  ct_test(pTest,is_eq(IM2_Matrix_to_string(M),
	       "0 6  12 18 24 \n"
	       "1 7  13 19 25 \n"
	       "2 8  14 20 26 \n"
	       "3 9  15 21 27 \n"
	       "4 10 16 22 28 \n"));

  F = IM2_FreeModule_make(R, 3);
  V = make_vector_array(3);
  for (i=0; i<3; i++)
    {
      const Vector *w = 
        make_vector(F, 
		    IM2_RingElement_from_Integer(R, make_integer(-i)),
		    IM2_RingElement_from_Integer(R, make_integer(-i+1)),
		    IM2_RingElement_from_Integer(R, make_integer(-i+2)));
      
      
      V->array[i] = w;
    }
  N = IM2_Matrix_make1(F,V);
  ct_test(pTest,is_eq(IM2_Matrix_to_string(N),
	       "0 -1 -2 \n"
	       "1 0  -1 \n"
	       "2 1  0  \n"));

  test_matrix_ops(pTest,M,N);
}
#endif

void test_matrices(Test *pTest)
{
  test_matrix_ZZ(pTest);
  test_coeffs(pTest);
}

Test * matrix_test(void)
{
  Test *result = ct_create("matrix tests", 0);
  ct_addTestFun(result, test_matrices);
  return result;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check "
// End:
*/
