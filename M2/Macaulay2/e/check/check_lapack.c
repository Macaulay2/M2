#include "engine.h"
#include "util.h"
#include "lapack.h"
#include "src_test/Ctest.h"
#include "src_test/Csuite.h"

void display_LMatrixCC(const LMatrixCC *m)
{
  if (m == 0)
    printf("%s\n", tocharstar(IM2_last_error_message()));
  else
    printf("%s\n", tocharstar(LP_LMatrixCC_to_string(m)));
}

void display_LMatrixRR(const LMatrixRR *m)
{
  if (m == 0)
    printf("%s\n", tocharstar(IM2_last_error_message()));
  else
    printf("%s\n", tocharstar(LP_LMatrixRR_to_string(m)));
}

M2_CC make_M2_CC(double re, double im)
{
  M2_CC c = (M2_CC) getmem_atomic(sizeof(double)*2);
  c->re = re;
  c->im = im;
  return c;
}

LMatrixRR * make_transpose_LMatrixRR(const LMatrixRR *A)
{
  LMatrixRR *M;
  int i, j, rows, cols;
  double entry;

  rows = LP_LMatrixRR_nrows(A);
  cols = LP_LMatrixRR_ncols(A);
  M = LP_LMatrixRR_make(cols, rows);
  for (i = 0; i < rows; i++) 
    for (j = 0; j < cols; j++) {
      LP_LMatrixRR_get_entry(A,i,j,&entry);
      LP_LMatrixRR_set_entry(M,j,i,entry);
    }
  return M;
}

LMatrixRR * make_diagonal_LMatrixRR(const LMatrixRR *vec)
{
  LMatrixRR *M;
  int i, rows;
  double entry;
  if (LP_LMatrixRR_ncols(vec) != 1) {
    printf("expected a column vector\n");
    return 0;
  }

  rows = LP_LMatrixRR_nrows(vec);
  M = LP_LMatrixRR_make(rows, rows);
  for (i = 0; i < rows; i++) {
    LP_LMatrixRR_get_entry(vec,i,0,&entry);
    LP_LMatrixRR_set_entry(M,i,i,entry);
  }
  return M;
}

LMatrixRR * make_diagonals_LMatrixRR(const LMatrixRR *vec, 
				     int nrows, int ncols)
{
  LMatrixRR *M;
  int i, rows;
  double entry;
  if (LP_LMatrixRR_ncols(vec) != 1) {
    printf("expected a column vector\n");
    return 0;
  }
  
  rows = LP_LMatrixRR_nrows(vec);
  if (rows > nrows) {
    printf("column vector longer than matrix\n");
    return 0;
  }
  
  M = LP_LMatrixRR_make(nrows, ncols);
  for (i = 0; i < rows; i++) {
    LP_LMatrixRR_get_entry(vec,i,0,&entry);
    LP_LMatrixRR_set_entry(M,i,i,entry);
  }
  return M;
}

LMatrixCC * make_diagonal_LMatrixCC(const LMatrixCC *vec)
{
  LMatrixCC *M;
  M2_CC z = (M2_CC) getmem_atomic(sizeof(double)*2);
  int i, rows;
  if (LP_LMatrixCC_ncols(vec) != 1) return 0; 

  rows = LP_LMatrixCC_nrows(vec);
  M = LP_LMatrixCC_make(rows, rows);
  for (i = 0; i < rows; i++) {
    LP_LMatrixCC_get_entry(vec,i,0,z);
    LP_LMatrixCC_set_entry(M,i,i,z);
  }
  return M;
}


void test_lapack(Test *pTest)
{
  int i,j;
  int size = 5;
  double tolerance = 0.0001;
  LMatrixRR *eigvals, *eigvecs;
  LMatrixRR *targ, *sols;
  LMatrixRR *N, *R1, *R2, *R3;
  LMatrixCC *eigenvals, *eigenvecs;
  LMatrixCC *target, *solutions;
  LMatrixCC *M, *C1, *C2, *C3;

  IM2_initialize();

  /* Allocate the matrices */

  N = LP_LMatrixRR_make(1,1);
  M = LP_LMatrixCC_make(1,1);
  eigenvals = LP_LMatrixCC_make(1,1);
  eigenvecs = LP_LMatrixCC_make(1,1);
  eigvals = LP_LMatrixRR_make(1,1);
  eigvecs = LP_LMatrixRR_make(1,1);
  target = LP_LMatrixCC_make(1,1);
  solutions = LP_LMatrixCC_make(1,1);
  targ = LP_LMatrixRR_make(1,1);
  sols = LP_LMatrixRR_make(1,1);
  R1 = LP_LMatrixRR_make(1,1);
  R2 = LP_LMatrixRR_make(1,1);
  R3 = LP_LMatrixRR_make(1,1);
  C1 = LP_LMatrixCC_make(1,1);
  C2 = LP_LMatrixCC_make(1,1);
  C3 = LP_LMatrixCC_make(1,1);

  /* TEST LAPACK OVER THE COMPLEX NUMBERS */

  LP_LMatrixRR_resize(N,4,4);
  LP_LMatrixRR_set_entry(N,0,3,-1);
  LP_LMatrixRR_set_entry(N,1,3,-1);
  LP_LMatrixRR_set_entry(N,2,3,-1);
  LP_LMatrixRR_set_entry(N,3,3,-1);
  LP_LMatrixRR_set_entry(N,1,0,1);
  LP_LMatrixRR_set_entry(N,2,1,1);
  LP_LMatrixRR_set_entry(N,3,2,1);

  LP_LMatrixRR_eigenvectors(N,eigenvals,eigenvecs);
  printf("matrix\n");
  display_LMatrixRR(N);
  printf("matrix*matrix\n");
  display_LMatrixRR(LP_LMatrixRR_mult(N,N));
  printf("eigenvalues\n");
  display_LMatrixCC(eigenvals);
  printf("eigenvectors\n");
  display_LMatrixCC(eigenvecs);

  ct_test(pTest,LP_LMatrixCC_is_close(
	 LP_LMatrixCC_mult(LP_LMatrixCC_from_LMatrixRR(N), eigenvecs),
         LP_LMatrixCC_mult(eigenvecs, make_diagonal_LMatrixCC(eigenvals)),
	 tolerance));

  LP_LMatrixRR_eigenvectors_symmetric(N,eigvals,eigvecs);
  printf("symmetric eigenvalues\n");
  display_LMatrixRR(eigvals);
  printf("symmetric eigenvectors\n");
  display_LMatrixRR(eigvecs);


  printf("matrix + matrix\n");
  display_LMatrixRR(LP_LMatrixRR_add(N, N));

  printf("-matrix\n");
  display_LMatrixRR(LP_LMatrixRR_negate(N));


  LP_LMatrixCC_resize(M,4,4);
  LP_LMatrixCC_set_entry(M,0,1, make_M2_CC(1,0));
  LP_LMatrixCC_set_entry(M,1,2, make_M2_CC(1,0));
  LP_LMatrixCC_set_entry(M,2,3, make_M2_CC(1,0));
  LP_LMatrixCC_set_entry(M,0,0, make_M2_CC(-1,-0.5));
  LP_LMatrixCC_set_entry(M,1,1, make_M2_CC(-1,-0.5));
  LP_LMatrixCC_set_entry(M,2,2, make_M2_CC(-1,-0.5));
  LP_LMatrixCC_set_entry(M,3,3, make_M2_CC(-1,-0.5));

  LP_LMatrixCC_eigenvectors(M,eigenvals,eigenvecs);
  printf("matrix\n");
  display_LMatrixCC(M);
  printf("eigenvalues\n");
  display_LMatrixCC(eigenvals);
  printf("eigenvectors\n");
  display_LMatrixCC(eigenvecs);

  ct_test(pTest,LP_LMatrixCC_is_close(
	 LP_LMatrixCC_mult(M, eigenvecs),
         LP_LMatrixCC_mult(eigenvecs, make_diagonal_LMatrixCC(eigenvals)),
	 tolerance));

  LP_LMatrixCC_set_entry(M,0,0, make_M2_CC(3,2.2));
  LP_LMatrixCC_set_entry(M,0,1, make_M2_CC(-1.2,-4.2));
  LP_LMatrixCC_set_entry(M,0,2, make_M2_CC(30,0.2));
  LP_LMatrixCC_set_entry(M,0,3, make_M2_CC(0,2.0));
  LP_LMatrixCC_set_entry(M,1,0, make_M2_CC(5.3,2.1));
  LP_LMatrixCC_set_entry(M,1,1, make_M2_CC(3.9,8.2));
  LP_LMatrixCC_set_entry(M,1,2, make_M2_CC(3,20.2));
  LP_LMatrixCC_set_entry(M,1,3, make_M2_CC(32,-12.2));
  LP_LMatrixCC_set_entry(M,2,0, make_M2_CC(3,2.2));
  LP_LMatrixCC_set_entry(M,2,1, make_M2_CC(3.33,7.5));
  LP_LMatrixCC_set_entry(M,2,2, make_M2_CC(1.23,2.2));
  LP_LMatrixCC_set_entry(M,2,3, make_M2_CC(2.32,8.12));
  LP_LMatrixCC_set_entry(M,3,0, make_M2_CC(5.3,-1.2));
  LP_LMatrixCC_set_entry(M,3,1, make_M2_CC(0.3,2.8));
  LP_LMatrixCC_set_entry(M,3,2, make_M2_CC(3,2.0));
  LP_LMatrixCC_set_entry(M,3,3, make_M2_CC(-0.3,2.8));

  LP_LMatrixCC_eigenvectors(M,eigenvals,eigenvecs);
  printf("matrix\n");
  display_LMatrixCC(M);
  printf("eigenvalues\n");
  display_LMatrixCC(eigenvals);
  printf("eigenvectors\n");
  display_LMatrixCC(eigenvecs);

  ct_test(pTest,LP_LMatrixCC_is_close(
	 LP_LMatrixCC_mult(M, eigenvecs),
         LP_LMatrixCC_mult(eigenvecs, make_diagonal_LMatrixCC(eigenvals)),
	 tolerance));

  LP_LMatrixCC_set_entry(M,0,0, make_M2_CC(3,0));
  LP_LMatrixCC_set_entry(M,0,1, make_M2_CC(-1.2,-4.2));
  LP_LMatrixCC_set_entry(M,0,2, make_M2_CC(30,0.2));
  LP_LMatrixCC_set_entry(M,0,3, make_M2_CC(0,2.0));
  LP_LMatrixCC_set_entry(M,1,0, make_M2_CC(-1.2,4.2));
  LP_LMatrixCC_set_entry(M,1,1, make_M2_CC(3.9,0));
  LP_LMatrixCC_set_entry(M,1,2, make_M2_CC(3,20.2));
  LP_LMatrixCC_set_entry(M,1,3, make_M2_CC(32,-12.2));
  LP_LMatrixCC_set_entry(M,2,0, make_M2_CC(30,-0.2));
  LP_LMatrixCC_set_entry(M,2,1, make_M2_CC(3,-20.2));
  LP_LMatrixCC_set_entry(M,2,2, make_M2_CC(1.23,0));
  LP_LMatrixCC_set_entry(M,2,3, make_M2_CC(2.32,8.12));
  LP_LMatrixCC_set_entry(M,3,0, make_M2_CC(0,-2.0));
  LP_LMatrixCC_set_entry(M,3,1, make_M2_CC(32,12.2));
  LP_LMatrixCC_set_entry(M,3,2, make_M2_CC(2.32,-8.12));
  LP_LMatrixCC_set_entry(M,3,3, make_M2_CC(-0.3,0));

  LP_LMatrixCC_eigenvectors_hermitian(M,eigvals,eigenvecs);
  printf("matrix, hermitian\n");
  display_LMatrixCC(M);
  printf("eigenvalues, hermitian\n");
  display_LMatrixRR(eigvals);
  printf("eigenvectors, hermitian\n");
  display_LMatrixCC(eigenvecs);

  ct_test(pTest,LP_LMatrixCC_is_close(
	 LP_LMatrixCC_eigenvectors(M, eigenvals, eigenvecs),
         LP_LMatrixCC_eigenvectors_hermitian(M, eigvals, eigenvecs),
	 tolerance));

  LP_LMatrixCC_resize(target,4,2);
  LP_LMatrixCC_set_entry(target,0,0, make_M2_CC(1,0));
  LP_LMatrixCC_set_entry(target,1,0, make_M2_CC(1,0));
  LP_LMatrixCC_set_entry(target,2,0, make_M2_CC(1,0));
  LP_LMatrixCC_set_entry(target,3,0, make_M2_CC(1,0));
  LP_LMatrixCC_set_entry(target,0,1, make_M2_CC(-1,-0.5));
  LP_LMatrixCC_set_entry(target,1,1, make_M2_CC(1,0.5));
  LP_LMatrixCC_set_entry(target,2,1, make_M2_CC(-1,-0.5));
  LP_LMatrixCC_set_entry(target,3,1, make_M2_CC(-2,3.5));

  LP_LMatrixCC_solve(M, target, solutions);
  printf("b\n");
  display_LMatrixCC(target);
  printf("x\n");
  display_LMatrixCC(solutions);
  printf("Ax\n");
  display_LMatrixCC(LP_LMatrixCC_mult(M,solutions));

  ct_test(pTest,LP_LMatrixCC_is_close(LP_LMatrixCC_mult(M, solutions),
			       target, tolerance));

  LP_LMatrixCC_LU(M, C1, C2, R3);
  printf("P\n");
  display_LMatrixRR(R3);
  printf("L\n");
  display_LMatrixCC(C1);
  printf("U\n");
  display_LMatrixCC(C2);

  printf("N + U\n");
  display_LMatrixCC(LP_LMatrixRC_add(N, C2));
  printf("N - U\n");
  display_LMatrixCC(LP_LMatrixRC_subtract(N, C2));
  printf("U - N\n");
  display_LMatrixCC(LP_LMatrixCR_subtract(C2, N));
  ct_test(pTest,LP_LMatrixCC_is_close(LP_LMatrixRC_add(N,C1),
			       LP_LMatrixCR_add(C1,N),
			       tolerance));

  printf("-L\n");
  display_LMatrixCC(LP_LMatrixCC_negate(C1));
  printf("L + U\n");
  display_LMatrixCC(LP_LMatrixCC_add(C1, C2));
  printf("L - U\n");
  display_LMatrixCC(LP_LMatrixCC_subtract(C1, C2));
  printf("U - L\n");
  display_LMatrixCC(LP_LMatrixCC_subtract(C2, C1));
  printf("L + L\n");
  display_LMatrixCC(LP_LMatrixCC_add(C1, C1));
  printf("L - L\n");
  display_LMatrixCC(LP_LMatrixCC_subtract(C1, C1));
  ct_test(pTest,LP_LMatrixCC_is_close(LP_LMatrixCC_add(C1,C2),
			       LP_LMatrixCC_add(C2,C1),
			       tolerance));


  /*  return 0; */

  LP_LMatrixCC_SVD(M,R1,C2,C3);
  printf("matrix for SVD\n");
  display_LMatrixCC(M);
  printf("Singular values\n");
  display_LMatrixRR(R1);
  printf("U matrix\n");
  display_LMatrixCC(C2);
  printf("VT matrix\n");
  display_LMatrixCC(C3);

  ct_test(pTest,LP_LMatrixCC_is_close(LP_LMatrixCC_mult(LP_LMatrixCR_mult(C2,
		    make_diagonal_LMatrixRR(R1)), C3),M,tolerance));

  LP_LMatrixCC_SVD_divide_conquer(M,R1,C2,C3);
  printf("Singular values, divide and conquer\n");
  display_LMatrixRR(R1);
  printf("U matrix, diveide and conquer\n");
  display_LMatrixCC(C2);
  printf("VT matrix, divide and conquer\n");
  display_LMatrixCC(C3);

  /* return 0; */


  /* TEST LAPACK OVER THE REAL NUMBERS */

  /* SOLUTIONS REAL MATRIX */
  LP_LMatrixRR_resize(N,3,3);
  LP_LMatrixRR_set_entry(N,0,0,3);
  LP_LMatrixRR_set_entry(N,0,1,-2);
  LP_LMatrixRR_set_entry(N,0,2,4);
  LP_LMatrixRR_set_entry(N,1,1,6);
  LP_LMatrixRR_set_entry(N,1,2,2);
  LP_LMatrixRR_set_entry(N,2,2,3);
  LP_LMatrixRR_resize(targ,3,2);
  LP_LMatrixRR_set_entry(targ,0,0,7);
  LP_LMatrixRR_set_entry(targ,1,0,-7);
  LP_LMatrixRR_set_entry(targ,2,0,7);
  LP_LMatrixRR_set_entry(targ,0,1,1);
  LP_LMatrixRR_set_entry(targ,1,1,1);
  LP_LMatrixRR_set_entry(targ,2,1,-1);

  LP_LMatrixRR_solve(N,targ,sols);
  printf("matrix A\n");
  display_LMatrixRR(N);
  printf("right-hand-side b\n");
  display_LMatrixRR(targ);
  printf("solutions x\n");
  display_LMatrixRR(sols);

  printf("product Ax\n");
  display_LMatrixRR(LP_LMatrixRR_mult(N,sols));
  ct_test(pTest,LP_LMatrixRR_is_close(targ, LP_LMatrixRR_mult(N, sols), tolerance));
  /* ct_test(pTest,LP_LMatrixRR_is_equal(targ, LP_LMatrixRR_mult(N, sols))); */
  /* doesn't work because of round-off errors! */

  /* LU DECOMPOSITION */
  N = LP_LMatrixRR_make(size,2*size);
  for (i=0; i<size; i++)
    for (j=0; j<2*size; j++) {
      LP_LMatrixRR_set_entry(N,i,j,3.0 * i + j + .234);
     }

  LP_LMatrixRR_LU(N,R1,R2,R3);
  printf("matrix\n");
  display_LMatrixRR(N);
  printf("L matrix\n");
  display_LMatrixRR(R1);
  printf("U matrix\n");
  display_LMatrixRR(R2);
  printf("P indices\n");
  display_LMatrixRR(R3);

  ct_test(pTest,LP_LMatrixRR_is_close(LP_LMatrixRR_mult(R3, LP_LMatrixRR_mult(R1,R2)),
			       N, tolerance));

  /* SVD REAL MATRIX */
  LP_LMatrixRR_resize(N,3,2);
  LP_LMatrixRR_set_entry(N,0,0,1);
  LP_LMatrixRR_set_entry(N,0,1,-1);
  LP_LMatrixRR_set_entry(N,1,0,-2);
  LP_LMatrixRR_set_entry(N,1,1,2);
  LP_LMatrixRR_set_entry(N,2,0,2);
  LP_LMatrixRR_set_entry(N,2,1,-2);

  LP_LMatrixRR_SVD(N,R1,R2,R3);
  printf("matrix for SVD\n");
  display_LMatrixRR(N);
  printf("Singular values\n");
  display_LMatrixRR(R1);
  printf("U matrix\n");
  display_LMatrixRR(R2);
  printf("VT matrix\n");
  display_LMatrixRR(R3);

  ct_test(pTest,LP_LMatrixRR_is_close(N,
	   LP_LMatrixRR_mult(LP_LMatrixRR_mult(R2,
	   make_diagonals_LMatrixRR(R1,3,2)), R3),
	   tolerance
	   ));

  LP_LMatrixRR_SVD_divide_conquer(N,R1,R2,R3);
  ct_test(pTest,LP_LMatrixRR_is_close(N,
	   LP_LMatrixRR_mult(LP_LMatrixRR_mult(R2,
	   make_diagonals_LMatrixRR(R1,3,2)), R3),
	   tolerance
	   ));

  /* EIGENVALUES AND EIGENVECTORS REAL SYMMETRIC MATRIX */
  LP_LMatrixRR_resize(N,3,3);
  LP_LMatrixRR_set_entry(N,0,0,3);
  LP_LMatrixRR_set_entry(N,1,1,6);
  LP_LMatrixRR_set_entry(N,2,2,3);
  LP_LMatrixRR_set_entry(N,0,1,-2); 
  LP_LMatrixRR_set_entry(N,1,0,-2);
  LP_LMatrixRR_set_entry(N,0,2,4);
  LP_LMatrixRR_set_entry(N,2,0,4);
  LP_LMatrixRR_set_entry(N,1,2,2);
  LP_LMatrixRR_set_entry(N,2,1,2);

  LP_LMatrixRR_eigenvectors_symmetric(N, eigvals, eigvecs);
  printf("matrix\n");
  display_LMatrixRR(N);
  printf("eigenvalues\n");
  display_LMatrixRR(eigvals);
  printf("eigenvectors\n");
  display_LMatrixRR(eigvecs);

  ct_test(pTest,LP_LMatrixRR_is_close(LP_LMatrixRR_mult(N,eigvecs),
         LP_LMatrixRR_mult(eigvecs, make_diagonal_LMatrixRR(eigvals)),
			       tolerance));

  /* COMPLEX MATRICES */
  LP_LMatrixCC_resize(M,3,5);
  printf("complex matrix\n");
  display_LMatrixCC(M);

  LP_LMatrixCC_set_entry(M,0,0, make_M2_CC(3,2.2));
  LP_LMatrixCC_set_entry(M,0,1, make_M2_CC(1.2,4.2));
  LP_LMatrixCC_set_entry(M,0,2, make_M2_CC(30,0.2));
  LP_LMatrixCC_set_entry(M,0,3, make_M2_CC(0,2.0));
  LP_LMatrixCC_set_entry(M,0,4, make_M2_CC(0.3,2.8));
  LP_LMatrixCC_set_entry(M,1,0, make_M2_CC(5.3,2.1));
  LP_LMatrixCC_set_entry(M,1,1, make_M2_CC(3.9,8.2));
  LP_LMatrixCC_set_entry(M,1,2, make_M2_CC(3,20.2));
  LP_LMatrixCC_set_entry(M,1,3, make_M2_CC(32,12.2));
  LP_LMatrixCC_set_entry(M,1,4, make_M2_CC(3,2.0));
  LP_LMatrixCC_set_entry(M,2,0, make_M2_CC(3,2.2));
  LP_LMatrixCC_set_entry(M,2,1, make_M2_CC(3.33,7.5));
  LP_LMatrixCC_set_entry(M,2,2, make_M2_CC(1.23,2.2));
  LP_LMatrixCC_set_entry(M,2,3, make_M2_CC(2.32,8.12));
  LP_LMatrixCC_set_entry(M,2,4, make_M2_CC(5.3,1.2));

  LP_LMatrixCC_LU(M,C1,C2,R3);
  printf("matrix\n");
  display_LMatrixCC(M);
  printf("L matrix\n");
  display_LMatrixCC(C1);
  printf("U matrix\n");
  display_LMatrixCC(C2);
  printf("P indices\n");
  display_LMatrixCC(C3);

  display_LMatrixCC(LP_LMatrixCC_mult(C3, LP_LMatrixCC_mult(C1,C2)));

  return 0;


  /* EIGENVALUES AND EIGENVECTORS REAL MATRIX */

  /*
  LP_LMatrixRR_resize(N,2,2);
  LP_LMatrixRR_set_entry(N,0,1,-2);
  LP_LMatrixRR_set_entry(N,1,0,1);
  printf("matrix\n");
  display_LMatrixRR(N);

  LP_LMatrixRR_eigenvectors(N,eigenvals,eigenvecs);
  printf("eigenvalues\n");
  display_LMatrixCC(eigenvals);
  printf("eigenvectors\n");
  display_LMatrixCC(eigenvecs);

  printf("N*eigenvectors\n");
  display_LMatrixCC(LP_LMatrixCC_mult(LP_LMatrixCC_from_LMatrixRR(N), 
				      eigenvecs));
  display_LMatrixCC(LP_LMatrixCC_mult(eigenvecs,
				      make_diagonal_LMatrixCC(eigenvals)));
  
  ct_test(pTest,LP_LMatrixCC_is_close(
	   LP_LMatrixCC_mult(LP_LMatrixCC_from_LMatrixRR(N), 
			     eigenvecs),
	   LP_LMatrixCC_mult(eigenvecs,
			     make_diagonal_LMatrixCC(eigenvals)),
	   tolerance));
  */

  /* 5TH ROOTS OF UNITY */
  /*
  LP_LMatrixRR_resize(N,4,4);
  LP_LMatrixRR_set_entry(N,0,3,-1);
  LP_LMatrixRR_set_entry(N,1,3,-1);
  LP_LMatrixRR_set_entry(N,2,3,-1);
  LP_LMatrixRR_set_entry(N,3,3,-1);
  LP_LMatrixRR_set_entry(N,1,0,1);
  LP_LMatrixRR_set_entry(N,2,1,1);
  LP_LMatrixRR_set_entry(N,3,2,1);

  LP_LMatrixRR_eigenvectors(N,eigenvals,eigenvecs);
  printf("eigenvalues\n");
  display_LMatrixCC(eigenvals);
  printf("eigenvectors\n");
  display_LMatrixCC(eigenvecs);

  printf("N*eigenvectors\n");
  display_LMatrixCC(LP_LMatrixCC_mult(LP_LMatrixCC_from_LMatrixRR(N), 
				      eigenvecs));
  display_LMatrixCC(LP_LMatrixCC_mult(eigenvecs,
				      make_diagonal_LMatrixCC(eigenvals)));

  return 0;

  for (i=0; i<size; i++)
    for (j=0; j<size; j++)
      if (i >= j) 
	LP_LMatrixCC_set_entry(M,i,j,3.0 * i + j + .234,-10.0);
  */

  /* Eigenvalues of M */
  /*
  for (i=0; i<size; i++)
    for (j=0; j<1; j++)
      LP_LMatrixCC_set_entry(target,i,j,1.0 * i + .224,0.0);  

  printf("original matrix\n");
  display_LMatrixCC(M);
  printf("\n");
  
  LP_LMatrixCC_eigenvalues(M,eigenvals);

  printf("final matrix\n");
  display_LMatrixCC(M);
  printf("\n");

  printf("eigenvalues\n");
  display_LMatrixCC(eigenvals);

  return 0;

  LP_LMatrixCC_eigenvectors(M,eigenvals,eigenvecs);

  printf("eigenvectors\n");
  display_LMatrixCC(eigenvecs);

  printf("eigenvalues\n");
  display_LMatrixCC(eigenvals);

  printf("original matrix\n");
  display_LMatrixCC(M);
  printf("\n");

  printf("original target\n");
  display_LMatrixCC(target);
  printf("\n");

  LP_LMatrixCC_solutions(M,target,solutions);

  printf("final matrix\n");
  display_LMatrixCC(M);
  printf("\n");

  printf("final target\n");
  display_LMatrixCC(target);
  printf("\n");

  printf("solutions\n");
  display_LMatrixCC(solutions);

  return 0;
  */

  /* LU DECOMPOSITION */
  /*
  R1 = LP_LMatrixRR_make(1,1);
  R2 = LP_LMatrixRR_make(1,1);
  R3 = LP_LMatrixRR_make(1,1);
  N = LP_LMatrixRR_make(size,2*size);

  for (i=0; i<size; i++)
    for (j=0; j<2*size; j++) {
      LP_LMatrixRR_set_entry(N,i,j,3.0 * i + j + .234);
     }

  printf("original matrix\n");
  display_LMatrixRR(N);
  printf("\n");

  LP_LMatrixRR_LU_decomposition(N,R1,R2,R3);

  printf("final matrix\n");
  display_LMatrixRR(N);
  printf("\n");

  printf("L matrix\n");
  display_LMatrixRR(R1);
  printf("\n");

  printf("U matrix\n");
  display_LMatrixRR(R2);
  printf("\n");

  printf("P indices\n");
  display_LMatrixRR(R3);
  printf("\n");

  prod1 = LP_LMatrixRR_multiply(R3,R1);
  printf("Product 1\n");
  display_LMatrixRR(prod1);
  printf("\n");  

  prod2 = LP_LMatrixRR_multiply(prod1,R2);
  printf("Product 2\n");
  display_LMatrixRR(prod2);
  printf("\n");  

  return 0;
  */
}

Test * lapack_test(void)
{
  Test *result = ct_create("lapack tests", 0);
  ct_addTestFun(result, test_lapack);
  return result;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check"
// End:
*/
