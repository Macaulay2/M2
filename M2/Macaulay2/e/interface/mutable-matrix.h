#ifndef _mutable_matrix_h_
#  define _mutable_matrix_h_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class Matrix;
class MutableMatrix;
class Ring;
class RingElement;
#  else
typedef struct Matrix Matrix;
typedef struct MutableMatrix MutableMatrix;
typedef struct Ring Ring;
typedef struct RingElement RingElement;
#  endif

/**
   MutableMatrix interface routines

   Also:
   Fraction free LU decomposition
   LLL bases
   Lapack routines for dense mutable matrices
   Fast linear algebra routines
   Special routines for objects over RRR,CCC
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

/**************************************************/
/**** MutableMatrix routines **********************/
/**************************************************/

MutableMatrix *IM2_MutableMatrix_identity(const Ring *R,
                                          int nrows,
                                          M2_bool prefer_dense);
/* drg: connected rawMutableIdentity, OK*/

MutableMatrix *IM2_MutableMatrix_make(const Ring *R,
                                      int nrows,
                                      int ncols,
                                      M2_bool prefer_dense);
/* drg: connected rawMutableMatrix, OK */

MutableMatrix *IM2_MutableMatrix_from_matrix(const Matrix *M,
                                             M2_bool prefer_dense);
/* drg: connected rawMutableMatrix, OK*/

const Matrix *IM2_MutableMatrix_to_matrix(const MutableMatrix *M);
/* drg: connected rawMatrix, OK*/

M2_string IM2_MutableMatrix_to_string(const MutableMatrix *M);
/* drg: connected toString, OK */

unsigned int rawMutableMatrixHash(const MutableMatrix *M);
/* drg: connected to "hash" */

int IM2_MutableMatrix_n_rows(const MutableMatrix *M);
/* drg: connected rawNumberOfRows, OK */

int IM2_MutableMatrix_n_cols(const MutableMatrix *M);
/* drg: connected rawNumberOfColumns, OK */

void rawMutableMatrixFillRandomDensity(MutableMatrix *M,
                                       double density,
                                       int special_type);
/* drg: connected rawMutableMatrixFillRandom */
/* special_type: 0 is general, 1 is (strictly) upper triangular. */

void rawMutableMatrixFillRandom(MutableMatrix *M, long nelems);
/* drg: connected rawMutableMatrixFillRandom */

MutableMatrix /* or null */ *rawMutableMatrixPromote(const Ring *R,
                                                     const MutableMatrix *f);
/* connected to rawPromote*/

MutableMatrix /* or null */ *rawMutableMatrixLift(int *success_return,
                                                  const Ring *R,
                                                  const MutableMatrix *f);
/* connected to rawLift */
// returns null if lifting not possible

const RingElement /* or null */ *
IM2_MutableMatrix_get_entry(const MutableMatrix *M, int r, int c);
/* drg: connected rawMatrixEntry, OK*/

/* Each of these routines returns false if there was an error. */

M2_bool IM2_MutableMatrix_set_entry(MutableMatrix *M,
                                    int r,
                                    int c,
                                    const RingElement *a);
/* drg: connected rawSetMatrixEntry, OK */

M2_bool IM2_MutableMatrix_row_swap(MutableMatrix *M, int i, int j);
/* drg: connected rawMatrixRowSwap, OK */

M2_bool IM2_MutableMatrix_column_swap(MutableMatrix *M, int i, int j);
/* drg: connected rawMatrixColSwap, OK*/

M2_bool IM2_MutableMatrix_row_operation(MutableMatrix *M,
                                        int i,
                                        const RingElement *r,
                                        int j,
                                        M2_bool opposite_mult);
/* drg: connected rawMatrixRowChange, OK */
/* row(i) <- row(i) + r * row(j), returns false if matrix is
   immutable, or rows are out of bounds */

M2_bool IM2_MutableMatrix_column_operation(MutableMatrix *M,
                                           int i,
                                           const RingElement *r,
                                           int j,
                                           M2_bool opposite_mult);
/* drg: connected rawMatrixColChange, OK*/
/* column(i) <- column(i) + r * column(j), returns false if matrix is
   immutable, or columns are out of bounds */

M2_bool IM2_MutableMatrix_row_scale(MutableMatrix *M,
                                    const RingElement *r,
                                    int i,
                                    M2_bool opposite_mult);
/* drg: connected rawMatrixRowScale, OK*/
/* row(i) <- r * row(i), returns false if matrix is immutable
   or row is out of bounds */

M2_bool IM2_MutableMatrix_column_scale(MutableMatrix *M,
                                       const RingElement *r,
                                       int i,
                                       M2_bool opposite_mult);
/* drg: connected rawMatrixColumnScale, OK */
/* column(i) <- r * column(i), returns false if matrix is immutable
   or row is out of bounds */

M2_bool IM2_MutableMatrix_insert_columns(MutableMatrix *M, int i, int n_to_add);
/* connected to rawInsertColumns, OK */
/* Insert n_to_add columns directly BEFORE column i. */

M2_bool IM2_MutableMatrix_insert_rows(MutableMatrix *M, int i, int n_to_add);
/* connected to rawInsertRows, OK */
/* Insert n_to_add rows directly BEFORE row i. */

M2_bool IM2_MutableMatrix_delete_columns(MutableMatrix *M, int i, int j);
/* connected to rawDeleteColumns, OK */
/* Delete columns i .. j from M */

M2_bool IM2_MutableMatrix_delete_rows(MutableMatrix *M, int i, int j);
/* connected to rawDeleteRows, OK  */
/* Delete rows i .. j from M */

M2_bool IM2_MutableMatrix_column_2by2(MutableMatrix *M,
                                      int c1,
                                      int c2,
                                      const RingElement *a1,
                                      const RingElement *a2,
                                      const RingElement *b1,
                                      const RingElement *b2,
                                      M2_bool opposite_mult);
/* connected to rawMatrixColumnOperation2, OK */
/* column(c1) <- a1 * column(c1) + a2 * column(c2)
   column(c2) <- b1 * column(c1) + b2 * column(c2)
*/

M2_bool IM2_MutableMatrix_row_2by2(MutableMatrix *M,
                                   int r1,
                                   int r2,
                                   const RingElement *a1,
                                   const RingElement *a2,
                                   const RingElement *b1,
                                   const RingElement *b2,
                                   M2_bool opposite_mult);
/* connected to rawMatrixRowOperation2, OK */
/* row(r1) <- a1 * row(r1) + a2 * row(r2)
   row(r2) <- b1 * row(r1) + b2 * row(r2)
*/

M2_bool IM2_MutableMatrix_sort_columns(MutableMatrix *M, int lo, int hi);
/* connected to rawSortColumns2, OK */
/* Returns false if M is not mutable, or lo, or hi are out of range */

M2_bool IM2_MutableMatrix_row_permute(MutableMatrix *M,
                                      int start,
                                      M2_arrayint perm);
/* connected to rawPermuteRows, OK */
/* if perm = [p0 .. pr], then row(start + i) --> row(start + pi), and
   all other rows are unchanged.  p0 .. pr should be a permutation of 0..r */

M2_bool IM2_MutableMatrix_column_permute(MutableMatrix *M,
                                         int start,
                                         M2_arrayint perm);
/* connected to rawPermuteColumns, OK */
/* if perm = [p0 .. pr], then column(start + i) --> column(start + pi), and
   all other rows are unchanged.  p0 .. pr should be a permutation of 0..r */

const RingElement *IM2_Matrix_dot_product(const MutableMatrix *M,
                                          int c1,
                                          int c2);
/* connected to rawColumnDotProduct */
/* Return the dot product of columns c1 and c2 of the matrix M.  If either c1 or
   c2 is out of range, 0 is returned. */

/**
   Is the matrix implemented as a contiguous array of elements?
 */
M2_bool rawMutableMatrixIsDense(const MutableMatrix *M);

M2_bool IM2_MutableMatrix_is_zero(const MutableMatrix *M);
/* drg: connected rawIsZero, OK */

M2_bool IM2_MutableMatrix_is_equal(const MutableMatrix *M,
                                   const MutableMatrix *N);
/* drg: connected to rawIsEqual for use with ==, not connected to '===', OK */
/* This checks that the entries of M,N are the same */

MutableMatrix *IM2_MutableMatrix_copy(MutableMatrix *M, M2_bool prefer_dense);
/* connected to rawMutableMatrix, OK */

M2_bool IM2_MutableMatrix_set_values(MutableMatrix *M,
                                     M2_arrayint rows,
                                     M2_arrayint cols,
                                     engine_RawRingElementArray values);
/* connected to rawSetMatrixValues, OK */
/* Given three arrays of the same length, 'rows', 'cols', 'values', set the
   corresponding values of M.  If any elements are out of range, ignore those
   triples.  If the type of ring element is not valid, or the sizes of the
   matrices do not match, return false. */

MutableMatrix /* or null */ *IM2_MutableMatrix_submatrix(const MutableMatrix *M,
                                                         M2_arrayint rows,
                                                         M2_arrayint cols);
/* drg: connected rawSubmatrix, OK */

MutableMatrix /* or null */ *IM2_MutableMatrix_submatrix1(
    const MutableMatrix *M,
    M2_arrayint cols);
/* drg: connected rawSubmatrix, OK */

M2_bool IM2_MutableMatrix_reduce_by_pivots(MutableMatrix *M);
/* connected rawReduceByPivots */
/* Using row and column operations, use unit pivots to reduce the matrix */
/* A return value of false means that the computation was interrupted */

/** return the transpose of A */
MutableMatrix *rawMutableMatrixTranspose(MutableMatrix *A);

/**************************************************/
/**** Fraction free LU decomposition **************/
/**************************************************/

M2_arrayintOrNull IM2_FF_LU(MutableMatrix *M);
/* connected to rawFFLU */
/* Replace M by a column echelon form.  No fractions are generated, but the
   base ring should be a domain.
   If M has a column change of basis matrix attached, it will be modified
   accordingly.
*/

/**************************************************/
/**** LLL bases ***********************************/
/**************************************************/

M2_bool rawLLL(MutableMatrix *M,
               MutableMatrix /* or null */ *U,
               gmp_QQ threshold,
               int strategy);
/* DAN: connected to rawLLL */
/* Given a mutable matrix M over ZZ, and a rational number threshold, 1/4 <
   threshold <= 1, modify M so that the columns form a Lenstra-Lenstra-Lovasz
   basis of the image of (the original) M.  ASSUMPTION: (strategy=0 case)
   the columns of M are already a a basis for the lattice.
   The algorithm used is that in Cohen's book on computational algebraic
   number theory, BUT: beware of the typos in the algorithm! If there is any
   error (interrupted, M or threshold not the correct kind), then false is
   returned, and LLL is set to 0. If M has a column change of basis matrix
   attached, it will be modified accordingly.

   strategy: 0 means use original Macaulay2 engine routine.
             2 means use NTL LLL
             (strategy%3) == 3 means use one of the real number variants:
             GramSchmidt or Givens: 0 or 4 (respectively)
             LLL or BKZ: 0 or 8 (respectively)
             FP, QP1, QP, XD, RR (respectively: 0, 16, 2*16, 3*16, 4*16
             Thus: strategy 3+4+8+16 means use NTL's GBKZ_QP1.

   For the RR variants, the suggested value of the threshold is 99/100.

   If U is not NULL, then it should be an m by m matrix over ZZ, where m is the
   number of columns of the matrix M.  In this case, it is set to be the
   invertible transform matrix such that Mold * U = Mnew.
*/

M2_bool IM2_SmithNormalForm(MutableMatrix *M);
/* connected rawSmithNormalForm */
/* Given a mutable matrix over ZZ, compute the Smith normal form for M.
   (replaces M with this normal form. Currently the algorithm used makes
   computing the change of basis matrices difficult (we use mod D arithmetic,
   where D is the determinant). If there is an error, then an error is flagged
   and false is returned.
*/

M2_bool IM2_HermiteNormalForm(MutableMatrix *M);
/* connect rawHermiteNormalForm */
/* Given a mutable matrix over ZZ, compute the Hermite normal form for M.
   (replaces M with this normal form. Currently the algorithm used makes
   computing the change of basis matrices difficult (we use mod D arithmetic,
   where D is the determinant). If there is an error, then an error is flagged
   and false is returned.
*/

/***************************************************
 ***** Lapack routines for dense mutable matrices **
 ***************************************************/

/* Each of the following routines accepts honest MutableMatrix arguments,
   and returns false if there is an error.  The return values are placed into
   some of the (already existing) parameters of the routine */

M2_arrayintOrNull rawLU(const MutableMatrix *A,
                        MutableMatrix *L,
                        MutableMatrix *U);
/* connected */
/* Returns the permutation array: we need to be more precise which one.
   A encodes both the L and the U part, as in lapack.
 */

/**
   Hi, this is rawLUincremental.

   Returns the permutation array: we need to be more precise which one.
   Given (first m columns of PLU = first m columns of A (not given), and
   given the (m+1)-st column of A, then returns a Q, modified from P,
   and changes LU (which encodes L and U)
   s.t.  (first (m+1) columns of QLU = first (m+1) columns of A (not given)
   Note: LU encodes L and U in the usual manner: lower triangular part is L,
   diagonal of L is all ones, and U is the upper triangular part.
*/
M2_arrayintOrNull rawLUincremental(M2_arrayintOrNull P, /* constant */
                                   MutableMatrix *LU, /* modified in routine */
                                   const MutableMatrix *v, /* constant */
                                   int m);

void rawTriangularSolve(MutableMatrix *Lv, /* modified in routine */
                        MutableMatrix *x,  /* modified in routine */
                        int m,
                        int strategy);

M2_bool rawEigenvalues(MutableMatrix *A,
                       MutableMatrix *eigenvalues,
                       M2_bool isHermitian); /* connected */
/*
 */

M2_bool rawEigenvectors(MutableMatrix *A,
                        MutableMatrix *eigenvalues,
                        MutableMatrix *eigenvectors,
                        M2_bool isHermitian); /* connected */
/*
 */

M2_bool rawSVD(MutableMatrix *A,
               MutableMatrix *Sigma,
               MutableMatrix *U,
               MutableMatrix *VT,
               M2_bool use_divide_and_conquer); /* connected */
/*
 */

M2_bool rawLeastSquares(MutableMatrix *A,
                        MutableMatrix *b,
                        MutableMatrix *x, /* return value: argument modified */
                        M2_bool assume_full_rank); /* connected */
/* Case 1: A is a dense matrix over RR.  Then so are b,x.
   Case 2: A is a dense matrix over CC.  Then so are b,x. */

M2_bool rawQR(const MutableMatrix *A, /* input m x n matrix */
              MutableMatrix *Q,   /* output m x n orthonormal columns matrix */
              MutableMatrix *R,   /* output R matrix: upper triangular,
                                     nonsingular if A has ker A = 0 */
              M2_bool return_QR); /* if false, the output is instead the lapack
                                     encoded Householder transformations */
/* if return_QR is false, then Q will contain the encoded Householder
   reflections and the multipliers tau_i will appear in R. MES TODO: be more
   specific here, once we know the exact format!
*/

/********************************/
/* Fast linear algebra routines */
/********************************/

/**
   returns the rank of the matrix M.  If 'rank' is not defined on this type of
   matrix, then returns -1 (and an error message is given).
 */
long rawLinAlgRank(MutableMatrix *M);

/** requires: M should be a square matrix.
    If not, or if the ring has not implemented this routine,
    then null is returned (and an error message is given).
 */
const RingElement *rawLinAlgDeterminant(MutableMatrix *A);

MutableMatrix *rawLinAlgInverse(MutableMatrix *A);

/** compute the row reduced echelon form of the matrix A.
    This is a matrix of the same shape as A.
    NULL, and an error, is returned if the ring is not
    equipped to compute this, or if it has not been
    implemented for that ring type yet
*/
MutableMatrix *rawLinAlgRREF(MutableMatrix *A);

M2_arrayintOrNull rawLinAlgRankProfile(MutableMatrix *A, M2_bool row_profile);

MutableMatrix *rawLinAlgNullSpace(MutableMatrix *A);

/** Returns X s.t. AX = B.  Assumptions:
    A has the same number of rows as B. A doesn't have to be invertible or
   square. If a usage error occurs, NULL is returned and 'success' is set to 0.
    In all other cases, 'success' is set to 1.
    If AX=B has no solutions, then NULL is returned,
    otherwise a matrix X solving AX=B is returned.
*/
MutableMatrix *rawLinAlgSolve(const MutableMatrix *A,
                              const MutableMatrix *B,
                              int *success);

/** Returns X s.t. AX = B.  Assumptions:
    A is a square matrix, with the same number of rows as B.
    If a usage error occurs, NULL is returned and 'success' is set to 0.
    In all other cases, 'success' is set to 1.
    If A turns out to be not invertible, NULL is returned,
    otherwise the unique matrix X solving AX=B is returned.
*/
MutableMatrix *rawLinAlgSolveInvertible(const MutableMatrix *A,
                                        const MutableMatrix *B,
                                        int *success);

/** A,B,C should be mutable matrices over the same ring, and a,b
   elements of this ring.
   C = b*C + a * op(A)*op(B),
   where op(A) = A or transpose(A), depending on transposeA
   where op(B) = B or transpose(B), depending on transposeB
*/

/** set C += A*B.  If not implemented, or sizes/rings are not compatible
    then false is returned.  Otherwise true is returned.
*/
M2_bool rawLinAlgAddMult(MutableMatrix *C,
                         const MutableMatrix *A,
                         const MutableMatrix *B);

/** set C -= A*B.  If not implemented, or sizes/rings are not compatible
    then false is returned.  Otherwise true is returned.
*/
M2_bool rawLinAlgSubMult(MutableMatrix *C,
                         const MutableMatrix *A,
                         const MutableMatrix *B);

/* return A*B, where A,B are mutable matrices, over same ring, same density
 * type.
 */
MutableMatrix * /* or null */ rawLinAlgMult(const MutableMatrix *A,
                                            const MutableMatrix *B);

engine_RawRingElementArrayOrNull rawLinAlgCharPoly(MutableMatrix *A);
// returns an array whose coefficients give the characteristic polynomial of the
// square matrix A

engine_RawRingElementArrayOrNull rawLinAlgMinPoly(MutableMatrix *A);
// returns an array whose coefficients give the minimal polynomial of the square
// matrix A

#  if 0
RingElement *rawFFPackDeterminant(MutableMatrix *M);
/* connected to rawFFPackDeterminant, MES */
/* requires: M should be a square matrix over a prime finite field */

size_t rawFFPackRank(MutableMatrix *M);
/* connected to rawFFPackRank, MES */
/* requires: M should be a matrix over a prime finite field */

MutableMatrix /* Or Null */ *rawFFPackNullSpace(MutableMatrix *M,
                                                M2_bool right_side);
/* connected to rawFFPackNullSpace, MES */
/* requires: M should be a matrix over a prime finite field */
/* computes either left or right nullspace */

MutableMatrix /* or null */ *rawFFPackSolve(MutableMatrix *A,
                                            MutableMatrix *B,
                                            M2_bool right_side);
/* connected to rawFFPackSolve, MES */
/* requires: M should be a matrix over a prime finite field */
/* returns solution of AX=B or XA=B, depending on right_side */

MutableMatrix /* or null */ *rawFFPackInvert(MutableMatrix *M);
/* connected to rawFFPackInvert, MES */
/* requires: M should be a square matrix over a prime finite field */

MutableMatrix /* or null */ *rawFFPackAddMultipleTo(MutableMatrix *C,
                                                    const MutableMatrix *A,
                                                    const MutableMatrix *B,
                                                    M2_bool transposeA,
                                                    M2_bool transposeB,
                                                    const RingElement *a,
                                                    const RingElement *b);
/* A,B,C should be mutable matrices over a finite prime field, and a,b
   elements of this field.
   C = b*C + a * op(A)*op(B),
   where op(A) = A or transpose(A), depending on transposeA
   where op(B) = B or transpose(B), depending on transposeB
   connected to rawFFPackAddMultipleTo, MES
*/

M2_arrayintOrNull rawFFPackRowRankProfile(MutableMatrix *A);
/* connected, MES */

M2_arrayintOrNull rawFFPackColumnRankProfile(MutableMatrix *A);
/* connected, MES */
#  endif

engine_RawArrayIntPairOrNull rawLQUPFactorization(MutableMatrix *A);

/**************************************************/
/**** Special routines for objects over RRR,CCC ***/
/**************************************************/

// FIXME: move these elsewhere

/* The code for these is in x-mutablemat.cpp */

/* These routines set any real or complex numbers whose absolute value is less
   than epsilon.  If the ring is not over RRR or CCC, then an error message is
   given, and NULL is returned. */

const Matrix /* or null */ *rawMatrixClean(gmp_RR epsilon, const Matrix *M);
const RingElement /* or null */ *rawRingElementClean(gmp_RR epsilon,
                                                     const RingElement *f);
MutableMatrix /* or null */ *rawMutableMatrixClean(gmp_RR epsilon,
                                                   MutableMatrix *M);
/* modifies M in place */

/* p is currently limited to infinity (with a given precision), and this routine
   returns the maximum norm of any RRR or CCC coefficient in the object.
   If the ring is not over RRR or CCC, then an error message is given, and NULL
   is returned */

gmp_RRorNull rawMatrixNorm(gmp_RR p, const Matrix *M);
gmp_RRorNull rawRingElementNorm(gmp_RR p, const RingElement *f);
gmp_RRorNull rawMutableMatrixNorm(gmp_RR p, const MutableMatrix *M);

#  if defined(__cplusplus)
}
#  endif

#endif /* _mutable-matrix_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
