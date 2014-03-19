// Copyright 2005-2013  Michael E. Stillman

#ifndef _dmat_LU_hpp_
#define _dmat_LU_hpp_

#include "engine.h"
#include "dmat.hpp"

/**
    @ingroup matrices
*/
template <typename CoeffRing>
class DMatLU
{
  typedef typename CoeffRing::elem elem;

  static void set_pivot_info(const DMat<CoeffRing> *A,
                      size_t ncols, // columns 0..ncols-1 are considered
                      M2_arrayint &pivotcols,
                      size_t &n_pivots);

  static void solveF(const DMat<CoeffRing> *L, // m by m
              M2_arrayint perm, // 0..m-1
              const elem *b, // length m
              elem *y); // length m

  static void solveB(const DMat<CoeffRing> *U, // m by n
                     M2_arrayint pivotcols,
                     size_t npivots,
                     const elem *y, // length m
                     elem *x); // length n

  static bool LU1(const DMat<CoeffRing> *A, // col-th column is modified
           DMat<CoeffRing> *L,
           DMat<CoeffRing> *U,
           M2_arrayint perm, // modified, should be of length >= A->n_rows()
           size_t &n_pivot_rows, // how many have already been set
           size_t col
           );

  static bool solve(const DMat<CoeffRing> *L,
                    const DMat<CoeffRing> *U,
                    M2_arrayint permutation, // should be of length  L->n_rows()
                    const DMat<CoeffRing> *b,
                    DMat<CoeffRing> *x); // x is replaced with a matrix with same #cols as b, same
                                         // number of columns as U
  // returns true iff every column of b has a solution

  static bool kernel(const DMat<CoeffRing> *L,
                     const DMat<CoeffRing> *U,
                     M2_arrayint permutation, // should be of length  L->n_rows()
                     DMat<CoeffRing> *x); // x is replaced with a matrix with the same
                                         // number of columns as U

  static size_t rank(const DMat<CoeffRing> *U); // don't need L or permutation

  static elem determinant(const DMat<CoeffRing> *L, // not needed
                          const DMat<CoeffRing> *U,
                          M2_arrayint permutation);
  // What about: rank, determinant, obtaining P, L, U as matrices?

  static size_t rank(DMat<CoeffRing> *U);

public:
  static M2_arrayint LU(const DMat<CoeffRing> *A,
                 DMat<CoeffRing> *L,
                 DMat<CoeffRing> *U
                 );

  static bool solve(const DMat<CoeffRing> *A,
                    const DMat<CoeffRing> *b,
                    DMat<CoeffRing> *x); // resulting solution set
  // returns true iff every column of b has a solution

  static void nullspaceU(const DMat<CoeffRing> *U,
                         DMat<CoeffRing> *x); // resulting kernel




#if 0
// public:
//   // START CODE TO BE REMOVED //
//
//   static M2_arrayint encode_permutation(M2_arrayint permutation);
//
//   static M2_arrayint decode_permutation(M2_arrayint perm);
//
//   static void LU1(DMat<CoeffRing> *A, // col-th column is modified
//                    M2_arrayint perm, // modified, should be of length >= A->n_rows()
//                    M2_arrayint pivotcols, // modified, of length >= A->n_rows()
//                    size_t &n_pivot_rows, // how many have already been set
//                    size_t col
//                    );
//
//   static bool solve1(DMat<CoeffRing> *A, // in LU format
//                  M2_arrayint perm, // modified, should be of length >= A->n_rows()
//                  M2_arrayint pivotcols, // modified, of length >= A->n_rows()
//                  size_t &n_pivot_rows, // how many have already been set
//                  DMat<CoeffRing> *b); // replaces b with the solution
//   // returns true iff every column of b has a solution
//
//   static void kernel1(DMat<CoeffRing> *A, // in LU format
//                  M2_arrayint perm, // modified, should be of length >= A->n_rows()
//                  M2_arrayint pivotcols, // modified, of length >= A->n_rows()
//                  size_t &n_pivot_rows, // how many have already been set
//                  DMat<CoeffRing> *result); // replaces this with a matrix whose
//                                               // columns form the kernel of the original A.
// public:
//   static M2_arrayint LU(DMat<CoeffRing> *A); // modifies A
//
//   static M2_arrayint LU_one_step(DMat<CoeffRing> *LU, // modifies last col of LU,
//                                                    // might also swap rows
//                               M2_arrayint permutation,
//                               size_t this_col);
//
//   static bool solve(DMat<CoeffRing> *LU, // in LU format
//                  M2_arrayint permutation, // should be of length >= A->n_rows()
//                  DMat<CoeffRing> *b); // replaces b with the solution
//   // returns true iff every column of b has a solution
//
//   static void kernel(DMat<CoeffRing> *LU, // in LU format
//                  M2_arrayint permutation, // should be of length >= A->n_rows()
//                  DMat<CoeffRing> *result); // replaces this with a matrix whose
//                                               // columns form the kernel of the original A.
//
//   static elem determinant(DMat<CoeffRing> *LU);
//   // What about: rank, determinant, obtaining P, L, U as matrices?
#endif
};

#include "text-io.hpp"

//#define MAT(M,i,j) (M)->array()[j*nrows+i]
#define MAT(M,i,j) (M)->entry(i,j)


//////////////////////////////////
// Private routines //////////////
//////////////////////////////////

template <typename CoeffRing>
bool DMatLU<CoeffRing>::LU1(const DMat<CoeffRing> *A, // col-th column is modified
                            DMat<CoeffRing> *L,
                            DMat<CoeffRing> *U,
                            M2_arrayint perm, // modified, should be of length >= A->n_rows()
                            size_t &n_pivot_rows, // how many have already been set
                            size_t col
                            )
  // Returns true if a new pivot is found
{
  const CoeffRing& K = A->ring();
  size_t nrows = A->numRows();
  elem sum;
  elem a;
  K.set_zero(a);

  size_t p = n_pivot_rows;
  size_t q = col;

  // Step 1. Set entries U[0..p-1, q]
  for (size_t r=0; r<p; r++)
    {
      // First grab the correct element of A: (P^-1 * A)[r,q]
      size_t rx = perm->array[r];
      K.init_set(sum,MAT(A,rx,q));
      for (size_t i=0; i<r; i++)
        {
          K.mult(a,MAT(L,r,i),MAT(U,i,q));
          K.subtract(sum,sum,a);
        }
      K.init_set(MAT(U,r,q),sum);
    }

  // Step 2. Set entries L[p..nrows-1, p]
  size_t new_pivot_row = static_cast<size_t>(-1);
  for (size_t r=p; r<nrows; r++)
    {
      // First grab the correct element of A: (P^-1 * A)[r,q]
      size_t rx = perm->array[r];
      K.init_set(sum,MAT(A,rx,q));
      for (size_t i=0; i<p; i++)
        {
          K.mult(a,MAT(L,r,i),MAT(U,i,q));
          K.subtract(sum,sum,a);
        }
      if (static_cast<long>(new_pivot_row) < 0 && !K.is_zero(sum))
        new_pivot_row = r;
      K.init_set(MAT(L,r,p),sum);
    }

  // Step 3. If no pivot: return
  //         If a pivot: Choose the next pivot, set U[p,q],
  //           modify perm, pivotcols,n_pivot_rows
  if (static_cast<long>(new_pivot_row) < 0)
    {
      return false; // Nothing else is needed: don't need to change arguments
    }
  if (new_pivot_row > n_pivot_rows)
    {
      // swap rows: new_pivot_row, p in L
      for (size_t i=0; i<=p; i++)
        {
          K.swap(MAT(L,new_pivot_row,i), MAT(L,p,i));
        }
      std::swap<int>(perm->array[p], perm->array[new_pivot_row]);
    }
  K.init_set(MAT(U,p,q), MAT(L,p,p));
  n_pivot_rows++;

  // Step 4. Now perform the divisions, if any
  K.invert(a,MAT(L,p,p)); // We could save one division here by setting L(p,p) = 1 after this
  for (size_t i=p; i<nrows; i++)
    K.mult(MAT(L,i,p),MAT(L,i,p),a);

  return true;
}

template <typename CoeffRing>
void DMatLU<CoeffRing>::solveF(const DMat<CoeffRing> *L, // m by m
                               M2_arrayint perm, // 0..m-1
                               const elem *b, // length m
                               elem *y) // length m
{
  const CoeffRing& K = L->ring();
  size_t nrows = L->numRows();
  elem a;
  K.set_zero(a);

  for (size_t i=0; i<nrows; i++)
    {
      size_t ix = perm->array[i];
      K.init_set(y[i], b[ix]);
      for (size_t j=0; j<i; j++)
        {
          K.mult(a,MAT(L,i,j),y[j]);
          K.subtract(y[i],y[i],a);
        }
    }
}

template <typename CoeffRing>
void DMatLU<CoeffRing>::solveB(const DMat<CoeffRing> *U, // m by n
                               M2_arrayint pivotcols,
                               size_t npivots,
                               const elem *y, // length m
                               elem *x) // length n
{
  const CoeffRing& K = U->ring();
  //  size_t nrows = U->numRows();
  elem a;
  K.set_zero(a);

  size_t r = npivots; // rank of U
  for (size_t i=r-1; i!=static_cast<size_t>(-1); i--)
    {
      size_t ix = pivotcols->array[i];
      K.init_set(x[ix], y[i]);
      for (size_t j=i+1; j<r; j++)
        {
          size_t jx = pivotcols->array[j];
          K.mult(a,MAT(U,i,jx),x[jx]);
          K.subtract(x[ix],x[ix],a);
        }
      K.divide(x[ix],x[ix],MAT(U,i,ix));
    }
}

template <typename CoeffRing>
void DMatLU<CoeffRing>::set_pivot_info(const DMat<CoeffRing> *U,
                                     size_t ncols, // columns 0..ncols-1 are considered
                                     M2_arrayint &pivotcols,
                                     size_t &n_pivots)
  // This allocates space for pivotcols. TODO: use iterator for U
{
  const CoeffRing& K = U->ring();
  int nrows = static_cast<int>(U->numRows());

  pivotcols = M2_makearrayint(nrows); // pivot columns 0..npivots-1

  const elem *loc = U->array();
  int this_row = 0;
  int this_col = 0;
  while (this_col < ncols && this_row < nrows)
    {
      if (!K.is_zero(*loc))
        {
          pivotcols->array[this_row++] = this_col;
          loc++; // to the next row
        }
      loc += nrows; // to the next column
      this_col++;
    }
  n_pivots = this_row;
}

////////////////////////////////
// Public routines /////////////
////////////////////////////////

template <typename CoeffRing>
M2_arrayint DMatLU<CoeffRing>::LU(const DMat<CoeffRing> *A,
                                  DMat<CoeffRing> *L,
                                  DMat<CoeffRing> *U
                                  )
{
  size_t nrows = A->numRows();
  size_t ncols = A->numColumns();

  L->resize(nrows,nrows);
  U->resize(nrows,ncols);

  // These will be set in LU1
  M2_arrayint perm = M2_makearrayint(static_cast<int>(nrows));
  size_t npivots = 0;
  for (int i=0; i<nrows; i++)
    perm->array[i] = i;

  for (size_t c=0; c<ncols; c++)
    LU1(A,L,U,perm,npivots,c); // modifies all arguments but 'c'

  return perm;
}

template <typename CoeffRing>
size_t DMatLU<CoeffRing>::rank(DMat<CoeffRing> *U)
// TODO: rewrite to handle row-major order
{
  const CoeffRing& K = U->ring();
  size_t nrows = U->numRows();
  size_t ncols = U->numColumns();

  elem *loc = U->array();
  size_t this_row = 0;
  for (size_t this_col=0; this_col<ncols; this_col++)
    {
      if (!K.is_zero(*loc))
        {
          this_row++;
          loc++; // to the next row
        }
      loc += nrows; // to the next column
    }
  return this_row;
}


template <typename CoeffRing>
bool DMatLU<CoeffRing>::solve(const DMat<CoeffRing> *A,
                              const DMat<CoeffRing> *b,
                              DMat<CoeffRing> *x) // resulting solution set
// returns true iff every column of b has a solution
// TODO: rewrite to handle row-major order 
{
  // Create L,U,y.  (P is created for us), make sure x has correct shape.
  // For each column of b, solveF, then solveB 9result into same col of x
  const CoeffRing& K = A->ring();

  DMat<CoeffRing> *L = new DMat<CoeffRing>(A->ring(), A->numRows(), A->numRows());
  DMat<CoeffRing> *U = new DMat<CoeffRing>(A->ring(), A->numRows(), A->numColumns());

  M2_arrayint P = LU(A,L,U);

  M2_arrayint pivotcols;
  size_t n_pivots;

  size_t nrowsA = A->numRows();

  set_pivot_info(U,U->numColumns(),pivotcols,n_pivots);

  x->resize(A->numColumns(), b->numColumns());
  elem *y = newarray(elem, nrowsA);
  for (size_t i=0; i<A->numRows(); i++)
    K.set_zero(y[i]);
  const elem *bcol = b->array();
  elem *xcol = x->array();
  for (size_t i=0; i<b->numColumns(); i++)
    {
      solveF(L,P,bcol,y);
      bool is_sol = true;
      for (size_t j=n_pivots; is_sol && j<nrowsA; j++)
        if (!K.is_zero(y[j]))
          is_sol=false;
      if (!is_sol)
        {
          ERROR("matrix is singular and no solution exists");
          deletearray(y);
          return false;
        }
      solveB(U,pivotcols,n_pivots,y,xcol);
      xcol += x->numRows();
      bcol += b->numRows();
    }

  deletearray(y);
  return true; // What to return?
}

template <typename CoeffRing>
void DMatLU<CoeffRing>::nullspaceU(const DMat<CoeffRing> *U,
                                   DMat<CoeffRing> *x) // resulting kernel
// TODO: rewrite to handle row-major order  
{
  const CoeffRing& K = U->ring();
  size_t nrows = U->numRows();

  M2_arrayint pivotcols;
  size_t n_pivots;

  set_pivot_info(U,U->numColumns(),pivotcols,n_pivots);

  x->resize(U->numColumns(), U->numColumns() - n_pivots);
  int *nextpivotcol = pivotcols->array;
  int *endpivotcol = nextpivotcol + pivotcols->len;
  size_t thiscol = 0;
  for (size_t c=0; c<U->numColumns(); c++)
    {
      if (nextpivotcol < endpivotcol && c == *nextpivotcol)
        {
          nextpivotcol++;
          continue;
        }
      solveB(U,
             pivotcols,n_pivots,
             U->array() + nrows*c,
             x->array() + x->numRows() * thiscol);

      K.set_from_long(MAT(x,c,thiscol), -1);
      //      K.from_ring_elem(MAT(x,c,thiscol), U->get_ring()->minus_one());
      thiscol++;
    }
}
#undef MAT

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
