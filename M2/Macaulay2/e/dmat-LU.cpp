#include "dmat-LU.hpp"

template <typename CoeffRing>
void DMatLU<CoeffRing>::LU_step(DMat<CoeffRing> *A, // col-th column is modified
				 M2_arrayint perm, // modified, should be of length >= A->n_rows()
				 M2_arrayint pivotcols, // modified, of length >= A->n_rows()
				 int &n_pivot_rows, // how many have already been set
				 int col
				 )
{
  const CoeffRing *K = A->get_CoeffRing();
  elem *array = A->get_array();
  int nrows = A->n_rows();
  int ncols = A->n_cols();
  elem sum;
  elem a;
  K->set_zero(a);
  int *pivotcolarray = pivotcols->array;

#define MAT(i,j) array[j*nrows+i]

  // Set the U elements in this column
  for (int r=0; r<n_pivot_rows; r++)
    {
      K->init_set(sum,MAT(r,col));
      for (int i=0; i<r; i++)
	{
	  K->mult(a,MAT(r,pivotcolarray[i]),MAT(i,col));
	  K->subtract(sum,sum,a);
	}
      K->init_set(MAT(r,col),sum);
    }

  // Now we set the rest of the elements in this column, looking for
  // a new pivot element.
  int new_pivot_row = -1;
  for (int r=n_pivot_rows; r<nrows; r++)
    {
      K->init_set(sum,MAT(r,col));
      for (int i=0; i<n_pivot_rows; i++)
	{
	  K->mult(a,MAT(r,pivotcolarray[i]),MAT(i,col));
	  K->subtract(sum,sum,a);
	}
      K->init_set(MAT(r,col),sum);
      if (new_pivot_row < 0 && !K->is_zero(sum))
	new_pivot_row = r;
    }

  if (new_pivot_row < 0) return; // Nothing else is needed: don't need to change arguments
  if (new_pivot_row > n_pivot_rows)
    {
      // Need to interchange rows 'n_pivot_rows' and 'new_pivot_row'
      for (int k=0; k<ncols; k++)
	{
	  elem tmp = MAT(n_pivot_rows,k);
	  MAT(n_pivot_rows,k) = MAT(new_pivot_row,k);
	  MAT(new_pivot_row,k) = tmp;
	}
    }
  perm->array[n_pivot_rows] = new_pivot_row;
  pivotcolarray[n_pivot_rows] = col;
  n_pivot_rows++;

  // Now perform the divisions, if any
  if (n_pivot_rows < nrows)
    {
      K->invert(a,MAT(n_pivot_rows-1,col));
      for (int i=n_pivot_rows; i<nrows; i++)
	K->mult(MAT(i,col),MAT(i,col),a);
    }
#undef MAT
}

template <typename CoeffRing>
M2_arrayint DMatLU<CoeffRing>::LU(DMat<CoeffRing> *A)
{
  int nrows = A->n_rows();
  int ncols = A->n_cols();

  M2_arrayint perm = makearrayint(nrows);
  M2_arrayint pivots = makearrayint(nrows); // pivot columns 0..npivots-1
  int npivots = 0;

  for (int c=0; c<ncols; c++)
    LU_step(A,perm,pivots,npivots,c); // modifies all arguments but 'c'

  return perm;
}

template <typename CoeffRing>
bool DMatLU<CoeffRing>::solve(DMat<CoeffRing> *A, // in LU format
			      M2_arrayint perm, // modified, should be of length >= A->n_rows()
			      M2_arrayint pivotcols, // modified, of length >= A->n_rows()
			      int &n_pivot_rows, // how many have already been set
			      DMat<CoeffRing> *b) // replaces b with the solution
  // returns true iff every column of b has a solution
{
#warning "write 'solve'"
  return false;
}

template <typename CoeffRing>
void DMatLU<CoeffRing>::kernel(DMat<CoeffRing> *A, // in LU format
			       M2_arrayint perm, // modified, should be of length >= A->n_rows()
			       M2_arrayint pivotcols, // modified, of length >= A->n_rows()
			       int &n_pivot_rows, // how many have already been set
			       DMat<CoeffRing> *result) // replaces this with a matrix whose
                                          // columns form the kernel of the original A.
{
#warning "write 'kernel'"
}

#include "coeffrings.hpp"
template class DMatLU<CoefficientRingRR>;
template class DMatLU<CoefficientRingCC>;
template class DMatLU<CoefficientRingZZp>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
