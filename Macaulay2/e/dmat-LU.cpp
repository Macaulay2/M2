#include "dmat-LU.hpp"

template <typename CoeffRing>
void DMatLU<CoeffRing>::LU1(DMat<CoeffRing> *A, // col-th column is modified
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
void DMatLU<CoeffRing>::set_pivot_info(DMat<CoeffRing> *A,
				     int ncols, // columns 0..ncols-1 are considered
				     M2_arrayint &pivotcols,
				     int &n_pivots)
  // This allocates space for pivotcols
{
  const CoeffRing *K = A->get_CoeffRing();
  int nrows = A->n_rows();

  pivotcols = makearrayint(nrows); // pivot columns 0..npivots-1

  elem *loc = A->get_array();
  int this_row = 0;
  int this_col = 0;
  while (this_col < ncols)
    {
      if (!K->is_zero(*loc))
	{
	  pivotcols->array[this_row++] = this_col;
	  loc++; // to the next row
	}
      loc += nrows; // to the next column
      this_col++;
    }
  n_pivots = this_row;
}

template <typename CoeffRing>
M2_arrayint DMatLU<CoeffRing>::encode_permutation(M2_arrayint permutation)
  // This allocates space for the result
{
  int nrows = permutation->len;
  M2_arrayint encoded_perm = makearrayint(nrows); // The last entry is never used?

  int *t = newarray_atomic(int,nrows);
  int *tinv = newarray_atomic(int,nrows);
  for (int i=0; i<nrows; i++)
    t[i] = i;
  for (int i=0; i<nrows; i++)
    tinv[i] = i;

  for (int i=0; i<=nrows-2; i++)
    {
      int a = t[permutation->array[i]];
      encoded_perm->array[i] = a;
      int b = tinv[i];
      int c = tinv[a];
      // swap(tinv,i,a)
      tinv[i] = c;
      tinv[a] = b;
      // swap(t, b,c);
      int d = t[b];
      t[b] = t[c];
      t[c] = d;
    }
  encoded_perm->array[nrows-1] = nrows-1;
  return encoded_perm;
}

template <typename CoeffRing>
M2_arrayint DMatLU<CoeffRing>::decode_permutation(M2_arrayint encoded_perm)
  // This allocates space for the result
{
  int nrows = encoded_perm->len;
  M2_arrayint result = makearrayint(nrows);

  for (int i=0; i<nrows; i++)
    result->array[i] = i;
  for (int i=0; i<nrows-1; i++)
    {
      int a = encoded_perm->array[i];
      // swap(result, i,a)
      int b = result->array[i];
      result->array[i] = result->array[a];
      result->array[a] = b;
    }
  return result;
}

template <typename CoeffRing>
M2_arrayint  DMatLU<CoeffRing>::LU_one_step(DMat<CoeffRing> *A, // col-th column is modified
				 M2_arrayint perm,
				 int col
				 )
{
  // First we create the perm structure as expected by LU_step above, set pivotcol,
  // call that routine, and then retranslate the permutation array.
#warning "write this"
}

template <typename CoeffRing>
M2_arrayint DMatLU<CoeffRing>::LU(DMat<CoeffRing> *A)
{
  int nrows = A->n_rows();
  int ncols = A->n_cols();

  // These will be set in LU1
  M2_arrayint perm = makearrayint(nrows);
  M2_arrayint pivots = makearrayint(nrows); // pivot columns 0..npivots-1
  int npivots = 0;

  for (int c=0; c<ncols; c++)
    LU1(A,perm,pivots,npivots,c); // modifies all arguments but 'c'

  // perm is encoded.  Here we display it, decode it, display that, encode that, then
  // return the unencoded version
  M2_arrayint result_perm = decode_permutation(perm);
  M2_arrayint reencode_perm = encode_permutation(result_perm);

  fprintf(stderr, "encoded: ");
  for (int i=0; i<perm->len; i++)
    fprintf(stderr, "%d ",perm->array[i]);
  fprintf(stderr,"\n");

  fprintf(stderr, "decoded: ");
  for (int i=0; i<result_perm->len; i++)
    fprintf(stderr, "%d ",result_perm->array[i]);
  fprintf(stderr,"\n");

  fprintf(stderr, "re-encoded: ");
  for (int i=0; i<reencode_perm->len; i++)
    fprintf(stderr, "%d ",reencode_perm->array[i]);
  fprintf(stderr,"\n");

  return result_perm;
}

template <typename CoeffRing>
bool DMatLU<CoeffRing>::solve1(DMat<CoeffRing> *A, // in LU format
			      M2_arrayint perm, // modified, should be of length >= A->n_rows()
			      M2_arrayint pivotcols, // modified, of length >= A->n_rows()
			      int &n_pivot_rows, // how many have already been set
			      DMat<CoeffRing> *b) // replaces b with the solution
  // returns true iff every column of b has a solution
{
  const CoeffRing *K = A->get_CoeffRing();
  elem *array = A->get_array();
  int *pivotcolarray = pivotcols->array;
  int nrows = A->n_rows();
  int ii=0;
  elem *bvec; // length nrows
  elem sum, a;
  K->set_zero(a);
#define MAT(i,j) array[j*nrows+i]

  // First solve PLy = b
  // perm encodes P in such a way that it is easy to use in place
  for (int i=0; i<=n_pivot_rows; i++)
    {
      int ip = perm->array[i];
      K->init_set(sum, bvec[ip]);
      bvec[ip]=bvec[i];
      if (ii)
	for (int j=ii; j<=i; j++)
	  {
	    K->mult(a, MAT(i,pivotcolarray[j]), bvec[j]);
	    K->subtract(sum, sum, a);
	  }
      else
	if (!K->is_zero(sum)) ii=i;
      K->init_set(bvec[i], sum);
    }
  // At this point, we must check that we have a solution
  // Now solve Ux = y
  for (int i=n_pivot_rows-1; i>=0; i--)
    {
      K->init_set(sum,bvec[i]);
      for (int j=i+1; j<n_pivot_rows; j++)
	{
	  K->mult(a,MAT(i,pivotcolarray[j]),bvec[j]);
	  K->subtract(sum,sum,a);
	}
      K->divide(bvec[pivotcolarray[i]], sum, MAT(i,pivotcolarray[i]));
    }
  return true;
#undef MAT
}

template <typename CoeffRing>
void DMatLU<CoeffRing>::kernel1(DMat<CoeffRing> *A, // in LU format
			       M2_arrayint perm, // modified, should be of length >= A->n_rows()
			       M2_arrayint pivotcols, // modified, of length >= A->n_rows()
			       int &n_pivot_rows, // how many have already been set
			       DMat<CoeffRing> *result) // replaces this with a matrix whose
                                          // columns form the kernel of the original A.
{
#warning "write 'kernel'"
}

template <typename CoeffRing>
bool DMatLU<CoeffRing>::solve(DMat<CoeffRing> *LU, // in LU format
	   M2_arrayint permutation, // should be of length >= A->n_rows()
	   DMat<CoeffRing> *b) // replaces b with the solution
  // returns true iff every column of b has a solution
{
}

template <typename CoeffRing>
void DMatLU<CoeffRing>::kernel(DMat<CoeffRing> *LU, // in LU format
		    M2_arrayint permutation, // should be of length >= A->n_rows()
		    DMat<CoeffRing> *result) // replaces this with a matrix whose
                                              // columns form the kernel of the original A.
{
#warning "write this"
}

template <typename CoeffRing>
int DMatLU<CoeffRing>::rank(DMat<CoeffRing> *LU)
{
  const CoeffRing *K = LU->get_CoeffRing();
  int nrows = LU->n_rows();
  int ncols = LU->n_cols();

  elem *loc = LU->get_array();
  int this_row = 0;
  for (int this_col=0; this_col<ncols; this_col++)
    {
      if (!K->is_zero(*loc))
	{
	  this_row++;
	  loc++; // to the next row
	}
      loc += nrows; // to the next column
    }
  return this_row;
}

template <typename CoeffRing>
typename CoeffRing::elem DMatLU<CoeffRing>::determinant(DMat<CoeffRing> *LU)
{
#warning "write this"
}

#include "coeffrings.hpp"
template class DMatLU<CoefficientRingRR>;
template class DMatLU<CoefficientRingCC>;
template class DMatLU<CoefficientRingZZp>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
