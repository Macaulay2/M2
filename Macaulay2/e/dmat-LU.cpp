#include "dmat-LU.hpp"

template <typename CoeffRing>
M2_arrayint DMatOps<CoeffRing>::LU(DMat<CoeffRing> *A)
{
  const CoeffRing *K = A->get_CoeffRing();
  int nrows = A->n_rows();
  int ncols = A->n_cols();
  elem *array = A->get_array();

  M2_arrayint result = makearrayint(nrows);
  for (int i=0; i<nrows; i++)
    result->array[i] = i;
#if 0
  for (int c=0; c<ncols; c++)
    {
      elem sum;
      // Set the U elements in this column
      for (int r=0; r<=c; r++)
	{
	  K->init_set(sum,getelem(r,c));
	  for (int i=0; i<r; i++)
	    {
	      K->mult(a,getelem(r,i),getelem(i,c));
	      K->subtract(sum,sum,a);
	    }
	  K->init_set(getelem(r,c),sum);
	}
      // Find a pivot element in this column
      //  If it is different, modify the permutation 'result' and swap partial rows
      if (K->is_zero(getelem(c,c)))
	{
	  // Find a non-zero element if possible
	  // If found: swap rows, modify 'result'
	  // If not found: continue the loop with the next c
	  bool found_pivot = false;
	  for (int i=c; i<nrows; i++)
	    {
	      if (!K->is_zero(getelem(i,c)))
		{
		  found_pivot = true;
		  swap(XXX);
		}
	      // Check if the element is non-zero.
	    }
	  if (!found_pivot) continue;
	}
      elem &pivot = getelem(c,c);

      // Now set the L elements in this column
      for (int r=c+1; r<nrows; r++)
	{
	  K->divide(getelem(r,c),getelem(r,c),pivot);
	}
    }
#endif
  return result;
}

#include "coeffrings.hpp"
template class DMatOps<CoefficientRingRR>;
template class DMatOps<CoefficientRingCC>;
template class DMatOps<CoefficientRingZZp>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
