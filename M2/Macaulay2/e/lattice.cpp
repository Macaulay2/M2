// Copyright 1997  Michael E. Stillman

#include "comp.hpp"
#include "lattice.hpp"
#include "sparsemat.hpp"
#include "text_io.hpp"

extern "C" char system_interrupted;
extern int gbTrace;

MatrixComputation::MatrixComputation(const Matrix *m, bool do_rowchange, bool do_colchange)
  : R(m->get_ring())
{
  gens = SparseMutableMatrix::make(m);
  if (do_rowchange)
    {
      SparseMutableMatrix *rowchange = SparseMutableMatrix::identity(R,m->n_rows());
      gens->setRowChangeMatrix(rowchange);
    }
  if (do_colchange)
    {
      SparseMutableMatrix *colchange = SparseMutableMatrix::identity(R,m->n_cols());
      gens->setColumnChangeMatrix(colchange);
    }

  last_col = m->n_cols() - 1;
  last_row = m->n_rows() - 1;
}

MatrixComputation::~MatrixComputation()
{
}

bool MatrixComputation::hermiteStep()
{
  // Assumed: 'row' is the last row.

  // For each 0..last_col: sort the elements of lead term 'row'
  // to the end.  For each such column, if the lead term is negative, negate it.
  // Keep track of this range.

  // Sort these elements.  If two equal values are found: subtract them, keep one.
  // Move the 
  // First step: for 0..last_col: if any lead term is 
  // negative, negate it.

  // Sort columns 0..last_col

  // 
  int c;
  if (last_col < 0) return true;
  gens->sortColumns(0,last_col);
  int pivot_row = gens->leadRow(last_col);
  if (pivot_row < 0) return true;
  ring_elem minus_one = R->from_int(-1);
  // For now: our pivot is the lead term in matrix[last_col].
  gens->normalizeColumn(last_col);  // Makes the lead coefficient positive.
  // Make one pass to check for equality, and scale all columns to be positive
  int prev_col = last_col;
  ring_elem a, prev_elem;
  gens->getEntry(pivot_row,last_col,prev_elem);
  for (c=last_col-1; c>=0; c--)
    {
      if (gens->leadRow(c) < pivot_row)
	break;
      gens->normalizeColumn(c);
      gens->getEntry(pivot_row,c,a);
      if (R->is_equal(a,prev_elem))
	gens->addColumnMultiple(prev_col, minus_one, c);
      else
	{
	  prev_col = c;
	  prev_elem = a;
	}
    }
  int bottom_col = c;

  // Now go back through, and reduce each element with the pivot
  for (c=last_col-1; c>bottom_col; c--)
    {
      if (gens->leadRow(c) < pivot_row)
	continue;
      gens->gcdColumnReduce(last_col,c);
    }

#if 0
  // Now reduce each column last_col+1..ncols-1
  for (c=last_col+1; c<gens->n_cols(); c++)
    gens->columnReduce(last_col,c);
#endif
  R->remove(minus_one);
  last_col--;
  return false;
}

int MatrixComputation::calc(int nsteps)
{
  if (nsteps < 0) nsteps = gens->n_cols();
  if (last_col < 0) return 0;	// Done
  while (last_col >= 0)
    {
      if (system_interrupted) return -1;
      if (nsteps-- <= 0) break;
      if (gbTrace >= 1)
	{
	  buffer o;
	  o << "." << last_col;
	  emit(o.str());
	}
      hermiteStep();
    }
  // If reductions are done at the end:
  if (gbTrace >= 1)
    {
      buffer o;
      o << "." << "reduce" << newline;
      emit(o.str());
    }
  for (int c=gens->n_cols()-1; c>=0; c--)
    {
      if (gens->leadRow(c) < 0) continue;  // Zero column: ignore.
      for (int d=c+1; d<gens->n_cols(); d++)
	gens->columnReduce(c,d);
    }

  return 0;
#if 0
  int i,r,c,best;
  // Find a pivot
  if (gens->findGoodUnitPivot(0,last_col,r,c,best))
    {
      // Move this element to the pivot location
      gens->interchangeRows(r,last_row);
      gens->interchangeColumns(c,last_col);
      // Now clear out the row 'last_row' using this unit as the pivot.
      for (i=0; i<last_col; i++)
	if (gens->leadComponent(i) != last_row)
	  continue;
	else
	  gens->columnReduce(last_col, i);
      // That's it for this element.
      last_col--;
      last_row--;
    }

  else if (gens->findGoodPivot(0,last_col,r,c,best))
    {
      // Move this element to the pivot location
      gens->interchangeRows(r,last_row);
      gens->interchangeColumns(c,last_col);
      // Now clear out the row 'last_row' using this unit as the pivot.
      for (i=0; i<last_col; i++)
	if (gens->leadComponent(i) != last_row)
	  continue;
	else
	  gens->gcdColumnReduce(last_col, i);
      // That's it for this element.
      last_col--;
      last_row--;
    }
#endif
  // At this point, we need to try to clear out the elements in the column 'c'.
  // If at any time, we find an element that is not divisible by our pivot, we
  // do the 2 by 2 gcd row modification, but then quit.
  return 0;
}

Matrix *MatrixComputation::getResultMatrix() const
{
  return gens->toMatrix();
}

Matrix *MatrixComputation::getRowChangeOfBasisMatrix() const
{
  SparseMutableMatrix *rowchange = gens->getRowChangeMatrix();
  if (rowchange == 0)
    return 0;
  else
    return rowchange->toMatrix();
}

Matrix *MatrixComputation::getColumnChangeOfBasisMatrix() const
{
  SparseMutableMatrix *colchange = gens->getColumnChangeMatrix();
  if (colchange == 0)
    return 0;
  else
    return colchange->toMatrix();
}

int MatrixComputation::getStatus() const
{
  return 0;
}

////////////////////////////////////////
// Fraction free gaussian elimination //
////////////////////////////////////////

FF_LUComputation::FF_LUComputation(SparseMutableMatrix *M0)
  : R(M0->getRing()),
    M(M0),
    col_perm(0),
    need_div(0)
{
  int ncols = M->n_cols();
  col_perm = new int[ncols];
  need_div = new bool[ncols];
  pivot_col = ncols;  // Will be decremented before use
  for (int i=0; i<ncols; i++)
    {
      col_perm[i] = i;
      need_div[i] = false;
    }

  pivot = R->from_int(1);
  lastpivot = R->from_int(1);
}

FF_LUComputation::~FF_LUComputation()
{
  R->remove(pivot);
  R->remove(lastpivot);
  delete [] col_perm;
  delete [] need_div;
}

bool FF_LUComputation::choose_pivot_column(int lo, int hi, int &result)
{
  int r = -1; // If this remains -1, then no better column was found
  int c = -1;
  for (int i=lo; i<=hi; i++)
    {
      int r1 = M->leadRow(i);
      if (r1 > r) 
	{
	  r = r1;
	  c = i;
	}
    }
  if (r == -1) return false;

  result = c;
  return true;
}

void FF_LUComputation::do_pivots(int lo, int hi, int pivotCol)
{
  // Here we clear out row r in columns lo..hi, using the pivot.
  R->remove(lastpivot);
  lastpivot = pivot;
  pivot = M->leadCoefficient(pivotCol);
  int pivot_row = M->leadRow(pivotCol);
  
  for (int i=lo; i<=hi; i++)
    {
      int r = M->leadRow(i);
      if (r == pivot_row)
	{
	  // Need to modify column i:
	  // col(i) := pivot*M[i] - M[pivot_row,i] * M[pivotCol]
	  ring_elem a = M->leadCoefficient(i);  // This does a copy.
	  R->negate_to(a);
	  M->scaleColumn(i,pivot);
	  M->addColumnMultiple(pivotCol,a,i);
	  R->remove(a);
	}	  

      if (need_div[i])
	M->divideColumn(i,lastpivot);

      need_div[i] = (r == pivot_row);
    }
}

bool FF_LUComputation::calc()
{
  int c1;
  while (choose_pivot_column(0,--pivot_col,c1))
    {
      if (system_interrupted)
	return false;

      if (pivot_col != c1)
	{
	  M->interchangeColumns(pivot_col,c1);
	  
	  // swap need_div[pivot_col], need_div[c1]
	  bool tmp = need_div[pivot_col];
	  need_div[pivot_col] = need_div[c1];
	  need_div[c1] = tmp;
	  
	  // swap col_perm
	  int ctmp = col_perm[pivot_col];
	  col_perm[pivot_col] = col_perm[c1];
	  col_perm[c1] = ctmp;
	}

      do_pivots(0,pivot_col-1,pivot_col);
    }
  return true;
}

M2_arrayint FF_LUComputation::get_column_permutation()
{
  int ncols = M->n_cols();
  M2_arrayint result = makearrayint(ncols);
  for (int i=0; i<ncols; i++)
    result->array[i] = col_perm[i];
  return result;
}

M2_arrayint_OrNull FF_LUComputation::DO(SparseMutableMatrix *M)
{
  FF_LUComputation F(M);
  if (!F.calc()) return NULL;
  M2_arrayint col_permutation = F.get_column_permutation();
  return col_permutation;
}



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
