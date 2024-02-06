// Copyright 2004 Michael E. Stillman

#include "fractionfreeLU.hpp"
#include "interrupted.hpp"

////////////////////////////////////////
// Fraction free gaussian elimination //
////////////////////////////////////////

FF_LUComputation::FF_LUComputation(MutableMatrix *M0)
    : R(M0->get_ring()), M(M0), col_perm(0), need_div(0)
{
  int ncols = static_cast<int>(M->n_cols());
  col_perm = newarray_atomic(int, ncols);
  need_div = newarray_atomic(bool, ncols);
  pivot_col = ncols;  // Will be decremented before use
  for (int i = 0; i < ncols; i++)
    {
      col_perm[i] = i;
      need_div[i] = false;
    }

  pivot = R->from_long(1);
  lastpivot = R->from_long(1);
}

FF_LUComputation::~FF_LUComputation()
{
  R->remove(pivot);
  R->remove(lastpivot);
  freemem(col_perm);
  freemem(need_div);
}

bool FF_LUComputation::choose_pivot_column(int lo, int hi, int &result)
{
  int r = -1;  // If this remains -1, then no better column was found
  int c = -1;
  for (int i = lo; i <= hi; i++)
    {
      int r1 = static_cast<int>(M->lead_row(i));
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
  size_t pivot_row = M->lead_row(
      pivotCol, pivot);  // pivot is the element at (pivot_row, pivotCol)

  for (int i = lo; i <= hi; i++)
    {
      ring_elem a;
      size_t r = M->lead_row(i, a);
      if (r == pivot_row)
        {
          // Need to modify column i:
          // col(i) := pivot*M[i] - M[pivot_row,i] * M[pivotCol]
          R->negate_to(a);
          M->scale_column(i, pivot);
          M->column_op(i, a, pivotCol);
        }

      if (need_div[i]) M->divide_column(i, lastpivot);

      need_div[i] = (r == pivot_row);
    }
}

bool FF_LUComputation::calc()
{
  int c1;
  while (choose_pivot_column(0, --pivot_col, c1))
    {
      if (system_interrupted()) return false;

      if (pivot_col != c1)
        {
          M->interchange_columns(pivot_col, c1);

          // swap need_div[pivot_col], need_div[c1]
          bool tmp = need_div[pivot_col];
          need_div[pivot_col] = need_div[c1];
          need_div[c1] = tmp;

          // swap col_perm
          int ctmp = col_perm[pivot_col];
          col_perm[pivot_col] = col_perm[c1];
          col_perm[c1] = ctmp;
        }

      do_pivots(0, pivot_col - 1, pivot_col);
    }
  return true;
}

M2_arrayint FF_LUComputation::get_column_permutation()
{
  int ncols = static_cast<int>(M->n_cols());
  M2_arrayint result = M2_makearrayint(ncols);
  for (int i = 0; i < ncols; i++) result->array[i] = col_perm[i];
  return result;
}

M2_arrayintOrNull FF_LUComputation::DO(MutableMatrix *M)
{
  FF_LUComputation F(M);
  if (!F.calc()) return NULL;
  M2_arrayint col_permutation = F.get_column_permutation();
  return col_permutation;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
