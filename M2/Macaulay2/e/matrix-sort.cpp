/**
 * @file matrix-sort.cpp
 * @brief `MatrixSorter` --- compute the column permutation that sorts a `Matrix` by degree / leading term.
 *
 * Defines `MatrixSorter`, the helper that the engine-level
 * `Matrix::sort(degorder, ringorder)` (defined at the bottom of
 * this file --- the only external entry point) hands off to.
 * It carries parallel per-column arrays (`sort_vals`, `sort_vecs`,
 * `sort_degs`) plus the `deg_ascending` and `ringorder_ascending`
 * flags from the constructor, which flip the sign of the degree
 * comparison and the ring-order comparison respectively. The
 * comparator walks degrees first (skipped entirely when
 * `deg_ascending == 0`), then leading monomials via
 * `R->compare_vecs`, returning the standard `-1` / `0` / `1`
 * comparator triple. A hand-rolled `sort_partition` /
 * `sort_range` quicksort drives the permutation; null vectors
 * sort to the end. The sorter does not mutate the input matrix
 * --- it computes a permutation `M2_arrayint result` and hands
 * that back via `MatrixSorter::value()`, so the same permutation
 * can be applied uniformly to a basis, its change-of-basis
 * matrix, and its syzygies in lockstep.
 *
 * The class is file-local: no other engine translation unit
 * references `MatrixSorter` directly; everyone goes through
 * `Matrix::sort`.
 *
 * @see matrix.hpp
 * @see style.hpp
 */

#include "matrix.hpp"

/**
 * @brief Helper that computes a column permutation for an engine `Matrix`
 * by degree-then-monomial-order sort.
 *
 * @details `deg_ascending` and `ringorder_ascending` are sign flags; the
 * comparator first orders by `sort_degs[i]` (the column's
 * heuristic degree) and then breaks ties with the ring's lead-term
 * comparison on `sort_vecs[i]`. Indices are sorted into `result`,
 * which the caller uses to permute the matrix columns. Used to
 * implement the engine-side `sort` matrix operation.
 */
class MatrixSorter
{
  const Ring *R;
  int deg_ascending;
  int ringorder_ascending;
  int *sort_vals;
  vec *sort_vecs;
  int *sort_degs;
  M2_arrayint result;

  int sort_compare(int i, int j)
  {
    if (i == j) return 0;
    vec v1 = sort_vecs[i];
    vec v2 = sort_vecs[j];
    if (v1 == nullptr) return 1;
    if (v2 == nullptr) return -1;
    if (deg_ascending != 0)
      {
        int d1 = sort_degs[i];
        int d2 = sort_degs[j];
        if (d1 > d2) return -deg_ascending;
        if (d1 < d2) return deg_ascending;
      }
    int cmp = R->compare_vecs(v1, v2);
    if (cmp > 0) return -ringorder_ascending;
    if (cmp < 0) return ringorder_ascending;
    return 0;
  }

  int sort_partition(int lo, int hi)
  {
    int pivot = sort_vals[lo];
    int i = lo - 1;
    int j = hi + 1;
    for (;;)
      {
        do
          {
            j--;
          }
        while (sort_compare(sort_vals[j], pivot) < 0);
        do
          {
            i++;
          }
        while (sort_compare(sort_vals[i], pivot) > 0);

        if (i < j)
          {
            int tmp = sort_vals[j];
            sort_vals[j] = sort_vals[i];
            sort_vals[i] = tmp;
          }
        else
          return j;
      }
  }

  void sort_range(int lo, int hi)
  {
    if (lo < hi)
      {
        int q = sort_partition(lo, hi);
        sort_range(lo, q);
        sort_range(q + 1, hi);
      }
  }

 public:
  MatrixSorter(const Matrix *m, int degorder, int ringorder);

  M2_arrayintOrNull value();
};

MatrixSorter::MatrixSorter(const Matrix *m, int degorder, int ringorder)
    : deg_ascending(degorder), ringorder_ascending(ringorder)
{
  R = m->get_ring();

  int nelems = m->n_cols();

  if (degorder != 0)
    {
      sort_degs = newarray_atomic(int, nelems);
      for (int i = 0; i < nelems; i++)
        sort_degs[i] = m->cols()->primary_degree(i);
    }

  result = M2_makearrayint(nelems);

  sort_vals = result->array;
  for (int i = 0; i < nelems; i++) sort_vals[i] = i;

  sort_vecs = newarray(vec, nelems);
  for (int i = 0; i < nelems; i++) sort_vecs[i] = m->elem(i);
}

M2_arrayintOrNull MatrixSorter::value()
{
  sort_range(0, result->len - 1);
  return result;
}

M2_arrayint Matrix::sort(int degorder, int ringorder) const
// Sort the columns of 'this': Place the column indices into 'result'.
// If degorder < 0, sort in descending degree order, if >0 ascending degree
// If ==0, or in the event that two columns have the same (simple) degree,
// use the ring order: ringorder > 0 means ascending, <0 means descending.
{
  MatrixSorter sorter(this, degorder, ringorder);
  return sorter.value();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
