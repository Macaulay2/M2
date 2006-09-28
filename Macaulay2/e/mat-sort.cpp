#include "matrix.hpp"

class MatrixSorter
{
  const PolynomialRing *R;
  const Monoid *M;
  int deg_ascending;
  int monorder_ascending;
  int * sort_vals;
  vec * sort_vecs;
  int * sort_degs;
  M2_arrayint result;

  int sort_compare(int i, int j)
  {
    if (i == j) return 0;
    vec v1 = sort_vecs[i];
    vec v2 = sort_vecs[j];
    if (v1 == NULL) return 1; 
    if (v2 == NULL) return -1; 
    if (deg_ascending != 0)
      {
	int d1 = sort_degs[i];
	int d2 = sort_degs[j];
	if (d1 > d2) return -deg_ascending;
	if (d1 < d2) return deg_ascending;
      }
    int cmp;
    if (M != 0)
      {
	const int *m1 = R->lead_flat_monomial(v1->coeff);
	const int *m2 = R->lead_flat_monomial(v2->coeff);
	cmp = M->compare(m1,v1->comp,m2,v2->comp);
      }
    else
      cmp = v1->comp - v2->comp;
    if (cmp > 0) return -monorder_ascending;
    if (cmp < 0) return monorder_ascending;  
#if 0
    if (K->is_ZZ())
      {
	// Compare coeficients as well.
	cmp = K->cast_to_Z()->compare(v1->coeff, v2->coeff);
	buffer o;
	o << "comparing ";
	K->elem_text_out(o, v1->coeff);
	o << " and ";
	K->elem_text_out(o, v2->coeff);
	o << " result = " << cmp << newline;
	emit(o.str());
	if (cmp < 0) return 1;
	if (cmp > 0) return -1;
      }
#endif
    return 0;
  }

  int sort_partition(int lo, int hi)
  {
    int pivot = sort_vals[lo];
    int i = lo-1;
    int j = hi+1;
    for (;;)
      {
	do { j--; }
	while (sort_compare(sort_vals[j], pivot) < 0);
	do { i++; }
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
	sort_range(q+1, hi);
      }
  }
  
public:
  MatrixSorter(const Matrix *m, int degorder, int monorder);

  M2_arrayint_OrNull value();
};

MatrixSorter::MatrixSorter(const Matrix *m, int degorder, int monorder)
  : deg_ascending(degorder),
    monorder_ascending(monorder)
{
  const Ring *R0 = m->get_ring();
  R = R0->cast_to_PolynomialRing();
  if (R == 0)
    M = 0;
  else
    M = R->getMonoid();

  int nelems = m->n_cols();
  
  if (degorder != 0)
    {
      sort_degs = newarray_atomic(int, nelems);
      for (int i=0; i<nelems; i++)
	sort_degs[i] = m->cols()->primary_degree(i);
    }

  result = makearrayint(nelems);

  sort_vals = result->array;
  for (int i=0; i<nelems; i++)
      sort_vals[i] = i;

  sort_vecs = newarray(vec, nelems);
  for (int i=0; i<nelems; i++)
    sort_vecs[i] = m->elem(i);
}

M2_arrayint_OrNull MatrixSorter::value()
{
  sort_range(0,result->len-1);
  return result;
}

M2_arrayint Matrix::sort(int degorder, int monorder) const
  // Sort the columns of 'this': Place the column indices into 'result'.
  // If degorder < 0, sort in descending degree order, if >0 ascending degree
  // If ==0, or in the event that two columns have the same (simple) degree,
  // use the monomial order: monorder > 0 means ascending, <0 means descending.
{
  MatrixSorter sorter(this, degorder, monorder);
  return sorter.value();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
