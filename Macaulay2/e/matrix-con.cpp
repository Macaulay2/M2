#include "matrix-con.hpp"
#include "matrix.hpp"

MatrixConstructor::MatrixConstructor()
  : R(0),
    rows(0),
    cols(0),
    cols_frozen(false),
    deg(0)
{
}

MatrixConstructor::MatrixConstructor(const FreeModule *target, int ncols)
  : R(target->get_ring()),
    rows(target),
    cols(0),
    cols_frozen(false),
    deg(0)
{
  cols = R->make_FreeModule(ncols);

  entries.reserve(ncols);
  for (int i=0; i<ncols; i++)
    entries.push_back(0);

  deg = R->degree_monoid()->make_one();
}

MatrixConstructor::MatrixConstructor(const FreeModule *target, 
				     const FreeModule *source, 
				     const int *deg0)
 : R(target->get_ring()),
   rows(target),
   cols(source),
   cols_frozen(true)
{
  entries.reserve(source->rank());
  for (int i=0; i<source->rank(); i++)
    entries.push_back(0);

  deg = (deg0 == 0 ? R->degree_monoid()->make_one() : R->degree_monoid()->make_new(deg0));
}

void MatrixConstructor::append(vec v) 
{
  assert(!cols_frozen);
  int *d = R->degree_monoid()->make_one();
  if (v != 0) R->vec_degree(rows,v, d);
  append(v,d);
  R->degree_monoid()->remove(d);

}
void MatrixConstructor::append(vec v, const int *deg0) 
{
  assert(!cols_frozen);
  entries.push_back(v); 
  FreeModule *mutable_cols = const_cast<FreeModule *>(cols);
  mutable_cols->append(deg0);
}

void MatrixConstructor::set_entry(int r, int c, ring_elem a)
{
  R->set_entry(entries[c], r, a);
}
void MatrixConstructor::set_column(int c, vec v)
{
  entries[c] = v;
}

void MatrixConstructor::compute_column_degrees()
 /* Takes into acount the matrix degree */
{
  assert(!cols_frozen);
  for (int i=0; i<cols->rank(); i++)
    compute_column_degree(i);
}

void MatrixConstructor::set_column_degree(int i, const int *deg0)
{
  assert(!cols_frozen);
  FreeModule *mutable_cols = const_cast<FreeModule *>(cols);
  mutable_cols->change_degree(i,deg0);
}

void MatrixConstructor::compute_column_degree(int i)
{

  assert(!cols_frozen);
  int *d = R->degree_monoid()->make_one();
  const vec v = entries[i];
  if (v != 0) R->vec_degree(rows, v, d);
  FreeModule *mutable_cols = const_cast<FreeModule *>(cols);
  mutable_cols->change_degree(i,d);
  R->degree_monoid()->remove(d);
}

void MatrixConstructor::set_matrix_degree(const int *deg0)
{
  deg = deg0;
}

Matrix * MatrixConstructor::to_matrix()
{
  //  if (!will_be_mutable && !cols->is_frozen)
  //    cols->freeze(hashval);
  return new Matrix(rows, cols, deg, entries);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
