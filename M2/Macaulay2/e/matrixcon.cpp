#include "matrixcon.hpp"
#include "matrix.hpp"

MatrixConstructor::MatrixConstructor()
  : R(0),
    rows(0),
    cols(0),
    deg(0),
    will_be_mutable(true)
{
}

MatrixConstructor::MatrixConstructor(const FreeModule *target, int ncols, bool is_mutable)
  : R(target->get_ring()),
    rows(target),
    cols(0),
    deg(0),
    will_be_mutable(is_mutable)
{
  cols = R->make_FreeModule(ncols); // MAKE THIS UNFROZEN...

  entries.reserve(ncols);
  for (int i=0; i<ncols; i++)
    entries.push_back(0);

  deg = R->degree_monoid()->make_one();
}

MatrixConstructor::MatrixConstructor(const FreeModule *target, const FreeModule *source, 
				     bool is_mutable, const int *deg0)
 : R(target->get_ring()),
   rows(target),
   cols(source),
   will_be_mutable(is_mutable)
{
  entries.reserve(source->rank());
  for (int i=0; i<source->rank(); i++)
    entries.push_back(0);

  deg = (deg0 == 0 ? R->degree_monoid()->make_one() : R->degree_monoid()->make_new(deg0));
}

void MatrixConstructor::append(vec v) 
{
  //  if (cols->is_immutable()) return; // HOW BEST TO HANDLE THIS??
  int *d = R->degree_monoid()->make_one();
  if (v != 0) rows->degree(v, d);
  append(v,d);
  R->degree_monoid()->remove(d);

}
void MatrixConstructor::append(vec v, const int *deg0) 
{
  //  if (cols->is_immutable()) return; // HOW BEST TO HANDLE THIS??
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
  // DON'T CHANGE IT IN THE IMMUTABLE SITUATION!!
  for (int i=0; i<cols->rank(); i++)
    compute_column_degree(i);
}

void MatrixConstructor::set_column_degrees(const FreeModule *source)
{
  cols = source;
}

void MatrixConstructor::set_column_degree(int i, const int *deg0)
{
  FreeModule *mutable_cols = const_cast<FreeModule *>(cols);
  mutable_cols->change_degree(i,deg0);
}

void MatrixConstructor::compute_column_degree(int i)
{

  int *d = R->degree_monoid()->make_one();
  const vec v = entries[i];
  if (v != 0) rows->degree(v, d);
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
  return new Matrix(rows, cols, deg, entries, will_be_mutable);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
