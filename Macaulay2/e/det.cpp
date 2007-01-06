// Copyright 1996 Michael E. Stillman.

#include "det.hpp"
#include "text_io.hpp"

extern char system_interruptedFlag;
extern int gbTrace;

DetComputation::DetComputation(const Matrix *M0, int p0,
			       bool do_exterior0,
			       int strategy0)
  : R(M0->get_ring()),
    M(M0),
    done(false),
    p(p0),
    do_exterior(do_exterior0),
    strategy(strategy0),
    row_set(NULL),
    col_set(NULL),
    this_row(0),
    this_col(0),
    D(0)
{
  if (do_exterior)
    {
      F = M->rows()->exterior(p);
      FreeModule *G = M->cols()->exterior(p);
      int *deg = R->degree_monoid()->make_new(M->degree_shift());
      R->degree_monoid()->power(deg, p, deg);
      result = MatrixConstructor(F,G,deg);
      R->degree_monoid()->remove(deg);
    }
  else
    {
      F = R->make_FreeModule(1);
      result = MatrixConstructor(F,0);
    }

  // Handle trivial cases
  if (p < 0)
    {
      // In either case, want a zero matrix
      done = true;
      return;
    }
  if (p == 0)
    {
      // We want a single element which is '1'
      if (do_exterior)
	result.set_entry(0,0,R->one());
      else
	result.append(R->make_vec(0,R->one()));
      done = true;
      return;
    }
  if (p > M->n_rows() || p > M->n_cols())
    {
      // Zero matrix in either case
      done = true;
      return;
    }
  done = false;


  row_set = newarray_atomic(int,p);
  col_set = newarray_atomic(int,p);

  for (int i=0; i<p; i++) 
    {
      row_set[i] = i;
      col_set[i] = i;
    }

  D = newarray(ring_elem *,p);
  for (int i=0; i<p; i++)
    {
      D[i] = newarray(ring_elem,p);
      for (int j=0; j<p;j++) D[i][j] = ZERO_RINGELEM;
    }
}

DetComputation::~DetComputation()
{
  deletearray(row_set);
  deletearray(col_set);

  if (D)
    {
      for (int i=0; i<p; i++)
	deletearray(D[i]);
      deletearray(D);
    }
}

int DetComputation::step()
     // Compute one more determinant of size p.
     // increments I and/or J and updates 'dets', 'table'.
{
  if (done) return COMP_DONE;

  ring_elem r;

  if (strategy == DET_BAREISS)
    {
      get_minor(row_set,col_set,p,D);
      r = bareiss_det();
    }
  else
    r = calc_det(row_set, col_set, p);

  if (!R->is_zero(r))
    {
      if (do_exterior)
	result.set_entry(this_row,this_col,r);
      else
	result.append(R->make_vec(0,r));
    }
  else
    R->remove(r);

  this_row++;
  if (!comb::increment(p, M->n_rows(), row_set))
    {
      // Now we increment column
      if (!comb::increment(p, M->n_cols(), col_set))
	{
	  done = true;
	  return COMP_DONE;
	}
      // Now set the row set back to initial value
      this_col++;
      this_row = 0;
      for (int i=0; i<p; i++) row_set[i]=i;
    }
  return COMP_COMPUTING;
}

void DetComputation::clear()
{
  if (do_exterior) return;
  result = MatrixConstructor(F,0);
}

void DetComputation::set_next_minor(const int *rows, const int *cols)
{
  if (do_exterior) return;
  int i;
  if (rows != NULL && comb::valid_subset(p, M->n_rows(), rows))
    for (i=0; i<p; i++) row_set[i] = rows[i];
  else
    for (i=0; i<p; i++)	row_set[i] = i;

  if (cols != NULL && comb::valid_subset(p, M->n_cols(), cols))
    for (i=0; i<p; i++) col_set[i] = cols[i];
  else
    for (i=0; i<p; i++)	col_set[i] = i;
}

int DetComputation::calc(int nsteps)
{
  for (;;)
    {
      int r = step();
      if (gbTrace >= 3)
	emit_wrapped(".");
      if (r == COMP_DONE)
	return COMP_DONE;
      if (--nsteps == 0)
	return COMP_DONE_STEPS;
      if (system_interruptedFlag)
	return COMP_INTERRUPTED;
    }
}

void DetComputation::get_minor(int *r, int *c, int p0, ring_elem **D0)
{
  for (int i=0; i<p0; i++)
    for (int j=0; j<p0; j++)
      D0[i][j] = M->elem(r[i],c[j]);
}

bool DetComputation::get_pivot(ring_elem **D0, int r, ring_elem &pivot, int &pivot_col)
  // Get a non-zero column 0..r in the r th row.
{
  // MES: it would be worthwhile to find a good pivot.
  for (int c=0; c<=r; c++)
    if (!R->is_zero(D0[r][c]))
    {
      pivot_col = c;
      pivot = D0[r][c];
      return true;
    }
  return false;
}

ring_elem DetComputation::detmult(ring_elem f1, ring_elem g1,
				  ring_elem f2, ring_elem g2,
				  ring_elem d)
{
  ring_elem a = R->mult(f1,g1);
  ring_elem b = R->mult(f2,g2);
  R->subtract_to(a,b);
  if (!R->is_zero(d))
    {
      ring_elem tmp = R->divide(a,d); // exact division
      R->remove(a);
      a = tmp;
    }
  R->remove(g1);
  return a;
}

void DetComputation::gauss(ring_elem **D0, int i, int r, int pivot_col, ring_elem lastpivot)
{
  ring_elem f = D0[i][pivot_col];
  ring_elem pivot = D0[r][pivot_col];

  for (int c=0; c<pivot_col; c++)
    D0[i][c] = detmult(pivot,D0[i][c],f,D0[r][c],lastpivot);

  for (int c=pivot_col+1; c<=r; c++)
    D0[i][c-1] = detmult(pivot,D0[i][c],f,D0[r][c],lastpivot);

  R->remove(f);
}

ring_elem DetComputation::bareiss_det()
{
  // Computes the determinant of the p by p matrix D. (dense form).
  int sign = 1;
  int pivot_col;

  ring_elem pivot = R->from_int(0);
  ring_elem lastpivot = R->from_int(0);

  for (int r=p-1; r>=1; --r)
    {
      R->remove(lastpivot);
      lastpivot = pivot;
      if (!get_pivot(D, r, pivot, pivot_col)) // sets pivot_col and pivot
	{
	  // Remove the rest of D.
	  for (int i=0; i<=r; i++)
	    for (int j=0; j<=r; j++)
	      R->remove(D[i][j]);
	  R->remove(lastpivot);
	  return R->from_int(0);
	}
      for (int i=0; i<r; i++)
	gauss(D,i,r,pivot_col,lastpivot);

      if (((r + pivot_col) % 2) == 1)
	sign = -sign;  // MES: do I need to rethink this logic?

      for (int c=0; c<=r; c++)
	if (c != pivot_col)
	  R->remove(D[r][c]);
	else
	  D[r][c] = ZERO_RINGELEM;
    }

  R->remove(pivot);
  R->remove(lastpivot);
  ring_elem r = D[0][0];
  D[0][0] = ZERO_RINGELEM;

  if (sign < 0) R->negate_to(r);

  return r;
}
ring_elem DetComputation::calc_det(int *r, int *c, int p0)
     // Compute the determinant of the minor with rows r[0]..r[p0-1]
     // and columns c[0]..c[p0-1].
{
//  int found;
//  const ring_elem &result = lookup(r,c,p0,found);
//  if (found) return result;
  int i;
  if (p0 == 1) return M->elem(r[0],c[0]);
  ring_elem answer = R->from_int(0);

  int negate = 1;
  for (i=p0-1; i>=0; i--)
    {
      std::swap(c[i],c[p0-1]);
      negate = !negate;
      ring_elem g = M->elem(r[p0-1],c[p0-1]);
      if (R->is_zero(g)) 
	{
	  R->remove(g);
	  continue;
	}
      ring_elem h = calc_det(r,c,p0-1);
      ring_elem gh = R->mult(g,h);
      R->remove(g);
      R->remove(h);
      if (negate)
	R->subtract_to(answer, gh);
      else
	R->add_to(answer, gh);
    }
  
  // pulling out the columns has disordered c. Fix it.
  
  int temp = c[p0-1];
  for (i=p0-1; i>0; i--)
    c[i] = c[i-1];
  c[0] = temp;

  return answer;
}

Matrix *Matrix::exterior(int p,int strategy) const
{
  DetComputation *d = new DetComputation(this,p,1,strategy);
  d->calc(-1);
  Matrix *result = d->determinants();
  deleteitem(d);
  return result;
}

Matrix *Matrix::minors(int p,int strategy) const
{
  DetComputation *d = new DetComputation(this,p,0,strategy);
  d->calc(-1);
  Matrix *result = d->determinants();
  deleteitem(d);
  return result;
}

Matrix *Matrix::minors(int p,
		       int strategy, 
		       int n_to_compute, // -1 means all
		       M2_arrayint_OrNull first_row, // possibly NULL
		       M2_arrayint_OrNull first_col // possibly NULL
		       ) const
{
  if (first_row != 0 || first_col != 0)
    {
      // Make sure these are the correct size, and both are given
      if (first_row == 0 || first_row->len != p)
	{
	  ERROR("row index set inappropriate");
	  return 0;
	}
      if (first_col == 0 || first_col->len != p)
	{
	  ERROR("column index set inappropriate");
	  return 0;
	}
    }
  DetComputation *d = new DetComputation(this,p,0,strategy);
  if (first_row != 0 && first_col != 0)
    d->set_next_minor(first_row->array, first_col->array);
  d->calc(n_to_compute);
  Matrix *result = d->determinants();
  deleteitem(d);
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
