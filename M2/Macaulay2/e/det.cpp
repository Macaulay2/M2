// Copyright 1996 Michael E. Stillman.

#include "det.hpp"
#include "text_io.hpp"
#include "bin_io.hpp"
#include "serial.hpp"

extern int comp_printlevel;

DetComputation::DetComputation(const Matrix &M, int p,
			       bool do_exterior)
  : R(M.get_ring()),
    M(M),
    done(false),
    p(p),
    do_exterior(do_exterior),
    row_set(NULL),
    col_set(NULL),
    this_row(0),
    this_col(0)
{
  bump_up(R);

  if (do_exterior)
    {
      FreeModule *F = M.rows()->exterior(p);
      FreeModule *G = M.cols()->exterior(p);
      int *deg = R->degree_monoid()->make_new(M.degree_shift());
      R->degree_monoid()->power(deg, p, deg);
      result = Matrix(F,G,deg);
      R->degree_monoid()->remove(deg);
    }
  else
    {
      FreeModule *F = R->make_FreeModule(1);
      result = Matrix(F);
      // MES: do I need to bump down F??
    }

  if (do_trivial_case())
    {
      done = true;
      return;
    }

  row_set = new int[p];
  col_set = new int[p];

  for (int i=0; i<p; i++) 
    {
      row_set[i] = i;
      col_set[i] = i;
    }
}

DetComputation::~DetComputation()
{
  bump_down((Ring *)R);
  delete [] row_set;
  delete [] col_set;
}

void DetComputation::write_object(object_writer &o) const
{
  int i;
  o << class_id()
    << p
    << do_exterior
    << done;
  for (i=0; i<p; i++)
    o << row_set[i];
  for (i=0; i<p; i++)
    o << col_set[i];
  o << this_row << this_col << *M << *result;
}

int DetComputation::step()
     // Compute one more determinant of size p.
     // increments I and/or J and updates 'dets', 'table'.
{
  if (done) return COMP_DONE;

  ring_elem r = calc_det(row_set, col_set, p);

  if (!R->is_zero(r))
    {
      if (do_exterior)
	{
	  vec v = result.rows()->term(this_row,r);
	  result.rows()->add_to(result[this_col], v);
	}
      else
	result.append(result.rows()->term(0,r));
    }
  R->remove(r);

  this_row++;
  if (!comb::increment(p, M.n_rows(), row_set))
    {
      // Now we increment column
      if (!comb::increment(p, M.n_cols(), col_set))
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

bool DetComputation::do_trivial_case()
{
  if (p < 0)
    {
      // In either case, want a zero matrix
      return true;
    }
  else if (p == 0)
    {
      // I suppose we want a single element which is '1'?
      return true;
    }
  else if (p > M.n_rows() || p > M.n_cols())
    {
      // Zero matrix in either case
      return true;
    }
  return false;
}

void DetComputation::clear()
{
  if (do_exterior) return;
  result = Matrix(result.rows());
}

void DetComputation::set_next_minor(const int *rows, const int *cols)
{
  if (do_exterior) return;
  int i;
  if (rows != NULL && comb::valid_subset(p, M.n_rows(), rows))
    for (i=0; i<p; i++) row_set[i] = rows[i];
  else
    for (i=0; i<p; i++)	row_set[i] = i;

  if (cols != NULL && comb::valid_subset(p, M.n_cols(), cols))
    for (i=0; i<p; i++) col_set[i] = cols[i];
  else
    for (i=0; i<p; i++)	col_set[i] = i;
}

int DetComputation::calc(int nsteps)
{
  for (;;)
    {
      int result = step();
      if (comp_printlevel >= 3)
	emit_wrapped(".");
      if (result == COMP_DONE)
	return COMP_DONE;
      if (--nsteps == 0)
	return COMP_DONE_STEPS;
      system_spincursor();
      if (system_interrupted)
	return COMP_INTERRUPTED;
    }
}

ring_elem DetComputation::calc_det(int *r, int *c, int p)
     // Compute the determinant of the minor with rows r[0]..r[p-1]
     // and columns c[0]..c[p-1].
{
//  int found;
//  const ring_elem &result = lookup(r,c,p,found);
//  if (found) return result;
  int i;
  if (p == 1) return M.elem(r[0],c[0]);
  ring_elem result = R->from_int(0);

  int negate = 1;
  for (i=p-1; i>=0; i--)
    {
#if 1
      swap(c[i],c[p-1]);
#else
      int tmp = c[i];
      c[i] = c[p-1];
      c[p-1] = tmp;
#endif
      negate = !negate;
      ring_elem g = M.elem(r[p-1],c[p-1]);
      if (R->is_zero(g)) 
	{
	  R->remove(g);
	  continue;
	}
      ring_elem h = calc_det(r,c,p-1);
      ring_elem gh = R->mult(g,h);
      R->remove(g);
      R->remove(h);
      if (negate)
	R->subtract_to(result, gh);
      else
	R->add_to(result, gh);
    }
  
  // pulling out the columns has disordered c. Fix it.
  
  int temp = c[p-1];
  for (i=p-1; i>0; i--)
    c[i] = c[i-1];
  c[0] = temp;

  return result;
}
