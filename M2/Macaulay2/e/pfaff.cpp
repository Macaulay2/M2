// Copyright 1996 Michael E. Stillman.

#include "pfaff.hpp"
#include "comb.hpp"
#include "interrupted.hpp"

PfaffianComputation::PfaffianComputation(const Matrix *M0, int p0)
    : R(M0->get_ring()), M(M0), p(p0), done(false), row_set(0)
{
  pfaffs = MatrixConstructor(R->make_FreeModule(1), 0);
  if (p == 0)
    {
      pfaffs.append(R->make_vec(0, R->one()));
      done = true;
      return;
    }
  if (p % 2 != 0 || p < 0 || (p > M->n_cols()) || (p > M->n_rows()))
    {
      done = true;
      return;
    }
  row_set = newarray_atomic(size_t, p);
  for (int i = 0; i < p; i++) row_set[i] = i;
}

PfaffianComputation::~PfaffianComputation() { freemem(row_set); }
int PfaffianComputation::step()
// Compute one more pfafferminant of size p.
// increments I and/or J and updates 'pfaffs', 'table'.
{
  if (done) return COMP_DONE;

  ring_elem r = calc_pfaff(row_set, p);
  if (!R->is_zero(r))
    pfaffs.append(R->make_vec(0, r));
  else
    R->remove(r);

  if (Subsets::increment(M->n_cols(), p, row_set)) return COMP_COMPUTING;

  // Otherwise, we are at the end.
  done = true;
  return COMP_DONE;
}

int PfaffianComputation::calc(int nsteps)
{
  for (;;)
    {
      int result = step();
      if (result == COMP_DONE) return COMP_DONE;
      if (--nsteps == 0) return COMP_DONE_STEPS;
      if (system_interrupted()) return COMP_INTERRUPTED;
    }
}

ring_elem PfaffianComputation::calc_pfaff(size_t *r, int p2)
// Compute the pfaffian of the (skew symmetric)
// minor with rows and columns r[0]..r[p2-1].
// assumption: p2 is an even number.
{
  if (p2 == 2) return M->elem(static_cast<int>(r[0]), static_cast<int>(r[1]));
  ring_elem result = R->from_long(0);

  bool negate = true;
  for (int i = p2 - 2; i >= 0; i--)
    {
      std::swap(r[i], r[p2 - 2]);
      negate = !negate;
      ring_elem g =
          M->elem(static_cast<int>(r[p2 - 2]), static_cast<int>(r[p2 - 1]));
      if (R->is_zero(g))
        {
          R->remove(g);
          continue;
        }
      ring_elem h = calc_pfaff(r, p2 - 2);
      ring_elem gh = R->mult(g, h);
      R->remove(g);
      R->remove(h);
      if (negate)
        R->subtract_to(result, gh);
      else
        R->add_to(result, gh);
    }

  // pulling out the columns has disordered r. Fix it.

  size_t temp = r[p2 - 2];
  for (size_t i = p2 - 2; i > 0; i--) r[i] = r[i - 1];
  r[0] = temp;

  return result;
}

Matrix *Matrix::pfaffians(int p) const
{
  if (get_ring()->get_precision() > 0)
    {
      ERROR("pfaffian computations over RR or CC not yet implemented");
      return 0;
    }
  PfaffianComputation d {this, p};
  d.calc();
  return d.pfaffians();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
