// Copyright 1996 Michael E. Stillman.

#include "pfaff.hpp"

PfaffianComputation::PfaffianComputation(const Matrix *M0, int p0)
  : R(M0->get_ring()),
    M(M0),
    pfaffs(new Matrix(R->make_FreeModule(1))),
    p(p0),
    I(0),
    row_set(p0)
{
  endI = comb::binom(M->n_rows(), p);
}

PfaffianComputation::~PfaffianComputation()
{
}

int PfaffianComputation::step()
     // Compute one more pfafferminant of size p.
     // increments I and/or J and updates 'pfaffs', 'table'.
{
  if (I == endI) return COMP_DONE;
  comb::decode(I++, row_set.raw(), p);
  ring_elem r = calc_pfaff(row_set.raw(), p);
  if (!R->is_zero(r))
    pfaffs->append(pfaffs->rows()->raw_term(r,0));
  else
    R->remove(r);
  return COMP_COMPUTING;
}

int PfaffianComputation::calc(int nsteps)
{
  for (;;)
    {
      int result = step();
      if (result == COMP_DONE)
	return COMP_DONE;
      if (--nsteps == 0)
	return COMP_DONE_STEPS;
      if (system_interrupted)
	return COMP_INTERRUPTED;
      
    }
}

ring_elem PfaffianComputation::calc_pfaff(int *r, int p2)
     // Compute the pfaffian of the (skew symmetric) 
     // minor with rows and columns r[0]..r[p2-1].
     // assumption: p2 is an even number.
{
//  int found;
//  const ring_elem &result = lookup(r,p2,found);
//  if (found) return result;
  int i;
  if (p2 == 2) return M->elem(r[0],r[1]);
  ring_elem result = R->from_int(0);

  int negate = 1;
  for (i=p2-2; i>=0; i--)
    {
#if 0
      int tmp = r[i];
      r[i] = r[p2-2];
      r[p2-2] = tmp;
#else
      swap(r[i],r[p2-2]);
#endif
      negate = !negate;
      ring_elem g = M->elem(r[p2-2],r[p2-1]);
      if (R->is_zero(g)) 
	{
	  R->remove(g);
	  continue;
	}
      ring_elem h = calc_pfaff(r,p2-2);
      ring_elem gh = R->mult(g,h);
      R->remove(g);
      R->remove(h);
      if (negate)
	R->subtract_to(result, gh);
      else
	R->add_to(result, gh);
    }
  
  // pulling out the columns has disordered r. Fix it.
  
  int temp = r[p2-2];
  for (i=p2-2; i>0; i--)
    r[i] = r[i-1];
  r[0] = temp;

  return result;
}

