// Copyright 1996 Michael E. Stillman.

#include "pfaff.hpp"

PfaffianComputation::PfaffianComputation(const Matrix &M, int p)
  : R(M.Ring_of()),
    M(M),
    pfaffs(new FreeModule(R,1)),
    p(p),
    I(0),
    row_set(p)
{
  bump_up((Ring *)R);
  bump_up((FreeModule *) pfaffs.rows());
  endI = comb::binom(M.n_rows(), p);
}

PfaffianComputation::~PfaffianComputation()
{
  bump_down((Ring *)R);
  bump_down((FreeModule *) pfaffs.rows());
}

int PfaffianComputation::step()
     // Compute one more pfafferminant of size p.
     // increments I and/or J and updates 'pfaffs', 'table'.
{
  if (I == endI) return COMP_DONE;
  comb::decode(I++, row_set.raw(), p);
  ring_elem r = calc_pfaff(row_set.raw(), p);
  if (!R->is_zero(r))
    pfaffs.append(pfaffs.rows()->term(0,r));

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

ring_elem PfaffianComputation::calc_pfaff(int *r, int p)
     // Compute the pfaffian of the (skew symmetric) 
     // minor with rows and columns r[0]..r[p-1].
     // assumption: p is an even number.
{
//  int found;
//  const ring_elem &result = lookup(r,p,found);
//  if (found) return result;
  int i;
  if (p == 2) return M.elem(r[0],r[1]);
  ring_elem result = R->from_int(0);

  int negate = 1;
  for (i=p-2; i>=0; i--)
    {
      swap(r[i],r[p-2]);
      negate = !negate;
      ring_elem g = M.elem(r[p-2],r[p-1]);
      if (R->is_zero(g)) 
	{
	  R->remove(g);
	  continue;
	}
      ring_elem h = calc_pfaff(r,p-2);
      ring_elem gh = R->mult(g,h);
      R->remove(g);
      R->remove(h);
      if (negate)
	R->subtract_to(result, gh);
      else
	R->add_to(result, gh);
    }
  
  // pulling out the columns has disordered r. Fix it.
  
  int temp = r[p-2];
  for (i=p-2; i>0; i--)
    r[i] = r[i-1];
  r[0] = temp;

  return result;
}

