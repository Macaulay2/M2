#include "gb_comp.hpp"

#include "gb.hpp"
#include "gbinhom.hpp"
#include "gbZZ.hpp"
#include "gauss.hpp"
#include "hermite.hpp"

extern gb_comp *make_EGB_comp(const Matrix &m, bool dosyz, int nsyz, int strategy);


gb_comp *gb_comp::make(Matrix &m, bool dosyz, int nsyz, int strategy)
{
  const Ring *R = m.get_ring();

  // Dispatch according to the kind of computation we are
  // asked to do.
  if (R->n_vars() == 0 && R->is_field())
    return new GaussElimComputation(m, dosyz, nsyz);

  if (R->is_Z()) // MES later: || R->is_pid())
    return new HermiteComputation(m, dosyz, nsyz);

  if (R->is_poly_ring() && R->Ncoeffs()->is_field())
    {
      if ((strategy & 3) == 3)
	{
	  // This next inserts the computation onto the stack.
	  return make_EGB_comp(m, dosyz, nsyz, strategy);
	}
      if (R->is_graded() && m.is_homogeneous())
	{
	  if ((strategy & 3) == 1)
	    {
	      //gStack.insert(new NGB_comp(m, dosyz, nsyz));
	      return new GB_comp(m, dosyz, nsyz, strategy);
	    }
	  else if ((strategy & 3) == 2)
	    return new GBinhom_comp(m, dosyz, nsyz, strategy);
	  else 
	    return new GB_comp(m, dosyz, nsyz, strategy);
	}
      else
	return new GBinhom_comp(m, dosyz, nsyz, strategy);
    }

  if (R->is_poly_ring() && R->Ncoeffs()->is_Z())
    return new GBZZ_comp(m, dosyz, nsyz, strategy);

  if (R->is_poly_ring() && R->Ncoeffs()->is_pid())
    {
      gError << "GB for polynomial rings over PID's not yet implemented";
      return 0;
    }

  gError << "cannot compute Groebner bases or syzygies over this ring";
  return 0;
}

gb_comp *gb_comp::make(Matrix &m, bool dosyz, int nsyz, RingElement &hf, int strategy)
{
  const Ring *R = m.get_ring();

  if (dosyz)
    return make(m,dosyz,nsyz,strategy);

  if (R->is_field())
    {
      gError << "GB for ring = field together with Hilbert function is not yet implemented";
      return 0;
    }
  
  if (R->is_Z()) // MES later: || R->is_pid())
    {
      gError << "GB for Z, using Hilbert function, is not yet implemented";
      return 0;
    }

  if (R->is_poly_ring() && R->Ncoeffs()->is_field())
    {
      if (R->is_graded() && m.is_homogeneous())
	{
	  return new GB_comp(m, dosyz, nsyz, hf, strategy);
	}
      else
	{
	  gError << "cannot use Hilbert function for an inhomogeneous GB";
	  return 0;
	}
    }
  
  if (R->is_poly_ring() && R->Ncoeffs()->is_pid())
    {
      gError << "GB for polynomial rings over PID's not yet implemented";
      return 0;
    }
  
  gError << "cannot compute Groebner bases or syzygies over this ring";
  return 0;
}

gb_comp *gb_comp::force(Matrix &gens, Matrix &gb, Matrix &change, Matrix &syz)
{
  const Ring *R = gens.get_ring();
  if (R->is_poly_ring())
    {
      if (R->Ncoeffs()->is_field())
	return new GB_comp(gens, gb, change, syz);
      else if (R->Ncoeffs()->is_Z())
	return new GBZZ_comp(gens, gb, change, syz);
    }

  gError << "Cannot create the desired forced Groebner basis";
  return 0;
}
