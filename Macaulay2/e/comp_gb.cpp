
#include "comp_gb.hpp"

#include "gb.hpp"
#include "gbinhom.hpp"
#if 0
#include "gbZZ.hpp"
#include "gauss.hpp"
#endif
#include "hermite.hpp"
#include "gbA.hpp"

Computation::~Computation()
{
}

ComputationOrNull *GBComputation::choose_gb(const Matrix *m,
					  M2_bool collect_syz,
					  int n_rows_to_keep,
					  M2_arrayint gb_degrees,
					  M2_bool use_max_degree,
					  int max_degree,
					  int algorithm,
					  int strategy)
{
  const Ring *R1 = m->get_ring();
  const PolynomialRing *R2 = R1->cast_to_PolynomialRing();

  if (R2 == 0)
    {
      // Look for the correct computation type here.
#warning "handle non polynomial rings"
      return 0;
    }
  const PolynomialRing *R = R2->get_flattened_ring();

  bool is_graded = (R->is_graded() && m->is_homogeneous());
  //bool ring_is_base = R->is_basic_ring();
  //bool base_is_ZZ = R->Ncoeffs()->is_ZZ(); 
#warning "NOT QUITE!!  Need to know if it is ZZ or QQ"
#warning "unused variable commented out"
  // bool base_is_field = !R->Ncoeffs()->is_ZZ();

  if (algorithm == 2)
    return gbA::create(m, 
		       collect_syz, 
		       n_rows_to_keep,
		       strategy,
		       use_max_degree,
		       max_degree);

  if (is_graded)
    return GB_comp::create(m, 
			   collect_syz, 
			   n_rows_to_keep,
			   strategy,
			   use_max_degree,
			   max_degree);
  
  return 0;
#if 0
  if (base_is_ZZ)
    {
      if (ring_is_base)
	{

	  return HermiteComputation::create(m, 
					    collect_syz, 
					    collect_change, 
					    n_rows_to_keep);
	  return 0;
	}
      // Question: should we separate between the graded, nongraded versions?
      return GBZZ::create(m, 
			  collect_syz, 
			  collect_change, 
			  n_rows_to_keep);
      return 0;
    }
  else
    {
      // Base is a field
      if (ring_is_base)
	{
	  return GaussElimComputation::create(m, 
					      collect_syz, 
					      collect_change, 
					      n_rows_to_keep); 
	  // This should be fraction free
	  return 0;
	}
      // Also allow the user to choose between them.
      if (is_graded)
	return GB_comp::create(m, 
			       collect_syz, 
			       n_rows_to_keep,
			       strategy,
			       use_max_degree,
			       max_degree);
      return GB_inhom_comp::create(m, 
				   collect_syz, 
				   collect_change, 
				   n_rows_to_keep,
				   stategy);
      return 0;
    }
#endif
}

ComputationOrNull * GBComputation::force(const Matrix *m,
					 const Matrix *gb,
					 const Matrix *change)
{
  return GB_comp::create_forced(m,gb,change);
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

