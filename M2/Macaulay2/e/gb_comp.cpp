#include "gb_comp.hpp"

#include "gb.hpp"
#include "gbinhom.hpp"
#include "gbZZ.hpp"
#include "gauss.hpp"
#include "hermite.hpp"
#include "gbA.hpp"


ComputationOrNull *
Computation:: set_stop_conditions(M2_bool always_stop,
				  M2_bool stop_after_degree,
				  M2_arrayint degree_limit,
				  int basis_element_limit,
				  int syzygy_limit,
				  int pair_limit,
				  int codim_limit,
				  int subring_limit,
				  M2_bool just_min_gens,
				  M2_arrayint length_limit)
{
  _Stop.always_stop = always_stop;
  _Stop.stop_after_degree = stop_after_degree;
  _Stop.degree_limit = degree_limit;
  _Stop.basis_element_limit = basis_element_limit;
  _Stop.syzygy_limit = syzygy_limit;
  _Stop.pair_limit = pair_limit;
  _Stop.use_codim_limit = (codim_limit >= 0);
  _Stop.codim_limit = codim_limit;
  _Stop.subring_limit = subring_limit;
  _Stop.just_min_gens = just_min_gens;
  _Stop.length_limit = length_limit;
  return this;
}

Computation::Computation()
{
  set_stop_conditions(false,
		      false,
		      0,
		      -1,
		      -1,
		      -1,
		      -1,
		      -1,
		      false,
		      0);
}

ComputationOrNull *Computation::choose_gb(const Matrix *m,
				 M2_bool collect_syz,
				 int n_rows_to_keep,
				 M2_bool use_max_degree,
				 int max_degree,
				 int algorithm,
				 int strategy)
{
  const Ring *R = m->get_ring()->get_flattened_ring();

  bool is_graded = (R->is_graded() && m->is_homogeneous());
  bool ring_is_base = R->is_basic_ring();
  bool base_is_ZZ = R->Ncoeffs()->is_ZZ(); 
         // NOT QUITE!!  Need to know if it is ZZ or QQ
  bool base_is_field = !R->Ncoeffs()->is_ZZ();

  if (algorithm == 1)
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
  if (base_is_ZZ)
    {
      if (ring_is_base)
	{
#if 0
	  return HermiteComputation::create(m, 
					    collect_syz, 
					    collect_change, 
					    n_rows_to_keep);
#endif
	  return 0;
	}
      // Question: should we separate between the graded, nongraded versions?
#if 0
      return GBZZ::create(m, 
			  collect_syz, 
			  collect_change, 
			  n_rows_to_keep);
#endif
      return 0;
    }
  else
    {
      // Base is a field
      if (ring_is_base)
	{
#if 0	      
	  return GaussElimComputation::create(m, 
					      collect_syz, 
					      collect_change, 
					      n_rows_to_keep); 
	  // This should be fraction free
#endif
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
#if 0
      return GB_inhom_comp::create(m, 
				   collect_syz, 
				   collect_change, 
				   n_rows_to_keep,
				   stategy);
#endif
      return 0;
    }
}
ComputationOrNull *Computation::choose_res(const Matrix *m,
					   M2_bool resolve_cokernel,
					   int max_level,
					   M2_bool use_max_slanted_degree,
					   int max_slanted_degree,
					   int algorithm,
					   int strategy
					   )
{
#if 0
      // Resolution algorithms: todo.
      if (!resolve_cokernel)
	{ 
	  if (algorithm != 2 && algorithm != 3)
	    algorithm = 2;
	}	    
      switch (algorithm) {
      case 0: 
	return res2_comp::create(m, max_level, max_slanted_degree, strategy);
      case 1:
	return res_comp::create(m, max_level, strategy);
      case 2:
	return gbres_comp::create(m, max_level, max_slanted_degree,
				  !resolve_cokernel, strategy);
      case 3:
	return gbres_comp::create(m, max_level, max_slanted_degree,
				  !resolve_cokernel, strategy | USE_HILB);
      }
#endif
}


ComputationOrNull * Computation::force(const Matrix *m,
				       const Matrix *gb,
				       const Matrix *change)
{
  return GB_comp::create_forced(m,gb,change);
}

Computation::~Computation()
{
}

//extern gb_comp *make_EGB_comp(const Matrix *m, bool dosyz, int nsyz, int strategy);
#if 0
gb_comp *gb_comp::make(const Matrix *m, bool dosyz, int nsyz, int strategy)
{
  const Ring *R = m->get_ring();

  // Dispatch according to the kind of computation we are
  // asked to do.
  if ((R->n_vars() == 0 && R->is_field())
       || R->cast_to_GF())
    return new GaussElimComputation(m, dosyz, nsyz);

  if (R->is_ZZ()) // MES later: || R->is_pid())
    return new HermiteComputation(m, dosyz, nsyz);

  if (R->is_poly_ring() && R->Ncoeffs()->is_field())
    {
      if ((strategy & 3) == 3)
	{
	  return gbA::create(m, dosyz, nsyz, strategy, 0, 0);
	}
      if (R->is_graded() && m->is_homogeneous())
	{
	  if ((strategy & 3) == 1)
	    {
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

  if (R->is_poly_ring() && R->Ncoeffs()->is_ZZ())
    return new GBZZ_comp(m, dosyz, nsyz, strategy);

  if (R->is_poly_ring() && R->Ncoeffs()->is_pid())
    {
      ERROR("GB for polynomial rings over PID's not yet implemented");
      return 0;
    }

  ERROR("cannot compute Groebner bases or syzygies over this ring");
  return 0;
}

gb_comp *gb_comp::make(const Matrix *m, 
		       bool dosyz, 
		       int nsyz, 
		       const RingElement *hf, 
		       int strategy)
{
  const Ring *R = m->get_ring();

  if (dosyz)
    return make(m,dosyz,nsyz,strategy);

  if (R->is_field())
    {
      ERROR("GB for ring = field together with Hilbert function is not yet implemented");
      return 0;
    }
  
  if (R->is_ZZ()) // MES later: || R->is_pid())
    {
      ERROR("GB for Z, using Hilbert function, is not yet implemented");
      return 0;
    }

  if (R->is_poly_ring() && R->Ncoeffs()->is_field())
    {
      if (R->is_graded() && m->is_homogeneous())
	{
	  return new GB_comp(m, dosyz, nsyz, hf, strategy);
	}
      else
	{
	  ERROR("cannot use Hilbert function for an inhomogeneous GB");
	  return 0;
	}
    }
  
  if (R->is_poly_ring() && R->Ncoeffs()->is_pid())
    {
      ERROR("GB for polynomial rings over PID's not yet implemented");
      return 0;
    }
  
  ERROR("cannot compute Groebner bases or syzygies over this ring");
  return 0;
}

gb_comp *gb_comp::force(const Matrix *gens, 
			const Matrix *gb, 
			const Matrix *change, 
			const Matrix *syz)
{
  const Ring *R = gens->get_ring();
  if (R->is_poly_ring())
    {
      if (R->Ncoeffs()->is_field())
	return new GB_comp(gens, gb, change, syz);
      else if (R->Ncoeffs()->is_ZZ())
	return new GBZZ_comp(gens, gb, change, syz);
    }

  ERROR("Cannot create the desired forced Groebner basis");
  return 0;
}

int GBComputation::computation_is_complete() const
{
  // This handles everything but _Stop.always, _Stop.degree_limit
  if (_state == GB_COMP_DONE)
    return COMP_DONE;
  if (_Stop.basis_element_limit > 0 && _n_gb > _Stop.basis_element_limit) 
    return COMP_DONE_GB_LIMIT;
  if (_Stop.syzygy_limit > 0 && _n_syz > _Stop.syzygy_limit)
    return COMP_DONE_SYZ_LIMIT;
  if (_Stop.pair_limit > 0 && _n_pairs_computed > _Stop.pair_limit)
    return COMP_DONE_PAIR_LIMIT;
  if (_Stop.just_min_gens && _n_gens_left == 0)
    return COMP_DONE_MIN_GENS;
  if (_Stop.subring_limit > 0 && _n_subring > _Stop.subring_limit)
    return COMP_DONE_SUBRING_LIMIT;
  if (_Stop.use_codim_limit)
    {
      // Compute the codimension
      int c = 0;
      //int c = codim_of_lead_terms();
      if (c >= _Stop.codim_limit)
	return COMP_DONE_CODIM;
    }
  return COMP_COMPUTING;
}
#endif
