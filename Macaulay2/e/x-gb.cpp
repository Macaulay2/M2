// Copyright 2002 Michael E. Stillman

#include "engine.h"
#include "hilb.hpp"
#include "comp-gb.hpp"
#include "comp-res.hpp"
#include "comp-gb-declared.hpp"
#include "text-io.hpp"
#include "sagbi.hpp"
#include "exceptions.hpp"
#include "gb-walk.hpp"

////////////////////////////////////
// new GB computations /////////////
////////////////////////////////////
EngineComputationOrNull *rawGB(const Matrix *m,
			       M2_bool collect_syz,
			       int n_rows_to_keep,
			       M2_arrayint gb_weights,
			       M2_bool use_max_degree,
			       int max_degree,
			       int algorithm,
			       int strategy,
			       int max_reduction_count) /* drg: connected rawGB */
{
  // Choose the correct computation here.
  try {
    clear_emit_size();
    EngineGBComputation::create(
	GBBComputation::choose_gb(
		 m,
		 collect_syz,
		 n_rows_to_keep,
		 gb_weights,
		 use_max_degree,
		 max_degree,
		 algorithm,
		 strategy,
		 max_reduction_count));
  } catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

EngineComputationOrNull *
rawGBSetHilbertFunction(EngineComputation *C,
			const RingElement *h)
{
  try {
    clear_emit_size();
    EngineGBComputation *G = C->cast_to_EngineGBComputation();
    if (G != 0)
      return G->set_hilbert_function(h);
    ERROR("computation type unknown or not implemented");
    return 0;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

void rawComputationSetStop(EngineComputation *G,
			   M2_bool always_stop,
			   M2_arrayint degree_limit,
			   int basis_element_limit,
			   int syzygy_limit,
			   int pair_limit,
			   int codim_limit,
			   int subring_limit,
			   M2_bool just_min_gens,
			   M2_arrayint length_limit)
/* LongPolynomial, Sort, Primary, Inhomogeneous, Homogeneous */
/* Res: SortStrategy, 0, 1, 2, 3 ?? */
{
  // No errors can surface here.
  clear_emit_size();
  G->set_stop_conditions(always_stop,
			 degree_limit,
			 basis_element_limit,
			 syzygy_limit,
			 pair_limit,
			 codim_limit,
			 subring_limit,
			 just_min_gens,
			 length_limit);
}

EngineComputationOrNull* 
rawStartEngineComputation(EngineComputation *C)
  /* start or continue the computation */
{
  try {
    clear_emit_size();
    C->start_computation();

    if (gbTrace == 15)
      {
	ComputationStatusCode ret = C->status();
	switch (ret) {
	case COMP_DONE_DEGREE_LIMIT:
	  emit_line("computation stopped at degree limit");
	  break;
	case COMP_DONE:
	  emit_line("computation of GB completed");
	  break;
	case COMP_DONE_PAIR_LIMIT:
	  emit_line("computation stopped at pair limit");
	  break;
	case COMP_NEED_RESIZE:
	case COMP_ERROR:
	case COMP_INTERRUPTED:
	case COMP_NOT_STARTED:
	case COMP_INITIAL_STOP:
	case COMP_DONE_LENGTH_LIMIT:
	case COMP_DONE_SYZYGY_LIMIT:
	case COMP_DONE_GB_LIMIT:
	case COMP_DONE_SYZ_LIMIT:
	case COMP_DONE_CODIM:
	case COMP_DONE_MIN_GENS:
	case COMP_DONE_STEPS:
	case COMP_DONE_SUBRING_LIMIT:
	case COMP_COMPUTING:
	case COMP_OVERFLOWED:
	  emit_line("computation stopped for some good reason");
	  break;
	default:
	  emit_line("incorrect status code encountered");
	  break;
	}
      }	
    return error() ? 0 : C;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }

}

enum ComputationStatusCode rawEngineStatus1(EngineComputation *C)
{
  return C->status();
}

long rawEngineStatus2(EngineComputation *C)
{
  return C->complete_thru_degree();
}

void rawShowEngineComputation(const EngineComputation *C)
{
  C->show();
}

const MatrixOrNull *rawEngineGBGetMatrix(EngineComputation *C)
  /* Get the minimal, auto-reduced GB of a GB computation.
     Each call to this will produce a different raw matrix */
{
  try {
    clear_emit_size();
    EngineGBComputation *G = C->cast_to_EngineGBComputation();
    if (G != 0)
      return G->get_GroebnerBasis()->get_gb();
    ERROR("computation type unknown or not implemented");
    return 0;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

const MatrixOrNull *rawEngineGBMinimalGenerators(EngineComputation *C)
  /* Yields a matrix whose columns form a minimal generating set
     for the ideal or submodule, as computed so far.  In the
     inhomogeneous case, this yields a generating set which is
     sometimes smaller than the entire Groebner basis. */
{
  try {
    clear_emit_size();
    EngineGBComputation *G = C->cast_to_EngineGBComputation();
    if (G != 0)
      return G->get_GroebnerBasis()->get_mingens();
    ERROR("computation type unknown or not implemented");
    return 0;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

const MatrixOrNull *rawEngineGBChangeOfBasis(EngineComputation *C)
  /* Yields the change of basis matrix from the Groebner basis to
     the original generators, at least if n_rows_to_keep was set
     when creating the GB computation.  This matrix, after the 
     computation has run to completion, should satisfy:
     (original matrix) = (GB matrix) * (change of basis matrix). */
{
  try {
    clear_emit_size();
    EngineGBComputation *G = C->cast_to_EngineGBComputation();
    if (G != 0)
      return G->get_GroebnerBasis()->get_change();
    ERROR("computation type unknown or not implemented");
    return 0;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

const MatrixOrNull *rawEngineGBGetLeadTerms(EngineComputation *C, int nparts)
{
  try {
    clear_emit_size();
    EngineGBComputation *G = C->cast_to_EngineGBComputation();
    if (G != 0)
      return G->get_GroebnerBasis()->get_initial(nparts);
    ERROR("computation type unknown or not implemented");
    return 0;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

const MatrixOrNull *rawEngineGBGetParallelLeadTerms(EngineComputation *C, M2_arrayint w)
{
  try {
    clear_emit_size();
    EngineGBComputation *G = C->cast_to_EngineGBComputation();
    if (G != 0)
      return G->get_GroebnerBasis()->get_parallel_lead_terms(w);
    ERROR("computation type unknown or not implemented");
    return 0;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

const MatrixOrNull *rawEngineGBSyzygies(EngineComputation *C)
  /* Yields a matrix containing the syzygies computed so far
     via the GB computation C, assuming that 'collect_syz' was
     set when the computation was created.  If 'n_rows_to_keep' was
     set to a non-negative integer, then only that many rows of each
     syzygy are kept. */
{
  try {
    clear_emit_size();
    EngineGBComputation *G = C->cast_to_EngineGBComputation();
    if (G != 0)
      return G->get_GroebnerBasis()->get_syzygies();
    ERROR("computation type unknown or not implemented");
    return 0;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

const MatrixOrNull *rawEngineGBMatrixRemainder(EngineComputation *C, 
					       const Matrix *m)
{
  try {
    clear_emit_size();
    EngineGBComputation *G = C->cast_to_EngineGBComputation();
    if (G != 0)
      return G->get_GroebnerBasis()->matrix_remainder(m);
    ERROR("computation type unknown or not implemented");
    return 0;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

void rawEngineGBMatrixDivMod(EngineComputation *C,
			     const Matrix *m,
			     MatrixOrNull **result_remainder,
			     MatrixOrNull **result_quotient
			     )
{
  try {
    clear_emit_size();
    EngineGBComputation *G = C->cast_to_EngineGBComputation();
    if (G != 0)
      G->get_GroebnerBasis()->matrix_lift(m, result_remainder, result_quotient);
    else ERROR("computation type unknown or not implemented");
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return;
  }
}

int rawEngineGBMatrixContains(EngineComputation *C, 
			      const Matrix *m)
{
  try {
    clear_emit_size();
    EngineGBComputation *G = C->cast_to_EngineGBComputation();
    if (G != 0)
      return G->get_GroebnerBasis()->contains(m);
    ERROR("computation type unknown or not implemented");
    return -2;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return -2;
  }
}

EngineComputationOrNull *
rawEngineGBDeclared(const Matrix *m, /* trimmed or minimal gens, may be the same as gb */
		    const Matrix *gb,
		    const Matrix *change, /* same number of columns as 'gb', if not 0 */
		    const Matrix *syz) /* possibly 0 too, otherwise same rows as change */
{
  try {
    return EngineGBComputation::create(GBDeclared::create(m,gb,change,syz));
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

EngineComputationOrNull *
rawMarkedEngineGB(const Matrix *leadterms,
		  const Matrix *m, /* trimmed or minimal gens, may be the same as gb */
		  const Matrix *gb,
		  const Matrix *change, /* same number of columns as 'gb', if not 0 */
		  const Matrix *syz) /* possibly 0 too, otherwise same rows as change */
{
  try {
    return EngineGBComputation::create(GBDeclared::create(leadterms,m,gb,change,syz));
  }
  catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
  }
}

EngineComputationOrNull *
rawEngineGroebnerWalk(const Matrix *gb,
		      const MonomialOrdering *order1)
{
  try {
    return EngineGBComputation::create(GBWalker::create(gb,order1));
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return NULL;
  }
}

M2_string rawEngineComputationToString(EngineComputation *C)
{
  buffer o;
  try {
    C->text_out(o);
    return o.to_string();
  }
  catch (exc::engine_error e) {
    o << "[unprintable computation]";
    return o.to_string();
  }
}

unsigned long rawEngineComputationHash(const Computation *C)
{
  return C->get_hash_value();
}

////////////////////////////////////
const RingElementOrNull * IM2_Matrix_Hilbert(const Matrix *M)
  /* This routine computes the numerator of the Hilbert series
     for coker leadterms(M), using the degrees of the rows of M. 
     NULL is returned if the ring is not appropriate for
     computing Hilbert series, or the computation was interrupted. */
{
     try {
	  return hilb_comp::hilbertNumerator(M);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

///////////////////////////////////////////////////////////////////////////////////
///////// The following will be reomoved once the new code is functional //////////
///////////////////////////////////////////////////////////////////////////////////
ComputationOrNull *IM2_GB_make(const Matrix *m,
			       M2_bool collect_syz,
			       int n_rows_to_keep,
			       M2_arrayint gb_weights,
			       M2_bool use_max_degree,
			       int max_degree,
			       int algorithm,
			       int strategy,
			       int max_reduction_count) /* drg: connected rawGB */
{
  // Choose the correct computation here.
     try {
	  clear_emit_size();
	  return GBComputation::choose_gb(
				  m,
				  collect_syz,
				  n_rows_to_keep,
				  gb_weights,
				  use_max_degree,
				  max_degree,
				  algorithm,
				  strategy,
				  max_reduction_count);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

ComputationOrNull *IM2_res_make(
           const Matrix *m,
	   M2_bool resolve_cokernel,
	   int max_level,
	   M2_bool use_max_slanted_degree,
	   int max_slanted_degree,
	   int algorithm,
	   int strategy
	   )
{
     try {
	  // Choose the correct computation here.
	  clear_emit_size();
	  return ResolutionComputation::choose_res(m,
				 resolve_cokernel,
				 max_level,
				 use_max_slanted_degree,
				 max_slanted_degree,
				 algorithm,
				 strategy);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

ComputationOrNull *
IM2_GB_set_hilbert_function(Computation *C,
			    const RingElement *h)
{
     try {
	  clear_emit_size();
	  GBComputation *G = C->cast_to_GBComputation();
	  if (G != 0)
	    return G->set_hilbert_function(h);
	  ERROR("computation type unknown or not implemented");
	  return 0;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

ComputationOrNull *
IM2_GB_force(const Matrix *m, /* trimmed or minimal gens, may be the same as gb */
	     const Matrix *gb,
	     const Matrix *change, /* same number of columns as 'gb', if not 0 */
	     const Matrix *syz) /* possibly 0 too, otherwise same rows as change */
{
     try {
	  return GBDeclared::create(m,gb,change,syz);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

ComputationOrNull *
rawMarkedGB(const Matrix *leadterms,
	     const Matrix *m, /* trimmed or minimal gens, may be the same as gb */
	     const Matrix *gb,
	     const Matrix *change, /* same number of columns as 'gb', if not 0 */
	     const Matrix *syz) /* possibly 0 too, otherwise same rows as change */
{
     try {
	  return GBDeclared::create(leadterms,m,gb,change,syz);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

ComputationOrNull *
rawGroebnerWalk(const Matrix *gb,
		const MonomialOrdering *order1)
{
     try {
       return GBWalker::create(gb,order1);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

ComputationOrNull* 
IM2_Computation_set_stop(Computation *G,
		M2_bool always_stop,
		M2_arrayint degree_limit,
		int basis_element_limit,
		int syzygy_limit,
		int pair_limit,
		int codim_limit,
		int subring_limit,
		M2_bool just_min_gens,
		M2_arrayint length_limit) /* TODO */
  /* LongPolynomial, Sort, Primary, Inhomogeneous, Homogeneous */
  /* Res: SortStrategy, 0, 1, 2, 3 ?? */
{
     try {
	  clear_emit_size();
	  return  G->set_stop_conditions(always_stop,
					 degree_limit,
					 basis_element_limit,
					 syzygy_limit,
					 pair_limit,
					 codim_limit,
					 subring_limit,
					 just_min_gens,
					 length_limit);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

ComputationOrNull* 
rawStartComputation(Computation *C)
  /* start or continue the computation */
{
     try {
	  clear_emit_size();
	  C->start_computation();

	  if (gbTrace == 15)
	    {
	      ComputationStatusCode ret = C->status();
	      switch (ret) {
	      case COMP_DONE_DEGREE_LIMIT:
		emit_line("computation stopped at degree limit");
		break;
	      case COMP_DONE:
		emit_line("computation of GB completed");
		break;
	      case COMP_DONE_PAIR_LIMIT:
		emit_line("computation stopped at pair limit");
		break;
	      case COMP_NEED_RESIZE:
	      case COMP_ERROR:
	      case COMP_INTERRUPTED:
	      case COMP_NOT_STARTED:
	      case COMP_INITIAL_STOP:
	      case COMP_DONE_LENGTH_LIMIT:
	      case COMP_DONE_SYZYGY_LIMIT:
	      case COMP_DONE_GB_LIMIT:
	      case COMP_DONE_SYZ_LIMIT:
	      case COMP_DONE_CODIM:
	      case COMP_DONE_MIN_GENS:
	      case COMP_DONE_STEPS:
	      case COMP_DONE_SUBRING_LIMIT:
	      case COMP_COMPUTING:
	      case COMP_OVERFLOWED:
		emit_line("computation stopped for some good reason");
		break;
	      default:
		emit_line("incorrect status code encountered");
		break;
	      }
	    }

	  return error() ? 0 : C;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

enum ComputationStatusCode rawStatus1(Computation *C)
{
  return C->status();
}

int rawStatus2(Computation *C)
{
  return C->complete_thru_degree();
}

void rawShowComputation(const Computation *C)
{
  C->show();
}

const MatrixOrNull *rawGBGetMatrix(Computation *C)
  /* Get the minimal, auto-reduced GB of a GB computation.
     Each call to this will produce a different raw matrix */
{
     try {
	  clear_emit_size();
	  GBComputation *G = C->cast_to_GBComputation();
	  if (G != 0)
	       return G->get_gb();
	  ERROR("computation type unknown or not implemented");
	  return 0;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull *rawGBMinimalGenerators(Computation *C)
  /* Yields a matrix whose columns form a minimal generating set
     for the ideal or submodule, as computed so far.  In the
     inhomogeneous case, this yields a generating set which is
     sometimes smaller than the entire Groebner basis. */
{
     try {
	  clear_emit_size();
	  GBComputation *G = C->cast_to_GBComputation();
	  if (G != 0)
	    return G->get_mingens();
	  ERROR("computation type unknown or not implemented");
	  return 0;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull *rawGBChangeOfBasis(Computation *C)
  /* Yields the change of basis matrix from the Groebner basis to
     the original generators, at least if n_rows_to_keep was set
     when creating the GB computation.  This matrix, after the 
     computation has run to completion, should satisfy:
     (original matrix) = (GB matrix) * (change of basis matrix). */
{
     try {
	  clear_emit_size();
	  GBComputation *G = C->cast_to_GBComputation();
	  if (G != 0)
	    return G->get_change();
	  ERROR("computation type unknown or not implemented");
	  return 0;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull *
rawGBGetLeadTerms(Computation *C, int nparts)
{
     try {
	  clear_emit_size();
	  GBComputation *G = C->cast_to_GBComputation();
	  if (G != 0)
	    return G->get_initial(nparts);
	  ERROR("computation type unknown or not implemented");
	  return 0;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull *
rawGBGetParallelLeadTerms(Computation *C, M2_arrayint w)
{
     try {
	  clear_emit_size();
	  GBComputation *G = C->cast_to_GBComputation();
	  if (G != 0)
	    return G->get_parallel_lead_terms(w);
	  ERROR("computation type unknown or not implemented");
	  return 0;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull *rawGBSyzygies(Computation *C)
  /* Yields a matrix containing the syzygies computed so far
     via the GB computation C, assuming that 'collect_syz' was
     set when the computation was created.  If 'n_rows_to_keep' was
     set to a non-negative integer, then only that many rows of each
     syzygy are kept. */
{
     try {
	  clear_emit_size();
	  GBComputation *G = C->cast_to_GBComputation();
	  if (G != 0)
	    return G->get_syzygies();
	  ERROR("computation type unknown or not implemented");
	  return 0;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull *
rawGBMatrixRemainder(Computation *C, 
		     const Matrix *m)
{
     try {
	  clear_emit_size();
	  GBComputation *G = C->cast_to_GBComputation();
	  if (G != 0)
	    return G->matrix_remainder(m);
	  ERROR("computation type unknown or not implemented");
	  return 0;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

void IM2_GB_matrix_lift(Computation *C,
			const Matrix *m,
			MatrixOrNull **result_remainder,
			MatrixOrNull **result_quotient
			)
{
     try {
	  clear_emit_size();
	  GBComputation *G = C->cast_to_GBComputation();
	  if (G != 0)
	    G->matrix_lift(m, result_remainder, result_quotient);
	  else ERROR("computation type unknown or not implemented");
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return;
     }
}

int 
IM2_GB_contains(Computation *C, 
		const Matrix *m)
{
     try {
	  clear_emit_size();
	  GBComputation *G = C->cast_to_GBComputation();
	  if (G != 0)
	    return G->contains(m);
	  ERROR("computation type unknown or not implemented");
	  return -2;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return -2;
     }
}

const MatrixOrNull *
rawResolutionGetMatrix(Computation *C, 
		       int level)
{
     try {
	  clear_emit_size();
	  ResolutionComputation *G = C->cast_to_ResolutionComputation();
	  if (G != 0)
	    return G->get_matrix(level);
	  ERROR("expected resolution computation type");
	  return 0;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const FreeModuleOrNull *
rawResolutionGetFree(Computation *C, 
		     int level)
{
     try {
	  clear_emit_size();
	  ResolutionComputation *G = C->cast_to_ResolutionComputation();
	  if (G != 0)
	    return G->get_free(level);
	  ERROR("expected resolution computation type");
	  return 0;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

int IM2_Resolution_status(Computation *C,
			  int * complete_up_through_this_degree,
			  int * complete_up_through_this_level)
{
#ifdef DEVELOPMENT
#warning "IM2_Resolution_status to be written"
#endif
  ERROR("not re-implemented yet");
  return -1;
}


enum ComputationStatusCode
IM2_Resolution_status_level(Computation *C, 
		    int level, 
		    M2_bool minimize,
		    int * complete_up_through_this_degree)
{
#ifdef DEVELOPMENT
#warning "IM2_Resolution_status to be written"
#endif
  ERROR("not re-implemented yet");
  return COMP_ERROR;
#if 0
//   ResolutionComputation *G = C->cast_to_ResolutionComputation();
//   if (G != 0)
//     return G->status_level(level, complete_up_through_this_degree);
//   ERROR("expected resolution computation type");
//   return 0;
#endif
}

M2_arrayint_OrNull
rawResolutionBetti(Computation *C,
	     int type)
  /* see engine.h for description of what 'type' should be */
{
     try {
	  ResolutionComputation *G = C->cast_to_ResolutionComputation();
	  if (G != 0)
	    return G->get_betti(type);
	  ERROR("expected resolution computation type");
	  return NULL;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

M2_string
IM2_GB_to_string(Computation *C)
  /* TODO */
{
     buffer o;
     try {
	  C->text_out(o);
	  return o.to_string();
     }
     catch (exc::engine_error e) {
	  o << "[unprintable gb]";
	  return o.to_string();
     }
}

unsigned long IM2_GB_hash(const Computation *C)
{
  return C->get_hash_value();
}

MatrixOrNull * rawSubduction(const Matrix *M,
			     const RingMap *F,
			     Computation *C)
{
     try {
	  GBComputation *G = C->cast_to_GBComputation();
	  if (G == 0)
	    {
	      ERROR("expected a Groebner basis computation");
	      return 0;
	    }
	  return sagbi::subduct(M,F,G);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
