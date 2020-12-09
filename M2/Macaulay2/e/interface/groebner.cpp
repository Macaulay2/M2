// Copyright 2002 Michael E. Stillman

#include "interface/groebner.h"

#include <iostream>
#include <limits>
#include <sstream>

#include "hilb.hpp"
#include "comp-gb.hpp"
#include "comp-res.hpp"
#include "comp-gb-declared.hpp"
#include "text-io.hpp"
#include "sagbi.hpp"
#include "exceptions.hpp"
#include "gb-walk.hpp"
#include "relem.hpp"

#include "poly.hpp"
#include "interrupted.hpp"
#include "f4/res-f4-computation.hpp"

class FreeModule;
struct MonomialOrdering;
struct MutableMatrix;
struct RingMap;

bool warning_given_for_gb_or_res_over_RR_or_CC = false;

void test_over_RR_or_CC(const Ring *R)
{
  // const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (R->get_precision() > 0)
    {
      if (!warning_given_for_gb_or_res_over_RR_or_CC)
        {
          buffer o;
          o << "-- warning: experimental computation over inexact field begun"
            << newline;
          o << "--          results not reliable (one warning given per "
               "session)"
            << newline;
          emit(o.str());
          warning_given_for_gb_or_res_over_RR_or_CC = true;
        }
    }
}
////////////////////////////////////
// new GB computations /////////////
////////////////////////////////////
EngineComputationOrNull *rawGB(
    const Matrix *m,
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
  try
    {
      clear_emit_size();
      return EngineGBComputation::create(
          GBBComputation::choose_gb(m,
                                    collect_syz,
                                    n_rows_to_keep,
                                    gb_weights,
                                    use_max_degree,
                                    max_degree,
                                    algorithm,
                                    strategy,
                                    max_reduction_count));
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

EngineComputationOrNull *rawGBSetHilbertFunction(EngineComputation *C,
                                                 const RingElement *h)
{
  try
    {
      clear_emit_size();
      EngineGBComputation *G = C->cast_to_EngineGBComputation();
      if (G != 0) return G->set_hilbert_function(h);
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
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

EngineComputationOrNull *rawStartEngineComputation(EngineComputation *C)
/* start or continue the computation */
{
  try
    {
      clear_emit_size();
      C->start_computation();

      if (M2_gbTrace == 15)
        {
          ComputationStatusCode ret = C->status();
          switch (ret)
            {
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
  } catch (const exc::engine_error& e)
    {
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

void rawShowEngineComputation(const EngineComputation *C) { C->show(); }
const Matrix /* or null */ *rawEngineGBGetMatrix(EngineComputation *C)
/* Get the minimal, auto-reduced GB of a GB computation.
   Each call to this will produce a different raw matrix */
{
  try
    {
      clear_emit_size();
      EngineGBComputation *G = C->cast_to_EngineGBComputation();
      if (G != 0) return G->get_GroebnerBasis()->get_gb();
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *rawEngineGBMinimalGenerators(EngineComputation *C)
/* Yields a matrix whose columns form a minimal generating set
   for the ideal or submodule, as computed so far.  In the
   inhomogeneous case, this yields a generating set which is
   sometimes smaller than the entire Groebner basis. */
{
  try
    {
      clear_emit_size();
      EngineGBComputation *G = C->cast_to_EngineGBComputation();
      if (G != 0) return G->get_GroebnerBasis()->get_mingens();
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *rawEngineGBChangeOfBasis(EngineComputation *C)
/* Yields the change of basis matrix from the Groebner basis to
   the original generators, at least if n_rows_to_keep was set
   when creating the GB computation.  This matrix, after the
   computation has run to completion, should satisfy:
   (original matrix) = (GB matrix) * (change of basis matrix). */
{
  try
    {
      clear_emit_size();
      EngineGBComputation *G = C->cast_to_EngineGBComputation();
      if (G != 0) return G->get_GroebnerBasis()->get_change();
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *rawEngineGBGetLeadTerms(EngineComputation *C,
                                                    int nparts)
{
  try
    {
      clear_emit_size();
      EngineGBComputation *G = C->cast_to_EngineGBComputation();
      if (G != 0) return G->get_GroebnerBasis()->get_initial(nparts);
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *rawEngineGBGetParallelLeadTerms(
    EngineComputation *C,
    M2_arrayint w)
{
  try
    {
      clear_emit_size();
      EngineGBComputation *G = C->cast_to_EngineGBComputation();
      if (G != 0) return G->get_GroebnerBasis()->get_parallel_lead_terms(w);
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *rawEngineGBSyzygies(EngineComputation *C)
/* Yields a matrix containing the syzygies computed so far
   via the GB computation C, assuming that 'collect_syz' was
   set when the computation was created.  If 'n_rows_to_keep' was
   set to a non-negative integer, then only that many rows of each
   syzygy are kept. */
{
  try
    {
      clear_emit_size();
      EngineGBComputation *G = C->cast_to_EngineGBComputation();
      if (G != 0) return G->get_GroebnerBasis()->get_syzygies();
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *rawEngineGBMatrixRemainder(EngineComputation *C,
                                                       const Matrix *m)
{
  try
    {
      clear_emit_size();
      EngineGBComputation *G = C->cast_to_EngineGBComputation();
      if (G != 0) return G->get_GroebnerBasis()->matrix_remainder(m);
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

void rawEngineGBMatrixDivMod(EngineComputation *C,
                             const Matrix *m,
                             const Matrix /* or null */ **result_remainder,
                             const Matrix /* or null */ **result_quotient)
{
  try
    {
      clear_emit_size();
      EngineGBComputation *G = C->cast_to_EngineGBComputation();
      if (G != 0)
        G->get_GroebnerBasis()->matrix_lift(
            m, result_remainder, result_quotient);
      else
        ERROR("computation type unknown or not implemented");
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return;
  }
}

int rawEngineGBMatrixContains(EngineComputation *C, const Matrix *m)
{
  try
    {
      clear_emit_size();
      EngineGBComputation *G = C->cast_to_EngineGBComputation();
      if (G != 0) return G->get_GroebnerBasis()->contains(m);
      ERROR("computation type unknown or not implemented");
      return -2;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return -2;
  }
}

EngineComputationOrNull *rawEngineGBDeclared(
    const Matrix *m, /* trimmed or minimal gens, may be the same as gb */
    const Matrix *gb,
    const Matrix *change, /* same number of columns as 'gb', if not 0 */
    const Matrix *syz)    /* possibly 0 too, otherwise same rows as change */
{
  try
    {
      return EngineGBComputation::create(
          GBDeclared::create(m, gb, change, syz));
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

EngineComputationOrNull *rawMarkedEngineGB(
    const Matrix *leadterms,
    const Matrix *m, /* trimmed or minimal gens, may be the same as gb */
    const Matrix *gb,
    const Matrix *change, /* same number of columns as 'gb', if not 0 */
    const Matrix *syz)    /* possibly 0 too, otherwise same rows as change */
{
  try
    {
      return EngineGBComputation::create(
          GBDeclared::create(leadterms, m, gb, change, syz));
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

EngineComputationOrNull *rawEngineGroebnerWalk(const Matrix *gb,
                                               const MonomialOrdering *order1)
{
  try
    {
      return EngineGBComputation::create(GBWalker::create(gb, order1));
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

M2_string rawEngineComputationToString(EngineComputation *C)
{
  buffer o;
  try
    {
      C->text_out(o);
      return o.to_string();
  } catch (const exc::engine_error& e)
    {
      o << "[unprintable computation]";
      return o.to_string();
  }
}

unsigned long rawEngineComputationHash(const Computation *C)
{
  return C->hash();
}

////////////////////////////////////
const RingElement /* or null */ *IM2_Matrix_Hilbert(const Matrix *M)
/* This routine computes the numerator of the Hilbert series
   for coker leadterms(M), using the degrees of the rows of M.
   NULL is returned if the ring is not appropriate for
   computing Hilbert series, or the computation was interrupted. */
{
  try
    {
      return hilb_comp::hilbertNumerator(M);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

///////////////////////////////////////////////////////////////////////////////////
///////// The following will be reomoved once the new code is functional
/////////////
///////////////////////////////////////////////////////////////////////////////////
Computation /* or null */ *IM2_GB_make(
    const Matrix *m,
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
  try
    {
      test_over_RR_or_CC(m->get_ring());
      clear_emit_size();
      return GBComputation::choose_gb(m,
                                      collect_syz,
                                      n_rows_to_keep,
                                      gb_weights,
                                      use_max_degree,
                                      max_degree,
                                      algorithm,
                                      strategy,
                                      max_reduction_count);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

Computation /* or null */ *IM2_res_make(const Matrix *m,
                                        M2_bool resolve_cokernel,
                                        int max_level,
                                        M2_bool use_max_slanted_degree,
                                        int max_slanted_degree,
                                        int algorithm,
                                        int strategy)
{
  try
    {
      test_over_RR_or_CC(m->get_ring());
      // Choose the correct computation here.
      clear_emit_size();
      return ResolutionComputation::choose_res(m,
                                               resolve_cokernel,
                                               max_level,
                                               use_max_slanted_degree,
                                               max_slanted_degree,
                                               algorithm,
                                               strategy);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

Computation /* or null */ *IM2_GB_set_hilbert_function(Computation *C,
                                                       const RingElement *h)
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G->get_ring()->get_degree_ring() != h->get_ring())
        {
          ERROR("expected Hilbert function hint to be in correct degree ring");
          return 0;
        }
      if (G != 0) return G->set_hilbert_function(h);
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

Computation /* or null */ *IM2_GB_force(
    const Matrix *m, /* trimmed or minimal gens, may be the same as gb */
    const Matrix *gb,
    const Matrix *change, /* same number of columns as 'gb', if not 0 */
    const Matrix *syz)    /* possibly 0 too, otherwise same rows as change */
{
  test_over_RR_or_CC(m->get_ring());
  try
    {
      return GBDeclared::create(m, gb, change, syz);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

Computation /* or null */ *rawMarkedGB(
    const Matrix *leadterms,
    const Matrix *m, /* trimmed or minimal gens, may be the same as gb */
    const Matrix *gb,
    const Matrix *change, /* same number of columns as 'gb', if not 0 */
    const Matrix *syz)    /* possibly 0 too, otherwise same rows as change */
{
  test_over_RR_or_CC(m->get_ring());
  try
    {
      return GBDeclared::create(leadterms, m, gb, change, syz);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

Computation /* or null */ *rawGroebnerWalk(const Matrix *gb,
                                           const MonomialOrdering *order1)
{
  try
    {
      return GBWalker::create(gb, order1);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

Computation /* or null */ *IM2_Computation_set_stop(
    Computation *G,
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
  try
    {
      clear_emit_size();
      return G->set_stop_conditions(always_stop,
                                    degree_limit,
                                    basis_element_limit,
                                    syzygy_limit,
                                    pair_limit,
                                    codim_limit,
                                    subring_limit,
                                    just_min_gens,
                                    length_limit);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

Computation /* or null */ *rawStartComputation(Computation *C)
/* start or continue the computation */
{
  try
    {
      clear_emit_size();
      C->start_computation();

      if (M2_gbTrace == 15)
        {
          ComputationStatusCode ret = C->status();
          switch (ret)
            {
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
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

enum ComputationStatusCode rawStatus1(Computation *C) { return C->status(); }
int rawStatus2(Computation *C) { return C->complete_thru_degree(); }
void rawShowComputation(const Computation *C) { C->show(); }
const Matrix /* or null */ *rawGBGetMatrix(Computation *C)
/* Get the minimal, auto-reduced GB of a GB computation.
   Each call to this will produce a different raw matrix */
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G != 0) return G->get_gb();
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *rawGBMinimalGenerators(Computation *C)
/* Yields a matrix whose columns form a minimal generating set
   for the ideal or submodule, as computed so far.  In the
   inhomogeneous case, this yields a generating set which is
   sometimes smaller than the entire Groebner basis. */
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G != 0) return G->get_mingens();
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *rawGBChangeOfBasis(Computation *C)
/* Yields the change of basis matrix from the Groebner basis to
   the original generators, at least if n_rows_to_keep was set
   when creating the GB computation.  This matrix, after the
   computation has run to completion, should satisfy:
   (original matrix) = (GB matrix) * (change of basis matrix). */
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G != 0) return G->get_change();
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *rawGBGetLeadTerms(Computation *C, int nparts)
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G != 0) return G->get_initial(nparts);
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *rawGBGetParallelLeadTerms(Computation *C,
                                                      M2_arrayint w)
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G != 0) return G->get_parallel_lead_terms(w);
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *rawGBSyzygies(Computation *C)
/* Yields a matrix containing the syzygies computed so far
   via the GB computation C, assuming that 'collect_syz' was
   set when the computation was created.  If 'n_rows_to_keep' was
   set to a non-negative integer, then only that many rows of each
   syzygy are kept. */
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G != 0) return G->get_syzygies();
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const Matrix /* or null */ *rawGBMatrixRemainder(Computation *C,
                                                 const Matrix *m)
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G != 0) return G->matrix_remainder(m);
      ERROR("computation type unknown or not implemented");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

M2_bool IM2_GB_matrix_lift(Computation *C,
                           const Matrix *m,
                           const Matrix /* or null */ **result_remainder,
                           const Matrix /* or null */ **result_quotient)
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G != 0)
        return G->matrix_lift(m, result_remainder, result_quotient);
      else
        ERROR("computation type unknown or not implemented");
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
  }
  return false;
}

int IM2_GB_contains(Computation *C, const Matrix *m)
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G != 0) return G->contains(m);
      ERROR("computation type unknown or not implemented");
      return -2;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return -2;
  }
}

const Matrix /* or null */ *rawResolutionGetMatrix(Computation *C, int level)
{
  try
    {
      clear_emit_size();
      ResolutionComputation *G = C->cast_to_ResolutionComputation();
      if (G != 0) return G->get_matrix(level);
      ERROR("expected resolution computation type");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

MutableMatrix /* or null */ *rawResolutionGetMatrix2(Computation *C,
                                                     int level,
                                                     int degree)
{
  try
    {
      clear_emit_size();
      ResolutionComputation *G = C->cast_to_ResolutionComputation();
      if (G != 0) return G->get_matrix(level, degree);
      ERROR("expected resolution computation type");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

MutableMatrix /* or null */ *rawResolutionGetMutableMatrixB(Computation *C,
                                                            const Ring *R,
                                                            int level)
// Perhaps a HACK that might change.
// First: C must be a nonminimal res computation, over QQ M.
// Second: R must be a polynomial ring with the same monoid M as C's,
//  and the coefficient ring must be either RR, or ZZ/p, where p is the (a)
//  prime being used in the computaiton.
{
  try
    {
      clear_emit_size();
      F4ResComputation *G = dynamic_cast<F4ResComputation *>(C);
      if (G != 0) return G->get_mutable_matrix(R, level);
      ERROR("expected fast nonminimal resolution computation type");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

MutableMatrix /* or null */ *rawResolutionGetMutableMatrix2B(
    Computation *C,
    const Ring *KK,  // should be RR, or a finite field used in C.
    int level,
    int degree)
{
  try
    {
      clear_emit_size();
      F4ResComputation *G = dynamic_cast<F4ResComputation *>(C);
      if (G != 0) return G->get_mutable_matrix(KK, level, degree);
      ERROR("expected fast nonminimal resolution computation type");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

const FreeModule /* or null */ *rawResolutionGetFree(Computation *C, int level)
{
  try
    {
      clear_emit_size();
      ResolutionComputation *G = C->cast_to_ResolutionComputation();
      if (G != 0) return G->get_free(level);
      ERROR("expected resolution computation type");
      return 0;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

int IM2_Resolution_status(Computation *C,
                          int *complete_up_through_this_degree,
                          int *complete_up_through_this_level)
{
#ifdef DEVELOPMENT
#warning "IM2_Resolution_status to be written"
#endif
  ERROR("not re-implemented yet");
  return -1;
}

enum ComputationStatusCode IM2_Resolution_status_level(
    Computation *C,
    int level,
    M2_bool minimize,
    int *complete_up_through_this_degree)
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

M2_arrayintOrNull rawResolutionBetti(Computation *C, int type)
/* see engine.h for description of what 'type' should be */
{
  try
    {
      ResolutionComputation *G = C->cast_to_ResolutionComputation();
      if (G != 0) return G->get_betti(type);
      ERROR("expected resolution computation type");
      return NULL;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

M2_string IM2_GB_to_string(Computation *C)
/* TODO */
{
  buffer o;
  try
    {
      C->text_out(o);
      return o.to_string();
  } catch (const exc::engine_error& e)
    {
      o << "[unprintable gb]";
      return o.to_string();
  }
}

unsigned int rawComputationHash(const Computation *C) { return C->hash(); }
Matrix /* or null */ *rawSubduction(int numparts, const Matrix *M,
                                    const RingMap *F,
                                    Computation *C)
{
  try
    {
      GBComputation *G = C->cast_to_GBComputation();
      if (G == 0)
        {
          ERROR("expected a Groebner basis computation");
          return 0;
        }
      return sagbi::subduct(numparts, M, F, G);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

#include "mathicgb.h"
#include "matrix-stream.hpp"
void rawDisplayMatrixStream(const Matrix *inputMatrix)
{
  const Ring *R = inputMatrix->get_ring();
  const PolyRing *P = R->cast_to_PolyRing();
  if (P == 0)
    {
      ERROR("expected a polynomial ring");
      return;
    }
  if (P->characteristic() > std::numeric_limits<int>::max())
    {
      ERROR("characteristic is too large for mathic gb computation");
      return;
    }
  int charac = static_cast<int>(P->characteristic());
  int nvars = P->n_vars();

  mgb::GroebnerConfiguration configuration(
      charac, nvars, inputMatrix->n_rows());
  mgb::GroebnerInputIdealStream input(configuration);

  std::ostringstream computedStr;
  mgb::IdealStreamLog<> computed(
      computedStr, charac, nvars, inputMatrix->n_rows());
  mgb::IdealStreamChecker<decltype(computed)> checked(computed);

  matrixToStream(inputMatrix, checked);

  std::cout << "result: " << std::endl;
  std::cout << computedStr.str() << std::endl;
}

class MGBCallback : public mgb::GroebnerConfiguration::Callback
{
 public:
  MGBCallback() : mCallCount(0), mInterrupted(false) {}
  size_t callCount() const { return mCallCount; }
  bool wasInterrupted() const { return mInterrupted; }
 protected:
  virtual Action call()
  {
    // printf("called callback\n");
    mCallCount++;
    if (system_interrupted())
      {
        mInterrupted = true;
        return StopWithNoOutputAction;
      }
    return ContinueAction;
  }

 private:
  size_t mCallCount;
  bool mInterrupted;
};

// TODO: The following (in x-monoid.cpp) needs to be put into a header file.
extern bool monomialOrderingToMatrix(
    const struct MonomialOrdering &mo,
    std::vector<int> &mat,
    bool &base_is_revlex,
    int &component_direction,     // -1 is Down, +1 is Up, 0 is not present
    int &component_is_before_row  // -1 means: at the end. 0 means before the
                                  // order.
    // and r means considered before row 'r' of the matrix.
    );

const Matrix *rawMGB(
    const Matrix *inputMatrix,
    int reducer,
    int spairGroupSize,  // a value of 0 means let the algorithm choose
    int nthreads,
    M2_string logging)
{
  try
    {
      const Ring *R = inputMatrix->get_ring();
      const PolyRing *P = R->cast_to_PolyRing();
      if (P == 0)
        {
          ERROR("expected a polynomial ring");
          return 0;
        }
      if (nthreads < 0)
        {
          ERROR("mgb: expected a non-negative number of threads");
          return 0;
        }
      if (P->characteristic() > std::numeric_limits<int>::max())
        {
          ERROR("characteristic is too large for mathic gb computation");
          return 0;
        }
      if (P->characteristic() == 0)
        {
          ERROR(
              "characteristic for mathic gb computation must be a prime "
              "number");
          return 0;
        }
      if (not P->getCoefficientRing()->isFinitePrimeField())
        {
          ERROR("coefficients for mathic gb computation must be a prime field");
        }
      int charac = static_cast<int>(P->characteristic());
      int nvars = P->n_vars();
      MGBCallback callback;
      mgb::GroebnerConfiguration configuration(
          charac, nvars, inputMatrix->n_rows());

      const auto reducerType = reducer == 0
                                   ? mgb::GroebnerConfiguration::ClassicReducer
                                   : mgb::GroebnerConfiguration::MatrixReducer;
      configuration.setReducer(reducerType);
      configuration.setMaxSPairGroupSize(spairGroupSize);
      configuration.setMaxThreadCount(nthreads);
      std::string log((char *)logging->array, logging->len);
      configuration.setLogging(log.c_str());
      configuration.setCallback(&callback);

      // Now set the monomial ordering info
      std::vector<int> mat;
      bool base_is_revlex = true;
      int component_direction = 1;
      int component_is_before_row = -1;
      if (!monomialOrderingToMatrix(*P->getMonoid()->getMonomialOrdering(),
                                    mat,
                                    base_is_revlex,
                                    component_direction,
                                    component_is_before_row))
        {
          ERROR(
              "monomial ordering is not appropriate for Groebner basis "
              "computation");
          return 0;
        }
      if (!configuration.setMonomialOrder(
              (base_is_revlex
                   ?
                   // mgb::GroebnerConfiguration::BaseOrder::ReverseLexicographicBaseOrder
                   mgb::GroebnerConfiguration::BaseOrder::
                       RevLexDescendingBaseOrder
                   : mgb::GroebnerConfiguration::BaseOrder::
                         LexDescendingBaseOrder),
              mat))
        {
          throw exc::engine_error("expected global monomial ordering");
        }

      if (component_is_before_row >= 0)
        configuration.setComponentBefore(component_is_before_row);
      configuration.setComponentsAscending(component_direction == 1);

#if 0
    // Debug information
    printf("Setting monomial order:");
    for (size_t i=0; i<mat.size(); i++) printf("%d ", mat[i]);
    printf("\n");
    printf("  Base=%d\n", base_is_revlex);
#endif

      mgb::GroebnerInputIdealStream input(configuration);

      std::ostringstream computedStr;
      mgb::IdealStreamLog<> computed(
          computedStr, charac, nvars, inputMatrix->n_rows());
      mgb::IdealStreamChecker<decltype(computed)> checkedOut(computed);

      matrixToStream(inputMatrix, input);
      MatrixStream matStream(inputMatrix->rows());
      //  mgb::computeGroebnerBasis(input, checked);
      mgb::computeGroebnerBasis(input, matStream);
      if (callback.wasInterrupted())
        {
          ERROR("computation was interrupted");
          return 0;
        }
      const Matrix *result = matStream.value();
      // printf("number of callbacks = %lu  result = %lu\n",
      // callback.callCount(), result);
      //  rawDisplayMatrixStream(result);
      return result;
  } catch (const std::runtime_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
