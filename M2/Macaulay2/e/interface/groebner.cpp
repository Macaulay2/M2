// Copyright 2002 Michael E. Stillman

#include "interface/groebner.h"

#include <iostream>
#include <limits>
#include <sstream>

#include "Eschreyer.hpp"
#include "hilb.hpp"
#include "comp-gb.hpp"
#include "comp-res.hpp"
#include "comp-gb-declared.hpp"
#include "text-io.hpp"
#include "sagbi.hpp"
#include "exceptions.hpp"
#include "gb-walk.hpp"
#include "relem.hpp"
#include "util.hpp"
#include "matrix-ncbasis.hpp"

#include "M2FreeAlgebra.hpp"
#include "NCAlgebras/FreeAlgebra.hpp"
#include "NCAlgebras/NCGroebner.hpp"
#include "NCAlgebras/NCF4.hpp"

#include "poly.hpp"
#include "interrupted.hpp"
#include "schreyer-resolution/res-f4-computation.hpp"

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
const RingElement /* or null */ *IM2_Matrix_Hilbert(const Matrix *M)
/* This routine computes the numerator of the Hilbert series
   for coker leadterms(M), using the degrees of the rows of M.
   nullptr is returned if the ring is not appropriate for
   computing Hilbert series, or the computation was interrupted. */
{
  try
    {
      return hilb_comp::hilbertNumerator(M);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix *rawKernelOfGB(const Matrix *M)
/* Assuming that the columns of G form a GB, this routine computes
   a Groebner basis of the kernel of these elements, using an
   appropriate Schreyer order on the source of G. */
{
  GBMatrix *N = new GBMatrix(M);
  GBKernelComputation G(N);
  G.calc();
  GBMatrix *syz = G.get_syzygies();
  return syz->to_matrix();
}

///////////////////////////////////////////////////////////////////////////////////
///////// The following will be removed once the new code is functional
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
      return nullptr;
  }
}

Computation /* or null */ *IM2_res_make(const Matrix *m,
                                        M2_bool resolve_cokernel,
                                        int max_level,
                                        M2_bool use_max_slanted_degree,
                                        int max_slanted_degree,
                                        int algorithm,
                                        int strategy,
                                        M2_bool parallelizeByDegree)
{
  try
    {
      test_over_RR_or_CC(m->get_ring());
      // Choose the correct computation here.
      
      // XXX

      // Grab max number of threads (settable from the front end).
      int numThreads = M2_numTBBThreads; // settable from front end.
      //std::cout << "Using numThreads = " << numThreads << std::endl;
      
      clear_emit_size();
      return ResolutionComputation::choose_res(m,
                                               resolve_cokernel,
                                               max_level,
                                               use_max_slanted_degree,
                                               max_slanted_degree,
                                               algorithm,
                                               strategy,
                                               numThreads,
                                               parallelizeByDegree);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
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
          return nullptr;
        }
      if (G != nullptr) return G->set_hilbert_function(h);
      ERROR("computation type unknown or not implemented");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
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
      return nullptr;
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
      return nullptr;
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
      return nullptr;
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
      return nullptr;
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

      return error() ? nullptr : C;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
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
      if (G != nullptr) return G->get_gb();
      ERROR("computation type unknown or not implemented");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
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
      if (G != nullptr) return G->get_mingens();
      ERROR("computation type unknown or not implemented");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
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
      if (G != nullptr) return G->get_change();
      ERROR("computation type unknown or not implemented");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawGBGetLeadTerms(Computation *C, int nparts)
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G != nullptr) return G->get_initial(nparts);
      ERROR("computation type unknown or not implemented");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawGBGetParallelLeadTerms(Computation *C,
                                                      M2_arrayint w)
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G != nullptr) return G->get_parallel_lead_terms(w);
      ERROR("computation type unknown or not implemented");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
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
      if (G != nullptr) return G->get_syzygies();
      ERROR("computation type unknown or not implemented");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawGBMatrixRemainder(Computation *C,
                                                 const Matrix *m)
{
  try
    {
      clear_emit_size();
      GBComputation *G = C->cast_to_GBComputation();
      if (G != nullptr) return G->matrix_remainder(m);
      ERROR("computation type unknown or not implemented");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
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
      if (G != nullptr)
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
      if (G != nullptr) return G->contains(m);
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
      if (G != nullptr) return G->get_matrix(level);
      ERROR("expected resolution computation type");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
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
      if (G != nullptr) return G->get_matrix(level, degree);
      ERROR("expected resolution computation type");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

MutableMatrix /* or null */ *rawResolutionGetMutableMatrixB(Computation *C,
                                                            const Ring *R,
                                                            int level)
// Perhaps a HACK that might change.
// First: C must be a nonminimal res computation, over QQ M.
// Second: R must be a polynomial ring with the same monoid M as C's,
//  and the coefficient ring must be either RR, or ZZ/p, where p is the (a)
//  prime being used in the computation.
{
  try
    {
      clear_emit_size();
      F4ResComputation *G = dynamic_cast<F4ResComputation *>(C);
      if (G != nullptr) return G->get_mutable_matrix(R, level);
      ERROR("expected fast nonminimal resolution computation type");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
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
      if (G != nullptr) return G->get_mutable_matrix(KK, level, degree);
      ERROR("expected fast nonminimal resolution computation type");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const FreeModule /* or null */ *rawResolutionGetFree(Computation *C, int level)
{
  try
    {
      clear_emit_size();
      ResolutionComputation *G = C->cast_to_ResolutionComputation();
      if (G != nullptr) return G->get_free(level);
      ERROR("expected resolution computation type");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
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
      if (G != nullptr) return G->get_betti(type);
      ERROR("expected resolution computation type");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
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
      if (G == nullptr)
        {
          ERROR("expected a Groebner basis computation");
          return nullptr;
        }
      return sagbi::subduct(numparts, M, F, G);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

Matrix /* or null */ *rawSubduction1(int numparts,
                                       const Ring *rawT,
                                       const Ring *rawS,
                                       const Matrix *m,
                                       const RingMap *inclusionAmbient,
                                       const RingMap *fullSubstitution,
                                       const RingMap *substitutionInclusion,
                                       Computation *rawGBI,
                                       Computation *rawGBReductionIdeal)
{
    try
    {
        GBComputation *gbReductionIdeal = rawGBReductionIdeal->cast_to_GBComputation();
        GBComputation *gbI = rawGBI->cast_to_GBComputation();
        if ((gbReductionIdeal == nullptr) || (gbI == nullptr))
        {
            ERROR("expected a Groebner basis computation");
            return nullptr;
        }
        return sagbi::subduct1(numparts, rawT, rawS, m, inclusionAmbient, fullSubstitution, substitutionInclusion, gbI, gbReductionIdeal);
    } catch (const exc::engine_error& e)
    {
        ERROR(e.what());
        return nullptr;
    }
}

#include "mathicgb.h"
#include "matrix-stream.hpp"
void rawDisplayMatrixStream(const Matrix *inputMatrix)
{
  const Ring *R = inputMatrix->get_ring();
  const PolyRing *P = R->cast_to_PolyRing();
  if (P == nullptr)
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
      if (P == nullptr)
        {
          ERROR("expected a polynomial ring");
          return nullptr;
        }
      if (nthreads < 0)
        {
          ERROR("mgb: expected a non-negative number of threads");
          return nullptr;
        }
      if (P->characteristic() > std::numeric_limits<int>::max())
        {
          ERROR("characteristic is too large for mathic gb computation");
          return nullptr;
        }
      if (P->characteristic() == 0)
        {
          ERROR(
              "characteristic for mathic gb computation must be a prime "
              "number");
          return nullptr;
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
          return nullptr;
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
          return nullptr;
        }
      const Matrix *result = matStream.value();
      // printf("number of callbacks = %lu  result = %lu\n",
      // callback.callCount(), result);
      //  rawDisplayMatrixStream(result);
      return result;
  } catch (const std::runtime_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

/////////////////////////////////////////////
// Noncommutative Groebner bases (2-sided) //
/////////////////////////////////////////////

ConstPolyList matrixToPolyList(const M2FreeAlgebraOrQuotient* A,
                               const Matrix* input)
{
  ConstPolyList result;
  result.reserve(input->n_cols() * input->n_rows());
  for (int i=0; i < input->n_rows(); i++)
    {
      for (int j=0; j < input->n_cols(); ++j)
      {
        ring_elem a = input->elem(i,j);
        auto f = reinterpret_cast<const Poly*>(a.get_Poly());
        result.push_back(f);
      }
    }
  return result;
}

// vectorToMatrix consumes 'elems': the same pointers are used for the resulting Matrix.
template<typename PolyL>
const Matrix* polyListToMatrix(const M2FreeAlgebraOrQuotient* A,
                               const PolyL& elems,
                               int numrows,
                               int numcols)
{
  if (elems.size() != numrows*numcols)
    ERROR("Number of elements in list does not match matrix size.");
  MatrixConstructor mat(A->make_FreeModule(numrows), numcols);
  for (auto i = 0; i < elems.size(); ++i)
    {
      int curCol = i % numcols;
      int curRow = (i - curCol) / numcols;
      ring_elem a = const_cast<Nterm*>(reinterpret_cast<const Nterm*>(elems[i]));
      mat.set_entry(curRow, curCol, a);
    }
  mat.compute_column_degrees();
  return mat.to_matrix();
}

const Matrix* rawNCGroebnerBasisTwoSided(const Matrix* input, int maxdeg, int strategy)
{
  const Ring* R = input->get_ring();
  const M2FreeAlgebra* A = R->cast_to_M2FreeAlgebra();
  if (A != nullptr and input->n_rows() == 1)
    {
      auto elems = matrixToPolyList(A, input);
      bool isF4 = strategy & 16;
      bool isParallel = strategy & 32;
      if (isF4)
        {
          int numthreads = M2_numTBBThreads; // settable from front end.
          // std::cout << "Using numthreads = " << numthreads << std::endl;
          NCF4 G(A->freeAlgebra(), elems, maxdeg, strategy, (isParallel ? numthreads : 1));
          G.compute(maxdeg); // this argument is actually the soft degree limit
          auto result = copyPolyVector(A, G.currentValue());
          return polyListToMatrix(A, result, 1, result.size()); // consumes the Poly's in result
        }
      else
        {
          NCGroebner G(A->freeAlgebra(), elems, maxdeg, strategy);
          G.compute(maxdeg); // this argument is actually the soft degree limit
          auto result = copyPolyVector(A, G.currentValue());
          return polyListToMatrix(A, result, 1, result.size()); // consumes the Poly's in result
        }

    }
  ERROR("expected a one row matrix over a noncommutative algebra");
  return nullptr;
}

const Matrix* rawNCReductionTwoSided(const Matrix* toBeReduced, const Matrix* reducerMatrix)
{
  const Ring* R = toBeReduced->get_ring();
  if (R != reducerMatrix->get_ring())
    {
      ERROR("expected matrices to be over the same ring");
      return nullptr;
    }
  const M2FreeAlgebra* A = R->cast_to_M2FreeAlgebra();
  if (A != nullptr and reducerMatrix->n_rows() == 1)
    {
      auto outRows = toBeReduced->n_rows();
      auto outCols = toBeReduced->n_cols();
      auto reducees = matrixToPolyList(A, toBeReduced);
      auto reducers = matrixToPolyList(A, reducerMatrix);
      NCGroebner G(A->freeAlgebra(),reducers, 0, 0);
      G.initReductionOnly();
      auto result = G.twoSidedReduction(reducees);
      return polyListToMatrix(A, result, outRows, outCols); // consumes the Poly's in result.
    }
  ERROR("expected a matrix over a noncommutative algebra");
  return nullptr;
}

const Matrix* rawNCBasis(const Matrix* gb2SidedIdeal,
                         M2_arrayint lo_degree,
                         M2_arrayint hi_degree,
                         int limit
                         )
{
  const Ring* R = gb2SidedIdeal->get_ring();
  if (R == nullptr)
    {
      ERROR("internal error: expected non-null Ring!");
      return nullptr;
    }
  try {
    const M2FreeAlgebra* A = R->cast_to_M2FreeAlgebra();
    if (A != nullptr)
      {
        ConstPolyList G = matrixToPolyList(A, gb2SidedIdeal);

        // WARNING: The following line creates new polynomials
        // which are used directly in vectorToMatrix (without copying)
        // but when result goes out of scope, the list is deleted,
        // but not the polynomials themselves, as they are pointers.
        PolyList result;
        bool worked = ncBasis(A->freeAlgebra(),
                              G,
                              M2_arrayint_to_stdvector<int>(lo_degree),
                              M2_arrayint_to_stdvector<int>(hi_degree),
                              limit,
                              result);
        if (not worked) return nullptr;
        return polyListToMatrix(A, result, 1, result.size()); // consumes entries of result
      }
    ERROR("expected a free algebra");
    return nullptr;
  }
  catch (exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
