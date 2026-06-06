// Copyright 1995 Michael E. Stillman

#include "interface/matrix.h"

#include <M2/math-include.h>

#include "NAG/NAG.hpp" // TODO: can this be removed?
#include "SLP/SLP-defs.hpp"
#include "buffer.hpp"
#include "basic-mutable-matrices/dmat.hpp"
#include "error.h"
#include "exceptions.hpp"
#include "free-modules/freemod.hpp"
#include "interface/NAG.h"
#include "interface/gmp-util.h"
#include "interface/monoid.h"
#include "basic-mutable-matrices/mat.hpp"
#include "matrices/matrix.hpp"
#include "mutable-matrices/mutablemat-defs.hpp"
#include "ring-elements/ring-element.hpp"
#include "rings/ring.hpp"
#include "rings/ringelem.hpp"
#include "BasicPolyList.hpp"
#include "BasicPolyListParser.hpp"
//#include "matrix-io.hpp"

namespace M2 { class ARingCC; }

const FreeModule *rawMatrixTarget(const Matrix *M) { return M->rows(); }
const FreeModule *rawMatrixSource(const Matrix *M) { return M->cols(); }
int rawMatrixNumRows(const Matrix *M) { return M->n_rows(); }
int rawMatrixNumColumns(const Matrix *M) { return M->n_cols(); }

M2_arrayint rawMatrixDegree(const Matrix *M)
{
  return to_degree_vector(M->get_ring()->degree_monoid(), M->degree_shift());
}

M2_string rawMatrixToString(const Matrix *M)
{
  buffer o;
  try
    {
      M->text_out(o);
      return o.to_string();
  } catch (const exc::engine_error& e)
    {
      o << "[unprintable matrix]";
      return o.to_string();
  }
}

unsigned int rawMatrixHash(const Matrix *M) { return M->hash(); }
const RingElement /* or null */ *rawMatrixEntry(const Matrix *M,
                                                      int r,
                                                      int c)
{
  try
    {
      return M->entry(r, c);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

/* Returns the entries of the matrix as an array of rows.
 */
engine_RawRingElementArrayArrayOrNull rawMatrixEntries(const Matrix *M)
{
  try
    {
      return M->entries();
    } catch (const exc::engine_error &e)
    {
      ERROR(e.what());
      return nullptr;
    }
  return nullptr;
}

const Matrix *rawMatrixIdentity(const FreeModule *F, int preference)
{
  (void) preference;
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::identity(F);
}

const Matrix /* or null */ *rawMatrixZero(const FreeModule *F,
                                            const FreeModule *G,
                                            int preference)
{
  (void) preference;
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::zero(F, G);
}

const Matrix /* or null */ *rawMatrix1(const FreeModule *target,
                                             int ncols,
                                             const engine_RawRingElementArray M,
                                             int preference)
{
  (void) preference;
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::make(target, ncols, M);
}

const Matrix /* or null */ *rawMatrix2(const FreeModule *target,
                                             const FreeModule *source,
                                             M2_arrayint deg,
                                             const engine_RawRingElementArray M,
                                             int preference)
{
  (void) preference;
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::make(target, source, deg, M);
}

const Matrix /* or null */ *rawSparseMatrix1(
    const FreeModule *target,
    int ncols,
    M2_arrayint rows,
    M2_arrayint cols,
    const engine_RawRingElementArray entries,
    int preference)
{
  (void) preference;
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::make_sparse(target, ncols, rows, cols, entries);
}

const Matrix /* or null */ *rawSparseMatrix2(
    const FreeModule *target,
    const FreeModule *source,
    M2_arrayint deg,
    M2_arrayint rows,
    M2_arrayint cols,
    const engine_RawRingElementArray entries,
    int preference)
{
  (void) preference;
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::make_sparse(target, source, deg, rows, cols, entries);
}

M2_bool rawMatrixIsDense(const Matrix *M)
/* Is the matrix M implemented as dense? */
{
  (void) M;
#ifdef DEVELOPMENT
#warning not implemented yet
#endif
  return 0;
}

const Matrix /* or null */ *rawMatrixRemake2(const FreeModule *target,
                                               const FreeModule *source,
                                               M2_arrayint deg,
                                               const Matrix *M,
                                               int preference)
/* Create a new matrix (mutable or immutable), from M, with new target,
   source, deg and/or mutable-ness. The new free modules must have
   the expected rank.
*/
{
  (void) preference;
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return M->remake(target, source, deg);
}

const Matrix /* or null */ *rawMatrixRemake1(const FreeModule *target,
                                               const Matrix *M,
                                               int preference)
/* Create a new matrix, from M, with new target,
   The target free module must have the expected rank.
   The source free module is computed heuristically from the target and the
   columns of the matrix.
*/
{
  (void) preference;
  try
    {
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
      return M->remake(target);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawRandomConstantMatrix(
    const Ring *R,
    int r,
    int c,
    double fraction_non_zero,
    int special_type,  // 0: general, 1:upper triangular, others?
    int preference)
{
  (void) preference;
#ifdef DEVELOPMENT
#warning preference not yet used
#endif
  return Matrix::random(R, r, c, fraction_non_zero, special_type);
}

const Matrix* /* or null */ rawMatrixReadMsolveString(const Ring* R, M2_string contents)
{
  try
    {
      std::string str = string_M2_to_std(contents);// TODO: this does a full copy.  Perhaps we just have readMsolveIdealContents take a string_view?
      auto Fs = parseMsolveFromString(str);
      return toMatrix(R->make_FreeModule(1), Fs);
    } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
    }
}

const Matrix* /* or null */ rawMatrixReadMsolveFile(const Ring* R, M2_string filename)
{
  try
    {
      std::string str = string_M2_to_std(filename);
      auto Fs = parseMsolveFile(str);
      return toMatrix(R->make_FreeModule(1), Fs);
    } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
    }
}

/////////////////////////////////////////////////////////////////////
M2_bool rawMatrixIsZero(const Matrix *M) { return M->is_zero(); }
int  // 1 = true, 0 = false, -1 = error
    rawMatrixIsEqual(const Matrix *M, const Matrix *N)
{
  try
    {
      /* This checks that the entries of M,N are the same, as well as
         that the source and target are the same (as graded free modules).
         Therefore, it can happen that M-N == 0, but M != N.
      */
      return M->is_equal(*N);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return -1;
  }
}

M2_bool rawMatrixIsHomogeneous(const Matrix *M) { return M->is_homogeneous(); }
const Matrix /* or null */ *rawMatrixConcat(const engine_RawMatrixArray Ms)
{
  try
    {
      return Matrix::concat(Ms->len, Ms->array);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawMatrixDirectSum(const engine_RawMatrixArray Ms)
{
  try
    {
      return Matrix::direct_sum(Ms->len, Ms->array);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawMatrixTensor(const Matrix *M, const Matrix *N)
{
  try
    {
      return M->tensor(N);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawModuleTensor(const Matrix *M, const Matrix *N)
{
  try
    {
      return M->module_tensor(N);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawMatrixDual(const Matrix *M)
{
  try
    {
      return M->transpose();
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawMatrixReshape(const Matrix *M,
                                               const FreeModule *F,
                                               const FreeModule *G)
{
  try
    {
      return M->reshape(F, G);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawMatrixFlip(const FreeModule *F,
                                            const FreeModule *G)
{
  try
    {
      return Matrix::flip(F, G);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawWedgeProduct(int p, int q, const FreeModule *F)
/* Constructs the map
   exterior(p,F) ** exterior(q,F) --> exterior(p+q,F)
*/
{
  try
    {
      return Matrix::wedge_product(p, q, F);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawMatrixSubmatrix(const Matrix *M,
                                                 M2_arrayint rows,
                                                 M2_arrayint cols)
{
  try
    {
      return M->sub_matrix(rows, cols);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawMatrixSubmatrixColumns(const Matrix *M,
                                                  M2_arrayint cols)
{
  try
    {
      return M->sub_matrix(cols);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawMatrixKoszul(int p, const Matrix *M)
{
  try
    {
      return M->koszul(p);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawKoszulMonomials(int nskew,
                                               const Matrix *M,
                                               const Matrix *N)
{
  try
    {
#ifdef DEVELOPMENT
#warning "check with 0.9.2 about what this should even do"
#endif
      if (M->get_ring() != N->get_ring())
        {
          ERROR("expected same ring");
          return nullptr;
        }
      return Matrix::koszul_monomials(nskew, M, N);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawMatrixSymmetricPower(int p, const Matrix *M)
{
  try
    {
      return M->symm(p);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawMatrixExteriorPower(int p,
                                                const Matrix *M,
                                                int strategy)
{
  try
    {
      return M->exterior(p, strategy);
    } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
    }

}

M2_arrayintOrNull rawMatrixSortColumns(const Matrix *M,
                                          int deg_order,
                                          int mon_order)
{
  try
    {
      return M->sort(deg_order, mon_order);
    } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
    }
}

const Matrix /* or null */ *rawMatrixMinors(int p,
                                              const Matrix *M,
                                              int strategy)
{
  try
    {
      return M->minors(p, strategy);
    } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
    }
}

const Matrix /* or null */ *rawMinors(
    int p,
    const Matrix *M,
    int strategy,
    int n_minors_to_compute, /* -1 means all */
    M2_arrayintOrNull first_row_set,
    M2_arrayintOrNull first_col_set)
/* If first_row_set or first_col_set is not NULL, they should both be non-NULL,
   and both have length p.  If not, NULL is returned.
   Compute n_minors_to_compute minors, starting at (first_row_set,first_col_set)
   if given,
   otherwise starting at the first (0..p-1,0..p-1).
*/
{
  try {
      return M->minors(
                       p, strategy, n_minors_to_compute,
                       first_row_set, first_col_set);
    } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
    }
}

const Matrix /* or null */ *rawMatrixPfaffians(int p, const Matrix *M)
{
  return M->pfaffians(p);
}

const RingElement /* or null */ *rawMatrixPfaffian(const Matrix *M)
{
  return RingElement::make_raw(M->get_ring(), M->pfaffian());
}

const Matrix /* or null */ *rawMatrixDiff(const Matrix *M, const Matrix *N)
{
  return M->diff(N, 1);
}

const Matrix /* or null */ *rawMatrixContract(const Matrix *M,
                                                const Matrix *N)
{
  return M->diff(N, 0);
}

const Matrix /* or null */ *rawMatrixHomogenize(const Matrix *M,
                                                  int var,
                                                  M2_arrayint wts)
{
  return M->homogenize(var, M2_arrayint_to_stdvector<int>(wts));
}

const Matrix /* or null */ *rawCoefficients(M2_arrayint vars,
                                            const Matrix *monoms,
                                            const Matrix *M)
{
  return M->coeffs(vars, monoms);
}

const Matrix /* or null */ *rawBasis(
    const Matrix *M,
    M2_arrayint lo_degree, /* possibly length 0 */
    M2_arrayint hi_degree,
    M2_arrayint wt,
    M2_arrayint vars,
    M2_bool do_truncation,
    int limit)
{
  return M->Matrix::basis(lo_degree, hi_degree, wt, vars, do_truncation, limit);
}

M2_arrayintOrNull rawMatrixIndices(const Matrix *f)
/* The list of indices of variables which occur in f is returned. */
/* currently requires a polynomial ring */ { return f->support(); }
const Matrix /* or null */ *rawMatrixMonomials(M2_arrayint vars,
                                                 const Matrix *M)
{
  return M->monomials(vars);
}

const Matrix *rawMatrixInitial(int nparts, const Matrix *M)
{
  return M->lead_term(nparts);
}

M2_arrayint rawMatrixEliminateVariables(int nparts, const Matrix *M)
{
  return M->elim_vars(nparts);
}

M2_arrayint rawMatrixKeepVariables(int nparts, const Matrix *M)
{
  return M->elim_keep(nparts);
}

engine_RawMatrixPairOrNull rawTopCoefficients(const Matrix *M)
{
  Matrix *coeffs;
  Matrix *monoms;
  coeffs = M->top_coefficients(monoms);
  if (coeffs == nullptr) return nullptr;
  engine_RawMatrixPair result = new engine_RawMatrixPair_struct;
  result->a = monoms;
  result->b = coeffs;
  return result;
}

engine_RawMatrixAndInt rawMatrixDivideByVariable(const Matrix *M,
                                                int var,
                                                int maxdegree)
/* If M = [v1, ..., vn], and x = 'var'th variable in the ring,
   return the matrix [w1,...,wn], where wi * x^(ai) = vi,
   and wi is not divisible by x, or ai = maxdegree,
   and the integer which is the maximum of the ai's.
   QUESTION: what rings should this work over?
*/
{
  int actualdegree;
  Matrix *N = M->divide_by_var(var, maxdegree, actualdegree);
  engine_RawMatrixAndInt result = new engine_RawMatrixAndInt_struct;
  result->M = N;
  result->i = actualdegree;
  return result;
}

const Matrix *rawMatrixCompress(const Matrix *M) { return M->compress(); }

const Matrix *rawRemoveMonomialFactors(const Matrix *m,
                                       M2_bool make_squarefree_only)
{
  return m->remove_monomial_factors(make_squarefree_only);
}

const Matrix *rawRemoveScalarMultiples(const Matrix *m)
{
  return m->remove_scalar_multiples();
}

// See engine.h for our definition of 'content'

const Matrix /* or null */ *rawMatrixContent(const Matrix *M)
// returns the matrix of the content of each column of M.
{
  return M->content();
}

const Matrix /* or null */ *rawMatrixRemoveContent(const Matrix *M)
{
  return M->remove_content();
}

const Matrix /* or null */ *rawMatrixSplitContent(
    const Matrix *M,
    const Matrix /* or null */ **result)
{
  return M->split_off_content(*result);
}

const Matrix /* or null */ *IM2_Matrix_remove_content(const Matrix *M)
{
  (void) M;
#ifdef DEVELOPMENT
#warning \
    "const Matrix /* or null */ * IM2_Matrix_remove_content(const Matrix *M) -- not implemented yet"
#endif
  return nullptr;
}

const Matrix /* or null */ *rawMatrixPromote(const FreeModule *newTarget,
                                               const Matrix *f)
{
  try
    {
      return f->promote(newTarget);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawMatrixLift(int *success_return,
                                            const FreeModule *newTarget,
                                            const Matrix *f)
{
  *success_return = 0;
  try
    {
      const Matrix *result = f->lift(newTarget);
      if (result == nullptr) return nullptr;
      *success_return = 1;
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

gmp_ZZ to_gmp_ZZ(int a)  // helper fn!!!
{
  mpz_ptr result = getmemstructtype(mpz_ptr);
  mpz_init(result);
  mpz_set_si(result, a);
  mpz_reallocate_limbs(result);
  return result;
}

M2Homotopy /* or null */ *rawHomotopy(M2SLEvaluator *Hx,
                                    M2SLEvaluator *Hxt,
                                    M2SLEvaluator *HxH)
{
  try {
    return new M2Homotopy(Hx->value().createHomotopy(&(Hxt->value()), &(HxH->value())));
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}

M2_bool rawHomotopyTrack(M2Homotopy *H,
                         const MutableMatrix *inputs,
                         MutableMatrix *outputs,
                         MutableMatrix *output_extras,
                         gmp_RR init_dt,
                         gmp_RR min_dt,
                         gmp_RR epsilon,  // o.CorrectorTolerance,
                         int max_corr_steps,
                         gmp_RR infinity_threshold,
                         M2_bool checkPrecision)
{
  try {
    return H->value().track(inputs,
                            outputs,
                            output_extras,
                            init_dt,
                            min_dt,
                            epsilon,  // o.CorrectorTolerance,
                            max_corr_steps,
                            infinity_threshold,
                            checkPrecision);
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return false;
  }
}

M2_string rawHomotopyToString(M2Homotopy *H)
{
  buffer o;
  H->value().text_out(o);
  return o.to_string();
}
unsigned int rawHomotopyHash(M2Homotopy *) { return 0; }
M2SLEvaluator /* or null */ *rawSLEvaluator(M2SLProgram *SLP,
                                          M2_arrayint constsPos,
                                          M2_arrayint varsPos,
                                          const MutableMatrix *consts)
{
  return consts->createSLEvaluator(SLP, constsPos, varsPos);
}

M2SLEvaluator /* or null */ *rawCompiledSLEvaluator(
                                                    M2_string libName,
                                                    int nInputs,
                                                    int nOutputs,
                                                    const MutableMatrix *empty)
{
  return empty->createCompiledSLEvaluator(libName, nInputs, nOutputs);
}

M2SLEvaluator /* or null */ *rawSLEvaluatorSpecialize(
    M2SLEvaluator *H,
    const MutableMatrix *parameters)
{
  try {
    return new M2SLEvaluator(H->value().specialize(parameters));
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}

M2_bool rawSLEvaluatorEvaluate(M2SLEvaluator *sle,
                               const MutableMatrix *inputs,
                               MutableMatrix *outputs)
{
  try {
    return sle->value().evaluate(inputs, outputs);
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return false;
  }
}

M2_string rawSLEvaluatorToString(M2SLEvaluator *sle)
{
  buffer o;
  sle->value().text_out(o);
  return o.to_string();
}
unsigned int rawSLEvaluatorHash(M2SLEvaluator *) { return 0; }
M2SLProgram /* or null */ *rawSLProgram(unsigned long nConstantsAndInputs)
{
  (void) nConstantsAndInputs;
  return new M2SLProgram(new SLProgram);
}
M2_string rawSLProgramToString(M2SLProgram *slp)
{
  buffer o;
  slp->value().text_out(o);
  return o.to_string();
}
unsigned int rawSLProgramHash(M2SLProgram *) { return 0; }
gmp_ZZ rawSLPInputGate(M2SLProgram *S) { return to_gmp_ZZ(S->value().addInput()); }
gmp_ZZ rawSLPSumGate(M2SLProgram *S, M2_arrayint a)
{
  return to_gmp_ZZ(S->value().addMSum(a));
}
gmp_ZZ rawSLPProductGate(M2SLProgram *S, M2_arrayint a)
{
  return to_gmp_ZZ(S->value().addMProduct(a));
}
gmp_ZZ rawSLPDetGate(M2SLProgram *S, M2_arrayint a)
{
  return to_gmp_ZZ(S->value().addDet(a));
}
gmp_ZZ rawSLPsetOutputPositions(M2SLProgram *S, M2_arrayint a)
{
  S->value().setOutputPositions(a);
  return to_gmp_ZZ(0);  // this function should have returned "void"
}
gmp_ZZ rawSLPDivideGate(M2SLProgram *S, M2_arrayint a)
{
  return to_gmp_ZZ(S->value().addDivide(a));
}

StraightLineProgram /* or null */ *rawSLP(const Matrix *consts,
                                          M2_arrayint program)
{
  try {
    return StraightLineProgram::make(consts, program);
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}

const Matrix /* or null */ *rawEvaluateSLP(StraightLineProgram *SLP,
                                           const Matrix *vals)
{
  try {
    return SLP->evaluate(vals);
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}

M2_string rawStraightLineProgramToString(StraightLineProgram *slp)
{
  buffer o;
  slp->text_out(o);
  return o.to_string();
}

unsigned int rawStraightLineProgramHash(StraightLineProgram *slp)
{
  return slp->hash();
}

/// PathTracker /////////////////////////////////////////////////////

PathTracker /* or null */ *rawPathTrackerPrecookedSLPs(
    StraightLineProgram *slp_pred,
    StraightLineProgram *slp_corr)
{
  try {
    return PathTracker::make(slp_pred, slp_corr);
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}

PathTracker /* or null */ *rawPathTracker(const Matrix *HH)
{
  try {
    return PathTracker::make(HH);
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}

PathTracker /* or null */ *rawPathTrackerProjective(const Matrix *S,
                                                    const Matrix *T,
                                                    gmp_RR productST)
{
  try {
    return PathTracker::make(S, T, productST);
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}

M2_string rawPathTrackerToString(PathTracker *p)
{
  buffer o;
  p->text_out(o);
  return o.to_string();
}

unsigned int rawPathTrackerHash(PathTracker *p) { return p->hash(); }
// PointArray

M2_string rawPointArrayToString(M2PointArray *pa)
{
  buffer o;
  pa->value().text_out(o);
  return o.to_string();
}

unsigned int rawPointArrayHash(M2PointArray *pa)
{
  return pa->hash();
}

M2PointArray /* or null */ *rawPointArray(double epsilon, int n)
{
  try {
    return new M2PointArray(new PointArray(epsilon, n));
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}

PointArray::RealVector getRealVector(const MutableMatrix *M, int col)
{
  PointArray::RealVector result;
  auto MC = dynamic_cast<const MutableMat<DMat<M2::ARingCC> > *>(M);
  // if (MC == nullptr)
  if (MC == nullptr)
    {
      throw exc::engine_error("expected mutable matrix over CC");
    }

  for (size_t r = 0; r < MC->getMat().numRows(); ++r)
    {
      result.push_back(MC->getMat().entry(r, col).re);
      result.push_back(MC->getMat().entry(r, col).im);
    }
  return result;
}

int rawPointArrayLookup(M2PointArray *pa, const MutableMatrix *M, int col)
{
  return pa->value().lookup(getRealVector(M, col));
}

int rawPointArrayLookupOrAppend(M2PointArray *pa, const MutableMatrix *M, int col)
{
  return pa->value().lookup_or_append(getRealVector(M, col));
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
