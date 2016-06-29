// Copyright 1995 Michael E. Stillman

#include "relem.hpp"
#include "matrix.hpp"
#include "matrix-con.hpp"
#include "LLL.hpp"
#include "text-io.hpp"
#include "exceptions.hpp"
#include "NAG.hpp"
#include "engine.h"

const FreeModule * IM2_Matrix_get_target(const Matrix *M)
{
  return M->rows();
}

const FreeModule * IM2_Matrix_get_source(const Matrix *M)
{
  return M->cols();
}

int IM2_Matrix_n_rows(const Matrix *M)
{
  return M->n_rows();
}

int IM2_Matrix_n_cols(const Matrix *M)
{
  return M->n_cols();
}

M2_arrayint IM2_Matrix_get_degree(const Matrix *M)
{
  return M->degree_monoid()->to_arrayint(M->degree_shift());
}

M2_string IM2_Matrix_to_string(const Matrix *M)
{
     buffer o;
     try {
          M->text_out(o);
          return o.to_string();
     }
     catch (exc::engine_error e) {
          o << "[unprintable matrix]";
          return o.to_string();
     }
}

unsigned int rawMatrixHash(const Matrix *M)
{
  return M->hash();
}

const RingElement /* or null */ * IM2_Matrix_get_entry(const Matrix *M, int r, int c)
{
     try {
          if (r < 0 || r >= M->n_rows())
            {
              ERROR("matrix row index %d out of range 0 .. %d", r, M->n_rows()-1);
              return 0;
            }
          if (c < 0 || c >= M->n_cols())
            {
              ERROR("matrix column index %d out of range 0 .. %d", c, M->n_cols()-1);
              return 0;
            }
          ring_elem result;
          result = M->elem(r,c);
          return RingElement::make_raw(M->get_ring(), result);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix * IM2_Matrix_identity(const FreeModule *F,
                                   int preference)
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::identity(F);
}

const Matrix /* or null */ * IM2_Matrix_zero(const FreeModule *F,
                                     const FreeModule *G,
                                     int preference)
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::zero(F,G);
}


const Matrix /* or null */ * IM2_Matrix_make1(const FreeModule *target,
                                      int ncols,
                                      const engine_RawRingElementArray M,
                                      int preference)
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::make(target, ncols, M);
}

const Matrix /* or null */ * IM2_Matrix_make2(const FreeModule *target,
                                      const FreeModule *source,
                                      M2_arrayint deg,
                                      const engine_RawRingElementArray M,
                                      int preference)
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::make(target, source, deg, M);
}

const Matrix /* or null */ * IM2_Matrix_make_sparse1(const FreeModule *target,
                                             int ncols,
                                             M2_arrayint rows,
                                             M2_arrayint cols,
                                             const engine_RawRingElementArray entries,
                                             int preference)
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::make_sparse(target, ncols, rows, cols, entries);
}

const Matrix /* or null */ * IM2_Matrix_make_sparse2(const FreeModule *target,
                                             const FreeModule *source,
                                             M2_arrayint deg,
                                             M2_arrayint rows,
                                             M2_arrayint cols,
                                             const engine_RawRingElementArray entries,
                                             int preference)
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::make_sparse(target, source, deg, rows, cols, entries);
}

M2_bool IM2_Matrix_is_implemented_as_dense(const Matrix *M)
  /* Is the matrix M implemented as dense? */
{
#ifdef DEVELOPMENT
#warning not implemented yet
#endif
  return 0;
}

const Matrix /* or null */ * IM2_Matrix_remake2(const FreeModule *target,
                                        const FreeModule *source,
                                        M2_arrayint deg,
                                        const Matrix *M,
                                        int preference)
  /* Create a new matrix (mutable or immutable), from M, with new target,
     source, deg and/or mutable-ness. The new free modules must have
     the expected rank.
  */
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return M->remake(target, source, deg);
}

const Matrix /* or null */ * IM2_Matrix_remake1(const FreeModule *target,
                                        const Matrix *M,
                                        int preference)
  /* Create a new matrix, from M, with new target,
     The target free module must have the expected rank.
     The source free module is computed heuristically from the the target and the
     columns of the matrix.
  */
{
     try {
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
          return M->remake(target);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ *IM2_Matrix_random(const Ring *R,
                                int r, int c,
                                double fraction_non_zero,
                                int special_type, // 0: general, 1:upper triangular, others?
                                int preference)
{
#ifdef DEVELOPMENT
#warning preference not yet used
#endif
  return Matrix::random(R,r,c,fraction_non_zero,special_type);
}

/////////////////////////////////////////////////////////////////////
M2_bool IM2_Matrix_is_zero(const Matrix *M)
{
  return M->is_zero();
}

int                             // 1 = true, 0 = false, -1 = error
IM2_Matrix_is_equal(const Matrix *M,
                                  const Matrix *N)
{
     try {
          /* This checks that the entries of M,N are the same, as well as
             that the source and target are the same (as graded free modules).
             Therefore, it can happen that M-N == 0, but M != N.
          */
          return M->is_equal(*N);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return -1;
     }
}

M2_bool IM2_Matrix_is_graded(const Matrix *M)
{
  return M->is_homogeneous();
}

const Matrix /* or null */ * IM2_Matrix_concat(const engine_RawMatrixArray Ms)
{
     try {
          unsigned int n = Ms->len;
          if (n == 0)
            {
              ERROR("matrix concat: expects at least one matrix");
              return 0;
            }
          const FreeModule *F = Ms->array[0]->rows();
          const Ring *R = F->get_ring();
          MatrixConstructor mat(Ms->array[0]->rows(), 0);
          int next=0;
          for (unsigned int i=0; i<n; i++)
            {
              const Matrix *M = Ms->array[i];
              if (F->get_ring() != M->get_ring())
                {
                  ERROR("matrix concat: different base rings");
                  return 0;
                }
              if (F->rank() != M->n_rows())
                {
                  ERROR("matrix concat: row sizes are not equal");
                  return 0;
                }
              for (int j=0; j<M->n_cols(); j++)
                {
                  mat.append(R->copy_vec(M->elem(j)));
                  mat.set_column_degree(next++, M->cols()->degree(j));
                }
            }
          return mat.to_matrix();
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ * IM2_Matrix_direct_sum(const engine_RawMatrixArray Ms)
{
     try {
          // Check that the matrices all have the same ring, and that there is
          // at least one matrix.
          unsigned int n = Ms->len;
          if (n == 0)
            {
              ERROR("matrix direct sum: expects at least one matrix");
              return 0;
            }
          const Matrix *result = Ms->array[0];
          const Ring *R = result->get_ring();
          for (unsigned int i=1; i<n; i++)
            if (R != Ms->array[i]->get_ring())
              {
                ERROR("matrix direct sum: different base rings");
                return 0;
              }
          for (unsigned int i=1; i<n; i++)
            result = result->direct_sum(Ms->array[i]);

          return result;
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ * IM2_Matrix_tensor(const Matrix *M,
                                       const Matrix *N)
{
     try {
          return M->tensor(N);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}


const Matrix /* or null */ * rawModuleTensor(const Matrix *M,
                                     const Matrix *N)
{
     try {
          return M->module_tensor(N);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ * IM2_Matrix_transpose(const Matrix *M)
{
     try {
          return M->transpose();
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ * IM2_Matrix_reshape(const Matrix *M,
                                        const FreeModule *F,
                                        const FreeModule *G)
{
     try {
          return M->reshape(F,G);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ * IM2_Matrix_flip(const FreeModule *F,
                                     const FreeModule *G)
{
     try {
          return Matrix::flip(F,G);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ * rawWedgeProduct(int p,
                                     int q,
                                     const FreeModule *F)
  /* Constructs the map
     exterior(p,F) ** exterior(q,F) --> exterior(p+q,F)
  */
{
     try {
          return Matrix::wedge_product(p,q,F);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ * IM2_Matrix_submatrix(const Matrix *M,
                                          M2_arrayint rows,
                                          M2_arrayint cols)
{
     try {
          return M->sub_matrix(rows,cols);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ * IM2_Matrix_submatrix1(const Matrix *M,
                                           M2_arrayint cols)
{
     try {
          return M->sub_matrix(cols);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ * IM2_Matrix_koszul(int p, const Matrix *M)
{
     try {
          return M->koszul(p);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ *
rawKoszulMonomials(int nskew,
                     const Matrix *M,
                     const Matrix *N)
{
     try {
#ifdef DEVELOPMENT
#warning "check with 0.9.2 about what this should even do"
#endif
          if (M->get_ring() != N->get_ring())
            {
              ERROR("expected same ring");
              return 0;
            }
          return Matrix::koszul_monomials(nskew,M,N);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ * IM2_Matrix_symm(int p, const Matrix *M)
{
     try {
          return M->symm(p);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ * IM2_Matrix_exterior(int p, const Matrix *M, int strategy)
{
  return M->exterior(p,strategy);
}

M2_arrayintOrNull IM2_Matrix_sort_columns(const Matrix *M,
                                                 int deg_order,
                                                 int mon_order)
{
     try {
          return M->sort(deg_order, mon_order);
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}


const Matrix /* or null */ * IM2_Matrix_minors(int p, const Matrix *M, int strategy)
{
  return M->minors(p,strategy);
}

const Matrix /* or null */ * rawMinors(int p,
                         const Matrix *M,
                         int strategy,
                         int n_minors_to_compute, /* -1 means all */
                         M2_arrayintOrNull first_row_set,
                         M2_arrayintOrNull first_col_set
                         )
/* If first_row_set or first_col_set is not NULL, they should both be non-NULL,
   and both have length p.  If not, NULL is returned.
   Compute n_minors_to_compute minors, starting at (first_row_set,first_col_set) if given,
   otherwise starting at the first (0..p-1,0..p-1).
*/
{
  return M->minors(p,strategy,n_minors_to_compute,first_row_set,first_col_set);
}

const Matrix /* or null */ * IM2_Matrix_pfaffians(int p, const Matrix *M)
{
  return M->pfaffians(p);
}

const Matrix /* or null */ * IM2_Matrix_diff(const Matrix *M,
                                     const Matrix *N)
{
  return M->diff(N,1);
}

const Matrix /* or null */ * IM2_Matrix_contract(const Matrix *M,
                                         const Matrix *N)
{
  return M->diff(N,0);
}

const Matrix /* or null */ * IM2_Matrix_homogenize(const Matrix *M,
                                           int var,
                                           M2_arrayint wts)
{
  return M->homogenize(var, wts);
}

const engine_RawMatrixPair_struct *IM2_Matrix_coeffs(const Matrix *M, M2_arrayint vars)
{
#ifdef DEVELOPMENT
#warning "implement IM2_Matrix_coeffs"
#endif
  return 0;
}

const Matrix /* or null */ * rawCoefficients(M2_arrayint vars,
                                     const Matrix *monoms,
                                     const Matrix *M)
{
  return M->coeffs(vars,monoms);
}

const Matrix /* or null */ * rawBasis(const Matrix *M,
                              M2_arrayint lo_degree, /* possibly length 0 */
                              M2_arrayint hi_degree,
                              M2_arrayint wt,
                              M2_arrayint vars,
                              M2_bool do_truncation,
                              int limit)
{
  return M->Matrix::basis(lo_degree,hi_degree,wt,vars,do_truncation,limit);
}

M2_arrayintOrNull rawMatrixIndices(const Matrix *f)
  /* The list of indices of variables which occur in f is returned. */
  /* currently requires a polynomial ring */
{
  return f->support();
}

const Matrix /* or null */ * IM2_Matrix_monomials(M2_arrayint vars,
                                          const Matrix *M)
{
  return M->monomials(vars);
}

const Matrix * IM2_Matrix_initial(int nparts, const Matrix *M)
{
  return M->lead_term(nparts);
}

M2_arrayint IM2_Matrix_elim_vars(int nparts, const Matrix *M)
{
  return M->elim_vars(nparts);
}

M2_arrayint IM2_Matrix_keep_vars(int nparts, const Matrix *M)
{
  return M->elim_keep(nparts);
}

engine_RawMatrixPairOrNull rawTopCoefficients(const Matrix *M)
{
  Matrix *coeffs;
  Matrix *monoms;
  coeffs = M->top_coefficients(monoms);
  if (coeffs == NULL)
    return NULL;
  engine_RawMatrixPair result = new engine_RawMatrixPair_struct;
  result->a = monoms;
  result->b = coeffs;
  return result;
}

engine_RawMatrixAndInt IM2_Matrix_divide_by_var(const Matrix *M, int var, int maxdegree)
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


const Matrix * rawMatrixCompress(const Matrix *M)
{
  return M->compress();
}

#include "Eschreyer.hpp"

const Matrix * IM2_kernel_of_GB(const Matrix *m)
  /* Assuming that the columns of G form a GB, this computes
     a Groebner basis of the kernel of these elements, using an appropriate Schreyer order on the
     source of G. */
{
  GBMatrix *n = new GBMatrix(m);
  GBKernelComputation G(n);
  G.calc();
  GBMatrix *syz = G.get_syzygies();
  return syz->to_matrix();
}

const Matrix * rawRemoveMonomialFactors(const Matrix *m, M2_bool make_squarefree_only)
{
  return m->remove_monomial_factors(make_squarefree_only);
}

const Matrix * rawRemoveScalarMultiples(const Matrix *m)
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

const Matrix /* or null */ *rawMatrixSplitContent(const Matrix *M, const Matrix /* or null */ **result)
{
  return M->split_off_content(*result);
}

const Matrix /* or null */ *IM2_Matrix_remove_content(const Matrix *M) {
#ifdef DEVELOPMENT
#warning "const Matrix /* or null */ * IM2_Matrix_remove_content(const Matrix *M) -- not implemented yet"
#endif
     return NULL;
}

const Matrix /* or null */ *IM2_Matrix_promote(const FreeModule *newTarget,
                                 const Matrix *f)
{
     try {
          ring_elem a;
          const Ring *R = f->get_ring();
          const Ring *S = newTarget->get_ring();
          MatrixConstructor mat(newTarget,f->n_cols());
          Matrix::iterator i(f);
          for (int c=0; c<f->n_cols(); c++)
            for (i.set(c); i.valid(); i.next())
              if (S->promote(R,i.entry(),a))
                mat.set_entry(i.row(), c, a);
              else
                {
                  ERROR("cannot promote given matrix");
                  return 0;
                }
          mat.compute_column_degrees();
          return mat.to_matrix();
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

const Matrix /* or null */ *IM2_Matrix_lift(int *success_return, const FreeModule *newTarget,
                              const Matrix *f)
{
     try {
          ring_elem a;
          const Ring *R = f->get_ring();
          const Ring *S = newTarget->get_ring();
          MatrixConstructor mat(newTarget,f->n_cols());
          Matrix::iterator i(f);
          for (int c=0; c<f->n_cols(); c++)
            for (i.set(c); i.valid(); i.next())
              if (R->lift(S,i.entry(),a))
                mat.set_entry(i.row(), c, a);
              else
                {
                  // ERROR("cannot lift given matrix");
                  return 0;
                }
          mat.compute_column_degrees();
          *success_return = 1;
          return mat.to_matrix();
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}

gmp_ZZ to_gmp_ZZ(int a) // helper fn!!!
{
  gmp_ZZ result = getmemstructtype(gmp_ZZ);
  mpz_init(result);
  mpz_set_si(result,a);
  return result;
}

Homotopy /* or null */ *rawHomotopy(SLEvaluator *Hx, SLEvaluator *Hxt, SLEvaluator *HxH) {
  return Hx->createHomotopy(Hxt,HxH);
}

M2_bool rawHomotopyTrack(Homotopy *H, const MutableMatrix *inputs, MutableMatrix *outputs, 
                         MutableMatrix* output_extras,  
                         gmp_RR init_dt, gmp_RR min_dt,
                         gmp_RR epsilon, // o.CorrectorTolerance,
                         int max_corr_steps, 
                         gmp_RR infinity_threshold,
                         M2_bool checkPrecision) 
{
  /*
  auto inp = dynamic_cast<const MutableMat<DMat<M2::ARingCCC>>*> (inputs);
  if (inp!=nullptr) { // check precision!!!
    std::cout << "-- precisions:" << std::endl;      
    auto& m = inp->getMat();
    for(int i=0; i<m.numRows(); i++)
      for(int j=0; j<m.numColumns(); j++) {
        auto& e = m.entry(i,j);
        auto p_re = mpfr_get_prec(&m.ring().realPartReference(e));
        auto p_im = mpfr_get_prec(&m.ring().imaginaryPartReference(e));
        std::cout << "(" << p_re << "," << p_im << ") ";
      }
    std::cout << std::endl;      
  }
  */
  return H->track(inputs,outputs, 
                  output_extras,  
                  init_dt, min_dt,
                  epsilon, // o.CorrectorTolerance,
                  max_corr_steps, 
                  infinity_threshold,
                  checkPrecision);
}

M2_string rawHomotopyToString(Homotopy * H) { 
  buffer o;
  H->text_out(o);
  return o.to_string();
  }
unsigned int rawHomotopyHash(Homotopy *) { return 0; }


SLEvaluator /* or null */ *rawSLEvaluator(SLProgram *SLP, M2_arrayint constsPos, M2_arrayint varsPos, const MutableMatrix *consts) {
  return consts->createSLEvaluator(SLP,constsPos,varsPos);
}

SLEvaluator /* or null */ *rawSLEvaluatorSpecialize(SLEvaluator *H, const MutableMatrix *parameters) {
  return H->specialize(parameters);
}

M2_bool rawSLEvaluatorEvaluate(SLEvaluator *sle, const MutableMatrix *inputs, MutableMatrix *outputs) {
  return sle->evaluate(inputs,outputs);
}

M2_string rawSLEvaluatorToString(SLEvaluator * sle) { 
  buffer o;
  sle->text_out(o);
  return o.to_string();
  }
unsigned int rawSLEvaluatorHash(SLEvaluator *) { return 0; }

SLProgram /* or null */ *rawSLProgram(unsigned long nConstantsAndInputs) { 
  return new SLProgram;
  //return nullptr; 
}
M2_string rawSLProgramToString(SLProgram * slp) { 
  buffer o;
  slp->text_out(o);
  return o.to_string();
  }
unsigned int rawSLProgramHash(SLProgram *) { return 0; }
gmp_ZZ rawSLPInputGate(SLProgram *S) { return to_gmp_ZZ(S->addInput()); }
gmp_ZZ rawSLPSumGate(SLProgram *S, M2_arrayint a) { return to_gmp_ZZ(S->addMSum(a)); }
gmp_ZZ rawSLPProductGate(SLProgram *S, M2_arrayint a) { return to_gmp_ZZ(S->addMProduct(a)); }
gmp_ZZ rawSLPDetGate(SLProgram *S, M2_arrayint a) { return to_gmp_ZZ(S->addDet(a)); }
gmp_ZZ rawSLPsetOutputPositions(SLProgram *S, M2_arrayint a) { 
  S->setOutputPositions(a); 
  return to_gmp_ZZ(0); // this function should have returned "void"
}
gmp_ZZ rawSLPDivideGate(SLProgram *S, M2_arrayint a) { return to_gmp_ZZ(S->addDivide(a)); }

StraightLineProgram /* or null */ *rawSLP(const Matrix *consts, M2_arrayint program)
{
  return StraightLineProgram::make(consts, program);
}

const Matrix /* or null */ *rawEvaluateSLP(StraightLineProgram *SLP, const Matrix *vals)
{
  return SLP->evaluate(vals);
}

M2_string rawStraightLineProgramToString(StraightLineProgram *slp) {
  buffer o;
  slp->text_out(o);
  return o.to_string();
}

unsigned int rawStraightLineProgramHash(StraightLineProgram *slp) {
  return slp->hash();
}

/// PathTracker /////////////////////////////////////////////////////

PathTracker /* or null */ *rawPathTrackerPrecookedSLPs(StraightLineProgram* slp_pred, StraightLineProgram* slp_corr)
{
  return PathTracker::make(slp_pred, slp_corr);
}

PathTracker /* or null */ *rawPathTracker(const Matrix *HH)
{
  return PathTracker::make(HH);
}

PathTracker /* or null */ *rawPathTrackerProjective(const Matrix *S, const Matrix *T, gmp_RR productST)
{
  return PathTracker::make(S,T,productST);
}

M2_string rawPathTrackerToString(PathTracker *p) {
  buffer o;
  p->text_out(o);
  return o.to_string();
}

unsigned int rawPathTrackerHash(PathTracker *p) {
  return p->hash();
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
