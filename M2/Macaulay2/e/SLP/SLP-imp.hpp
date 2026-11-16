// Copyright 2015 Anton Leykin and Mike Stillman

// Anton Leykin's code in this file is in the public domain.

#ifndef M2_SLP_SLP_IMP_HPP_
#define M2_SLP_SLP_IMP_HPP_

//#include "m2tbb.hpp"
#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <dlfcn.h>
#include <gc/gc.h>
#include "timing.hpp"
//#include <tbb/tbb.h>
#include <tbb/parallel_for.h>
#include <tbb/task_arena.h>
#include <atomic>
#include <mutex>
#include <string>

// SLEvaluator
template <typename RT>
SLEvaluatorConcrete<RT>::SLEvaluatorConcrete(
    SLProgram* SLP,
    M2_arrayint cPos,
    M2_arrayint vPos,
    const MutableMat<SMat<RT> >* consts /* DMat<RT>& DMat_consts */)
    : mRing(consts->getMat().ring())
{
  (void) SLP;
  (void) cPos;
  (void) vPos;
  std::cerr << "SLEvaluatorConcrete constructor not defined for sparse matrices\n";  
  abort();
}

template <typename RT>
SLEvaluatorConcrete<RT>::SLEvaluatorConcrete(
      M2_string libName,
      int nInputs,
      int nOutputs,
      const MutableMat<SMat<RT> >* empty
      ): mRing(empty->getMat().ring())
{
  (void) libName;
  (void) nInputs;
  (void) nOutputs;
  std::cerr << "SLEvaluatorConcrete constructor not defined for sparse matrices\n";  
  abort();
}

template <typename RT>
SLEvaluatorConcrete<RT>::SLEvaluatorConcrete
(M2_string libName,
 int nInputs,
 int nOutputs,
 const MutableMat<DMat<RT> >* empty
 ): mRing(empty->getMat().ring()), isCompiled(true), nInputs(nInputs), nOutputs(nOutputs), nParams(0), parametersAndInputs(nullptr)
{
  slp = NULL;
  //const char* funname = "evaluate";
  const char* funnameRR = "_Z8evaluatePKdPd";
  const char* funnameCC = "_Z8evaluatePKSt7complexIdEPS0_";
  auto libName_std = string_M2_to_std(libName);
  const char* lib_name = libName_std.c_str();
  printf("loading from %s\n", lib_name);
  void* handle = dlopen(lib_name, RTLD_LAZY | RTLD_GLOBAL);
  if (handle == NULL) ERROR("can't load library %s", lib_name);
  compiled_fn = (void (*)(ElementType const*, ElementType*)) dlsym(handle, funnameRR);
  if (compiled_fn == NULL)
    compiled_fn = (void (*)(ElementType const*, ElementType*)) dlsym(handle, funnameCC);
  if (compiled_fn == NULL) 
    std::cerr << "can't link function " << funnameRR << " or " << funnameCC << " from library " << lib_name << std::endl;
}
  
// copy constructor
template <typename RT>
SLEvaluatorConcrete<RT>::SLEvaluatorConcrete(const SLEvaluatorConcrete<RT>& a)
  : SLEvaluator(a), mRing(a.ring()), isCompiled(a.isCompiled), nInputs(a.nInputs), nOutputs(a.nOutputs), values(a.values.size()), compiled_fn(a.compiled_fn), nParams(a.nParams)
{
  slp = a.slp;
  varsPos = a.varsPos;
  auto i = values.begin();
  auto j = a.values.begin();
  for (; i != values.end(); ++i, ++j) ring().init_set(*i, *j);
  // std::cout << "SLEvaluatorConcrete: copy constructor for " << this << std::endl;
  if (a.parametersAndInputs==nullptr) parametersAndInputs = nullptr; else {
    parametersAndInputs = new ElementType[nParams+nInputs];
    std::copy(a.parametersAndInputs,a.parametersAndInputs+nParams, parametersAndInputs);
  }
}

template <typename RT>
SLEvaluatorConcrete<RT>::~SLEvaluatorConcrete()
{
  // std::cout << "~SLEvaluatorConcrete: " << this << std::endl
  if (isCompiled) {// dlclose???
    if (parametersAndInputs!=nullptr) delete[] parametersAndInputs;
  } else {
    for (auto& v : values) ring().clear(v);
    for (auto& vals : mThreadValues)
      for (auto& v : vals) ring().clear(v);
  }
}

template <typename RT>
SLEvaluatorConcrete<RT>::SLEvaluatorConcrete
(SLProgram* SLP,
 M2_arrayint cPos,
 M2_arrayint vPos,
 const MutableMat<DMat<RT> >* consts /* DMat<RT>& DMat_consts */
 ): mRing(consts->getMat().ring()), isCompiled(false), compiled_fn(NULL), nParams(0), parametersAndInputs(nullptr)
{
  slp = SLP;
  // for(int i=0; i<cPos->len; i++)
  //  constsPos.push_back(slp->inputCounter+cPos->array[i]);
  for (int i = 0; i < vPos->len; i++)
    varsPos.push_back(slp->inputCounter + vPos->array[i]);
  values.resize(slp->inputCounter + slp->mNodes.size());
  for (auto i = values.begin(); i != values.end(); ++i) ring().init(*i);
  for (int i = 0; i < cPos->len; i++)
    ring().set(values[slp->inputCounter + cPos->array[i]],
               consts->getMat().entry(0, i));
  // std::cout << "SLEvaluatorConcrete(MutableMat): " << this << std::endl;
  nInputs = varsPos.size();
  nOutputs = slp->mOutputPositions.size();
}

template <typename RT>
SLEvaluator* SLEvaluatorConcrete<RT>::specialize(
    const MutableMatrix* parameters) const
{
  // std::cout << "SLEvaluatorConcrete::specialize:" << this << std::endl;
  auto p = dynamic_cast<const MutableMat<DMat<RT> >*>(parameters);
  if (p == nullptr)
    throw exc::engine_error("specialize: expected a dense mutable matrix");
  return specialize(p);
}

template <typename RT>
SLEvaluator* SLEvaluatorConcrete<RT>::specialize(
    const MutableMat<DMat<RT> >* parameters) const
{
  size_t nNewParams = parameters->n_rows();  
  if (parameters->n_cols() != 1 || nNewParams > nInputs) 
    throw exc::engine_error("1-column matrix expected; or #parameters > #vars");
  SLEvaluatorConcrete<RT>* e = new SLEvaluatorConcrete<RT>(*this);
  if (isCompiled) {
    e->nParams = nParams + nNewParams;
    e->nInputs -= nNewParams;
    delete[] e->parametersAndInputs;
    e->parametersAndInputs = new ElementType[e->nParams+e->nInputs];
    std::copy(parametersAndInputs, parametersAndInputs+nParams, e->parametersAndInputs); // old parameters values
    for (int i = 0; i < nNewParams; ++i)
      ring().set(e->parametersAndInputs[nParams+i], parameters->getMat().entry(i, 0));
  }
  else {
    for (int i = 0; i < nNewParams; ++i)
      ring().set(e->values[varsPos[i]], parameters->getMat().entry(i, 0));
    e->varsPos.erase(e->varsPos.begin(), e->varsPos.begin() + nNewParams);
    e->nInputs = e->varsPos.size();
    e->nParams = nParams + nNewParams;
  }
  return e;
}

template <typename RT>
void SLEvaluatorConcrete<RT>::computeNextNode(
    std::vector<SLProgram::GATE_TYPE>::iterator& nIt,
    std::vector<SLProgram::GATE_SIZE>::iterator& numInputsIt,
    std::vector<SLProgram::GATE_POSITION>::iterator& inputPositionsIt,
    typename std::vector<ElementType>::iterator& vIt)
{
  ElementType& v = *vIt;
  switch (*nIt++)
    {
      case SLProgram::MProduct:
        ring().set_from_long(v, 1);
        for (int i = 0; i < *numInputsIt; i++)
          ring().mult(v, v, *(vIt + (*inputPositionsIt++)));
        numInputsIt++;
        break;
      case SLProgram::MSum:
        ring().set_zero(v);
        for (int i = 0; i < *numInputsIt; i++)
          ring().add(v, v, *(vIt + (*inputPositionsIt++)));
        numInputsIt++;
        break;
      case SLProgram::Det:
        {
          int n = static_cast<int>(sqrt(*numInputsIt++));
          DMat<RT> mat(ring(), n, n);
          for (int i = 0; i < n; i++)
            for (int j = 0; j < n; j++)
              ring().set(mat.entry(i, j), *(vIt + (*inputPositionsIt++)));
          DMatLinAlg<RT>(mat).determinant(v);
        }
        break;
      case SLProgram::Divide:
        ring().set(v, *(vIt + (*inputPositionsIt++)));
        ring().divide(v, v, *(vIt + (*inputPositionsIt++)));
        break;
      default:
        std::cerr << "unknown node type\n";
        abort();
    }
}

template <typename RT>
bool SLEvaluatorConcrete<RT>::evaluate(const MutableMatrix* inputs,
                                       MutableMatrix* outputs)
{
  auto inp = dynamic_cast<const MutableMat<DMat<RT> >*>(inputs);
  auto out = dynamic_cast<MutableMat<DMat<RT> >*>(outputs);
  if (inp == nullptr)
    {
      ERROR("inputs: expected a dense mutable matrix");
      return false;
    }
  if (out == nullptr)
    {
      ERROR("outputs: expected a dense mutable matrix");
      return false;
    }
  if (&ring() != &inp->getMat().ring())
    {
      ERROR("inputs are in a different ring");
      return false;
    }
  if (&ring() != &out->getMat().ring())
    {
      ERROR("outputs are in a different ring");
      return false;
    }
  return evaluate(inp->getMat(), out->getMat());
}

template <typename RT>
bool SLEvaluatorConcrete<RT>::evaluate(const DMat<RT>& inputs,
                                       DMat<RT>& outputs)
{
  if (nInputs != inputs.numRows() * inputs.numColumns()) {
    ERROR(
          "inputs: the number of inputs does not match the number of entries "
          "in the inputs matrix");
    std::cout << nInputs
              << " != " << inputs.numRows() * inputs.numColumns()
              << std::endl;
    return false;
  }
  if (nOutputs != outputs.numRows() * outputs.numColumns()) {
    ERROR(
          "outputs: the number of outputs does not match the number of entries "
          "in the outputs matrix");
    std::cout << nOutputs << " != " << outputs.numRows()
              << " * " << outputs.numColumns() << std::endl;
    return false;
  }

  if (isCompiled) {
    if(parametersAndInputs==nullptr) {
      (*compiled_fn)(inputs.unsafeArray(), outputs.unsafeArray());
    } else {
      std::copy(inputs.unsafeArray(),inputs.unsafeArray()+nInputs,parametersAndInputs+nParams); 
      (*compiled_fn)(parametersAndInputs, outputs.unsafeArray());
    }
    return true;
  } else {
    // thread-local working copy, deep-copied from `this->values` on first use
    auto& values = mThreadValues.local();
    if (values.size() != this->values.size()) {
      values.resize(this->values.size());
      auto i = values.begin();
      auto j = this->values.begin();
      for (; i != values.end(); ++i, ++j) ring().init_set(*i, *j);
    }
    size_t i = 0;
    for (size_t r = 0; r < inputs.numRows(); r++)
      for (size_t c = 0; c < inputs.numColumns(); c++)
        ring().set(values[varsPos[i++]], inputs.entry(r, c));
    auto nIt = slp->mNodes.begin();
    auto numInputsIt = slp->mNumInputs.begin();
    auto inputPositionsIt = slp->mInputPositions.begin();
    for (auto vIt = values.begin() + slp->inputCounter; vIt != values.end(); ++vIt)
      computeNextNode(nIt, numInputsIt, inputPositionsIt, vIt);
    i = 0;
    for (size_t r = 0; r < outputs.numRows(); r++)
      for (size_t c = 0; c < outputs.numColumns(); c++)
        ring().set(outputs.entry(r, c), values[ap(slp->mOutputPositions[i++])]);
    return true;
  }
}

// First-order rounding-error bound for the last evaluate() on this thread.
// Every node value v computed in precision p carries an error e(v) (in units
// of u = 2^-p) with
//   e(input var)   = 0                    (the point itself is taken as exact)
//   e(constant)    = |c|                  (rounded once when stored)
//   e(a+b)         = e(a) + e(b) + |a+b|
//   e(a*b)         = |a| e(b) + |b| e(a) + |a b|
//   e(a/b)         = (e(a) + |a/b| e(b)) / |b| + |a/b|
// where |.| are the magnitudes of the values actually computed, so
// cancellation inside a sum is accounted for by the (small) magnitude of the
// result rather than by the (large) magnitudes of its terms. Complex
// arithmetic costs a small constant factor more; that is left to the safety
// margin of the caller. Det is bounded crudely via row 1-norms.
template <typename RT>
bool SLEvaluatorConcrete<RT>::evaluationErrorBound(std::vector<double>& bound)
{
  if (isCompiled) return false;
  auto& values = mThreadValues.local();
  if (values.size() != this->values.size()) return false;
  const auto& R = ring().real_ring();
  const size_t N = values.size();
  std::vector<double> mag(N), err(N, 0.0);
  typename RT::RealRingType::Element a2(R);
  for (size_t i = 0; i < N; i++)
    {
      ring().abs_squared(a2, values[i]);
      mag[i] = std::sqrt(R.coerceToDouble(a2));
    }
  const size_t nIn = slp->inputCounter;
  for (size_t i = 0; i < nIn; i++) err[i] = mag[i];  // constants, parameters
  for (auto p : varsPos) err[p] = 0.0;                // variables
  auto nIt = slp->mNodes.begin();
  auto numInputsIt = slp->mNumInputs.begin();
  auto ipIt = slp->mInputPositions.begin();
  for (size_t k = nIn; k < N; k++)
    switch (*nIt++)
      {
        case SLProgram::MSum:
          {
            double acc = 0.0, e = 0.0;
            for (int i = 0; i < *numInputsIt; i++)
              {
                size_t j = k + (*ipIt++);
                acc += mag[j];
                e += err[j] + acc;
              }
            numInputsIt++;
            err[k] = e;
            break;
          }
        case SLProgram::MProduct:
          {
            double acc = 1.0, e = 0.0;
            for (int i = 0; i < *numInputsIt; i++)
              {
                size_t j = k + (*ipIt++);
                e = e * mag[j] + acc * err[j] + acc * mag[j];
                acc *= mag[j];
              }
            numInputsIt++;
            err[k] = e;
            break;
          }
        case SLProgram::Divide:
          {
            size_t ja = k + (*ipIt++);
            size_t jb = k + (*ipIt++);
            if (mag[jb] == 0.0)
              err[k] = HUGE_VAL;
            else
              {
                double q = mag[ja] / mag[jb];
                err[k] = (err[ja] + q * err[jb]) / mag[jb] + q;
              }
            break;
          }
        case SLProgram::Det:
          {
            int n = static_cast<int>(sqrt(*numInputsIt++));
            std::vector<double> rowMag(n, 0.0), rowErr(n, 0.0);
            for (int i = 0; i < n; i++)
              for (int j = 0; j < n; j++)
                {
                  size_t jj = k + (*ipIt++);
                  rowMag[i] += mag[jj];
                  rowErr[i] += err[jj];
                }
            double B = 1.0;  // Hadamard-type bound on |det|
            for (int i = 0; i < n; i++) B *= rowMag[i];
            double e = double(n) * double(n) * B;
            for (int i = 0; i < n; i++)
              if (rowMag[i] > 0.0) e += rowErr[i] * (B / rowMag[i]);
            err[k] = e;
            break;
          }
        default:
          std::cerr << "unknown node type\n";
          abort();
      }
  bound.resize(slp->mOutputPositions.size());
  for (size_t i = 0; i < bound.size(); i++)
    bound[i] = err[ap(slp->mOutputPositions[i])];
  if (M2_numericalAlgebraicGeometryTrace > 9)
    {
      size_t imax = 0;
      for (size_t i = 1; i < N; i++) if (mag[i] > mag[imax]) imax = i;
      std::cout << "evaluationErrorBound: " << nIn << " inputs, " << N - nIn
                << " nodes; largest magnitude " << mag[imax] << " at " << imax
                << (imax < nIn ? " (input)" : "") << ", err " << err[imax]
                << "; input magnitudes:";
      for (size_t i = 0; i < nIn; i++) std::cout << " " << mag[i];
      std::cout << std::endl;
    }
  return true;
}

template <typename RT>
void SLEvaluatorConcrete<RT>::text_out(buffer& o) const
{
  if(isCompiled) {
    o << "compiled SLEvaluator("
      << nInputs << " inputs, "
      << nOutputs << " outputs, "
      << nParams << " parameters, mRing = ";
    ring().text_out(o);
    o << ")" << newline;
  } else {
    o << "SLEvaluator(slp = ";
    slp->text_out(o);
    o << ", mRing = ";
    ring().text_out(o);
    o << ")" << newline;
  }
}

template <typename RT>
Homotopy* SLEvaluatorConcrete<RT>::createHomotopy(SLEvaluator* Hxt,
                                                  SLEvaluator* HxH)
{
  auto castHxt = dynamic_cast<SLEvaluatorConcrete<RT>*>(Hxt);
  auto castHxH = dynamic_cast<SLEvaluatorConcrete<RT>*>(HxH);
  if (not castHxt or not castHxH)
    {
      ERROR("expected SLEvaluators in the same ring");
      return nullptr;
    }
  return new HomotopyConcrete<RT, typename HomotopyAlgorithm<RT>::Algorithm>(
      *this, *castHxt, *castHxH);
}

template <typename RT>
inline void norm2(const DMat<RT>& M,
                  size_t n,
                  typename RT::RealElementType& result)
{
  const auto& C = M.ring();
  const auto& R = C.real_ring();
  typename RT::RealRingType::Element c(R);
  R.set_zero(result);
  for (size_t i = 0; i < n; i++)
    {
      C.abs_squared(c, M.entry(0, i));
      R.add(result, result, c);
    }
}

enum SolutionStatus {
  UNDETERMINED,
  PROCESSING,
  REGULAR,
  SINGULAR,
  INFINITY_FAILED,
  MIN_STEP_FAILED,
  ORIGIN_FAILED,
  INCREASE_PRECISION,
  DECREASE_PRECISION
};

template <typename RT>
class ARingElement
{
  typename RT::ElementType mValue;
  const RT& mRing;

 public:
  ARingElement(const RT& R0) : mRing(R0) { mRing.init(mValue); }
  ~ARingElement() { mRing.clear(mValue); }
  typename RT::ElementType& get() { return mValue; }
  const typename RT::ElementType& get() const { return mValue; }
  const RT& ring() const { return mRing; }
  typename RT::RealElementType& getRealPart() { return realPart(mValue); }
};

// ****************************** XXX
// **************************************************

// Solve A x = b for a square n x n matrix A and an n x 1 vector b by partial
// pivot LU, using only the caller-provided workspace: LU (n x n), perm (size
// n), and three scratch elements. Rows are addressed through perm, so nothing
// is swapped physically. This exists because the per-step solves run inside
// the parallel path tracker, where MatrixOps::solveLinear's temporary DMat
// copies all go through the GC allocator and serialize on its global lock.
// Returns false if a zero pivot is met.
template <typename RT>
static bool solveSquareNoAlloc(const RT& C,
                               const DMat<RT>& A,
                               const DMat<RT>& b,
                               DMat<RT>& x,
                               DMat<RT>& LU,
                               std::vector<size_t>& perm,
                               typename RT::RealRingType::Element& absP,
                               typename RT::RealRingType::Element& absMax)
{
  const auto& R = C.real_ring();
  const size_t n = A.numRows();
  for (size_t i = 0; i < n; i++)
    {
      perm[i] = i;
      for (size_t j = 0; j < n; j++) C.set(LU.entry(i, j), A.entry(i, j));
    }
  for (size_t k = 0; k < n; k++)
    {
      // pivot: row with largest |entry| in column k among rows k..n-1
      size_t p = k;
      C.abs_squared(absMax, LU.entry(perm[k], k));
      for (size_t i = k + 1; i < n; i++)
        {
          C.abs_squared(absP, LU.entry(perm[i], k));
          if (R.compare_elems(absP, absMax) > 0)
            {
              R.set(absMax, absP);
              p = i;
            }
        }
      if (R.is_zero(absMax)) return false;
      std::swap(perm[k], perm[p]);
      const size_t pr = perm[k];
      for (size_t i = k + 1; i < n; i++)
        {
          const size_t r = perm[i];
          typename RT::ElementType& l = LU.entry(r, k);
          C.divide(l, l, LU.entry(pr, k));  // multiplier l_{ik}
          for (size_t j = k + 1; j < n; j++)
            C.subtract_multiple(LU.entry(r, j), l, LU.entry(pr, j));
        }
    }
  // x = P b; forward substitution with unit lower L; back substitution with U
  for (size_t i = 0; i < n; i++)
    {
      typename RT::ElementType& xi = x.entry(i, 0);
      C.set(xi, b.entry(perm[i], 0));
      for (size_t j = 0; j < i; j++)
        C.subtract_multiple(xi, LU.entry(perm[i], j), x.entry(j, 0));
    }
  for (size_t i = n; i-- > 0;)
    {
      typename RT::ElementType& xi = x.entry(i, 0);
      for (size_t j = i + 1; j < n; j++)
        C.subtract_multiple(xi, LU.entry(perm[i], j), x.entry(j, 0));
      C.divide(xi, xi, LU.entry(perm[i], i));
    }
  return true;
}

// Adaptive precision (Precision => infinity): see the precision check in track() below.
#define PRECISION_SAFETY_BITS 4        // ~1 decimal digit (default): ||J^-1|| estimate is a lower bound; complex-arithmetic constants
#define PRECISION_HYSTERESIS_BITS 4    // spare bits the next lower precision must still have
#define PRECISION_LOW_NEED_STEPS 5     // ... for this many consecutive steps before decreasing
#define PRECISION_FAIL_STEPS 3         // consecutive corrector failures with insufficient precision before increasing

template <typename RT>
bool HomotopyConcrete<RT, FixedPrecisionHomotopyAlgorithm>::track(
    const MutableMatrix* inputs,
    MutableMatrix* outputs,
    MutableMatrix* output_extras,
    gmp_RR init_dt,
    gmp_RR min_dt,
    gmp_RR epsilon,  // o.CorrectorTolerance,
    int max_corr_steps,
    gmp_RR infinity_threshold,
    bool checkPrecision)
{
  std::chrono::steady_clock::time_point start =
      std::chrono::steady_clock::now();
  size_t solveLinearTime = 0, solveLinearCount = 0, evaluateTime = 0;
  const Ring* matRing = inputs->get_ring();
  if (outputs->get_ring() != matRing)
    {
      ERROR("outputs and inputs are in different rings");
      return false;
    }
  auto inp = dynamic_cast<const MutableMat<DMat<RT> >*>(inputs);
  auto out = dynamic_cast<MutableMat<DMat<RT> >*>(outputs);
  auto out_extras =
      dynamic_cast<MutableMat<DMat<M2::ARingZZGMP> >*>(output_extras);
  if (inp == nullptr)
    {
      ERROR("inputs: expected a dense mutable matrix");
      return false;
    }
  if (out == nullptr)
    {
      ERROR("outputs: expected a dense mutable matrix");
      return false;
    }
  if (out_extras == nullptr)
    {
      ERROR("output_extras: expected a dense mutable matrix");
      return false;
    }

  auto& in = inp->getMat();
  auto& ou = out->getMat();
  auto& oe = out_extras->getMat();
  size_t n_sols = in.numColumns();
  size_t n = in.numRows() - 1;  // number of x vars

  if (ou.numColumns() != n_sols or ou.numRows() != n + 2)
    {
      ERROR("output: wrong shape");
      return false;
    }
  if (oe.numColumns() != n_sols or oe.numRows() != 2)
    {
      ERROR("output_extras: wrong shape");
      return false;
    }

  const RT& C = in.ring();
  typename RT::RealRingType R = C.real_ring();

  typedef typename RT::ElementType ElementType;
  typedef typename RT::Element Element;
  typedef typename RT::RealRingType::Element RealElement;
  typedef typename RT::RealRingType::ElementType RealElementType;
  typedef MatElementaryOps<DMat<RT> > MatOps;

  tbb::task_arena arena(M2_numTBBThreads == 0
                        ? tbb::task_arena::automatic
                        : M2_numTBBThreads);
  // Batch paths into ~3·numThreads chunks to amortize per-lambda DMat setup cost.
  int numThreads = (M2_numTBBThreads == 0) ? arena.max_concurrency()
                                            : M2_numTBBThreads;
  int grainSize  = (int)std::max<size_t>(1, n_sols / (10 * numThreads));

  // Loop-invariant scalars: computed once, read-only inside the parallel_for.
  RealElement t_step(R), min_step2(R), epsilon2(R), infinity_threshold2(R);
  R.set_from_BigReal(t_step, init_dt);  // initial step
  R.set_from_BigReal(min_step2, min_dt);
  R.mult(min_step2, min_step2, min_step2);  // min_step^2
  R.set_from_BigReal(epsilon2, epsilon);
  R.mult(epsilon2, epsilon2, epsilon2);  // epsilon^2
  R.set_from_BigReal(infinity_threshold2, infinity_threshold);
  R.mult(infinity_threshold2, infinity_threshold2, infinity_threshold2);
  const int num_successes_before_increase = 3;

  // Progress bar: append-only (no \r), so it also works in front ends such as
  // the WebApp clients that don't treat \r as a return to the start of a line.
  std::atomic<int> bar_done{0};
  std::mutex        bar_mutex;
  const int         bar_width = 40;
  const bool        show_bar = M2_numericalAlgebraicGeometryTrace > 0 && n_sols > 0;
  if (show_bar)
    std::cerr << "-- " << n_sols << " paths [" << std::flush;

  // Allow the TBB worker threads to register themselves with the GC (see the
  // lambda below). Without this, GC_register_my_thread aborts with "Threads
  // explicit registering is not previously enabled" whenever no GC-wrapped
  // pthread_create has happened yet, e.g. when M2 runs with --no-threads.
  GC_allow_register_threads();

  arena.execute([&]{
  tbb::parallel_for(tbb::blocked_range<int>(0, (int)n_sols, grainSize),
  [&](tbb::blocked_range<int> r) {
    // Register this TBB worker with the Boehm GC. TBB workers are created
    // inside the prebuilt libtbb dylib, which never sees bdwgc's pthread_create
    // redirect macro, so the workers are unknown to the GC by default; without
    // this, a collection triggered from inside the loop aborts with
    // "Collecting from unknown thread".
    bool gc_was_registered = GC_thread_is_registered();
    if (!gc_was_registered) {
      struct GC_stack_base sb;
      GC_get_stack_base(&sb);
      GC_register_my_thread(&sb);
    }
    RealElement t0(R), dt(R), one_minus_t0(R), dx_norm2(R), x_norm2(R), abs2dc(R),
        inv_x_norm2(R);

    // constants
    RealElement one(R), two(R), four(R), six(R), one_half(R), one_sixth(R);
    RealElementType& dt_factor = one_half;
    R.set_from_long(one, 1);
    R.set_from_long(two, 2);
    R.set_from_long(four, 4);
    R.set_from_long(six, 6);
    R.divide(one_half, one, two);
    R.divide(one_sixth, one, six);

    Element c_init(C), c_end(C), dc(C), one_half_dc(C);

    // think: x_0..x_(n-1), c
    // c = the homotopy continuation parameter "t" upstair, varies on a (staight
    // line) segment of complex plane (from c_init to c_end)
    // t = a real running in the interval [0,1]

    DMat<RT> x0c0(C, n + 1, 1);
    DMat<RT> x1c1(C, n + 1, 1);
    DMat<RT> xc(C, n + 1, 1);
    DMat<RT> HxH(C, n, n + 1);
    DMat<RT>& Hxt = HxH;  // the matrix has the same shape: reuse memory
    DMat<RT> LHSmat(C, n, n);
    auto LHS = submatrix(LHSmat);
    DMat<RT> RHSmat(C, n, 1);
    auto RHS = submatrix(RHSmat);
    DMat<RT> dx(C, n, 1);
    DMat<RT> dx1(C, n, 1);
    DMat<RT> dx2(C, n, 1);
    DMat<RT> dx3(C, n, 1);
    DMat<RT> dx4(C, n, 1);
    // allocation-free linear-solve workspace (see solveSquareNoAlloc)
    DMat<RT> LUws(C, n, n);
    std::vector<size_t> permws(n);
    RealElement absP(R), absMax(R);
    // adaptive precision: ||H(x)||^2 and the largest ||dx||^2/||H(x)||^2 seen
    // in the current step's corrector iterations (a lower bound for ||J^{-1}||^2)
    RealElement h_norm2(R), jinv2(R), ratio2(R);
    std::vector<double> evalErr;  // per-output rounding-error bounds of HxH

    ElementType& c0 = x0c0.entry(n, 0);
    ElementType& c1 = x1c1.entry(n, 0);
    ElementType& c = xc.entry(n, 0);
    RealElementType& tol2 = epsilon2;  // current tolerance squared
    bool linearSolve_success;
    //end// initial vars setup
    for (size_t s = r.begin(); s < r.end(); ++s) {
      SolutionStatus status = PROCESSING;
      // set initial solution and initial value of the continuation parameter
      // for(size_t i=0; i<=n; i++)
      //  C.set(x0c0.entry(i,0), in.entry(i,s));
      submatrix(x0c0) = submatrix(const_cast<DMat<RT>&>(in), 0, s, n + 1, 1);
      C.set(c_init, c0);
      C.set(c_end, ou.entry(n, s));

      R.set_zero(t0);
      bool t0equals1 = false;

      // t_step is actually the initial (absolute) length of step on the
      // interval [c_init,c_end]
      // dt is an increment for t on the interval [0,1]
      R.set(dt, t_step);
      C.subtract(dc, c_end, c_init);
      C.abs(abs2dc, dc);  // don't want to create new temporary elts: reusing dc
                          // and abs2dc
      R.divide(dt, dt, abs2dc);

      int predictor_successes = 0;
      int count = 0;  // number of steps
      int low_need_streak = 0;  // adaptive precision: steps in a row with spare bits
      int fail_streak = 0;      // adaptive precision: failed steps in a row that asked for more bits
      // track the real segment (1-t)*c0 + t*c1, a\in [0,1]
      while (status == PROCESSING and not t0equals1)
        {
          if (M2_numericalAlgebraicGeometryTrace > 3)
            {
              buffer o;
              R.elem_text_out(o, t0, true, false, false);
              std::cout << "t0 = " << o.str();
              o.reset();
              C.elem_text_out(o, c0, true, false, false);
              std::cout << ", c0 = " << o.str() << std::endl;
            }
          R.subtract(one_minus_t0, one, t0);
          if (R.compare_elems(dt, one_minus_t0) > 0)
            {
              R.set(dt, one_minus_t0);
              t0equals1 = true;
              C.subtract(dc, c_end, c0);
              C.set(c1, c_end);
            }
          else
            {
              C.subtract(dc, c_end, c0);
              C.mult(dc, dc, dt);
              C.divide(dc, dc, one_minus_t0);
              C.add(c1, c0, dc);
            }

          // PREDICTOR in: x0c0,dt
          //           out: dx
          /*  top-level code for Runge-Kutta-4
              dx1 := solveHxTimesDXequalsMinusHt(x0,t0);
              dx2 := solveHxTimesDXequalsMinusHt(x0+(1/2)*dx1*dt,t0+(1/2)*dt);
              dx3 := solveHxTimesDXequalsMinusHt(x0+(1/2)*dx2*dt,t0+(1/2)*dt);
              dx4 := solveHxTimesDXequalsMinusHt(x0+dx3*dt,t0+dt);
              (1/6)*dt*(dx1+2*dx2+2*dx3+dx4)
          */

          C.mult(one_half_dc, dc, one_half);

          // dx1
          submatrix(xc) = submatrix(x0c0);
          TIME(evaluateTime, mHxt.evaluate(xc, Hxt))

          LHS = submatrix(Hxt, 0, 0, n, n);
          RHS = submatrix(Hxt, 0, n, n, 1);
          MatrixOps::negateInPlace(RHSmat);

          TIME(solveLinearTime,
               linearSolve_success =
                   solveSquareNoAlloc(C, LHSmat, RHSmat, dx1, LUws, permws, absP, absMax));
          solveLinearCount++;

          // dx2
          if (linearSolve_success)
            {
              submatrix(dx1) *= one_half_dc;  // "dx1" := (1/2)*dx1*dt
              submatrix(xc, 0, 0, n, 1) += submatrix(dx1);
              C.add(c, c, one_half_dc);

              TIME(evaluateTime, mHxt.evaluate(xc, Hxt))

              LHS = submatrix(Hxt, 0, 0, n, n);
              RHS = submatrix(Hxt, 0, n, n, 1);
              MatrixOps::negateInPlace(RHSmat);

              TIME(solveLinearTime,
                   linearSolve_success =
                       solveSquareNoAlloc(C, LHSmat, RHSmat, dx2, LUws, permws, absP, absMax);)
              solveLinearCount++;
            }

          // dx3
          if (linearSolve_success)
            {
              submatrix(dx2) *= one_half_dc;  // "dx2" := (1/2)*dx2*dt
              submatrix(xc, 0, 0, n, 1) = submatrix(x0c0, 0, 0, n, 1);
              submatrix(xc, 0, 0, n, 1) += submatrix(dx2);
              // C.add(c,c,one_half_dc); // c should not change here??? or copy
              // c two lines above???

              TIME(evaluateTime, mHxt.evaluate(xc, Hxt));

              LHS = submatrix(Hxt, 0, 0, n, n);
              RHS = submatrix(Hxt, 0, n, n, 1);
              MatrixOps::negateInPlace(RHSmat);

              TIME(solveLinearTime,
                   linearSolve_success =
                       solveSquareNoAlloc(C, LHSmat, RHSmat, dx3, LUws, permws, absP, absMax););
              solveLinearCount++;
            }

          // dx4
          if (linearSolve_success)
            {
              submatrix(dx3) *= dc;  // "dx3" := dx3*dt
              submatrix(xc) = submatrix(
                  x0c0);  // sets c=c0 as well (not needed for dx2???,dx3)
              submatrix(xc, 0, 0, n, 1) += submatrix(dx3);
              C.add(c, c, dc);

              TIME(evaluateTime, mHxt.evaluate(xc, Hxt));

              LHS = submatrix(Hxt, 0, 0, n, n);
              RHS = submatrix(Hxt, 0, n, n, 1);
              MatrixOps::negateInPlace(RHSmat);

              TIME(solveLinearTime,
                   linearSolve_success =
                       solveSquareNoAlloc(C, LHSmat, RHSmat, dx4, LUws, permws, absP, absMax););
              solveLinearCount++;
            }

          // "dx1" = .5*dx1*dt, "dx2" = .5*dx2*dt, "dx3" = dx3*dt
          if (linearSolve_success)
            {
              submatrix(dx4) *= dc;  // "dx4" = dx4*dt
              submatrix(dx) =
                  submatrix(dx4);  // dx = (1/6)*dt*(dx1+2*dx2+2*dx3+dx4)
                                   //    = (1/6)*(2*"dx1"+4*"dx2"+2*"dx3"+"dx4")
              submatrix(dx1) *= two;
              submatrix(dx) += dx1;
              // submatrix(dx).addMultipleTo(two,dx1);
              submatrix(dx2) *= four;
              submatrix(dx) += dx2;
              submatrix(dx3) *= two;
              submatrix(dx) += dx3;
              submatrix(dx) *= one_sixth;

              // update x0c0
              submatrix(x1c1) = submatrix(x0c0);
              submatrix(x1c1, 0, 0, n, 1) += dx;
              C.add(c1, c0, dc);
            }

          // CORRECTOR
          bool is_successful;
          int n_corr_steps = 0;
          R.set_zero(jinv2);
          if (linearSolve_success)
            {
              do
                {
                  n_corr_steps++;

                  TIME(evaluateTime, mHxH.evaluate(x1c1, HxH));

                  LHS = submatrix(HxH, 0, 0, n, n);
                  RHS = submatrix(HxH, 0, n, n, 1);
                  MatrixOps::negateInPlace(RHSmat);
                  if (checkPrecision) normSquared(submatrix(RHSmat), h_norm2);

                  TIME(solveLinearTime,
                       linearSolve_success =
                           solveSquareNoAlloc(C, LHSmat, RHSmat, dx, LUws, permws, absP, absMax););
                  solveLinearCount++;

                  // x1 += dx
                  submatrix(x1c1, 0, 0, n, 1) += dx;

                  normSquared(submatrix(dx), dx_norm2);
                  if (checkPrecision and not R.is_zero(h_norm2))
                    {
                      // dx = -J^{-1} H(x1), so ||dx||/||H(x1)|| <= ||J^{-1}||
                      R.divide(ratio2, dx_norm2, h_norm2);
                      if (R.compare_elems(ratio2, jinv2) > 0) R.set(jinv2, ratio2);
                    }
                  normSquared(submatrix(x1c1, 0, 0, n, 1), x_norm2);

                  R.mult(x_norm2, x_norm2, tol2);
                  is_successful = R.compare_elems(dx_norm2, x_norm2) < 0;
                  if (M2_numericalAlgebraicGeometryTrace > 4)
                    std::cout << "  corrector " << n_corr_steps
                              << ": dt = " << R.coerceToDouble(dt)
                              << ", |H|^2 = " << R.coerceToDouble(h_norm2)
                              << ", |dx|^2 = " << R.coerceToDouble(dx_norm2)
                              << ", tol^2 |x|^2 = " << R.coerceToDouble(x_norm2)
                              << (is_successful ? " ok" : " no") << std::endl;
                }
              while (not is_successful and n_corr_steps < max_corr_steps);
            }
          // std::cout << "past corrector loop...\n";
          if (not linearSolve_success or not is_successful)
            {
              // predictor failure
              predictor_successes = 0;
              R.mult(dt, dt, dt_factor);
              t0equals1 = false;
              C.abs_squared(abs2dc, dc);
              if (R.compare_elems(abs2dc, min_step2) < 0)
                status = MIN_STEP_FAILED;
            }
          else
            {
              // predictor success
              predictor_successes = predictor_successes + 1;
              MatOps::setFromSubmatrix(x1c1, 0, n, 0, 0, x0c0);  // x1=x0
              R.add(t0, t0, dt);  // increment t: so far only s was incremented
              count++;
              if (predictor_successes >= num_successes_before_increase)
                {
                  predictor_successes = 0;
                  R.divide(dt, dt, dt_factor);
                }
            }

          normSquared(submatrix(x0c0, 0, 0, n, 1),
                      x_norm2);  // x_norm2 = ||x0||^2

          if (not linearSolve_success)
            status = SINGULAR;
          // infinity/origin checks: terminal states happen before the precision check 
          if (status == PROCESSING)
            {
              if (R.compare_elems(infinity_threshold2, x_norm2) < 0)
                status = INFINITY_FAILED;
              else
                {
                  if (R.is_zero(x_norm2))
                    status = ORIGIN_FAILED;
                  else
                    {
                      R.divide(inv_x_norm2, one, x_norm2);  // 1/||x||^2
                      if (R.compare_elems(infinity_threshold2, inv_x_norm2) < 0)
                        status = ORIGIN_FAILED;
                    }
                }
            }

          if (status == PROCESSING and checkPrecision and not t0equals1 and
              not R.is_zero(jinv2))
            {
              // Precision check, after Bates-Hauenstein-Sommese-Wampler
              // (adaptive multiprecision path tracking), with u = 2^-prec.
              // The corrector's Newton step gives a free lower bound
              // jinv = ||dx||/||H(x)|| <= ||J^{-1}||, and the evaluator gives
              // the bounds psiH, psiJ on the rounding error (in units of u) of
              // H and of the Jacobian at the current point.
              //  Rule C: the error u*(jinv*psiH + ||x||) of a Newton step
              //          must stay below the (relative) tolerance tol*||x||,
              //          i.e. prec > log2(1/tol) + log2(jinv*psiH/||x|| + 1).
              //  Rule A: the rounded Jacobian must stay nonsingular,
              //          u*jinv*psiJ < 1, i.e. prec > log2(jinv*psiJ).
              // The safety margin covers the lower-bound nature of jinv and
              // the constants of complex arithmetic not in the error model.
              double tol2d = R.coerceToDouble(tol2);
              int tolerance_bits = (tol2d > 0 and tol2d < 1)
                                       ? (int)std::ceil(-0.5 * std::log2(tol2d))
                                       : 0;
              double jinv = std::sqrt(R.coerceToDouble(jinv2));
              double xnorm = std::sqrt(R.coerceToDouble(x_norm2));
              double psiH = xnorm, psiJ = 0;  // fallback: H evaluated to ~u relative
              if (mHxH.evaluationErrorBound(evalErr))
                {
                  // HxH outputs are row-major n x (n+1): J then the column H.
                  // Max-norm bounds times the dimension factor (no squaring:
                  // the bounds can exceed 1e154 at a wild predictor point).
                  double hmax = 0, jmax = 0;
                  for (size_t i = 0; i < n; i++)
                    {
                      for (size_t jj = 0; jj < n; jj++)
                        jmax = std::max(jmax, evalErr[i * (n + 1) + jj]);
                      hmax = std::max(hmax, evalErr[i * (n + 1) + n]);
                    }
                  psiH = std::sqrt((double)n) * hmax;
                  psiJ = (double)n * jmax;
                }
              // bits needed to resolve an amplification factor; an infinite
              // or NaN bound (overflow in the error model) asks for more than
              // any precision on the ladder
              auto bitsFor = [](double amp) -> int {
                if (std::isnan(amp) or amp > 1e300) return 1 << 20;
                return (amp > 1) ? (int)std::ceil(std::log2(amp)) : 0;
              };
              double ampC = (xnorm > 0) ? jinv * psiH / xnorm + 1 : jinv * psiH;
              int condition_bits = bitsFor(ampC);
              int jacobian_bits = bitsFor(jinv * psiJ);
              int precision_needed =
                  PRECISION_SAFETY_BITS +
                  std::max(tolerance_bits + condition_bits, jacobian_bits);
              int prec = (int)R.get_precision();
              // the front end's precision ladder is 53, 128, 256, 512, ...
              int lower_prec = (prec <= 128) ? 53 : prec / 2;
              if (prec < precision_needed)
                {
                  // On an accepted step the verdict is trustworthy. On a
                  // failed step it was formed at the predictor's point, which
                  // may be wild if the step was too long, so cut the step
                  // (done above) and only escalate once several consecutive
                  // failures agree.
                  if (is_successful or ++fail_streak >= PRECISION_FAIL_STEPS)
                    status = INCREASE_PRECISION;
                  low_need_streak = 0;
                }
              else if (not is_successful)
                {
                  low_need_streak = 0;  // only accepted steps count toward a decrease
                  fail_streak = 0;
                }
              else if (prec > 53 and
                       lower_prec >= precision_needed + PRECISION_HYSTERESIS_BITS)
                {
                  // the next lower precision has spare bits; decrease only
                  // once this has held for several consecutive accepted steps
                  if (++low_need_streak >= PRECISION_LOW_NEED_STEPS)
                    status = DECREASE_PRECISION;
                  fail_streak = 0;
                }
              else
                {
                  low_need_streak = 0;
                  fail_streak = 0;
                }
              if (M2_numericalAlgebraicGeometryTrace > 3)
                std::cout << "precision needed = " << precision_needed << " = "
                          << PRECISION_SAFETY_BITS << "(safety) + max("
                          << tolerance_bits << "(tolerance) + "
                          << condition_bits << "(condition), "
                          << jacobian_bits << "(jacobian)), current = " << prec
                          << "; |J^-1| >= " << jinv << ", psiH = " << psiH
                          << ", psiJ = " << psiJ << ", |x| = " << xnorm
                          << ", low-need streak = " << low_need_streak
                          << ", fail streak = " << fail_streak
                          << ", status = " << status << std::endl;
            }

        }
      // record the solution
      // set initial solution and initial value of the continuation parameter
      for (size_t i = 0; i <= n; i++) C.set(ou.entry(i, s), x0c0.entry(i, 0));
      // store the increment to attempt next (the step size after the last
      // success or failure was accounted for): resuming the path at another
      // precision starts from it rather than repeating a step that just failed
      C.subtract(dc, c_end, c_init);
      C.mult(dc, dc, dt);
      C.set(ou.entry(n + 1, s), dc);
      if (status == PROCESSING) status = REGULAR;
      oe.ring().set_from_long(oe.entry(0, s), status);
      oe.ring().set_from_long(oe.entry(1, s), count);
      // Update progress bar: append the marks earned by this path (bar_width
      // marks in total; a path may earn more than one when n_sols < bar_width).
      if (show_bar) {
        int done = ++bar_done;
        int marks = done * bar_width / (int)n_sols
                    - (done - 1) * bar_width / (int)n_sols;
        if (marks > 0) {
          std::lock_guard<std::mutex> lk(bar_mutex);
          std::cerr << std::string(marks, '#') << std::flush;
        }
      }
    }
    if (!gc_was_registered)
      GC_unregister_my_thread();
  }, tbb::simple_partitioner{}); //(end) tbb::parallel_for
  }); //(end) arena.execute
  if (show_bar) std::cerr << "]" << std::endl;  // finish progress bar line
  
  std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
  if (M2_numericalAlgebraicGeometryTrace > 1)
    {
      std::cout << "-- numThreads = " << numThreads
                << ", grainSize = " << grainSize << std::endl;
      std::cout << "-- track took "
                << std::chrono::duration_cast<std::chrono::milliseconds>(end -
                                                                         start)
                       .count()
                << "ms.\n";
      std::cout << "-- # of solveLinear calls = " << solveLinearCount
                << std::endl;
      std::cout << "-- time of solveLinear calls = " << solveLinearTime
                << " ns." << std::endl;
      std::cout << "-- time of evaluate calls = " << evaluateTime << " ns."
                << std::endl;
    }
  return true;
}

template <typename RT, typename Algorithm>
bool HomotopyConcrete<RT, Algorithm>::track(
    const MutableMatrix* inputs,
    MutableMatrix* outputs,
    MutableMatrix* output_extras,
    gmp_RR init_dt,
    gmp_RR min_dt,
    gmp_RR epsilon,  // o.CorrectorTolerance,
    int max_corr_steps,
    gmp_RR infinity_threshold,
    bool checkPrecision)
{
  (void) inputs;
  (void) outputs;
  (void) output_extras;
  (void) init_dt;
  (void) min_dt;
  (void) epsilon;
  (void) max_corr_steps;
  (void) infinity_threshold;
  (void) checkPrecision;
  ERROR("track: not implemented for this type of ring");
  return false;
}

template <typename RT, typename Algorithm>
void HomotopyConcrete<RT, Algorithm>::text_out(buffer& o) const
{
  o << "HomotopyConcrete<...,...> : track not implemented" << newline;
}
template <typename RT>
HomotopyConcrete<RT, FixedPrecisionHomotopyAlgorithm>::HomotopyConcrete(
    HomotopyConcrete<RT, FixedPrecisionHomotopyAlgorithm>::EType& Hx,
    HomotopyConcrete<RT, FixedPrecisionHomotopyAlgorithm>::EType& Hxt,
    HomotopyConcrete<RT, FixedPrecisionHomotopyAlgorithm>::EType& HxH)
    : mHx(Hx), mHxt(Hxt), mHxH(HxH)
{
}

template <typename RT>
void HomotopyConcrete<RT, FixedPrecisionHomotopyAlgorithm>::text_out(
    buffer& o) const
{
  o << "HomotopyConcrete<...,fixed precision>(Hx = ";
  mHx.text_out(o);
  o << ", Hxt = ";
  mHxt.text_out(o);
  o << ", HxH = ";
  mHxH.text_out(o);
  o << ")" << newline;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
