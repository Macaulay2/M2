// Copyright 2015 Anton Leykin and Mike Stillman

// Anton Leykin's code in this file is in the public domain.

#ifndef _slp_defs_hpp_
#define _slp_defs_hpp_

/**
 * @file SLP-defs.hpp
 * @brief Type declarations for the SLP DAG, its evaluator hierarchy, and the homotopy abstraction.
 *
 * Declares the type lattice the templated SLP machinery is built
 * on:
 *
 * - `SLProgram` --- the straight-line program itself, a DAG of
 *   arithmetic gates (`Copy`, `MCopy`, `Sum`, `Product`, `MSum`,
 *   `MProduct`, `Det`, `Divide`) over a fixed set of inputs and
 *   constants, with `mNodes`, `mNumInputs`, `mInputPositions`, and
 *   `mOutputPositions` storing the wired topology.
 * - `SLEvaluator` and `SLEvaluatorConcrete<RT>` --- the abstract
 *   evaluator and its templated specialisation that walks the
 *   program with a `std::vector<RT::ElementType>` of node values;
 *   the template implementations of `evaluate` /
 *   `createHomotopy` live in `SLP-imp.hpp`.
 * - `Homotopy` and `HomotopyConcrete<RT, Algorithm>` --- the
 *   predictor-corrector path-tracker abstraction, with two
 *   `Algorithm` tag types (`TrivialHomotopyAlgorithm`,
 *   `FixedPrecisionHomotopyAlgorithm`,
 *   `VariablePrecisionHomotopyAlgorithm`) and a
 *   `HomotopyAlgorithm<RT>` traits class that maps each numeric
 *   ring (`ARingCC`, `ARingCCC`) to its preferred algorithm.
 * - The three matching `MutableEngineObject` wrappers
 *   `M2SLProgram`, `M2SLEvaluator`, `M2Homotopy` --- owned via
 *   `std::unique_ptr` / raw pointer and exposing `value()` as
 *   the back door for engine code that needs the unwrapped
 *   object.
 *
 * The wrappers exist so the templated machinery does not leak
 * through M2-visible names: the interpreter handles
 * `M2SLProgram*` / `M2SLEvaluator*` / `M2Homotopy*` as opaque
 * pointers while the numerical specialisation does the actual
 * work behind them.
 *
 * @see SLP.hpp
 * @see SLP-imp.hpp
 * @see NAG.hpp
 */

// SLP
class SLProgram;

/**
 * @brief `MutableEngineObject` wrapper that owns an `SLProgram` via `unique_ptr`.
 *
 * @details Exposes the unwrapped DAG through `value()` so engine code that holds
 * an `M2SLProgram*` can reach the actual program without leaking its
 * concrete type to interpreter callers.
 */
class M2SLProgram : public MutableEngineObject
{
  std::unique_ptr<SLProgram> mSLProgram;
public:
  M2SLProgram(SLProgram* pa) : mSLProgram(pa) {}

  SLProgram& value() { return *mSLProgram; }
};

/**
 * @brief A straight-line program: a directed acyclic graph of arithmetic gates
 * over a fixed list of inputs and constants.
 *
 * @details The DAG topology is stored in four parallel vectors (`mNodes`,
 * `mNumInputs`, `mInputPositions`, `mOutputPositions`) that wire each
 * gate to its operand positions. Gate kinds are listed in
 * `GATE_TYPE` (`Copy`, `MCopy`, `Sum`, `Product`, `MSum`, `MProduct`,
 * `Det`, `Divide`). Positions are relative non-negative indices into
 * `mNodes` for gate references and negative indices for variables and
 * constants, except in `mOutputPositions` which uses absolute indices.
 * Evaluation against a concrete coefficient ring lives in
 * `SLEvaluatorConcrete<RT>`.
 */
class SLProgram
{
 public:
  enum GATE_TYPE { Copy, MCopy, Sum, Product, MSum, MProduct, Det, Divide };
  typedef int GATE_SIZE;
  typedef int GATE_POSITION;  // gate position is RELATIVE (exception: ABSOLUTE
                              // for mOutputPositions)
  std::vector<GATE_TYPE> mNodes;      // nodes types
  std::vector<GATE_SIZE> mNumInputs;  // corresponding nodes sizes
  std::vector<GATE_POSITION>
  mInputPositions; /* which nodes does input come from?
                      !!! this vector could be longer than mNodes !!!
                      !!! since there could be several inputs per node !!!
                      (nonnegative = node position,
                      negative = var or const) */
  std::vector<GATE_POSITION> mOutputPositions; /* which nodes are outputs
                                                  (nonnegative = node position,
                                                  negative = var or const) */
  /* LOOKUP TABLE */
  int inputCounter;  // this is the count; the position numbering is -1, -2, ...
 public:
  SLProgram();
  virtual ~SLProgram();
  // GATE_POSITION addCopy(GATE_POSITION p);
  // GATE_POSITION addMCopy(GATE_POSITION p, GATE_SIZE s);
  // GATE_POSITION addSum(GATE_POSITION a, GATE_POSITION b);
  // GATE_POSITION addProduct(GATE_POSITION a, GATE_POSITION b);
  
  // !!! replace M2_arrayint with std::vector (M2_arrayint pertains to front end) 
  GATE_POSITION addInput() { return -(++inputCounter); }
  GATE_POSITION addMSum(const M2_arrayint);
  GATE_POSITION addMProduct(const M2_arrayint);
  GATE_POSITION addDet(const M2_arrayint);
  GATE_POSITION addDivide(const M2_arrayint);
  void setOutputPositions(const M2_arrayint);
  void text_out(buffer&) const;
};

class Homotopy;

/**
 * @brief `MutableEngineObject` wrapper that owns a `Homotopy` via `unique_ptr`.
 *
 * @details Mirrors `M2SLProgram` / `M2SLEvaluator`: the interpreter handles an
 * opaque `M2Homotopy*` while the templated `HomotopyConcrete<RT,
 * Algorithm>` does the numerical path tracking behind `value()`.
 */
// needs a finalizer???
class M2Homotopy : public MutableEngineObject
{
  std::unique_ptr<Homotopy> mHomotopy;
public:
  M2Homotopy(Homotopy* pa) : mHomotopy(pa) {}

  Homotopy& value() { return *mHomotopy; }
};

/**
 * @brief Tag type selecting the no-op homotopy algorithm.
 *
 * @details Used as the `Algorithm` template parameter of
 * `HomotopyConcrete<RT, Algorithm>` for rings that have no
 * specialised tracker; the corresponding `track()` body is the
 * fallback implementation.
 */
class TrivialHomotopyAlgorithm
{
};
/**
 * @brief Tag type selecting the fixed-precision homotopy algorithm.
 *
 * @details Picked by `HomotopyAlgorithm<RT>` for `M2::ARingCC` and
 * `M2::ARingCCC`, where path tracking runs in a single working
 * precision rather than adapting.
 */
class FixedPrecisionHomotopyAlgorithm
{
};
/**
 * @brief Tag type selecting the variable-precision homotopy algorithm.
 *
 * @details Reserved for adaptive-precision tracking; not yet wired into
 * `HomotopyAlgorithm<RT>` for any current ring.
 */
class VariablePrecisionHomotopyAlgorithm
{
};

/**
 * @brief Traits class mapping a coefficient ring `RT` to its preferred
 * homotopy algorithm tag.
 *
 * @details The primary template selects `TrivialHomotopyAlgorithm`; explicit
 * specialisations override that choice for specific numeric rings.
 * `SLEvaluatorConcrete<RT>::createHomotopy` consults
 * `HomotopyAlgorithm<RT>::Algorithm` to pick the right
 * `HomotopyConcrete` specialisation.
 */
template <typename RT>
struct HomotopyAlgorithm
{
  typedef TrivialHomotopyAlgorithm Algorithm;
};
/**
 * @brief Selects `FixedPrecisionHomotopyAlgorithm` for the double-precision
 * complex ring `M2::ARingCC`.
 */
template <>
struct HomotopyAlgorithm<M2::ARingCC>
{
  typedef FixedPrecisionHomotopyAlgorithm Algorithm;
};
/**
 * @brief Selects `FixedPrecisionHomotopyAlgorithm` for the arbitrary-precision
 * complex ring `M2::ARingCCC`.
 */
template <>
struct HomotopyAlgorithm<M2::ARingCCC>
{
  typedef FixedPrecisionHomotopyAlgorithm Algorithm;
};
/*
template<>
struct HomotopyAlgorithm<M2::ARingRR> {
  typedef FixedPrecisionHomotopyAlgorithm Algorithm;
};
template<>
struct HomotopyAlgorithm<M2::ARingRRR> {
  typedef FixedPrecisionHomotopyAlgorithm Algorithm;
};
*/

class SLEvaluator;

/**
 * @brief `MutableEngineObject` wrapper holding a raw `SLEvaluator*`.
 *
 * @details Stores a raw pointer rather than `unique_ptr` --- per the inline
 * comment, this is a deliberate leak that avoids a heap-corruption bug
 * triggered when ownership is transferred. `value()` returns the
 * underlying abstract evaluator for engine code that needs to call
 * `evaluate` / `specialize` directly.
 */
class M2SLEvaluator : public MutableEngineObject
{
  SLEvaluator* mSLEvaluator; //!!! this is a hack to avoid memory corruption, it results in a memory leak
  // std::unique_ptr<SLEvaluator> mSLEvaluator;
public:
  M2SLEvaluator(SLEvaluator* pa) : mSLEvaluator(pa) {}

  SLEvaluator& value() { return *mSLEvaluator; }
};

/**
 * @brief Abstract base for the SLP evaluator hierarchy.
 *
 * @details Holds an `SLProgram*` plus iterators into its gate arrays and
 * declares the pure-virtual interface (`evaluate`, `specialize`,
 * `createHomotopy`, `text_out`) used by interpreter callers. Concrete
 * arithmetic happens in `SLEvaluatorConcrete<RT>`, which carries the
 * `std::vector<RT::ElementType>` of node values and (optionally) a
 * compiled function pointer.
 */
class SLEvaluator
{
 public:
  virtual ~SLEvaluator() {}
  virtual SLEvaluator* specialize(const MutableMatrix* parameters) const = 0;
  virtual bool evaluate(const MutableMatrix* inputs,
                        MutableMatrix* outputs) = 0;
  virtual void text_out(buffer& o) const = 0;
  virtual Homotopy* createHomotopy(SLEvaluator* Hxt, SLEvaluator* HxH) = 0;

 protected:
  int ap(int rp) { return rp + slp->inputCounter; }  // absolute position

  SLProgram* slp; //!!! can we make it a reference???
  std::vector<SLProgram::GATE_POSITION> varsPos;  // the rest of inputs with neg rel position
  std::vector<SLProgram::GATE_TYPE>::iterator nIt;  // slp nodes
  std::vector<SLProgram::GATE_SIZE>::iterator numInputsIt;
  std::vector<SLProgram::GATE_POSITION>::iterator inputPositionsIt;
};

template <typename RT>
class SLEvaluatorConcrete : public SLEvaluator
{
 public:
  SLEvaluatorConcrete(const SLEvaluatorConcrete<RT>&);  // copy constructor
  SLEvaluatorConcrete(
      SLProgram* SLP,
      M2_arrayint constsPos,
      M2_arrayint varsPos,
      const MutableMat<DMat<RT> >* consts /*const DMat<RT>& DMat_consts */);
  SLEvaluatorConcrete(
      SLProgram* SLP,
      M2_arrayint constsPos,
      M2_arrayint varsPos,
      const MutableMat<SMat<RT> >* consts /*const SMat<RT>& consts*/);
  SLEvaluatorConcrete(
      M2_string libName,
      int nInputs,
      int nOutputs,
      const MutableMat<DMat<RT> >* empty
      );
  SLEvaluatorConcrete(
      M2_string libName,
      int nInputs,
      int nOutputs,
      const MutableMat<SMat<RT> >* empty
      );
  ~SLEvaluatorConcrete();
  SLEvaluator* specialize(const MutableMatrix* parameters) const;
  SLEvaluator* specialize(const MutableMat<DMat<RT> >* parameters) const;
  const RT& ring() const { return mRing; }
  bool evaluate(const MutableMatrix* inputs, MutableMatrix* outputs);
  bool evaluate(const DMat<RT>& inputs, DMat<RT>& outputs);
  // TODO: bool evaluate(DMat<RT>& inputs, DMat<RT>& outputs);
  void text_out(buffer& o) const;
  Homotopy* createHomotopy(SLEvaluator* Hxt, SLEvaluator* HxH);

 private:
  void computeNextNode();  // !!! should this and vIt be here???
  using ElementType = typename RT::ElementType;
  typename std::vector<ElementType>::iterator vIt;  // values

  // common data
  const RT& mRing;
  bool isCompiled;
  int nInputs, nOutputs;
  
  // data used by interpreted evaluation
  std::vector<ElementType> values; /* should be a vector of values
                                      starting with inputCounter many vars and consts and
                                      continuing with the values of other GATEs */

  // data used by compiled evaluation
  void (*compiled_fn)(ElementType const*, ElementType*);  //void (*compiled_fn)(double const*, double*);
  int nParams;
  ElementType* parametersAndInputs;  
};

/**
 * @brief Abstract base for the predictor-corrector path-tracker hierarchy.
 *
 * @details The single virtual entry point `track()` walks columns of `inputs`
 * (each carrying an initial solution plus the start value of the
 * continuation parameter `t` in its last coordinate) toward target
 * values, writing the result into `outputs` and per-path status into
 * `output_extras`. Implementation lives in
 * `HomotopyConcrete<RT, Algorithm>` with `Algorithm` chosen by the
 * `HomotopyAlgorithm<RT>` traits class above.
 */
class Homotopy : public MutableEngineObject
{
 public:
  virtual ~Homotopy() {}
  virtual bool track(const MutableMatrix* inputs,
                     MutableMatrix* outputs,
                     MutableMatrix* output_extras,
                     gmp_RR init_dt,
                     gmp_RR min_dt,
                     gmp_RR epsilon,  // o.CorrectorTolerance,
                     int max_corr_steps,
                     gmp_RR infinity_threshold,
                     bool checkPrecision) = 0;
  virtual void text_out(buffer& o) const = 0;
};

template <typename RT, typename Algorithm>
class HomotopyConcrete : public Homotopy
{
 public:
  typedef SLEvaluatorConcrete<RT> EType;

  HomotopyConcrete(EType& Hx, EType& Hxt, EType& HxH)
      : mHx(Hx), mHxt(Hxt), mHxH(HxH)
  {
  }
  /* columns of inputs are initial solutions (last coordinate is the initial
     value of continuation parameter t,
     outputs have the same shape as inputs (last coordinate of outputs is set to
     the desired value of t),
     output_extras: the first row gives the status of the solutions (or path) */

  bool track(const MutableMatrix* inputs,
             MutableMatrix* outputs,
             MutableMatrix* output_extras,
             gmp_RR init_dt,
             gmp_RR min_dt,
             gmp_RR epsilon,  // o.CorrectorTolerance,
             int max_corr_steps,
             gmp_RR infinity_threshold,
             bool checkPrecision);
  void text_out(buffer& o) const;

 private:
  EType &mHx, &mHxt, &mHxH;
  // struct Evaluators {SLEvaluator *mHx, *mHxt, *mHxH;};
  // std::vector<Evaluators> mE; // a vector of evaluators increasing in
  // precision
  // std::vector<Ring*> mR; // a vector of available rings (corresponding to
  // mE?)
};

template <typename RT>
class HomotopyConcrete<RT, FixedPrecisionHomotopyAlgorithm> : public Homotopy
{
 public:
  typedef SLEvaluatorConcrete<RT> EType;
  HomotopyConcrete(EType& Hx, EType& Hxt, EType& HxH);
  /* columns of inputs are initial solutions (last coordinate is the initial
     value of continuation parameter t,
     outputs have the same shape as inputs (last coordinate of outputs is set to
     the desired value of t),
     output_extras: the first row gives the status of the solutions (or path) */
  bool track(const MutableMatrix* inputs,
             MutableMatrix* outputs,
             MutableMatrix* output_extras,
             gmp_RR init_dt,
             gmp_RR min_dt,
             gmp_RR epsilon,  // o.CorrectorTolerance,
             int max_corr_steps,
             gmp_RR infinity_threshold,
             bool checkPrecision);
  void text_out(buffer& o) const;

 private:
  EType &mHx, &mHxt, &mHxH;
  // struct Evaluators {SLEvaluator *mHx, *mHxt, *mHxH;};
  // std::vector<Evaluators> mE; // a vector of evaluators increasing in
  // precision
  // std::vector<Ring*> mR; // a vector of available rings (corresponding to
  // mE?)
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
