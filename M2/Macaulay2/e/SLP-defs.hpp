// Copyright 2015 Anton Leykin and Mike Stillman

// Anton Leykin's code in this file is in the public domain.

#ifndef _slp_defs_hpp_
#define _slp_defs_hpp_

// SLP
class SLProgram 
{
  //  friend class SLEvaluator;
public:
  enum GATE_TYPE {Copy, MCopy, Sum, Product, MSum, MProduct, Det, Divide};
  typedef int GATE_SIZE;
  typedef int GATE_POSITION; // gate position is ABSOLUTE
  std::vector<GATE_TYPE> mNodes; // nodes types
  std::vector<GATE_SIZE> mNumInputs; // corresponding nodes sizes
  std::vector<GATE_POSITION> mInputPositions;/* which nodes does input come from?
                                                !!! this vector could be longer than mNodes !!! 
                                                !!! since there could be several inputs per node !!!
                                                (nonnegative = node position, 
                                                negative = var or const) */
  std::vector<GATE_POSITION> mOutputPositions; /* which nodes are outputs
                                                  (nonnegative = node position, 
                                                  negative = var or const) */
  /* LOOKUP TABLE */
  int inputCounter; // this is the count; the position numbering is -1, -2, ...
public:
  SLProgram();
  virtual ~SLProgram();
  // GATE_POSITION addCopy(GATE_POSITION p);
  // GATE_POSITION addMCopy(GATE_POSITION p, GATE_SIZE s);
  // GATE_POSITION addSum(GATE_POSITION a, GATE_POSITION b);
  // GATE_POSITION addProduct(GATE_POSITION a, GATE_POSITION b);
  GATE_POSITION addInput() { return -(++inputCounter); }
  GATE_POSITION addMSum(const M2_arrayint);
  GATE_POSITION addMProduct(const M2_arrayint);
  GATE_POSITION addDet(const M2_arrayint);
  GATE_POSITION addDivide(const M2_arrayint);
  void setOutputPositions(const M2_arrayint); 
  void text_out(buffer&) const;
};

class SLEvaluator {
public:
  virtual bool evaluate(const MutableMatrix* inputs, MutableMatrix* outputs) = 0;
  virtual void text_out(buffer& o) const = 0;
protected:
  int ap(int rp) { return rp+slp->inputCounter; } // absolute position
  SLProgram* slp;
  std::vector<SLProgram::GATE_POSITION> constsPos; // absolute position of consts in mValues (slp.inputCounter + rel position) 
  std::vector<SLProgram::GATE_POSITION> varsPos; // the rest of inputs with neg rel position
  std::vector<SLProgram::GATE_TYPE>::iterator nIt; // slp nodes
  std::vector<SLProgram::GATE_SIZE>::iterator numInputsIt; 
  std::vector<SLProgram::GATE_POSITION>::iterator inputPositionsIt;
};

template<typename RT>
class SLEvaluatorConcrete : public SLEvaluator
{
public:
  SLEvaluatorConcrete(SLProgram *SLP, M2_arrayint constsPos, M2_arrayint varsPos, 
		      const MutableMat< DMat<RT> >* consts /*const DMat<RT>& DMat_consts */);
  SLEvaluatorConcrete(SLProgram *SLP, M2_arrayint constsPos, M2_arrayint varsPos, 
		      const MutableMat< SMat<RT> >* consts /*const SMat<RT>& consts*/);
  const RT& ring() const { return mRing; }
  bool evaluate(const MutableMatrix* inputs, MutableMatrix* outputs);
  // TODO: bool evaluate(DMat<RT>& inputs, DMat<RT>& outputs);
  void text_out(buffer& o) const;
private:
  //typedef ring_elem ElementType; 
  typedef typename RT::ElementType ElementType; 
  void computeNextNode();
  const Ring* R;
  std::vector<ElementType> values; /* should be a vector of values 
                              starting with inputCounter many vars and consts and 
                              continuing with the values of other GATEs */  
  typename std::vector<ElementType>::iterator vIt; // values
  const RT& mRing;
};

class Homotopy {
public:
  Homotopy(SLEvaluator& Hx, SLEvaluator& Hxt, SLEvaluator& HxH) : masterHx(Hx), masterHxt(Hxt), masterHxH(HxH) { }
  virtual bool track(const MutableMatrix* inputs, MutableMatrix* outputs, 
                     gmp_RR init_dt, gmp_RR min_dt,
                     gmp_RR epsilon, // o.CorrectorTolerance,
                     int max_corr_steps, 
                     gmp_RR infinity_threshold
                     );
  virtual void text_out(buffer& o) const;
private:
  const SLEvaluator &masterHx, &masterHxt, &masterHxH;
  struct Evaluators {SLEvaluator *mHx, *mHxt, *mHxH;};
  std::vector<Evaluators> mE; // a vector of evaluators increasing in precision 
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
