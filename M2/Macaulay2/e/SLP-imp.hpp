// Copyright 2015 Anton Leykin and Mike Stillman

// Anton Leykin's code in this file is in the public domain.

#ifndef _slp_imp_hpp_
#define _slp_imp_hpp_

// SLEvaluator

template<typename RT>
SLEvaluatorConcrete<RT>::SLEvaluatorConcrete(SLProgram *SLP, M2_arrayint cPos,  M2_arrayint vPos, const DMat<RT>& DMat_consts)  
{
  Matrix* consts =  "how to make Matrix from DMat<RT>";
  slp = SLP;
  //  std::cout << "in SLEvaluator::SLEvaluator" << std::endl;
  for(int i=0; i<cPos->len; i++) 
    constsPos.push_back(slp->inputCounter+cPos->array[i]);
  for(int i=0; i<vPos->len; i++) 
    varsPos.push_back(slp->inputCounter+vPos->array[i]);
  if (consts.n_rows() != 1 || consts.n_cols() != constsPos.size())
    ERROR("1-row matrix expected; or numbers of constants don't match");
  R = consts.get_ring();
  values.resize(slp->inputCounter+slp->mNodes.size());
  for (int i=0; i<constsPos.size(); i++) 
    values[constsPos[i]] = R->copy(consts.elem(0,i));
}


template<typename RT>
void SLEvaluatorConcrete<RT>::computeNextNode()
{
  ring_elem v;
  switch (*nIt++) {
  case SLProgram::MProduct:
    v = R->one();
    for (int i=0; i<*numInputsIt; i++)
      R->mult_to(v,values[ap(*inputPositionsIt++)]);
    numInputsIt++;
    break;
  case SLProgram::MSum:
    v = R->zero();
    for (int i=0; i<*numInputsIt; i++)
      R->add_to(v,values[ap(*inputPositionsIt++)]);
    numInputsIt++;
    break;
  case SLProgram::Det:
    {
      int n = sqrt(*numInputsIt++);
      FreeModule* S = R->make_FreeModule(n);
      MatrixConstructor mat(S,S);
      for (int i=0; i<n; i++)
        for (int j=0; j<n; j++)
          mat.set_entry(i,j,values[ap(*inputPositionsIt++)]);
      MutableMatrix* M = MutableMatrix::from_matrix(mat.to_matrix(), true); 
      v = M->determinant()->get_value();
      delete M;
    }
    break;
  case SLProgram::Divide:
    v = values[ap(*inputPositionsIt++)];
    v = R->divide(v,values[ap(*inputPositionsIt++)]);
    break;
  default: ERROR("unknown node type");
  }
  *vIt = v;  
}

template<typename RT>
bool SLEvaluatorConcrete<RT>::evaluate(const MutableMatrix* inputs, MutableMatrix* outputs)
{
  if (R != inputs->get_ring()) { 
    ERROR("inputs are in a different ring");
    return nullptr;
  }
  if (inputs->n_rows() != 1 || inputs->n_cols() != varsPos.size()) {
    ERROR("1-row matrix expected; or numbers of inputs and vars don't match");
    return nullptr;
  }
  for (int i=0; i<varsPos.size(); i++) 
    values[varsPos[i]] = R->copy(inputs->elem(0,i));
  // values[varsPos[i]] = inputs->elem(0,i); // should work
  
  nIt = slp->mNodes.begin();
  numInputsIt = slp->mNumInputs.begin();
  inputPositionsIt = slp->mInputPositions.begin();
  for (vIt = values.begin()+slp->inputCounter; vIt != values.end(); ++vIt) 
    computeNextNode();

  FreeModule* S = R->make_FreeModule(slp->mOutputPositions.size());
  FreeModule* T = R->make_FreeModule(1);
  MatrixConstructor mat(T,S);
  for(int i = 0; i < slp->mOutputPositions.size(); i++)
    mat.set_entry(0,i,values[ap(slp->mOutputPositions[i])]);
  // outputs = mat.to_matrix();
  outputs = mat.clone();
  return true;
}


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
