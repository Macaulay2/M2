// Copyright 2015 Anton Leykin and Mike Stillman

// Anton Leykin's code in this file is in the public domain.

#ifndef _slp_imp_hpp_
#define _slp_imp_hpp_

// SLEvaluator
template<typename RT>
SLEvaluatorConcrete<RT>::SLEvaluatorConcrete(SLProgram *SLP, M2_arrayint cPos,  M2_arrayint vPos, 
					     const MutableMat< SMat<RT> >* consts /* DMat<RT>& DMat_consts */)
  : mRing(consts->getMat().ring())
{
  ERROR("not implemented");
}    

template<typename RT>
SLEvaluatorConcrete<RT>::SLEvaluatorConcrete(SLProgram *SLP, M2_arrayint cPos,  M2_arrayint vPos, 
					     const MutableMat< DMat<RT> >* consts /* DMat<RT>& DMat_consts */)
  : mRing(consts->getMat().ring())    
{
  slp = SLP;
  // std::cout << "in SLEvaluator::SLEvaluator" << std::endl;
  for(int i=0; i<cPos->len; i++) 
    constsPos.push_back(slp->inputCounter+cPos->array[i]);
  for(int i=0; i<vPos->len; i++) 
    varsPos.push_back(slp->inputCounter+vPos->array[i]);
  if (consts->n_rows() != 1 || consts->n_cols() != constsPos.size())
    ERROR("1-row matrix expected; or numbers of constants don't match");
  values.resize(slp->inputCounter+slp->mNodes.size());
  for(auto i=values.begin(); i!=values.end(); ++i)
    ring().init(*i);
  R = consts->get_ring();
  for (int i=0; i<constsPos.size(); i++) 
    ring().set(values[constsPos[i]],consts->getMat().entry(0,i));
}


template<typename RT>
void SLEvaluatorConcrete<RT>::computeNextNode()
{
  ElementType& v = *vIt;
  switch (*nIt++) {
  case SLProgram::MProduct:
    ring().set_from_long(v,1);
    for (int i=0; i<*numInputsIt; i++)
      ring().mult(v,v,values[ap(*inputPositionsIt++)]);
    numInputsIt++;
    break;
  case SLProgram::MSum:
    ring().set_zero(v);
    for (int i=0; i<*numInputsIt; i++)
      ring().add(v,v,values[ap(*inputPositionsIt++)]);
    numInputsIt++;
    break;
  case SLProgram::Det:
    {
      int n = static_cast<int>(sqrt(*numInputsIt++));
      DMat<RT> mat(ring(),n,n);
      for (int i=0; i<n; i++)
        for (int j=0; j<n; j++)
          ring().set(mat.entry(i,j),values[ap(*inputPositionsIt++)]);
      DMatLinAlg<RT>(mat).determinant(v);      
    }
    break;
  case SLProgram::Divide:
    ring().set(v,values[ap(*inputPositionsIt++)]);
    ring().divide(v,v,values[ap(*inputPositionsIt++)]);
    break;
  default: ERROR("unknown node type");
  }
  /*  
  ElementType v;
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
      int n = static_cast<int>(sqrt(*numInputsIt++));
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
  */  
}

template<typename RT>
bool SLEvaluatorConcrete<RT>::evaluate(const MutableMatrix* inputs, MutableMatrix* outputs)
{
  if (R != inputs->get_ring()) { 
    ERROR("inputs are in a different ring");
    return false;
  }
  if (R != outputs->get_ring()) { 
    ERROR("outputs are in a different ring");
    return false;
  }
  if (inputs->n_rows() != 1 || inputs->n_cols() != varsPos.size()) {
    ERROR("1-row matrix expected; or numbers of inputs and vars don't match");
    return false;
  }
  if (outputs->n_rows() != 1 || outputs->n_cols() != slp->mOutputPositions.size()) {
    ERROR("1-row matrix expected; or number of outputs doesn't match");
    return false;
  }

  auto inp = dynamic_cast<const MutableMat< DMat<RT> >*>(inputs);
  auto out = dynamic_cast<MutableMat< DMat<RT> >*>(outputs);
  if (inp == nullptr) { 
    ERROR("inputs: expected a dense mutable matrix");
    return false;
  }
  if (out == nullptr) { 
    ERROR("inputs: expected a dense mutable matrix");
    return false;
  }

  for (int i=0; i<varsPos.size(); i++) 
    ring().set(values[varsPos[i]],inp->getMat().entry(0,i));
  
  nIt = slp->mNodes.begin();
  numInputsIt = slp->mNumInputs.begin();
  inputPositionsIt = slp->mInputPositions.begin();
  for (vIt = values.begin()+slp->inputCounter; vIt != values.end(); ++vIt) 
    computeNextNode();

  for(int i = 0; i < slp->mOutputPositions.size(); i++)
    ring().set(out->getMat().entry(0,i),values[ap(slp->mOutputPositions[i])]);
  return true;
}

template<typename RT>
void SLEvaluatorConcrete<RT>::text_out(buffer& o) const { 
  o << "SLEvaluator(slp = ";
  slp->text_out(o);
  o << ", mRing = ";
  ring().text_out(o);
  o << ")" << newline; 
}

template <typename RT>
Homotopy* SLEvaluatorConcrete<RT>::createHomotopy(SLEvaluator* Hxt, SLEvaluator* HxH)
{
  auto castHxt = dynamic_cast < SLEvaluatorConcrete < RT > * > (Hxt);
  auto castHxH = dynamic_cast < SLEvaluatorConcrete < RT > * > (HxH);
  if (not castHxt or not castHxH) { 
    ERROR("expected SLEvaluators in the same ring");
    return nullptr;
  } 
  return new HomotopyConcrete<RT>(*this, *castHxt, *castHxH);
}

template <typename RT> 
bool HomotopyConcrete<RT>::track(const MutableMatrix* inputs, MutableMatrix* outputs, 
                     M2_arrayint output_status,  
                     gmp_RR init_dt, gmp_RR min_dt,
                     gmp_RR epsilon, // o.CorrectorTolerance,
                     int max_corr_steps, 
                     gmp_RR infinity_threshold
                   ) 
{
  // double the_smallest_number = 1e-13;
  const Ring* R = inputs->get_ring();
  if (outputs->get_ring()!= R) { 
    ERROR("outputs and inputs are in different rings");
    return false;
  }
  /*
  double epsilon2 = mpfr_get_d(epsilon,GMP_RNDN); epsilon2 *= epsilon2; //epsilon^2
  double t_step = mpfr_get_d(init_dt,GMP_RNDN); // initial step
  double dt_min_dbl = mpfr_get_d(min_dt,GMP_RNDN);
  double dt_increase_factor_dbl = mpfr_get_d(dt_increase_factor,GMP_RNDN);
  double dt_decrease_factor_dbl = mpfr_get_d(dt_decrease_factor,GMP_RNDN);
  double infinity_threshold2 = mpfr_get_d(infinity_threshold,GMP_RNDN); infinity_threshold2 *= infinity_threshold2;
  double end_zone_factor_dbl = mpfr_get_d(end_zone_factor,GMP_RNDN);
  */
  return true;
}

template<typename RT>
void HomotopyConcrete<RT>::text_out(buffer& o) const { 
  o << "HomotopyConcrete(Hx = ";
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
