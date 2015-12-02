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
  auto inp = dynamic_cast<const MutableMat< DMat<RT> >*>(inputs);
  auto out = dynamic_cast<MutableMat< DMat<RT> >*>(outputs);
  if (inp == nullptr) { 
    ERROR("inputs: expected a dense mutable matrix");
    return false;
  }
  if (out == nullptr) { 
    ERROR("outputs: expected a dense mutable matrix");
    return false;
  }
  
  return evaluate(inp->getMat(),out->getMat());
}


template<typename RT> 
bool SLEvaluatorConcrete<RT>::evaluate(const DMat<RT>& inputs, DMat<RT>& outputs)
{
  if (varsPos.size() != inputs.numRows()*inputs.numColumns()) { 
    ERROR("inputs: the number of outputs does not match the number of entries in the outputs matrix");
    return false;
  }
  size_t i=0;
  for(size_t r=0; r<inputs.numRows(); r++)
    for(size_t c=0; c<inputs.numColumns(); c++)
      ring().set(values[varsPos[i++]],inputs.entry(r,c));
  
  nIt = slp->mNodes.begin();
  numInputsIt = slp->mNumInputs.begin();
  inputPositionsIt = slp->mInputPositions.begin();
  for (vIt = values.begin()+slp->inputCounter; vIt != values.end(); ++vIt) 
    computeNextNode();

  if (slp->mOutputPositions.size() != outputs.numRows()*outputs.numColumns()) { 
    ERROR("outputs: the number of outputs does not match the number of entries in the outputs matrix");
    // std::cout <<  slp->mOutputPositions.size() << " != " << outputs.numRows() << " * " << outputs.numColumns() << std::endl;
    return false;
  }
  i=0;
  for(size_t r=0; r<outputs.numRows(); r++)
    for(size_t c=0; c<outputs.numColumns(); c++)
      ring().set(outputs.entry(r,c),values[ap(slp->mOutputPositions[i++])]);
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


template <> 
inline bool HomotopyConcrete<M2::ARingCC>::track(const MutableMatrix* inputs, MutableMatrix* outputs, 
                     MutableMatrix* output_extras,  
                     gmp_RR init_dt, gmp_RR min_dt,
                     gmp_RR epsilon, // o.CorrectorTolerance,
                     int max_corr_steps, 
                     gmp_RR infinity_threshold
                   ) 
{
  std::cout << "inside HomotopyConcrete<M2::ARingCC>::track" << std::endl;
  // double the_smallest_number = 1e-13;
  const Ring* R = inputs->get_ring();
  if (outputs->get_ring()!= R) { 
    ERROR("outputs and inputs are in different rings");
    return false;
  }
  auto inp = dynamic_cast<const MutableMat< DMat<M2::ARingCC> >*>(inputs);
  auto out = dynamic_cast<MutableMat< DMat<M2::ARingCC> >*>(outputs);
  if (inp == nullptr) { 
    ERROR("inputs: expected a dense mutable matrix");
    return false;
  }
  if (out == nullptr) { 
    ERROR("outputs: expected a dense mutable matrix");
    return false;
  }

  double t_step = mpfr_get_d(init_dt,GMP_RNDN); // initial step
  double dt_min_dbl = mpfr_get_d(min_dt,GMP_RNDN);
  double epsilon2 = mpfr_get_d(epsilon,GMP_RNDN); epsilon2 *= epsilon2; //epsilon^2
  double infinity_threshold2 = mpfr_get_d(infinity_threshold,GMP_RNDN); infinity_threshold2 *= infinity_threshold2;
  /*
  for(int sol_n =0; sol_n<n_sols; sol_n++, s_s+=n, t_s++) {
    t_s->make(n,s_s); // cook a Solution
    t_s->status = PROCESSING;
    bool end_zone = false;
    double tol2 = epsilon2; // current tolerance squared, will change in end zone
    copy_complex_array<ComplexField>(n,s_s,x0);
    *t0 = complex(0,0);

    *dt = complex(t_step);
    int predictor_successes = 0;
    int count = 0; // number of steps
    while (t_s->status == PROCESSING && 1 - t0->getreal() > the_smallest_number) {
      if (dt->getreal() > 1 - t0->getreal() )
        *dt = complex(1);
  
      // PREDICTOR in: x0t0,dt,pred_type
      //           out: dx

      // make prediction
      copy_complex_array<ComplexField>(n+1,x0t0,x1t1);
      //      add_to_complex_array<ComplexField>(n+1,x1t1,dxdt);

      // CORRECTOR
      int n_corr_steps = 0;
      bool is_successful;
      do {
        n_corr_steps++;
        //
        evaluate_slpHxH(n,x1t1,HxH);
        LHS = HxH;
        RHS = HxH+n*n; // i.e., H
        //
        negate_complex_array<ComplexField>(n,RHS);
        LAPACK_success = LAPACK_success && solve_via_lapack_without_transposition(n,LHS,1,RHS,dx);
        add_to_complex_array<ComplexField>(n,x1t1,dx);
        is_successful = norm2_complex_array<ComplexField>(n,dx) < tol2*norm2_complex_array<ComplexField>(n,x1t1);
      } while (!is_successful and n_corr_steps<max_corr_steps);

      if (!is_successful) {
        // predictor failure
        predictor_successes = 0;
        *dt = complex(dt_decrease_factor_dbl)*(*dt);
        if (dt->getreal() < dt_min_dbl)
          t_s->status = MIN_STEP_FAILED;
      } else {
        // predictor success
        predictor_successes = predictor_successes + 1;
        copy_complex_array<ComplexField>(n+1, x1t1, x0t0);
        count++;
        if (is_successful && predictor_successes >= num_successes_before_increase) {
          predictor_successes = 0;
          *dt  = complex(dt_increase_factor_dbl)*(*dt);
        }
      }
      if (norm2_complex_array<ComplexField>(n,x0) > infinity_threshold2)
        t_s->status = INFINITY_FAILED;
      if (!LAPACK_success)
        t_s->status = SINGULAR;
    }
    // record the solution
    copy_complex_array<ComplexField>(n, x0, t_s->x);
    t_s->t = t0->getreal();
    if (t_s->status == PROCESSING)
      t_s->status = REGULAR;
    }
  }
  */

  return true;
}

template <typename RT>
inline void norm2(const DMat<RT>& M, size_t n, typename RT::RealElementType& result)
{
  const auto& C = M.ring();
  const auto& R = C.real_ring();
  typename RT::RealElementType c;
  R.init(c);
  R.set_zero(result);
  for(size_t i=0; i<n; i++) {
    C.abs_squared(c,M.entry(0,i));
    R.add(result,result,c);
  }
  R.clear(c);
}

enum SolutionStatus {UNDETERMINED, PROCESSING, REGULAR, SINGULAR, INFINITY_FAILED, MIN_STEP_FAILED};

// ****************************** XXX **************************************************
template <> 
inline bool HomotopyConcrete<M2::ARingCCC>::track(const MutableMatrix* inputs, MutableMatrix* outputs, 
                     MutableMatrix* output_extras,  
                     gmp_RR init_dt, gmp_RR min_dt,
                     gmp_RR epsilon, // o.CorrectorTolerance,
                     int max_corr_steps, 
                     gmp_RR infinity_threshold
                   ) 
{
  typedef M2::ARingCCC RT;
  std::cout << "inside HomotopyConcrete<M2::ARingCCC>::track" << std::endl;
  // double the_smallest_number = 1e-13;
  const Ring* matRing = inputs->get_ring();
  if (outputs->get_ring()!= matRing) { 
    ERROR("outputs and inputs are in different rings");
    return false;
  }
  auto inp = dynamic_cast<const MutableMat< DMat<RT> >*>(inputs);
  auto out = dynamic_cast<MutableMat< DMat<RT> >*>(outputs);
  auto out_extras = dynamic_cast<MutableMat< DMat<M2::ARingZZGMP> >*>(output_extras);
  if (inp == nullptr) { 
    ERROR("inputs: expected a dense mutable matrix");
    return false;
  }
  if (out == nullptr) { 
    ERROR("outputs: expected a dense mutable matrix");
    return false;
  }
  if (out_extras == nullptr) { 
    ERROR("output_extras: expected a dense mutable matrix");
    return false;
  }

  auto& in = inp->getMat();
  auto& ou = out->getMat();
  auto& oe = out_extras->getMat();
  size_t n_sols = in.numColumns();  
  size_t n = in.numRows()-1; // number of x vars

  if (ou.numColumns() != n_sols or ou.numRows() != n+1) { 
    ERROR("output: wrong shape");
    return false;
  }
  if (oe.numColumns() != n_sols or oe.numRows() != 2) { 
    ERROR("output_extras: wrong shape");
    return false;
  }
  
  const RT& C = in.ring();  
  RT::RealRingType R = C.real_ring();  

  typedef typename RT::ElementType ElementType;
  typedef typename RT::RealRingType::ElementType RealElementType;
  typedef MatElementaryOps< DMat< RT > > MatOps;
 
  RealElementType t_step;
  RealElementType dt_min;
  RealElementType epsilon2;
  RealElementType infinity_threshold2;
  R.init(t_step);
  R.init(dt_min);
  R.init(epsilon2);
  R.init(infinity_threshold2);
  R.set_from_BigReal(t_step,init_dt); // initial step
  R.set_from_BigReal(dt_min,min_dt); 
  R.set_from_BigReal(epsilon2,epsilon); 
  R.mult(epsilon2, epsilon2, epsilon2); //epsilon^2
  R.set_from_BigReal(infinity_threshold2,infinity_threshold); 
  R.mult(infinity_threshold2, infinity_threshold2, infinity_threshold2);
  int num_successes_before_increase = 3;

  RealElementType t0,one,dt,one_minus_t0,dt_factor,dx_norm2,x_norm2;
  R.init(t0);
  R.init(dt);
  R.init(one_minus_t0);
  R.init(dt_factor);
  R.set_from_double(dt_factor,0.5);
  R.init(one);
  R.set_from_long(one,1);    
  R.init(dx_norm2);
  R.init(x_norm2);
  ElementType c_init,c_end,dc;
  C.init(c_init);
  C.init(c_end);
  C.init(dc);
  // think: x_0..x_(n-1), c
  // c = the homotopy continuation parameter "t" upstair, varies on a (staight line) segment of complex plane (from c_init to c_end)  
  // t = a real running in the interval [0,1] 
  DMat<RT> x0c0(C,n+1,1); 
  DMat<RT> dx(C,n,1);
  DMat<RT> x1c1(C,n+1,1);
  DMat<RT> HxH(C,n,n+1);
  DMat<RT> LHS(C,n,n);
  DMat<RT> RHS(C,n,1);

  ElementType& c0 = x0c0.entry(n,0);
  ElementType& c1 = x1c1.entry(n,0);  
  RealElementType& tol2 = epsilon2; // current tolerance squared
  bool linearSolve_success;
  for(size_t s=0; s<n_sols; s++) {
    SolutionStatus status = PROCESSING;
    // set initial solution and initial value of the continuation parameter
    for(size_t i=0; i<n+1; i++)   
      C.set(x0c0.entry(i,0), in.entry(i,s));
    C.set(c_init,c0);
    C.set(c_end,ou.entry(n,s));

    R.set_zero(t0);
    bool t0equals1 = false;
    R.set(dt,t_step);
    int predictor_successes = 0;
    int count = 0; // number of steps
    // track the real segment (1-t)*c0 + t*c1, a\in [0,1]
    while (status == PROCESSING and not t0equals1) {
      buffer o; 
      R.elem_text_out(o,t0,true,false,false);
      std::cout << "t0 = " << o.str();
      o.reset();
      C.elem_text_out(o,c0,true,false,false);
      std::cout << ", c0 = " << o.str() << std::endl;

      R.subtract(one_minus_t0,one,t0);
      if (R.compare_elems(dt,one_minus_t0)>0) {
        R.set(dt,one_minus_t0);
        t0equals1 = true;
        C.subtract(dc,c_end,c0);
        C.set(c1,c_end);
      } else {
        C.subtract(dc,c_end,c0);
        C.mult(dc,dc,dt);
        C.divide(dc,dc,one_minus_t0);
        C.add(c1,c0,dc);
      }
      
      // PREDICTOR in: x0c0,dt,pred_type
      //           out: dx
      
      // make prediction
      for(size_t i=0; i<n; i++)   
        C.set_zero(dx.entry(i,0)); // "zero"-th order predictor
      
      for(size_t i=0; i<n; i++)   
        C.add(x1c1.entry(i,0),x0c0.entry(i,0),dx.entry(i,0));  
      C.add(c1,c0,dc);  
      
      // CORRECTOR
      int n_corr_steps = 0;
      bool is_successful;
      do {
        n_corr_steps++;
        //
        //std::cout << "evaluate...\n";
        mHxH.evaluate(x1c1,HxH);
        
        //std::cout << "setFromSubmatrix 1...\n";
        MatOps::setFromSubmatrix(HxH,0,n-1,0,n-1,LHS); // Hx
        //std::cout << "setFromSubmatrix 2...\n";
        MatOps::setFromSubmatrix(HxH,0,n-1,n,n,RHS); // H
        //std::cout << "negate...\n";
        MatrixOps::negateInPlace(RHS);
        //solve LHS*dx = RHS

        //std::cout << "solveLinear...\n";
        linearSolve_success = MatrixOps::solveLinear(LHS,RHS,dx);
        //std::cout << "solveLinear done\n";

        // x1 += dx
        for(size_t i=0; i<n; i++)   
          C.add(x1c1.entry(i,0),x1c1.entry(i,0),dx.entry(i,0));  
        
        norm2(dx,n,dx_norm2);
        norm2(x1c1,n,x_norm2);
        R.mult(x_norm2,x_norm2,tol2);
        is_successful = R.compare_elems(dx_norm2,x_norm2) < 0;
      } while (not is_successful and n_corr_steps<max_corr_steps);
      //std::cout << "past corrector loop...\n";
      if (not is_successful) {
        // predictor failure
        predictor_successes = 0;
        R.mult(dt,dt,dt_factor);
        if (R.compare_elems(dt,dt_min)<0)
          status = MIN_STEP_FAILED;
      } else {
        // predictor success
        predictor_successes = predictor_successes + 1;
        // x0c0 = x1c1
        // std::cout << "before...\n";
        //displayMat(x0c0);
        //std::cout << std::endl;
        //displayMat(x1c1);
        MatOps::setFromSubmatrix(x1c1,0,n,0,0,x0c0);
        //std::cout << "after...\n";
        //displayMat(x0c0);
        //std::cout << std::endl;
        //displayMat(x1c1);
        R.add(t0,t0,dt);
        count++;
        if (predictor_successes >= num_successes_before_increase) {
          predictor_successes = 0;
          R.divide(dt,dt,dt_factor);
        }       
      }
      
      norm2(x0c0,n,x_norm2);
      if (R.compare_elems(infinity_threshold2,x_norm2) < 0)
        status = INFINITY_FAILED;
      if (not linearSolve_success)
        status = SINGULAR;
    }
    // record the solution
    // set initial solution and initial value of the continuation parameter
    for(size_t i=0; i<=n; i++)   
      C.set(ou.entry(i,s),x0c0.entry(i,0));
    if (status == PROCESSING)
      status = REGULAR;
    oe.ring().set_from_long(oe.entry(0,s),status);
    oe.ring().set_from_long(oe.entry(1,s),count); 
  }


  C.clear(c_init);
  C.clear(c_end);
  C.clear(dc);

  R.clear(t0);
  R.clear(dt);
  R.clear(one_minus_t0);
  R.clear(dt_factor);
  R.clear(one);
  R.clear(dx_norm2);
  R.clear(x_norm2);

  R.clear(t_step);
  R.clear(dt_min);
  R.clear(epsilon2);
  R.clear(infinity_threshold2);
  return true;
}











template <typename RT> 
bool HomotopyConcrete<RT>::track(const MutableMatrix* inputs, MutableMatrix* outputs, 
                     MutableMatrix* output_extras,  
                     gmp_RR init_dt, gmp_RR min_dt,
                     gmp_RR epsilon, // o.CorrectorTolerance,
                     int max_corr_steps, 
                     gmp_RR infinity_threshold
                   ) 
{
  std::cout << "inside HomotopyConcrete<RT>::track" << std::endl;
  // double the_smallest_number = 1e-13;
  const Ring* R = inputs->get_ring();
  if (outputs->get_ring()!= R) { 
    ERROR("outputs and inputs are in different rings");
    return false;
  }
  auto inp = dynamic_cast<const MutableMat< DMat< RT > >*>(inputs);
  auto out = dynamic_cast<MutableMat< DMat< RT > >*>(outputs);
  if (inp == nullptr) { 
    ERROR("inputs: expected a dense mutable matrix");
    return false;
  }
  if (out == nullptr) { 
    ERROR("outputs: expected a dense mutable matrix");
    return false;
  }
  /*
  double epsilon2 = mpfr_get_d(epsilon,GMP_RNDN); epsilon2 *= epsilon2; //epsilon^2
  double t_step = mpfr_get_d(init_dt,GMP_RNDN); // initial step
  double dt_min_dbl = mpfr_get_d(min_dt,GMP_RNDN);
  double infinity_threshold2 = mpfr_get_d(infinity_threshold,GMP_RNDN); infinity_threshold2 *= infinity_threshold2;
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
