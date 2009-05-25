// Copyright 2008 Anton Leykin and Mike Stillman

#include "NAG.hpp"
#include "matrix-con.hpp"
#include <dlfcn.h>
#include <time.h>
#include "lapack.hpp"

 
// Straiight Line Program class

StraightLineProgram::StraightLineProgram()
{
  handle = NULL;
  eval_time = 0;
  n_calls = 0;
}

StraightLineProgram::~StraightLineProgram()
{ 
  if (handle!=NULL) {
    printf("closing library\n");
    dlclose(handle);
  }
}

int StraightLineProgram::num_slps = 0;
StraightLineProgram* StraightLineProgram::catalog[MAX_NUM_SLPs];

StraightLineProgram_OrNull *StraightLineProgram::make(Matrix *m_consts, M2_arrayint program)
{
  // todo: move some of these lines to rawSLP
  StraightLineProgram* res;
  if (num_slps>MAX_NUM_SLPs) {
    ERROR("max number of slps exceeded");
    res = NULL;
  };
  if (program->len < 3) {
    ERROR("invalid SLP");
    res = NULL;
  } else {
    res = new StraightLineProgram;
    catalog[num_slps++] = res;
    res->num_consts = program->array[0];
    if (m_consts->n_cols() != res->num_consts) 
      ERROR("different number of constants expected");
    res->num_inputs = program->array[1];
    res->rows_out = program->array[2];
    res->cols_out = program->array[3];
    res->program = program;
    switch (program->array[4]) {
    case slpCOMPILED: {
      // nodes = constants + input + output
      res->nodes = newarray_atomic(complex, res->num_consts + res->num_inputs + res->rows_out*res->cols_out);
      char libname[100]; 
      sprintf(libname, "%s%d.dylib", libPREFIX, program->array[5]);//Mac OS
      //sprintf(libname, "%s%d.so", libPREFIX, program->array[5]);//Linux (not working yet)
      const char *funname = "slpFN";
      printf("loading slpFN from %s\n", libname);
      res->handle = dlopen(libname, RTLD_LAZY | RTLD_GLOBAL);
      if (res->handle == NULL) ERROR("can't load library %s", libname);
      res->compiled_fn = (void (*)(complex*,complex*)) dlsym(res->handle, funname);
      if (res->compiled_fn == NULL) ERROR("can't link function %s from library %s",funname,libname);
    } break;
    case slpPREDICTOR: 
    case slpCORRECTOR:
      res->nodes = newarray_atomic(complex, res->num_consts+res->num_inputs+res->rows_out*res->cols_out);
      break;
    default: res->nodes = newarray_atomic(complex, program->len+res->num_consts+res->num_inputs);
    }
    for (int i=0; i<res->num_consts; i++) { 
      res->nodes[i] = complex(BIGCC_VAL(m_consts->elem(0,i)));
    }
  }
  return res;
}

void StraightLineProgram::evaluate(int n, const complex* values, complex* ret)
{
  if (n != num_inputs) ERROR("wrong number of inputs");
 
  complex* out = NULL; // used by compiledSLP                                                                                                                
  int out_entries_shift = 0; // position of "out matrix"

  int cur_node = num_consts;
  int i;
  copy_complex_array(num_inputs, values, nodes+cur_node);
  cur_node += num_inputs;

  clock_t start_t = clock(); // clock execution time

  switch (program->array[4]) {
  case slpPREDICTOR:
    out = nodes+num_consts+num_inputs;
    predictor();
    break;
  case slpCORRECTOR:
    out = nodes+num_consts+num_inputs;
    corrector();
    break;
  case slpCOMPILED:
    // evaluation via dynamically linked function
    // input: nodes (shifted by number of consts)
    // output: out
    out = nodes+num_consts+num_inputs;
    compiled_fn(nodes+num_consts,out);
    break;
  default: // evaluation by interpretation 
    i=4;
    for(; program->array[i] != slpEND; cur_node++) {
      switch (program->array[i]) {
      case slpCOPY: 
	nodes[cur_node] = nodes[program->array[(++i)++]];
	break;
      case slpMULTIsum: 
	{	
	  int n_summands = program->array[i+1];
	  nodes[cur_node] = (n_summands>0) ?  // create a temp var?  
	    nodes[program->array[i+2]] : complex(0,0); // zero if empty sum
	  for(int j=1; j<n_summands; j++)
	    nodes[cur_node] = nodes[cur_node]+nodes[program->array[i+j+2]];
	  i += n_summands+2; 
	}
	break;
      case slpPRODUCT:
	nodes[cur_node] = nodes[program->array[i+1]] * nodes[program->array[i+2]];
	i+=3;
	break;
      default:
	ERROR("unknown SLP operation");
      }
    }
    out_entries_shift = i+1;
    //end: evaluation by interpretation
  }

  eval_time += clock()-start_t;
  n_calls++;  

  switch(program->array[4]) {
  case slpPREDICTOR:
  case slpCOMPILED:
    //dynamically linked
    copy_complex_array(rows_out*cols_out, out, ret);
    break;
  default: 
    //interptretation
    complex* c = ret;
    for(i=0; i<rows_out; i++)
      for(int j=0; j<cols_out; j++) { 
	*c = nodes[program->array[i*cols_out+j+out_entries_shift]];
	c++;
      }
    //end: interpretation
  }
}

Matrix *StraightLineProgram::evaluate(const Matrix *values)
{
 
  complex* out = NULL; // used by compiledSLP 
  int out_entries_shift = 0; // position of "out matrix" in slp->program

  int cur_node = num_consts;
  int i;
  if (values->n_cols() != num_inputs) 
    ERROR("different number of inputs expected");
  for(i=0; i<num_inputs; i++, cur_node++)
    nodes[cur_node] = complex(BIGCC_VAL(values->elem(0,i)));

  clock_t start_t = clock(); // clock execution time

  switch (program->array[4]) {
  case slpPREDICTOR:
    out = nodes+num_consts+num_inputs;
    predictor();
    break;
  case slpCORRECTOR:
    out = nodes+num_consts+num_inputs;
    corrector();
    break;
  case slpCOMPILED:
    // evaluation via dynamically linked function
    // input: nodes (shifted by number of consts)
    // output: out
    out = nodes+num_consts+num_inputs;
    compiled_fn(nodes+num_consts,out);
    break;
  default: // evaluation by interpretation 
    i=4;
    for(; program->array[i] != slpEND; cur_node++) {
      switch (program->array[i]) {
      case slpCOPY: 
	nodes[cur_node] = nodes[program->array[(++i)++]];
	break;
      case slpMULTIsum: 
	{	
	  int n_summands = program->array[i+1];
	  nodes[cur_node] = (n_summands>0) ?  // create a temp var?  
	    nodes[program->array[i+2]] : complex(0,0); // zero if empty sum
	  for(int j=1; j<n_summands; j++)
	    nodes[cur_node] = nodes[cur_node]+nodes[program->array[i+j+2]];
	  i += n_summands+2; 
	}
	break;
      case slpPRODUCT:
	nodes[cur_node] = nodes[program->array[i+1]] * nodes[program->array[i+2]];
	i+=3;
	break;
      default:
	ERROR("unknown SLP operation");
      }
    }
    out_entries_shift = i+1;
    //end: evaluation by interpretation
  }

  eval_time += clock()-start_t;
  n_calls++;

  const CCC* R = values->get_ring()->cast_to_CCC(); 
  //CCC* R = CCC::create(53); //values->get_ring();
  FreeModule* S = R->make_FreeModule(cols_out); 
  FreeModule* T = R->make_FreeModule(rows_out);
  MatrixConstructor mat(T,S);
  mpfr_t re, im;
  mpfr_init(re); mpfr_init(im);
  switch(program->array[4]) {
  case slpPREDICTOR:
  case slpCORRECTOR:
  case slpCOMPILED: {
    //printf("predictor output: %d by %d\n", rows_out, cols_out);
    complex* c = out; 
    for(i=0; i<rows_out; i++)
      for(int j=0; j<cols_out; j++,c++) {
	//printf("%lf %lf \n", c->getreal(), c->getimaginary());
	mpfr_set_d(re, c->getreal(), GMP_RNDN);
	mpfr_set_d(im, c->getimaginary(), GMP_RNDN);
	ring_elem e = R->from_BigReals(re,im);
	mat.set_entry(i,j,e);
      } 
  } break;
  default: //interptretation
    for(i=0; i<rows_out; i++)
      for(int j=0; j<cols_out; j++) {
	complex c = nodes[program->array[i*cols_out+j+out_entries_shift]]; 
	mpfr_set_d(re, c.getreal(), GMP_RNDN);
	mpfr_set_d(im, c.getimaginary(), GMP_RNDN);
	ring_elem e = R->from_BigReals(re,im);
	mat.set_entry(i,j,e);
      }
    //end: interpretation
  }
  mpfr_clear(re); mpfr_clear(im);
  return mat.to_matrix(); 
}

void StraightLineProgram::text_out(buffer& o) const
{
  if (program->array[4]==slpCOMPILED) {
    o << "(SLP is precompiled) " << newline;
  }
  if (program->array[4]!=slpPREDICTOR && program->array[4]!=slpCORRECTOR){
    o << "Called " << n_calls << " times, total evaluation time = " << (eval_time / CLOCKS_PER_SEC) << "." << (eval_time%CLOCKS_PER_SEC) << " sec" << newline;
    return;
  }
  o<<"CONSTANT (count = "<< num_consts;
  o<<") nodes:\n";
  int cur_node = 0;
  int i,j;
  for(i=0; i<num_consts; i++, cur_node++) {
    char s[100];
    nodes[cur_node].sprint(s);
    o << s << ", ";
  }
  o<<newline;   
  o<<"INPUT (count = " << num_inputs <<") nodes:\n";
  for(i=0; i<num_inputs; i++, cur_node++)
    o << cur_node << " ";
  o<<newline;   

  switch (program->array[4]) {
  case slpPREDICTOR:
    o << "Predictor: type "  << program->array[5] << newline;
    o << "SLPs: " << program->array[6] << "," << program->array[7] << "," << program->array[8] << newline;
  default:
    for(i=4; program->array[i] != slpEND; cur_node++) {
      o<<cur_node<<" => ";
      switch (program->array[i]) {
      case slpCOPY: 
	o<<"copy "<< program->array[(++i)++];
	break;
      case slpMULTIsum:
	{
	  o<<"sum";
	  int n_summands = program->array[i+1];
	  for(j=0; j<n_summands; j++)
	    o<<" "<<program->array[i+j+2];
	  i += n_summands+2; 
	}
	break;
      case slpPRODUCT:
	o<<"product "<<program->array[i+1]<<" "<<program->array[i+2];
	i+=3;
	break;
      default:
	o<<"BLA i="<<i++;
      }
      o<<newline;
    }
    int out_shift = i+1;
    o<<"OUTPUT ("<< rows_out << "x" << cols_out << ") nodes:\n";
    for(i=0; i<rows_out; i++){
      for(j=0; j<cols_out; j++)
	o << program->array[out_shift+i*cols_out+j] << " ";
      o<<newline;
    }   
  }
}
// end StraightLineProgram


void copy_complex_array(int n, const complex* a, complex* b)
{
  for (int i=0; i<n; i++,a++,b++)
    *b = *a;
}

complex* make_copy_complex_array(int n, const complex* a)
{
  complex* b = newarray_atomic(complex, n);
  for (int i=0; i<n; i++,a++)
    b[i] = *a;
  return b;
}


void multiply_complex_array_scalar(int n, complex* a, const complex b)
{
  for (int i=0; i<n; i++,a++)
    *a = *a * b; 
} 

void add_to_complex_array(int n, complex* a, const complex* b)
{
  for (int i=0; i<n; i++,a++)
    *a = *a + b[i]; 
} 

void negate_complex_array(int n, complex* a)
{
  for (int i=0; i<n; i++,a++)
    *a = -*a; 
} 

double norm2_complex_array(int n, complex* a) // square of 2-norm
{
  double t = 0;
  for (int i=0; i<n; i++,a++)
    t += a->getreal()*a->getreal()+a->getimaginary()*a->getimaginary();
  return t;
}

// lapack solve routine (direct call) 
// matrix A is transposed
bool solve_via_lapack(
		   int size, const complex* A, // size-by-size matrix of complex #s
		   int bsize, const complex* b, // bsize-by-size RHS of Ax=b 
		   complex* x //solution
		   )
{

#if !LAPACK
  ERROR("lapack not present");
  return false;
#else

  bool ret = true;
  int info;

  int *permutation = newarray_atomic(int, size);
  complex* At = newarray(complex, size*size);
  int i,j;
  for(i=0; i<size; i++) for(j=0; j<size; j++) // transpose the matrix: lapack solves A^t x = b
    *(At+i*size+j) = *(A+j*size+i);
  double *copyA = (double*) At; 
  copy_complex_array(size,b,x);
  double *copyb = (double*) x; // result is stored in copyb

  /*
  printf("-----------(solve)-----------------------------------\ncopyA:\n");
  for (i=0; i<size*size; i++)
    printf("(%lf %lf) ", *(copyA+2*i), *(copyA+2*i+1));
  printf("\nb:\n");
  for (i=0; i<size; i++)
    printf("(%lf %lf) ", *(copyb+2*i), *(copyb+2*i+1));
  */
  zgesv_(&size, &bsize,
	 copyA,
	 &size, permutation, 
	 copyb,
	 &size, &info);
  /*
  printf("\nx = b:\n");
  for (i=0; i<size; i++)
    printf("(%lf %lf) ", *(copyb+2*i), *(copyb+2*i+1));
  printf("\n");
  */
  if (info > 0)       
    {
      ERROR("according to zgesv, matrix is singular");
      ret = false;
    }
  else if (info < 0)
    {
      ERROR("argument passed to zgesv had an illegal value");
      ret = false;
    }

  deletearray(permutation);
  deletearray(At);

  return ret;
#endif
}

// lapack solve routine (direct call) 
bool solve_via_lapack_without_transposition(
		   int size, const complex* A, // size-by-size matrix of complex #s
		   int bsize, const complex* b, // bsize-by-size RHS of Ax=b 
		   complex* x //solution
		   )
{

#if !LAPACK
  ERROR("lapack not present");
  return false;
#else

  bool ret = true;
  int info;

  int *permutation = newarray_atomic(int, size);
  // int i,j;
  double *copyA = (double*) A; 
  copy_complex_array(size,b,x);
  double *copyb = (double*) x; // result is stored in copyb

  /*
  printf("-----------(solve)-----------------------------------\ncopyA:\n");
  for (i=0; i<size*size; i++)
    printf("(%lf %lf) ", *(copyA+2*i), *(copyA+2*i+1));
  printf("\nb:\n");
  for (i=0; i<size; i++)
    printf("(%lf %lf) ", *(copyb+2*i), *(copyb+2*i+1));
  */
  zgesv_(&size, &bsize,
	 copyA,
	 &size, permutation, 
	 copyb,
	 &size, &info);
  /*
  printf("\nx = b:\n");
  for (i=0; i<size; i++)
    printf("(%lf %lf) ", *(copyb+2*i), *(copyb+2*i+1));
  printf("\n");
  */
  if (info > 0)       
    {
      ERROR("according to zgesv, matrix is singular");
      ret = false;
    }
  else if (info < 0)
    {
      ERROR("argument passed to zgesv had an illegal value");
      ret = false;
    }

  deletearray(permutation);

  return ret;
#endif
}

void StraightLineProgram::predictor()
{
  int n = num_inputs - 2; // n = size of vectors and matrices 
  const complex* x0t0 = nodes+num_consts;
  const complex* dt = x0t0+n+1;
  complex* dx = nodes+num_consts+num_inputs;
  int predictor_type = program->array[5];
  StraightLineProgram* Hx = catalog[program->array[6]];
  StraightLineProgram* Ht = catalog[program->array[7]];
  //StraightLineProgram* H = catalog[program->array[8]];

  complex* RHS = newarray_atomic(complex, n);
  complex* LHS = newarray_atomic(complex, n*n);
  switch(predictor_type) {
  case TANGENT: {
    Ht->evaluate(n+1,x0t0, RHS);
    multiply_complex_array_scalar(n,RHS,-*dt);
    Hx->evaluate(n+1,x0t0, LHS);
    solve_via_lapack(n,LHS,1,RHS,dx);
  } break;
  case RUNGE_KUTTA: {
    complex one_half(0.5,0);

    complex* xt = newarray_atomic(complex,n+1);
    copy_complex_array(n+1,x0t0,xt);
    complex* dx1 = newarray_atomic(complex,n);
    Ht->evaluate(n+1, xt, RHS);
    negate_complex_array(n,RHS);
    Hx->evaluate(n+1, xt, LHS);
    solve_via_lapack(n,LHS,1,RHS,dx1);
    
    complex* dx2 = newarray_atomic(complex,n);
    multiply_complex_array_scalar(n,dx1,one_half*(*dt)); 
    add_to_complex_array(n,xt,dx1); // x0+.5dx1*dt
    xt[n] += one_half*(*dt); // t0+.5dt
    Ht->evaluate(n+1, xt, RHS);
    negate_complex_array(n,RHS);
    Hx->evaluate(n+1, xt, LHS);
    solve_via_lapack(n,LHS,1,RHS,dx2);
    
    complex* dx3 = newarray_atomic(complex,n);
    multiply_complex_array_scalar(n,dx2,one_half*(*dt));
    copy_complex_array(n,x0t0,xt); // spare t
    add_to_complex_array(n,xt,dx2); // x0+.5dx2*dt
    // xt[n] += one_half*(*dt); // t0+.5dt (SAME)
    Ht->evaluate(n+1, xt, RHS);
    negate_complex_array(n,RHS);
    Hx->evaluate(n+1, xt, LHS);
    solve_via_lapack(n,LHS,1,RHS,dx3);
    
    complex* dx4 = newarray_atomic(complex,n);
    multiply_complex_array_scalar(n,dx3,*dt);
    copy_complex_array(n+1,x0t0,xt);
    add_to_complex_array(n,xt,dx3); // x0+dx3*dt
    xt[n] += *dt; // t0+dt
    Ht->evaluate(n+1, xt, RHS);
    negate_complex_array(n,RHS);
    Hx->evaluate(n+1, xt, LHS);
    solve_via_lapack(n,LHS,1,RHS,dx4);
    
    // "dx1" = .5*dx1*dt, "dx2" = .5*dx2*dt, "dx3" = dx3*dt
    multiply_complex_array_scalar(n,dx4,*dt);
    multiply_complex_array_scalar(n,dx1,2);
    multiply_complex_array_scalar(n,dx2,4);
    multiply_complex_array_scalar(n,dx3,2);
    add_to_complex_array(n,dx4,dx1);
    add_to_complex_array(n,dx4,dx2);
    add_to_complex_array(n,dx4,dx3);
    multiply_complex_array_scalar(n,dx4,1.0/6);
    copy_complex_array(n,dx4,dx);
    deletearray(dx1);
    deletearray(dx2);
    deletearray(dx3);
    deletearray(dx4);
  } break;
  default: ERROR("unknown predictor"); 
  };
  deletearray(LHS);
  deletearray(RHS);
}


void StraightLineProgram::corrector()
{
  int n = num_inputs - 2; // n = size of vectors and matrices 
  double epsilon2 = 1e-10; // square of the tolerance
  double theSmallestNumber = 1e-12; 
  double minStep = 1e-6;

  complex* x0t = nodes+num_consts;
  complex* t = x0t+n; 
  complex* dt = t+1;
  StraightLineProgram* Hx = catalog[program->array[5]];
  StraightLineProgram* H = catalog[program->array[6]];
  int maxCorSteps = program->array[7]; 
  if (1-t->getreal()<theSmallestNumber && dt->getreal()<=minStep) 
    maxCorSteps = program->array[8]; // finalMaxCorSteps 
  
  complex* x1 = nodes+num_consts+num_inputs; // output
  complex* dx = x1+n; // on return: estimate of the error

  complex* x1t = newarray(complex, n+1); 
  copy_complex_array(n+1, x0t, x1t);
  complex* RHS = newarray_atomic(complex, n);
  complex* LHS = newarray_atomic(complex, n*n);
  int i=0; // number of steps
  do {
    i++;
    H->evaluate(n+1,x1t, RHS);
    negate_complex_array(n,RHS);
    Hx->evaluate(n+1,x1t, LHS);
    solve_via_lapack(n,LHS,1,RHS,dx);
    add_to_complex_array(n,x1t,dx);
  } while (norm2_complex_array(n,dx)>epsilon2*norm2_complex_array(n,x1t) and i<maxCorSteps);

  copy_complex_array(n,x1t,x1);

  deletearray(x1t);
  deletearray(LHS);
  deletearray(RHS);
}

////////////////// PathTracker //////////////////////////////////////////////////////////////////////////////////////////////

int PathTracker::num_path_trackers = 0;
PathTracker* PathTracker::catalog[MAX_NUM_PATH_TRACKERS];

PathTracker::PathTracker(){}

PathTracker::~PathTracker()
{ 
}

// a function that creates a PathTracker object, builds the homotopy, slps for predictor and corrector given a target system
// input: a (1-row) matrix of polynomials 
// out: the number of PathTracker
PathTracker_OrNull* PathTracker::make(Matrix *HH) 
{
  if (HH->n_rows()!=1) { 
    ERROR("1-row matrix expected");
    return NULL;
  };
  PathTracker *p = new PathTracker;
  p->H = HH;
  return p;
}

int PathTracker::makeFromHomotopy(Matrix *HH) 
{
  if (num_path_trackers>MAX_NUM_PATH_TRACKERS) {
    ERROR("max number of path trackers exceeded");
    return -1;
  };
  PathTracker* p = catalog[num_path_trackers] = new PathTracker;
  p->number = num_path_trackers++;
  if (HH->n_rows()!=1) { 
    ERROR("1-row matrix expected");
    return -1;
  };
  return p->number;
}
  


// this is an engine function
const MatrixOrNull * rawTrackPaths(StraightLineProgram* slp_pred, StraightLineProgram* slp_corr, const Matrix* start_sols, 
				   M2_bool is_projective,
				   M2_RRR init_dt, M2_RRR min_dt, M2_RRR max_dt, 
				   M2_RRR dt_increase_factor, M2_RRR dt_decrease_factor, int num_successes_before_increase, 
				   M2_RRR epsilon, int max_corr_steps,
				   int pred_type)
{
  PathTracker P;
  P.slpHxt = P.slpHxtH = slp_pred;
  P.slpHxH = slp_corr;
  P.is_projective = is_projective;
  P.init_dt = init_dt;
  P.min_dt = min_dt;
  P.max_dt = max_dt;
  P.dt_increase_factor = dt_increase_factor; 
  P.dt_decrease_factor = dt_decrease_factor;
  P.num_successes_before_increase = num_successes_before_increase;
  P.epsilon = epsilon;
  P.max_corr_steps = max_corr_steps;
  P.pred_type = pred_type;
  P.track(start_sols);
  return P.getAllSolutions();
}

int PathTracker::track(const Matrix* start_sols)
{
  double the_smallest_number = 1e-13;
  double epsilon2 = mpfr_get_d(epsilon,GMP_RNDN); epsilon2 *= epsilon2; //epsilon^2
  double t_step = mpfr_get_d(init_dt,GMP_RNDN); // initial step
  double dt_min_dbl = mpfr_get_d(min_dt,GMP_RNDN);
  double dt_increase_factor_dbl = mpfr_get_d(dt_increase_factor,GMP_RNDN);
  double dt_decrease_factor_dbl = mpfr_get_d(dt_decrease_factor,GMP_RNDN);
  
  const CCC* R = start_sols->get_ring()->cast_to_CCC(); 
  int n= start_sols->n_cols();  
  int n_sols = start_sols->n_rows();

  printf("epsilon2 = %e, t_step = %lf, dt_min_dbl = %lf, dt_increase_factor_dbl = %lf, dt_decrease_factor_dbl = %lf\n", 
	 epsilon2, t_step, dt_min_dbl, dt_increase_factor_dbl, dt_decrease_factor_dbl);

  // memory distribution for arrays
  complex* s_sols = newarray(complex,n*n_sols);
  complex* t_sols = newarray(complex,n*n_sols);
  complex* x0t0 = newarray(complex,n+1); 
    complex* x0 =  x0t0;
    complex* t0 = x0t0+n;
  complex* x1t1 = newarray(complex,n+1); 
  //  complex* x1 =  x1t1;
  //  complex* t1 = x1t1+n;
  complex* dxdt = newarray(complex,n+1); 
    complex* dx =  dxdt;
    complex* dt = dxdt+n;
  complex* Hxt = newarray_atomic(complex, n*(n+1));
  complex* HxtH = newarray_atomic(complex, n*(n+2));
  complex* HxH = newarray_atomic(complex, n*(n+1));
    complex *LHS, *RHS;
  complex one_half(0.5,0);
  complex* xt = newarray_atomic(complex,n+1);
  complex* dx1 = newarray_atomic(complex,n);
  complex* dx2 = newarray_atomic(complex,n);
  complex* dx3 = newarray_atomic(complex,n);
  complex* dx4 = newarray_atomic(complex,n);

  // read solutions: rows are solutions
  int i,j;
  complex* c = s_sols;
  for(i=0; i<n_sols; i++) 
    for(j=0; j<n; j++,c++) 
      *c = complex(BIGCC_VAL(start_sols->elem(i,j)));
				
  complex* t_s = t_sols; //current target solution
  complex* s_s = s_sols; //current start solution
  				
  for(int sol_n =0; sol_n<n_sols; sol_n++, s_s+=n, t_s+=n) {

    copy_complex_array(n,s_s,x0);
    *t0 = complex(0,0);

    *dt = complex(t_step);
    int predictor_successes = 0;
    int count = 0; // number of computed points
    bool is_infinity = false;
    while (!is_infinity && 1 - t0->getreal() > the_smallest_number) {
      if ( dt->getreal() > 1 - t0->getreal() ) 
	*dt = complex(1-t0->getreal());
      
      //printf("p: dt = %lf\n", dt->getreal()); 
      
      // PREDICTOR in: x0t0,dt,pred_type
      //           out: dx
      switch(pred_type) {
      case TANGENT: {
	slpHxt->evaluate(n+1,x0t0, Hxt);
	LHS = Hxt; 	
	RHS = Hxt+n*n; 
	multiply_complex_array_scalar(n,RHS,-*dt);
	solve_via_lapack_without_transposition(n,LHS,1,RHS,dx);
      } break;
      case EULER: {
	slpHxtH->evaluate(n+1,x0t0, HxtH); // for Euler "H" is attached
        LHS = HxtH;
        RHS = HxtH+n*(n+1); // H
	complex* Ht = RHS-n; 
	multiply_complex_array_scalar(n,Ht,*dt);
	add_to_complex_array(n,RHS,Ht);
	negate_complex_array(n,RHS);
	solve_via_lapack_without_transposition(n,LHS,1,RHS,dx);
      } break;
      case RUNGE_KUTTA: {
	copy_complex_array(n+1,x0t0,xt);
	
	// dx1
	slpHxt->evaluate(n+1,xt, Hxt);
	LHS = Hxt; 	
	RHS = Hxt+n*n; 
	//
	negate_complex_array(n,RHS);
	solve_via_lapack_without_transposition(n,LHS,1,RHS,dx1);
	
	// dx2
	multiply_complex_array_scalar(n,dx1,one_half*(*dt)); 
	add_to_complex_array(n,xt,dx1); // x0+.5dx1*dt
	xt[n] += one_half*(*dt); // t0+.5dt
	//
	slpHxt->evaluate(n+1,xt, Hxt);
	LHS = Hxt; 	
	RHS = Hxt+n*n; 
	//
	negate_complex_array(n,RHS);
	solve_via_lapack_without_transposition(n,LHS,1,RHS,dx2);
    
	// dx3
	multiply_complex_array_scalar(n,dx2,one_half*(*dt));
	copy_complex_array(n,x0t0,xt); // spare t
	add_to_complex_array(n,xt,dx2); // x0+.5dx2*dt
	// xt[n] += one_half*(*dt); // t0+.5dt (SAME)
	//
	slpHxt->evaluate(n+1,xt, Hxt);
	LHS = Hxt; 	
	RHS = Hxt+n*n; 
	//
	negate_complex_array(n,RHS);
	solve_via_lapack_without_transposition(n,LHS,1,RHS,dx3);
    
	// dx4
	multiply_complex_array_scalar(n,dx3,*dt);
	copy_complex_array(n+1,x0t0,xt);
	add_to_complex_array(n,xt,dx3); // x0+dx3*dt
	xt[n] += *dt; // t0+dt
	//
	slpHxt->evaluate(n+1,xt, Hxt);
	LHS = Hxt; 	
	RHS = Hxt+n*n; 
	//
	negate_complex_array(n,RHS);
	solve_via_lapack_without_transposition(n,LHS,1,RHS,dx4);
    
	// "dx1" = .5*dx1*dt, "dx2" = .5*dx2*dt, "dx3" = dx3*dt
	multiply_complex_array_scalar(n,dx4,*dt);
	multiply_complex_array_scalar(n,dx1,2);
	multiply_complex_array_scalar(n,dx2,4);
	multiply_complex_array_scalar(n,dx3,2);
	add_to_complex_array(n,dx4,dx1);
	add_to_complex_array(n,dx4,dx2);
	add_to_complex_array(n,dx4,dx3);
	multiply_complex_array_scalar(n,dx4,1.0/6);
	copy_complex_array(n,dx4,dx);
      } break;
      default: ERROR("unknown predictor"); 
      };

      copy_complex_array(n+1,x0t0,x1t1);
      add_to_complex_array(n+1,x1t1,dxdt);

      // CORRECTOR
      int n_corr_steps=0; 
      bool is_successful;
      do {
	n_corr_steps++;
	//
	slpHxH->evaluate(n+1,x1t1, HxH);
	LHS = HxH; 	
	RHS = HxH+n*n; // i.e., H
	//
	negate_complex_array(n,RHS);
	solve_via_lapack_without_transposition(n,LHS,1,RHS,dx);
	add_to_complex_array(n,x1t1,dx);
	is_successful = norm2_complex_array(n,dx) < epsilon2*norm2_complex_array(n,x1t1);
	//printf("c: |dx|^2 = %lf\n", norm2_complex_array(n,dx));
      } while (!is_successful and n_corr_steps<max_corr_steps);
    
      if (dt->getreal() > dt_min_dbl && !is_successful) { 
	// predictor failure 
	predictor_successes = 0;
	*dt = complex(dt_decrease_factor_dbl)*(*dt);
      } else { 
	// predictor success
	predictor_successes = predictor_successes + 1;
	copy_complex_array(n+1, x1t1, x0t0);
	count++;
	if (is_successful && predictor_successes >= num_successes_before_increase) { 
	  predictor_successes = 0;
	  *dt  = complex(dt_increase_factor_dbl)*(*dt);	
	}
      }
    }
    copy_complex_array(n,x0,t_s);
    
    //printf("(%d)", count); fflush(stdout);
  }
  
  // construct output 
  FreeModule* S = R->make_FreeModule(n); 
  FreeModule* T = R->make_FreeModule(n_sols);
  MatrixConstructor mat(T,S);
  mpfr_t re, im;
  mpfr_init(re); mpfr_init(im);
  c = t_sols;
  for(i=0; i<n_sols; i++) 
    for(j=0; j<n; j++,c++) {
      mpfr_set_d(re, c->getreal(), GMP_RNDN);
      mpfr_set_d(im, c->getimaginary(), GMP_RNDN);
      ring_elem e = R->from_BigReals(re,im);
      mat.set_entry(i,j,e);
    }
  mpfr_clear(re); mpfr_clear(im);
  
  // clear arrays
  deletearray(t_sols);
  deletearray(s_sols);
  deletearray(x0t0);
  deletearray(x1t1);
  deletearray(dxdt);
  deletearray(xt);
  deletearray(dx1);
  deletearray(dx2);
  deletearray(dx3);
  deletearray(dx4);
  deletearray(Hxt);
  deletearray(HxtH);
  deletearray(HxH);

  solutions = mat.to_matrix();
  return n_sols;
}

MatrixOrNull* PathTracker::getAllSolutions() { return solutions; }

void PathTracker::text_out(buffer& o) const
{
  o << "H:" << newline;
  H->text_out(o);
}
 
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

