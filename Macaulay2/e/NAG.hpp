// Copyright 2008 Anton Leykin and Mike Stillman

#ifndef _nag_
#define _nag_

#include "matrix.hpp"
#include "CCC.hpp"

// Simple complex number class                                                                                                                                                                          
class complex
{
private:
  double real;  // Real Part
  double imag;      //  Imaginary Part                                                                                                       
public:
  complex();
  complex(double);
  complex(double,double);
  complex(const complex&);
  complex(M2_CCC);
  complex operator +(complex);
  complex operator -(complex);
  complex operator -() const;
  complex operator *(complex);
  complex operator /(complex);
  complex& operator +=(const complex);
  complex getconjugate();
  complex getreciprocal();
  double getmodulus();
  double getreal();
  double getimaginary();
  bool operator ==(complex);
  void operator =(complex);
  void sprint(char*);
};

//                                        CONSTRUCTOR
inline  complex::complex() { }

inline complex::complex(double r)
{
  real=r;
  imag=0;
}

inline complex::complex(double r, double im)
{
  real=r;
  imag=im;
}
 
//                                 COPY CONSTRUCTOR
inline complex::complex(const complex &c)
{
  this->real=c.real;
  this->imag=c.imag;
}
 
inline complex::complex(M2_CCC mpfrCC)
{
  real = mpfr_get_d(mpfrCC->re,GMP_RNDN);
  imag = mpfr_get_d(mpfrCC->im,GMP_RNDN);
}
 
inline void complex::operator =(complex c)
{
  real=c.real;
  imag=c.imag;
}
 
 
inline complex complex::operator +(complex c)
{
  complex tmp;
  tmp.real=this->real+c.real;
  tmp.imag=this->imag+c.imag;
  return tmp;
}

inline complex& complex::operator +=(const complex c)
{
  this->real+=c.real;
  this->imag+=c.imag;
  return *this;
}
 
inline complex complex::operator -(complex c)
{
  complex tmp;
  tmp.real=this->real - c.real;
  tmp.imag=this->imag - c.imag;
  return tmp;
}

inline complex complex::operator -() const
{
  complex tmp;
  tmp.real=-this->real;
  tmp.imag=-this->imag;
  return tmp;
}
 
inline complex complex::operator *(complex c)
{
  complex tmp;
  tmp.real=(real*c.real)-(imag*c.imag);
  tmp.imag=(real*c.imag)+(imag*c.real);
  return tmp;
}
 
inline complex complex::operator /(complex c)
{
  double div=(c.real*c.real) + (c.imag*c.imag);
  complex tmp;
  tmp.real=(real*c.real)+(imag*c.imag);
  tmp.real/=div;
  tmp.imag=(imag*c.real)-(real*c.imag);
  tmp.imag/=div;
  return tmp;
}
 
inline complex complex::getconjugate()
{
  complex tmp;
  tmp.real=this->real;
  tmp.imag=this->imag * -1;
  return tmp;
}
 
inline complex complex::getreciprocal()
{
  complex t;
  t.real=real;
  t.imag=imag * -1;
  double div;
  div=(real*real)+(imag*imag);
  t.real/=div;
  t.imag/=div;
  return t;
}
 
inline double complex::getmodulus()
{
  double z;
  z=(real*real)+(imag*imag);
  z=sqrt(z);
  return z;
}
 
inline double complex::getreal()
{
  return real;
}
 
inline double complex::getimaginary()
{
  return imag;
}
 
inline bool complex::operator ==(complex c)
{
  return (real==c.real)&&(imag==c.imag) ? 1 : 0;
}

inline void complex::sprint(char* s)
{
  sprintf(s, "(%lf) + i*(%lf)", real, imag);
}


void copy_complex_array(int n, const complex* a, complex* b);
complex* make_copy_complex_array(int n, const complex* a);
void multiply_complex_array_scalar(int n, complex* a, const complex b);

// see ../packages/NAG.m2 for the description of the structure of SLPs

#define libPREFIX "/tmp/slpFN."
#define slpCOMPILED 100
#define slpPREDICTOR 101
#define slpCORRECTOR 102
#define slpEND 0
#define slpCOPY 1
#define slpMULTIsum 2
#define slpPRODUCT 3

// types of predictors
#define RUNGE_KUTTA 1
#define TANGENT 2
#define EULER 3

#define MAX_NUM_SLPs 100
#define MAX_NUM_PATH_TRACKERS 10

class StraightLineProgram : public object
{
  static StraightLineProgram* catalog[MAX_NUM_SLPs];
  static int num_slps;

  M2_arrayint program;
  complex* nodes; // array of CCs
  int num_consts, num_inputs, rows_out, cols_out;

  void *handle; //dynamic library handle
  void (*compiled_fn)(complex*,complex*);
  long precision;
  clock_t eval_time; // accumulates time spent in evaluation 
  int n_calls; // number of times called   

  StraightLineProgram();
  //static StraightLineProgram_OrNull *make(const RingElement*);

  void predictor(); // evaluates a predictor
  void corrector(); // evaluates a corrector
public:
  static StraightLineProgram_OrNull *make(Matrix *consts, M2_arrayint program);
  virtual ~StraightLineProgram();

  void text_out(buffer& o) const;
  void evaluate(int n, const complex* values, complex* out);
  Matrix *evaluate(const Matrix *vals);
};

class PathTracker : public object
{
  static PathTracker* catalog[MAX_NUM_PATH_TRACKERS];
  static int num_path_trackers;

  int number; // trackers are enumerated

public:
  Matrix *target;
  Matrix *H; // homotopy
  StraightLineProgram *slpHxt, *slpHxtH, *slpHxH; // slps for evaluating H_{x,t}, H_{x,t}|H, H_{x}|H 
  
  Matrix *solutions;

  // parameters
  M2_bool is_projective;
  M2_RRR init_dt, min_dt, max_dt;
  M2_RRR dt_increase_factor, dt_decrease_factor;
  int num_successes_before_increase;
  M2_RRR epsilon;
  int max_corr_steps;
  int pred_type;

  void make_slps(); // creates slpHxt and alpHxH

  PathTracker();
public:
  static PathTracker_OrNull *make(Matrix*); // from homotopy
  virtual ~PathTracker();

  void text_out(buffer& o) const;
  int makeFromHomotopy(Matrix*);
  MatrixOrNull* getAllSolutions();
  int track(const Matrix*); 
  
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
