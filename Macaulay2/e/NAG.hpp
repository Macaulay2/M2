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

#define MAX_NUM_SLPs 100

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

  StraightLineProgram();
  void predictor(); // evaluates a predictor
  void corrector(); // evaluates a corrector
public:
  static StraightLineProgram_OrNull *make(Matrix *consts, M2_arrayint program);
  virtual ~StraightLineProgram();

  void text_out(buffer& o) const;
  void evaluate(int n, const complex* values, complex* out);
  Matrix *evaluate(const Matrix *vals);
};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
