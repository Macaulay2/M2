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
  complex(double,double);
  complex(const complex&);
  complex(M2_CCC);
  complex operator +(complex);
  complex operator -(complex);
  complex operator *(complex);
  complex operator /(complex);
  complex getconjugate();
  complex getreciprocal();
  double getmodulus();
  double getreal();
  double getimaginary();
  bool operator ==(complex);
  void operator =(complex);
  void sprint(char*);
};

// see ../packages/NAG.m2 for the description of the structure of SLPs

#define libPREFIX "/tmp/slpFN."
#define slpCOMPILED 100
#define slpEND 0
#define slpCOPY 1
#define slpMULTIsum 2
#define slpPRODUCT 3

class StraightLineProgram : public object
{
  M2_arrayint program;
  complex* nodes; // array of CCs
  int num_consts, num_inputs, rows_out, cols_out;
  bool is_compiled; //compiled and dynamically linked
  void *handle; //dynamic library handle
  void (*compiled_fn)(complex*,complex*);
  long precision;

  StraightLineProgram();
public:
  static StraightLineProgram_OrNull *make(Matrix *consts, M2_arrayint program);
  virtual ~StraightLineProgram();

  void text_out(buffer& o) const;
  Matrix *evaluate(const Matrix *vals);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
