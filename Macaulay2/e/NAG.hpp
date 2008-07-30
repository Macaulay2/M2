// Copyright 2008 Anton Leykin and Mike Stillman

#ifndef _nag_
#define _nag_

#include "matrix.hpp"
#include "complex.h"

// see ../packages/NAG.m2 for the description of the structure of SLPs

#define slpCOPY 1
#define slpMULTIsum 2
#define slpPRODUCT 3

class StraightLineProgram : public object
{
  M2_arrayint program;
  M2_CCC* nodes; // array of CCs
  int num_consts, num_inputs, num_outputs;
  M2_CCC* output;
  bool evaluated;
  long precision;

  StraightLineProgram();
public:
  static StraightLineProgram_OrNull *make(Matrix *consts, M2_arrayint program);
  virtual ~StraightLineProgram() {}
  
  void text_out(buffer& o) const;

  Matrix *evaluate(const Matrix *vals);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
