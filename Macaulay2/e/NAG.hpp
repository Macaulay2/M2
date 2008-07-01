// Copyright 2008 Anton Leykin and Mike Stillman

#ifndef _nag_
#define _nag_

#include "matrix.hpp"

class StraightLineProgram : public object
{
  StraightLineProgram();
public:
  static StraightLineProgram_OrNull *make(Matrix *consts, M2_arrayint program);
  virtual ~StraightLineProgram() {}

  Matrix *evaluate(const Matrix *vals);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
