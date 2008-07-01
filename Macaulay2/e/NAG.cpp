// Copyright 2008 Anton Leykin and Mike Stillman

#include "NAG.hpp"

StraightLineProgram::StraightLineProgram()
{
}

StraightLineProgram_OrNull *StraightLineProgram::make(Matrix *consts, M2_arrayint program)
{
  ERROR("not implemented yet");
  return 0;
}

Matrix *StraightLineProgram::evaluate(const Matrix *values)
{
  ERROR("not implemented yet");
  return 0;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
