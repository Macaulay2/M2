// Copyright 1997  Michael E. Stillman

#ifndef _lattice_hpp_
#define _lattice_hpp_

#include "matrix.hpp"

class LatticeOperations
{
  // What are the integer matrix operations that we desire?

  static Matrix gram_matrix(const Matrix &m);
  static Matrix LLL(const Matrix &m);
  static Matrix smith(const Matrix &m);
  static Matrix kernelLLL(const Matrix &m);
};

#endif
