// Copyright 1997  Michael E. Stillman

#ifndef _lattice_hpp_
#define _lattice_hpp_

#include "matrix.hpp"

class MatrixComputation : public type
{
  SparseMutableMatrix *gens;

  const Ring *R;

  int last_row;
  int last_col;
  
  bool hermiteStep();
  bool smithStep();
  bool gaussStep();
public:
  const int Algorithm_Hermite = 0;
  const int Algorithm_Hermite_noAutoReduce = 1;
  const int Algorithm_Smith = 2;
  const int Algorithm_Gauss = 3;  // Requires that the ring be a field.
  const int Algorithm_LLL = 4;
public:
  MatrixComputation(const Matrix &m, bool do_rowchange, bool do_colchange);
  virtual ~MatrixComputation();

  const Ring *getRing() const {return R;}
  int calc(int nsteps);

  Matrix getResultMatrix() const;
  Matrix getRowChangeOfBasisMatrix() const;
  Matrix getColumnChangeOfBasisMatrix() const;
  int getStatus() const;

  // Infrastructure
  class_identifier class_id() const { return CLASS_MatrixComputation; }
  type_identifier  type_id () const { return TY_MatrixComputation; }
  const char * type_name   () const { return "MatrixComputation"; }

  MatrixComputation * cast_to_MatrixComputation() { return this; }

  void text_out(buffer &o) const { o << "MatrixComputation"; }
};
#endif
