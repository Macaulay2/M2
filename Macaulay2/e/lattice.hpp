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
#define Algorithm_Hermite 0
#define Algorithm_Hermite_noAutoReduce 1
#define Algorithm_Smith 2
#define Algorithm_Gauss 3
#define Algorithm_LLL 4
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

class FF_LUComputation
{
  // This is a class encapsulating the LU decomposition
  // over a domain, using fraction free Gaussian elimination.
  
  const Ring *R;		// R should be a domain.
  SparseMutableMatrix *M;
  int *col_perm;
  bool *need_div;
  int pivot_col;
  ring_elem lastpivot;
  ring_elem pivot;	

private:
  FF_LUComputation(SparseMutableMatrix *M);
  ~FF_LUComputation();

  bool choose_pivot_column(int lo, int hi, int &result);
  // Chooses a pivot in the column range lo..hi, among those with 
  // the highest index row.  Returns true if there is a non-zero 
  // column in the range lo..hi, and sets 'result' in this case.
  // If all such columns are zero, returns false.

  void do_pivots(int lo, int hi, int pivot_col);
  // Use the lead element (pivot, in row r, in pivot_col to clear all
  // elements in row r in columns lo..hi.  This uses fraction-free
  // methods, and uses 'need_div' to determine whether division
  // by the previous pivot should be done.  It also sets 'need_div'
  // for the next time.

  bool calc();
  // Returns true if the computation completed.  False if it was
  // user interrupted.

  void get_column_permutation(intarray &result);
public:
  static bool DO(SparseMutableMatrix *M, intarray &col_permutation);
  // returns true if the computation was not user interrupted.  If it was,
  // false is returned, and M is in an intermediate state.

};
#endif
