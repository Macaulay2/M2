// Copyright 1997  Michael E. Stillman

#ifndef _lattice_hpp_
#define _lattice_hpp_

#include "matrix.hpp"
#include "sparsemat.hpp"

class MatrixComputation : public mutable_object
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
  MatrixComputation(const Matrix *m, bool do_rowchange, bool do_colchange);
  virtual ~MatrixComputation();

  const Ring *getRing() const {return R;}
  int calc(int nsteps);

  Matrix *getResultMatrix() const;
  Matrix *getRowChangeOfBasisMatrix() const;
  Matrix *getColumnChangeOfBasisMatrix() const;
  int getStatus() const;

  MatrixComputation * cast_to_MatrixComputation() { return this; }
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

  M2_arrayint get_column_permutation();
public:
  static M2_arrayint_OrNull DO(SparseMutableMatrix *M);
  // Replace M with its column echelon form.  If M has
  // column recording going on, then one obtains the whole
  // LU decomposition.
  // If the computation was interrupted, or M is in a ring which is found to not
  // be a domain (e.g. a non-commutative ring), then NULL is returned, and M is
  // left in an intermediate state.
  // Otherwise, M is modified, and the column permutation needed
  // to obtain the resulting M is returned.
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
