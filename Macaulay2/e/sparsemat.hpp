// Copyright 1998  Michael E. Stillman

#ifndef _sparsemat_hpp_
#define _sparsemat_hpp_

#include "ring.hpp"
class Matrix;

struct sparse_vector
{
  sparse_vector *next;
  int component;
  ring_elem coefficient;
};

class VectorOperations
{
  const Ring *K;
  static stash *vecstash;

  // private sparse_vector operations
  sparse_vector *new_sparse_vector() const;
  void remove_sparse_vector_node(sparse_vector *n) const;
public:
  VectorOperations(const Ring *K);
  ~VectorOperations();

  sparse_vector *make_sparse_vector(int r, ring_elem a) const;
  sparse_vector *clone(const sparse_vector *v) const;
  void remove(sparse_vector *v) const;
  void row2by2(sparse_vector *&, int r1, int r2,
	       ring_elem a1, ring_elem a2,
	       ring_elem b1, ring_elem b2) const;
  void scale(sparse_vector *&v, const ring_elem a) const;
  void scaleRow(sparse_vector *&v, int r, const ring_elem a) const;
  void divide(sparse_vector *&v, const ring_elem a) const;
  void divideRow(sparse_vector *&v, int r, const ring_elem a) const;
  void interchangeRows(sparse_vector *&v, int r1, int r2) const;
  void add(sparse_vector *&v, sparse_vector *&w) const;
  void addRowMultiple(sparse_vector *&v, int r1, ring_elem a, int r) const;
  ring_elem dotProduct(const sparse_vector *v, const sparse_vector *w) const;

  bool getEntry(sparse_vector *v, int r, ring_elem &result) const;
  void setEntry(sparse_vector *&v, int r, ring_elem a) const;
};

class SparseMutableMatrix : public mutable_object
{
  const Ring *K;
  VectorOperations *V;
  ring_elem one;		// Stashed here for easy check against one.
  ring_elem minus_one;

  int nrows;
  int ncols;
  sparse_vector **matrix;
  int *colSize;
  int *rowSize;

  SparseMutableMatrix *rowOps;
  SparseMutableMatrix *colOps;	// Transpose of column matrix

  void initialize(const Ring *KK, int nr, int nc);

  int compare_sparse_vectors(sparse_vector *v, sparse_vector *w);
  int sort_partition(int lo, int hi, int *sortvals);
  void sort1(int lo, int hi, int *sortvals);

  bool errorColumnBound(int c) const;
  bool errorRowBound(int r) const;

  SparseMutableMatrix(const Matrix *m);
  SparseMutableMatrix(const Ring *K, int nrows, int ncols);
public:
  static SparseMutableMatrix * make(const Ring *K, int nrows, int ncols);
  static SparseMutableMatrix * make(const Matrix *m);

  ~SparseMutableMatrix();
  
  int n_rows() const;
  int n_cols() const;
  const Ring *getRing() const;

  void setRowChangeMatrix(SparseMutableMatrix *rops);
  void setColumnChangeMatrix(SparseMutableMatrix *cops);
  SparseMutableMatrix *getRowChangeMatrix();
  SparseMutableMatrix *getColumnChangeMatrix();

  static SparseMutableMatrix *identity(const Ring *K, int n);
  Matrix *toMatrix() const;

  int numNonZeroRow(int r) const;
  int numNonZeroColumn(int c) const;

  int leadRow(int c) const;  // -1 means this sparse_vector is zero.
  ring_elem leadCoefficient(int c) const; // Can be zero.

  bool getEntry(int r, int c, ring_elem &result) const;
  void setEntry(int r, int c, ring_elem a);
  void setRow(int r, sparse_vector *v);
  void setColumn(int c, sparse_vector *v);
  sparse_vector *getRow(int r) const;		// Copies the row
  sparse_vector *getColumn(int c) const;	// Copies the column

  void column2by2(int c1, int c2, 
		  ring_elem a1, ring_elem a2,
		  ring_elem b1, ring_elem b2,
		  bool doRecording=true);
  void row2by2(int r1, int r2, 
	       ring_elem a1, ring_elem a2,
	       ring_elem b1, ring_elem b2,
	       bool doRecording=true);

  void interchangeRows(int r1, int r2, bool doRecording=true);
  void interchangeColumns(int c1, int c2, bool doRecording=true);

  void scaleRow(int r, ring_elem a, bool doRecording=true);
  void scaleColumn(int c, ring_elem a, bool doRecording=true);

  void divideRow(int r, ring_elem a, bool doRecording=true);
  void divideColumn(int c, ring_elem a, bool doRecording=true);

  void addRowMultiple(int r1, ring_elem a, int r, bool doRecording=true);
    // replace [row r] by [row r] + a*[row r1].

  void addColumnMultiple(int c1, ring_elem a, int c, bool doRecording=true);
    // replace [col c] by [col c] + a*[col c1].

  ring_elem dotProduct(int c1, int c2) const;

  void columnReduce(int pivot_column, int c, bool doRecording=true);
  // Let c1, c2 denote these two columns pivot_column, c.
  // If a1,a2 are the lead coefficients of c1, c2, respectively, then set:
  //   matrix[c2] = c2 - (a2/a1) * c1.
  // If a1 is '1', then division is not performed.

  // Let c1, c2 denote these two columns pivot_column, c.
  // If a1,a2 are the lead coefficients of c1, c2, respectively,
  // and if x*a1+y*a2=d=gcd(a1,a2), then set
  //   matrix[column c1] = x*c1+y*c2
  //   matrix[column c2] = (a1/d) c1 - (a2/d) c2
  // This assumes that gcdExtended is defined in the base ring.
  void gcdColumnReduce(int pivot_column, int c, bool doRecording=true);
  void gcdColumnReduce(int r, int pivot_column, int c, bool doRecording=true);
  void gcdRowReduce(int c, int pivot_row, int r, bool doRecording=true);

  void sortColumns(int lo, int hi, bool doRecording=true);

  void permuteColumns(int lo, int hi, int *permutation, bool doRecording=true);
  // Permute the columns of the matrix, at least those in the range lo..hi.
  // i.e. matrix[c] is moved to matrix[sortval[c]].

  void normalizeColumn(int c, bool doRecording=true);

  // Find a good 'one' pivot location, if any '1's or '-1's
  void setSizes(int c_lo, int c_hi);
  bool findGoodUnitPivot(int c_lo, int c_hi, int &r, int &c, int &best);
  bool findGoodPivot(int c_lo, int c_hi, int &r, int &c, int &best);

  // Harry's routines
  void reducePivots();
  
  // Infrastructure
  void text_out(buffer &o) const;
  void display() const;
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
