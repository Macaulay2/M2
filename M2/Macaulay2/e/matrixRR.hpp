#ifndef _matrixRR_hpp_
#define _matrixRR_hpp_

#include "engine.h"
#include "error.h"
#include "hash.hpp"
#include "text_io.hpp"
#include "lapack.h"

class LMatrixRR : public mutable_object
{
  friend class LMatrixCC;

  static double _epsilon; 
  // matrices are considered equal if all entries are _epsilon-close
  // _epsilon is initialized to 0.

  int _nrows;
  int _ncols;
  double *_array; // array has length _nrows*_ncols
                  // columns stored one after another

  void initialize(int nrows, int ncols, double *array);
public:
  LMatrixRR() : _nrows(0), _ncols(0), _array(0) {}
  LMatrixRR(int nrows, int ncols);

  static void set_epsilon(double epsilon) { _epsilon = epsilon; }
  static double get_epsilon() { return _epsilon; }

  int nrows() const { return _nrows; }
  int ncols() const { return _ncols; }
  void text_out(buffer &o) const; 
  void resize(int new_nrows, int new_ncols);
  LMatrixRR * copy() const;

  void get_entry(int r, int c, double *re) const;

  void set_entry(int r, int c, double re);
  void set_row(int r, double *vals);
  void set_column(int c, double *vals);
  void set_matrix(double *vals);
  void set_matrix(LMatrixRR *mat);

  void set_values(M2_arrayint rowcols, M2_double_array *vals);

  LMatrixRROrNull * operator+(const LMatrixRR *N) const;
  LMatrixCCOrNull * operator+(const LMatrixCC *N) const;
  LMatrixRROrNull * operator-(const LMatrixRR *N) const;
  LMatrixCCOrNull * operator-(const LMatrixCC *N) const;
  LMatrixRR * operator-() const;
  LMatrixRROrNull * operator*(const LMatrixRR *N) const;
  LMatrixCCOrNull * operator*(const LMatrixCC *N) const;
  LMatrixRR * sub_matrix(const M2_arrayint rows, const M2_arrayint cols) const;

  bool is_equal(const LMatrixRR &N) const;
  bool is_close(const LMatrixRR &N, double tolerance) const;

public:
  LMatrixRROrNull * solve(LMatrixRR *b, LMatrixRR *x);
  // returns NULL if an error occurs, else returns solutions to 'Ax=b'
  // where A is invertible n by n matrix, and b is an n by m matrix.

  LMatrixRROrNull * LU(LMatrixRR *L, LMatrixRR *U, LMatrixRR *P);
  // returns NULL if an error occurs, else returns the matrix 'U'
  // in the LU decomposition of 'A = PLU', where 'L' is lower triangular
  // with 1's on diagonal, 'U' is upper triangular, and 'P is permutation.
  // The matrices 'L', 'U', and 'P' are set to these matrices.

  LMatrixCCOrNull * eigenvalues(LMatrixCC *eigenvalues);
  // returns NULL if an error occurs, else returns eigenvalues matrix,
  // which is a complex matrix.  'eigenvalues' must be initialized first.  
  // It does not need to have the correct size.

  LMatrixCC * eigenvectors(LMatrixCC *eigenvalues, LMatrixCC *eigenvectors);
  // returns NULL if an error occurs, else returns eigenvalues matrix,
  // which is a complex matrix.  'eigenvalues' must be initialized first.  
  // It does not need to have the correct size.

  LMatrixRROrNull * eigenvalues_symmetric(LMatrixRR *eigenvalues);
  // returns NULL if an error occurs, else returns eigenvalues matrix
  // for a symmetric matrix.  'eigenvalues' needs to be initialized first.  
  // It does not need to have the correct size.
  // Assumes symmetric matrix by using upper triangular part only.

  LMatrixRROrNull * eigenvectors_symmetric(LMatrixRR *eigenvalues, 
					   LMatrixRR *eigenvectors); 
  // returns NULL if an error occurs, else returns eigenvector matrix
  // and eigenvalues of a symmetric matrix.  
  // 'eigenvalues' and 'eigenvectors' need to be initialized first. 
  // They do not need to have the correct size.
  // Assumes symmetric matrix by using upper triangular part only.

  LMatrixRROrNull * SVD(LMatrixRR *Sigma, LMatrixRR *U, LMatrixRR *VT);
  // returns NULL if an error occurs, else returns the singular values
  // 'Sigma' in the singular value decomposition 'A = U*Diag(Sigma)*VT'.  
  // Note the routine returns VT, which is the transpose of V.

  LMatrixRROrNull * SVD_divide_conquer(LMatrixRR *Sigma, LMatrixRR *U, 
				 LMatrixRR *VT);
  // better algorithm for SVD, especially for large matrices
  // might fail on hexadecimal / decimal machines?


  LMatrixRROrNull * least_squares(LMatrixRR *b, LMatrixRR *x);
  // This routine assumes the matrix has full rank.
  // return NULL if an error occurs, else returns the solutions to the
  // linear least squares problem which minimizes |Ax-b| if A has more
  // rows than columns and minimizes |x| satisfying Ax=b if A has more
  // columns than rows.

  LMatrixRROrNull * least_squares_deficient(LMatrixRR *b, LMatrixRR *x);
  // This routine can handle matrices with deficient rank.  It uses SVD.
  // return NULL if an error occurs, else returns the minimum norm
  // solution to the linear least squares problem which minimizes |Ax-b|

};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
