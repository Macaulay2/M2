#ifndef _matrixCC_hpp_
#define _matrixCC_hpp_

#include "engine.h"
#include "error.h"
#include "hash.hpp"
#include "text_io.hpp"
#include "lapack.h"

class LMatrixCC : public mutable_object
{
  friend class LMatrixRR;

  static double _epsilon;
  // matrices are considered equal if real and imaginary components of
  // all entries are _epsilon-close.  _epsilon is initialized to 0.

  int _nrows;
  int _ncols;
  double *_array;  // stored as double to assure contiguous memory
                   // array has length 2*_nrows*_ncols
                   // columns stored one after another

  void initialize(int nrows, int ncols, double *array);
public:
  LMatrixCC() : _nrows(0), _ncols(0), _array(0) {}
  LMatrixCC(int nrows, int ncols);
  LMatrixCC(LMatrixRR *N);

  static void set_epsilon(double epsilon) { _epsilon = epsilon; }
  static double get_epsilon() { return _epsilon; }

  int nrows() const { return _nrows; }
  int ncols() const { return _ncols; }
  void text_out(buffer &o) const;
  void resize(int new_nrows, int new_ncols);
  LMatrixCC * copy() const;

  void get_entry(int r, int c, M2_CC result) const;
  void set_entry(int r, int c, M2_CC val);
  void set_row(int r, M2_CC vals);
  void set_column(int c, M2_CC vals);
  void set_matrix(M2_CC vals);
  void set_matrix(LMatrixCC *mat);
  void set_values(M2_arrayint rowcols, M2_complex_array *vals);

  void get_entry(int r, int c, double &re, double &im) const;
  void set_entry(int r, int c, double re, double im);
  void set_row(int r, double *vals);
  void set_column(int c, double *vals);
  void set_matrix(double *vals);

  LMatrixCCOrNull * operator+(const LMatrixCC *N) const;
  LMatrixCCOrNull * operator+(const LMatrixRR *N) const;
  LMatrixCCOrNull * operator-(const LMatrixCC *N) const;
  LMatrixCCOrNull * operator-(const LMatrixRR *N) const;
  LMatrixCC * operator-() const;
  LMatrixCCOrNull * operator*(const LMatrixCC *N) const;
  LMatrixCCOrNull * operator*(const LMatrixRR *N) const;
  LMatrixCC * sub_matrix(const M2_arrayint rows, const M2_arrayint cols) const;

  bool is_equal(const LMatrixCC &N) const;
  bool is_close(const LMatrixCC &N, double tolerance) const;
public:
  LMatrixCCOrNull * eigenvalues(LMatrixCC *eigenvalues);
  // returns NULL if an error occurs, else returns eigenvalues matrix
  // 'eigenvalues' needs to be initialized first.  It does not need to
  // have the correct size.

  LMatrixCCOrNull * eigenvectors(LMatrixCC *eigenvalues, LMatrixCC *eigenvectors); 
  // returns NULL if an error occurs, else returns eigenvector matrix
  // 'eigenvalues' and 'eigenvectors' need to be initialized first.  
  // They do not need to have the correct size.

  LMatrixRROrNull * LMatrixCC::eigenvalues_hermitian(LMatrixRR *eigenvalues);
  // returns NULL if an error occurs, else returns eigenvalues matrix
  // for a hermitian matrix.  'eigenvalues' needs to be initialized first.  
  // It does not need to have the correct size.
  // Assumes hermitian matrix by using upper triangular part only.

  LMatrixCCOrNull * LMatrixCC::eigenvectors_hermitian(LMatrixRR *eigenvalues,
						      LMatrixCC *eigenvectors);
  // returns NULL if an error occurs, else returns eigenvector matrix
  // and eigenvalues of a hermitaian matrix.  
  // 'eigenvalues' and 'eigenvectors' need to be initialized first. 
  // They do not need to have the correct size, and are resized if necessary,
  // Assumes hermitian matrix by using upper triangular part only.

  LMatrixCCOrNull * solve(LMatrixCC *b, LMatrixCC *x);
  // returns NULL if an error occurs, else returns the matrix of solutions
  // to the equation 'Ax = b' where 'b' may contain multiple
  // right-hand-sides.

  LMatrixCCOrNull * LU(LMatrixCC *L, LMatrixCC *U, LMatrixRR *P);
  // returns NULL if an error occurs, else returns the matrix 'U'
  // in the LU decomposition of 'A = PLU', where 'L' is lower triangular
  // with 1's on diagonal, 'U' is upper triangular, and 'P is permutation.
  // The matrices 'L', 'U', and 'P' are set to these matrices.

  LMatrixRROrNull * SVD(LMatrixRR *Sigma, LMatrixCC *U, LMatrixCC *VT);
  // returns NULL if an error occurs, else returns the singular values
  // 'Sigma' in the singular value decomposition 'A = U*Diag(Sigma)*VT'.  
  // Note the routine returns VT, which is the transpose of V.

  LMatrixRROrNull * SVD_divide_conquer(LMatrixRR *Sigma, LMatrixCC *U, 
				       LMatrixCC *VT);
  // better algorithm for SVD, especially for large matrices
  // might fail on hexadecimal / decimal machines?

  LMatrixCCOrNull * least_squares(LMatrixCC *b, LMatrixCC *x);
  // This routine assumes the matrix has full rank.
  // return NULL if an error occurs, else returns the solutions to the
  // linear least squares problem which minimizes |Ax-b| if A has more
  // rows than columns and minimizes |x| satisfying Ax=b if A has more
  // columns than rows.

  LMatrixCCOrNull * least_squares_deficient(LMatrixCC *b, LMatrixCC *x);
  // This routine can handle matrices with deficient rank.  It uses SVD.
  // return NULL if an error occurs, else returns the minimum norm
  // solution to the linear least squares problem which minimizes |Ax-b|

};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
