// Copyright 1998 by Michael Stillman
#ifndef __Ematrix_hpp_
#define __Ematrix_hpp_

#include "Efreemod.hpp"
#include "Evector.hpp"

class EMatrix : public type
{
  const EFreeModule *target;
  const EFreeModule *source;
  int ncolumns;			// Same as source->rank()
  EVector **columns;
  const monomial *mapdegree;		// A degree shift.  This is a monomial in D = 
				// target->getRing()->getDegreeMonoid(),
				// which is a commutative monoid, and the actual values can be obtained
				// via D->to_exponents(degree), or degree->exponents
  EGB *gb;			// A GB computation for the columns, if not NULL.
private:
  static EVector **allocate_columns(int c);
  static EVector **initialize_columns(const EFreeModule *F, int c);
  static void delete_columns(int ncols, EVector **cols);

  const EPolynomialRing *getRing() const { return target->getRing(); }
  const EMonoid *getDegreeMonoid() const { return target->getDegreeMonoid(); }
public:
  EMatrix(const EFreeModule *F,
	  const EFreeModule *G,
	  EVector **elements,
	  const monomial *d);  // GRABS the array and elements in 'elements'.

  virtual ~EMatrix();

  static EMatrix *make(const EFreeModule *F,
	  const EFreeModule *G,
	  EVector **elements,
	  const monomial *d);  // GRABS the array and elements in 'elements'.

  void text_out(ostream &o) const;
  virtual void text_out(buffer &o) const;
  void binary_out(ostream &o) const;
  static EFreeModule *binary_in(istream &i);

  virtual EMatrix * cast_to_EMatrix() { return this; }
  virtual const EMatrix * cast_to_EMatrix() const { return this; }

  class_identifier class_id() const { return CLASS_EMatrix; }
  type_identifier  type_id () const { return TY_EMatrix; }
  const char * type_name   () const { return "EMatrix"; }
public:
  // Parts of the matrix
  const EFreeModule *getTarget() const { return target; }
  const EFreeModule *getSource() const { return source; }
  const monomial *getMapDegree() const { return mapdegree; }
  
  EGB *getGB() const { return gb; }

  int n_rows() const { return target->rank(); }
  int n_cols() const { return source->rank(); }
  const EVector *column(int i) const { if (i >= 0 && i < ncolumns) return columns[i]; else return 0; }
  ERingElement *entry(int r, int c) const; // Caller owns the result.

  // matrix operations
  EMatrix *add(const EMatrix *m) const;
  EMatrix *subtract(const EMatrix *m) const;
  EMatrix *negate() const;
  EMatrix *transpose() const;
  EMatrix *multiply(const EMatrix *m) const;       // Return this*m 
  EVector *multiply(const EVector *r) const;       // Return r*this /* TODO */
  EVector *rightMultiply(const EVector *r) const;  // Return this*r
  EMatrix *submatrix(const intarray &rows, const intarray &cols) const;
  EMatrix *submatrix(const intarray &cols) const;
  EMatrix *homogenize(int v, const int *wts) const; /* TODO */

  EMatrix *tensor(const EMatrix *m) const;       // Return this ** m
  EMatrix *moduleTensor(const EMatrix *m) const; // Return the presentation matrix of 
				                 // coker this ** coker m
  EMatrix *concatenate(const EMatrix *m) const;
  EMatrix *directSum(const EMatrix *m) const;

  EMatrix *koszul(int p) const;	// Requires matrix to have one row
				// Change this: to give the Eagon-Northcott matrix...?
  static EMatrix *koszul(const EMatrix *rows, const EMatrix *cols);

  EMatrix *diff(const EMatrix *m, bool use_coef) const;/* TODO */

  EMatrix symm(int n) const;/* TODO */
  EMatrix exterior(int p) const;/* TODO */

  EMatrix *reshape(const EFreeModule *G, const EFreeModule *H) const;
  
  // New matrices
  static EMatrix *identity(const EFreeModule *F);
  static EMatrix *zero(const EFreeModule *F, const EFreeModule *G);
  static EMatrix *exteriorProduct(int p, int q, const EFreeModule *F);
  static EMatrix *flip(const EFreeModule *G, const EFreeModule *H);
  static EMatrix *random(const EPolynomialRing *R, int r, int c);

  // Predicates
  bool isZero() const;
  bool isEqual(const EMatrix *m) const;
  bool entriesEqual(const EMatrix *m) const;
  bool isGraded() const;

  void sort(int degorder, int monorder, intarray &result_permutation) const;/* TODO */

  // Parts of this matrix
  EMatrix *leadTerm(int n,bool same_component_only) const;  // Take the terms which are largest in the first n slots
					// In the monomial order.
  EMatrix *leadTermWeights(const int *wts) const; // Return the terms which are greatest w.r.t. the 'wts' order.
        /* TODO */
  EMatrix *leadCoefficients(const int *vars, int n = -1) const;/* TODO */
  EMatrix *leadCoefficients(const int *vars, const int *wts) const;/* TODO */

  EMatrix *coefficients(const int *vars, EMatrix *&result_monomials);/* TODO */

  void selectInSubring(intarray &result, int n=-1) const;
  void selectInSubringWeights(intarray &result, int *wts) const;/* TODO */
  EMatrix *divideByVariable(int n, int maxd, int &highest) const;
  
  // Simplification ??/* TODO */

  // To/From monomial ideals ??/* TODO */
};
#if 0
class EMutableMatrix : public EMatrix
{
public:
  // make a mutable matrix  from a non-mutable one (clone)
  // forget the mutable-ness.
  // Insert a new row/column.
  // Delete a row/column
  // Swap two rows/columns
  // Replace an entry
  // Add a multiple of one row/col to another.
};
#endif
#endif
