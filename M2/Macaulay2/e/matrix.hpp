// Copyright 1995  Michael E. Stillman

#ifndef _matrix_hh_
#define _matrix_hh_

#include "monoid.hpp"
#include "freemod.hpp"
#include "vector.hpp"
#include "monideal.hpp"

class Matrix_rec : public object_element
{
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  friend class Matrix;
  FreeModule *rows;
  FreeModule *cols;
  int *degree_shift;		// An element of the degree monoid
  array<vec> entries;
  
  Matrix_rec(const FreeModule *r, const FreeModule *c, const int *deg) 
    : object_element(), 
      rows((FreeModule *)r), 
      cols((FreeModule *)c),
      degree_shift(r->Ring_of()->degree_monoid()->make_new(deg))
      { bump_up(rows); bump_up(cols); 
	vec zero = NULL;
        for (int i=0; i<c->rank(); i++) 
	  entries.append(zero); }
  ~Matrix_rec();

  class_identifier class_id() const { return CLASS_Matrix; }
  type_identifier  type_id () const { return TY_MATRIX; }
  const char * type_name   () const { return "Matrix"; }

  Matrix cast_to_Matrix();
  
  int length_of() const { return entries.length(); }

  void bin_out(buffer &o) const;
  void text_out(buffer &o) const;
};

class Matrix
{
  friend class FreeModule;
  POINTER(Matrix, Matrix_rec)
private:
  vec &elem(int i) { return obj->entries[i]; }
  const vec &elem(int i) const { return obj->entries[i]; }

  // These two routines are private to 'coeffs'
  vec strip_vector(vec &f, const int *vars, 
		       const FreeModule *F, vec &vmonom) const;
  int moneq(const int *exp, int *m, const int *vars, int *exp2) const;

  void k_basis0(int topvar) const;
  void k_basis1(int topvar) const;
  void k_basis_insert() const;
  
  void symm1(Matrix &result, 
	     vec f,	       // product so far generated
	     int lastn,        // can use lastn..n_cols()-1 in product
	     int pow) const;   // remaining power to take

public:
  Matrix(const FreeModule *r, 
	 const FreeModule *c,
	 const int *deg);

  Matrix(const FreeModule *r, 
	 const FreeModule *c);

  Matrix(const FreeModule *r);

  Matrix(const MonomialIdeal &mi);

  const Ring *Ring_of() const { return rows()->Ring_of(); }
  const Monoid *degree_monoid() const { return Ring_of()->degree_monoid(); }

  vec &operator[](int i) { return obj->entries[i]; }
  const vec &operator[](int i) const { return obj->entries[i]; }
  ring_elem elem(int i, int j) const 
    { return rows()->get_coefficient(elem(j), i); }

  FreeModule *rows() { return obj->rows; }
  FreeModule *cols() { return obj->cols; }
  const FreeModule *rows() const { return obj->rows; }
  const FreeModule *cols() const { return obj->cols; }

  int n_rows() const { return rows()->rank(); }
  int n_cols() const { return cols()->rank(); }

  void append(vec v);
  void append(vec v, const int *d);

  // The degree shift
  const int *degree_shift() const { return obj->degree_shift; }
  int *degree_shift() { return obj->degree_shift; }

  intarray get_degree_shift() const;
  void set_degree_shift(const intarray &deg);

  // to/from monideals
  MonomialIdeal make_monideal(int n) const;
  MonomialIdeal make_skew_monideal(int n) const;
  void append_monideal(const MonomialIdeal &mi, int k);

  // Matrix operations
  Matrix sub_matrix(const intarray &r, const intarray &c) const;
  Matrix sub_matrix(const intarray &c) const;
  Matrix transpose() const;
  Matrix operator+(const Matrix &m) const;
  Matrix operator-() const;
  Matrix operator-(const Matrix &m) const;
  Matrix operator*(const ring_elem r) const;
  Matrix operator*(const Matrix &r) const;
  Matrix concat(const Matrix &m) const;

  static Matrix identity(const FreeModule *F);
  static Matrix zero(const FreeModule *F, const FreeModule *G);

  Matrix koszul(int p) const;
  static Matrix koszul(const Matrix &rows, const Matrix &cols);

  Matrix reshape(const FreeModule *G, const FreeModule *H) const;
  static Matrix flip(const FreeModule *G, const FreeModule *H);

  Matrix direct_sum(const Matrix &m) const;
  Matrix module_tensor(const Matrix &m) const;
  Matrix tensor(const Matrix &m) const;
  Matrix diff(const Matrix &m, int use_coef) const;
  Matrix symm(int n) const;

  Matrix coeffs(const int *vars, Matrix &result_monoms) const;
  Matrix lead_var_coefficient(Matrix &monoms) const;

  Matrix k_basis(Matrix bot, const int *d, int do_trunc) const;
  Matrix k_basis(Matrix bot) const;

  Matrix exterior(int p) const;
  static Matrix wedge_product(int p, int q, const FreeModule *F);
//  static Matrix wedge_dual(int p, const FreeModule *F);

  // equality, zero
  bool is_equal(const Matrix &m) const;
  bool is_zero() const;

  // degrees
  int is_homogeneous() const;
  Matrix homogenize(int v, const int *wts) const;

  // Simplification of column set
  Matrix simplify(int n) const;
  Matrix auto_reduce() const;	// An error is given, if their are two lead terms
				// one which divides another.

  // Sorting the columns of the matrix (new positions into 'result')
  void sort(int degorder, int monorder, intarray &result) const;

  // Matrix selection
  Matrix lead_term(int n=-1) const; // Select those monomials in each column
				  // which are maximal in the order under
				  // the first n weight vectors

  void elim(int n, intarray &result) const;
  Matrix sat(int n, int maxd) const; // maxd<0 means divide by as much as possible

  static Matrix random(const Ring *R, int r, int c);
  void text_out(buffer &o) const;
};

inline Matrix Matrix_rec::cast_to_Matrix() 
{ return Matrix(this,caster); }

inline void Matrix::append(vec v, const int *d)
{
  cols()->append(d);
  obj->entries.append(v);
}

inline void Matrix::append(vec v)
{
  int *d = degree_monoid()->make_one();
  if (! rows()->is_zero(v)) rows()->degree(v, d);
  append(v,d);
  degree_monoid()->remove(d);
}

#endif
