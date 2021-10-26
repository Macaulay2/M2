// Copyright 1995  Michael E. Stillman

#ifndef _matrix_hh_
#define _matrix_hh_

#include "monoid.hpp"
#include "freemod.hpp"
#include "monideal.hpp"

#include <vector>

class MatrixConstructor;

/**
 * \ingroup matrices
 */
class Matrix : public EngineObject
{
  FreeModule * mTarget; // _rows;
  FreeModule * mSource; // _cols;
  int * mDegreeShift; // _degree_shift;  // An element of the degree monoid
  VECTOR(vec) mEntries; // array<vec> _entries;

  friend class FreeModule;

 private:
  friend class MatrixConstructor;
  Matrix(const FreeModule *rows,
         const FreeModule *cols,
         const int *degree_shift,
         VECTOR(vec) & entries);

  static bool make_sparse_vecs(MatrixConstructor &mat,
                               const FreeModule *target,
                               int ncols,
                               M2_arrayint rows,
                               M2_arrayint cols,
                               const engine_RawRingElementArray entries);
  // returns false if an error, true otherwise.
  // Places the elements into 'mat'.

  // These two routines are private to 'coeffs'
  vec strip_vector(vec &f,
                   const int *vars,
                   const FreeModule *F,
                   vec &vmonom) const;
  int moneq(const int *exp, int *m, const int *vars, int *exp2) const;

 protected:
  virtual unsigned int computeHashValue() const;

 public:
  static const Matrix /* or null */ *make(const FreeModule *target,
                                          int ncols,
                                          const engine_RawRingElementArray M);

  static const Matrix /* or null */ *make(const FreeModule *target,
                                          const FreeModule *source,
                                          M2_arrayint deg,
                                          const engine_RawRingElementArray M);

  static const Matrix /* or null */ *make_sparse(
      const FreeModule *target,
      int ncols,
      M2_arrayint rows,
      M2_arrayint cols,
      const engine_RawRingElementArray entries);

  static const Matrix /* or null */ *make_sparse(
      const FreeModule *target,
      const FreeModule *source,
      M2_arrayint deg,
      M2_arrayint rows,
      M2_arrayint cols,
      const engine_RawRingElementArray entries);

  const Matrix /* or null */ *remake(const FreeModule *target,
                                     const FreeModule *source,
                                     M2_arrayint deg) const;

  const Matrix /* or null */ *remake(const FreeModule *target) const;

  static const Matrix *make(const MonomialIdeal *mi);

  const Ring *get_ring() const { return rows()->get_ring(); }
  const Monoid *degree_monoid() const { return get_ring()->degree_monoid(); }
  /* The following 5 routines will go away, or change name */
  vec &operator[](int i) { return mEntries[i]; }
  const vec &operator[](int i) const { return mEntries[i]; }
  ring_elem elem(int i, int j) const;
  vec &elem(int i) { return mEntries[i]; }
  const vec &elem(int i) const { return mEntries[i]; }
  /*****************************************/

  /* The non-const versions of these will go away */
  const FreeModule *rows() const { return mTarget; }
  const FreeModule *cols() const { return mSource; }
  int n_rows() const { return rows()->rank(); }
  int n_cols() const { return cols()->rank(); }
  // The degree shift
  const int *degree_shift() const { return mDegreeShift; }
  // to/from monideals
  MonomialIdeal *make_monideal(
      int n,
      bool use_only_monomials_with_unit_coeffs = false) const;

  // matrices over RRR, CCC
  Matrix /* or null */ *clean(gmp_RR epsilon) const;
  gmp_RRorNull norm(gmp_RR p) const;

  // Matrix operations
  Matrix /* or null */ *sub_matrix(M2_arrayint r, M2_arrayint c) const;
  Matrix /* or null */ *sub_matrix(M2_arrayint c) const;
  Matrix *transpose() const;
  Matrix *operator+(const Matrix &m) const;
  Matrix *operator-() const;
  Matrix *operator-(const Matrix &m) const;
  Matrix *scalar_mult(const ring_elem r, bool opposite_mult) const;
  Matrix *mult(const Matrix *m, bool opposite_mult) const;
  Matrix *concat(const Matrix &m) const;

  static Matrix *identity(const FreeModule *F);
  static Matrix /* or null */ *zero(const FreeModule *F, const FreeModule *G);

  Matrix /* or null */ *koszul(int p) const;
  static Matrix /* or null */ *koszul(const Matrix *rows, const Matrix *cols);
  static Matrix /* or null */ *koszul_monomials(int nskew,
                                                const Matrix *rows,
                                                const Matrix *cols);

  Matrix /* or null */ *reshape(const FreeModule *G, const FreeModule *H) const;
  static Matrix /* or null */ *flip(const FreeModule *G, const FreeModule *H);

  Matrix /* or null */ *direct_sum(const Matrix *m) const;
  Matrix /* or null */ *module_tensor(const Matrix *m) const;
  Matrix /* or null */ *tensor(const Matrix *m) const;
  Matrix /* or null */ *diff(const Matrix *m, int use_coef) const;

  Matrix /* or null */ *symm(int n) const;  // in symm.cpp

  Matrix /* or null */ *coeffs(const int *vars, Matrix *&result_monoms) const;

  Matrix /* or null */ *coeffs(M2_arrayint vars, const Matrix *monoms) const;

  Matrix /* or null */ *monomials(M2_arrayint vars) const;

  M2_arrayintOrNull support() const;
  // gives error if not a polynomial ring, other wise the array of indices
  // appearing in this.

  Matrix *top_coefficients(Matrix *&monoms) const;

  const Matrix /* or null */ *basis(M2_arrayint lo_degree,
                                    M2_arrayint hi_degree,
                                    M2_arrayint wt,
                                    M2_arrayint vars,
                                    bool do_truncation,
                                    int limit) const;

  Matrix *exterior(int p, int strategy) const;
  Matrix *minors(int p, int strategy) const;
  Matrix /* or null */ *minors(int p,
                               int strategy,
                               int n_to_compute,             // -1 means all
                               M2_arrayintOrNull first_row,  // possibly NULL
                               M2_arrayintOrNull first_col   // possibly NULL
                               ) const;

  Matrix *pfaffians(int p) const;  // in pfaff.cpp
  static Matrix *wedge_product(int p, int q, const FreeModule *F);
  //  static Matrix wedge_dual(int p, const FreeModule *F);

  // equality, zero
  bool is_equal(const Matrix &m) const;
  bool is_zero() const;

  // degrees
  bool is_homogeneous() const;
  Matrix *homogenize(int v, M2_arrayint wts) const;

  // Simplification of column set
  Matrix *simplify(int n) const;
  Matrix *auto_reduce()
      const;  // An error is given, if there are two lead terms
              // one which divides another.

  // Sorting the columns of the matrix (new positions into 'result')
  //  void sort(int degorder, int monorder, intarray &result) const;
  M2_arrayint sort(int degorder, int monorder) const;

  // Matrix selection
  Matrix *lead_term(int n = -1) const;  // Select those monomials in each column
                                        // which are maximal in the order under
                                        // the first n weight vectors
  // If n is -1, then the flat lead terms are returned,
  // If n is > 0, then the first n parts of the monomial order are used.
  //   HOWEVER: in this case, the Schreyer order is not used: the usual order
  //   of the ring (for free modules) is used.

  // Module operations
  int dimension1() const;  // Compute the dimension of the quotient of the
                           // submodule generated by the lead terms of the
  // columns of the matrix, modulo any lead terms of the
  // presentation ideal of the ring of m.
  // Over ZZ, this gives the dimension over QQ.

  // See engine.h for the definition of 'content' here
  const Matrix /* or null */ *content() const;
  const Matrix /* or null */ *remove_content() const;
  const Matrix /* or null */ *split_off_content(
      const Matrix /* or null */ *&result) const;

 private:
  void minimal_lead_terms_ZZ(intarray &result) const;

 public:
  void minimal_lead_terms(intarray &result) const;

  M2_arrayint elim_vars(int nparts) const;
  M2_arrayint elim_keep(int nparts) const;
  Matrix *divide_by_var(int n, int maxd, int &maxdivided)
      const;  // maxd<0 means divide by as much as possible

  Matrix *compress() const;  // Remove zero columns
  Matrix *remove_monomial_factors(bool make_squarefree_only) const;
  Matrix *remove_scalar_multiples() const;

  static Matrix *random(const Ring *R, int r, int c);

  static Matrix *random(
      const Ring *R,
      int r,
      int c,
      double fraction_non_zero,
      int special_type);  // 0: general, 1:upper triangular, others?

  void text_out(buffer &o) const;

  class iterator : public our_new_delete
  {
    const Matrix *M;  // all matrices are immutable
    int col;
    vec v;

   public:
    //    iterator(const Matrix *M0, int col0=0) : M(M0), col(col0),
    //    v(M0->elem(col0)) {}
    iterator(const Matrix *M0) : M(M0), col(-1), v(0) {}
    void set(int newcol)
    {
      col = newcol;
      v = M->elem(col);
    }
    void next() { v = v->next; }
    bool valid() { return v != 0; }
    int row() { return v->comp; }
    ring_elem entry() { return v->coeff; }
  };

  class column_iterator
  {
    const vecterm * v;
  public:
    column_iterator(const Matrix *M, int c) : v(M->elem(c)) {}
    column_iterator(const Matrix *M) : v(nullptr) {}

    column_iterator& operator++() { v = v->next; return *this; }
    const vecterm* operator *() { return v; }
    bool operator!=(const column_iterator b) { return v != b.v; }
  };
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
