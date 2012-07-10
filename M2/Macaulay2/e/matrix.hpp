// Copyright 1995  Michael E. Stillman

#ifndef _matrix_hh_
#define _matrix_hh_

#include "monoid.hpp"
#include "freemod.hpp"
#include "monideal.hpp"

// #include <vector>
class MatrixConstructor;

/**
 * \ingroup matrices
 */
class M2_Matrix : public object
{
  FreeModule *_rows;
  FreeModule *_cols;
  int *_degree_shift;           // An element of the degree monoid
  array<vec> _entries;

  friend class FreeModule;
private:
  friend class MatrixConstructor;
  M2_Matrix(const FreeModule *rows,
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
  vec strip_vector(vec &f, const int *vars,
                       const FreeModule *F, vec &vmonom) const;
  int moneq(const int *exp, int *m, const int *vars, int *exp2) const;

public:
  static const M2_Matrix /* or null */ * make(const FreeModule *target,
                                   int ncols,
                                   const engine_RawRingElementArray M);

  static const M2_Matrix /* or null */ * make(const FreeModule *target,
                                   const FreeModule *source,
                                   M2_arrayint deg,
                                   const engine_RawRingElementArray M);

  static const M2_Matrix /* or null */ * make_sparse(const FreeModule *target,
                                          int ncols,
                                          M2_arrayint rows,
                                          M2_arrayint cols,
                                          const engine_RawRingElementArray entries);

  static const M2_Matrix /* or null */ * make_sparse(const FreeModule *target,
                                          const FreeModule *source,
                                          M2_arrayint deg,
                                          M2_arrayint rows,
                                          M2_arrayint cols,
                                          const engine_RawRingElementArray entries);

  const M2_Matrix /* or null */ * remake(const FreeModule *target,
                              const FreeModule *source,
                              M2_arrayint deg) const;

  const M2_Matrix /* or null */ * remake(const FreeModule *target) const;

  static const M2_Matrix * make(const MonomialIdeal * mi);

  const Ring *get_ring() const { return rows()->get_ring(); }
  const Monoid *degree_monoid() const { return get_ring()->degree_monoid(); }

  /* The following 5 routines will go away, or change name */
  vec &operator[](int i) { return _entries[i]; }
  const vec &operator[](int i) const { return _entries[i]; }
  ring_elem elem(int i, int j) const;
  vec &elem(int i) { return _entries[i]; }
  const vec &elem(int i) const { return _entries[i]; }
  /*****************************************/

  /* The non-const versions of these will go away */
  const FreeModule *rows() const { return _rows; }
  const FreeModule *cols() const { return _cols; }

  int n_rows() const { return rows()->rank(); }
  int n_cols() const { return cols()->rank(); }

  // The degree shift
  const int *degree_shift() const { return _degree_shift; }

  // to/from monideals
  MonomialIdeal * make_monideal(int n, bool use_only_monomials_with_unit_coeffs=false) const;

  // matrices over RRR, CCC
  M2_Matrix /* or null */ *clean(gmp_RR epsilon) const;
  gmp_RRorNull norm(gmp_RR p) const;

  // M2_Matrix operations
  M2_Matrix /* or null */ *sub_matrix(M2_arrayint r, M2_arrayint c) const;
  M2_Matrix /* or null */ *sub_matrix(M2_arrayint c) const;
  M2_Matrix *transpose() const;
  M2_Matrix *operator+(const M2_Matrix &m) const;
  M2_Matrix *operator-() const;
  M2_Matrix *operator-(const M2_Matrix &m) const;
  M2_Matrix *scalar_mult(const ring_elem r, bool opposite_mult) const;
  M2_Matrix *mult(const M2_Matrix *m, bool opposite_mult) const;
  M2_Matrix *concat(const M2_Matrix &m) const;

  static M2_Matrix *identity(const FreeModule *F);
  static M2_Matrix /* or null */ *zero(const FreeModule *F, const FreeModule *G);

  M2_Matrix /* or null */ *koszul(int p) const;
  static M2_Matrix /* or null */ *koszul(const M2_Matrix *rows, const M2_Matrix *cols);
  static M2_Matrix /* or null */ *koszul_monomials(int nskew, const M2_Matrix *rows, const M2_Matrix *cols);

  M2_Matrix /* or null */ *reshape(const FreeModule *G, const FreeModule *H) const;
  static M2_Matrix /* or null */ *flip(const FreeModule *G, const FreeModule *H);

  M2_Matrix /* or null */ *direct_sum(const M2_Matrix *m) const;
  M2_Matrix /* or null */ *module_tensor(const M2_Matrix *m) const;
  M2_Matrix /* or null */ *tensor(const M2_Matrix *m) const;
  M2_Matrix /* or null */ *diff(const M2_Matrix *m, int use_coef) const;

  M2_Matrix /* or null */ *contract0(int n_top_variables, const M2_Matrix *N) const;
  // see doc for IM2_Matrix_contract0 to see what this does, and
  // what assumptions there are.

  M2_Matrix /* or null */ *symm(int n) const; // in symm.cpp

  M2_Matrix /* or null */ *coeffs(const int *vars, M2_Matrix * &result_monoms) const;

  M2_Matrix /* or null */ *coeffs(M2_arrayint vars, const M2_Matrix *monoms) const;

  M2_Matrix /* or null */ *monomials(M2_arrayint vars) const;

  M2_arrayintOrNull support() const;
  // gives error if not a polynomial ring, other wise the array of indices appearing in this.

  M2_Matrix *top_coefficients(M2_Matrix * &monoms) const;

  const M2_Matrix /* or null */ * basis(M2_arrayint lo_degree,
                             M2_arrayint hi_degree,
                             M2_arrayint wt,
                             M2_arrayint vars,
                             bool do_truncation,
                             int limit) const;

  M2_Matrix *exterior(int p,int strategy) const;
  M2_Matrix *minors(int p,int strategy) const;
  M2_Matrix /* or null */ *minors(int p,
                       int strategy,
                       int n_to_compute, // -1 means all
                       M2_arrayintOrNull first_row, // possibly NULL
                       M2_arrayintOrNull first_col // possibly NULL
                       ) const;

  M2_Matrix *pfaffians(int p) const;  // in pfaff.cpp
  static M2_Matrix *wedge_product(int p, int q, const FreeModule *F);
//  static M2_Matrix wedge_dual(int p, const FreeModule *F);

  // equality, zero
  bool is_equal(const M2_Matrix &m) const;
  bool is_zero() const;

  // degrees
  bool is_homogeneous() const;
  M2_Matrix *homogenize(int v, M2_arrayint wts) const;

  // Simplification of column set
  M2_Matrix *simplify(int n) const;
  M2_Matrix *auto_reduce() const;  // An error is given, if there are two lead terms
                                // one which divides another.

  // Sorting the columns of the matrix (new positions into 'result')
  //  void sort(int degorder, int monorder, intarray &result) const;
  M2_arrayint sort(int degorder, int monorder) const;

  // M2_Matrix selection
  M2_Matrix *lead_term(int n=-1) const; // Select those monomials in each column
                                  // which are maximal in the order under
                                  // the first n weight vectors
  // If n is -1, then the flat lead terms are returned,
  // If n is > 0, then the first n parts of the monomial order are used.
  //   HOWEVER: in this case, the Schreyer order is not used: the usual order
  //   of the ring (for free modules) is used.

  // Module operations
  int dimension1() const;       // Compute the dimension of the quotient of the
                                // submodule generated by the lead terms of the
                                // columns of the matrix, modulo any lead terms of the
                                // presentation ideal of the ring of m.
                                // Over ZZ, this gives the dimension over QQ.

  // See engine.h for the definition of 'content' here
  const M2_Matrix /* or null */ *content() const;
  const M2_Matrix /* or null */ *remove_content() const;
  const M2_Matrix /* or null */ *split_off_content(const M2_Matrix /* or null */ *&result) const;

private:
  void minimal_lead_terms_ZZ(intarray &result) const;
public:
  void minimal_lead_terms(intarray &result) const;

  M2_arrayint elim_vars(int nparts) const;
  M2_arrayint elim_keep(int nparts) const;
  M2_Matrix *divide_by_var(int n, int maxd, int &maxdivided) const; // maxd<0 means divide by as much as possible

  M2_Matrix *compress() const; // Remove zero columns
  M2_Matrix *remove_monomial_factors(bool make_squarefree_only) const;
  M2_Matrix *remove_scalar_multiples() const;

  static M2_Matrix *random(const Ring *R, int r, int c);

  static M2_Matrix *random(const Ring *R,
                        int r, int c,
                        double fraction_non_zero,
                        int special_type); // 0: general, 1:upper triangular, others?

  void text_out(buffer &o) const;

  class iterator : public our_new_delete
  {
    const M2_Matrix *M;  // all matrices are immutable
    int col;
    vec v;
  public:
    //    iterator(const M2_Matrix *M0, int col0=0) : M(M0), col(col0), v(M0->elem(col0)) {}
    iterator(const M2_Matrix *M0) : M(M0), col(-1), v(0) {}
    void set(int newcol) { col = newcol; v = M->elem(col); }
    void next() { v = v->next; }
    bool valid() { return v != 0; }
    int row() { return v->comp; }
    ring_elem entry() { return v->coeff; }
  };
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
