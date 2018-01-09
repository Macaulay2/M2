#ifndef _polynomial_algebra_hpp_
#define _polynomial_algebra_hpp_

#include "Polynomial.hpp"

struct CoefficientRingTypeExample
{
  typedef ring_elem ElementType;
};

class NCMonoid
{
  // types of monomials: (MES: just note to ourselves: remove it eventually).
  //  1. packed varpower (region of memory filled with int's)
  //  2. pointer into a region of int's.
  //  3. (NCMonoid, pointer into a region of int's)
  //  4. exponent vector (only used for commutative case)
  //  5. Monomial format.
public:
  using MonomialInserter = std::vector<int>;

  void one(MonomialInserter& m) const;

  bool is_one(const Monom& m) const;

  void copy(const Monom& m, MonomialInserter& result) const;
  
  void mult(const Monom& m1, const Monom& m2, MonomialInserter& result) const;

  int compare(const Monom& m1, const Monom& m2) const;

  // index_of_variable: returns 0..numgens-1, if monomial is that, otherwise returns -1.  
  int index_of_variable(const Monom& m) const; 

  void var(int v, MonomialInserter& result) const;

  // display (to a buffer, and to a ostream)
  void elem_text_out(buffer& o,
                     const Monom& m1,
                     const std::vector<std::string>& variableNames
                     ) const;

  // transfer to Monomial, from Monomial

  // getMonomial:
  // Input is of the form: [len deg v1 v2 ... vn]
  //                        where len = n + 2 and deg is the sum of the degree of vi
  // The output is of the form, and stored in result.
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
  void getMonomial(Monom monom, std::vector<int>& result) const;

  // fromMonomial:
  // Input is of the form: [2n+1 v1 e1 v2 e2 ... vn en] (in 'varpower' format)
  // The output is of the form, and stored in result.
  // [len deg v1 v2 v3 ... vn], where each ei > 0, (in 'varpower' format)
  // where len = n+2 and deg = sum of the degrees of the vi 
  void fromMonomial(const int* monom, MonomialInserter& result) const;
  
};

class PolynomialAlgebra : public Ring
{
  PolynomialAlgebra(const Ring* K,
                M2_ArrayString names);
public:
  using Poly = Polynomial<CoefficientRingTypeExample>;
  
  static PolynomialAlgebra* create(const Ring* K,
                               M2_ArrayString names,
                               const PolynomialRing* degreeRing);

  const Ring* getCoefficientRing() const { return &mCoefficientRing; }
  const NCMonoid& monoid() const { return mMonoid; }

  // these are all the functions from Ring that must exist for PolynomialAlgebra to be instantiated
  virtual int index_of_var(const ring_elem a) const;
  
  virtual void text_out(buffer &o) const;
  virtual unsigned int computeHashValue(const ring_elem a) const;
  virtual ring_elem from_coefficient(const ring_elem a) const;
  virtual ring_elem from_long(long n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual bool from_rational(const mpq_ptr q, ring_elem &result) const;

  virtual ring_elem var(int v) const;
  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;
  virtual int compare_elems(const ring_elem f, const ring_elem g) const;
  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const;
  virtual ring_elem negate(const ring_elem f) const;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;

  virtual ring_elem power(const ring_elem f, mpz_t n) const;
  virtual ring_elem power(const ring_elem f, int n) const;

  virtual ring_elem invert(const ring_elem f) const;
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const;
  virtual void syzygy(const ring_elem a, const ring_elem b,
                      ring_elem &x, ring_elem &y) const;
  virtual void elem_text_out(buffer &o,
                             const ring_elem f,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const;
  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const;

  virtual engine_RawArrayPairOrNull list_form(const Ring *coeffR,
                                              const ring_elem f) const;

  virtual bool is_homogeneous(const ring_elem f) const;
  virtual void degree(const ring_elem f, int *d) const;
  virtual bool multi_degree(const ring_elem f, int *d) const;
  
  long n_terms(const ring_elem f) const;
  
  // some internal functions for the above routines
  ring_elem mult_by_term_right(const ring_elem f, const ring_elem c, const Monom) const;
  ring_elem mult_by_term_left(const ring_elem f, const ring_elem c, const Monom) const;
  void add_to_end(ring_elem f, const ring_elem g) const;

  // casting functions
  virtual const PolynomialAlgebra * cast_to_PolynomialAlgebra()  const { return this; }
  virtual       PolynomialAlgebra * cast_to_PolynomialAlgebra()        { return this; }

  void debug_display(const Poly* f) const;
  void debug_display(const ring_elem ff) const;

  ring_elem makeTerm(const ring_elem a, const int* monom) const;
  // 'monom' is in 'varpower' format
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
  
private:
  std::vector<std::string> mVariableNames;
  const Ring& mCoefficientRing;
  const NCMonoid mMonoid;
  unsigned int mNumVars;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
