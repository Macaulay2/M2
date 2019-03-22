#ifndef _free_monoid_hpp_
#define _free_monoid_hpp_

#include "polyring.hpp"
#include "Polynomial.hpp"

class FreeMonoid : public our_new_delete
{
  // types of monomials: (MES: just note to ourselves: remove it eventually).
  //  1. packed varpower (region of memory filled with int's)
  //  2. pointer into a region of int's.
  //  3. (FreeMonoid, pointer into a region of int's)
  //  4. exponent vector (only used for commutative case)
  //  5. Monomial format.
private:
  const std::vector<std::string> mVariableNames;
  const PolynomialRing* mDegreeRing;
  const std::vector<int> mDegrees;
  VECTOR(const int*) mDegreeOfVar;
public:
  FreeMonoid(
           const std::vector<std::string>& variableNames,
           const PolynomialRing* degreeRing,
           const std::vector<int>& degrees);

  // Informational
  const std::vector<std::string>& variableNames() const { return mVariableNames; }
  const std::vector<int>& flattenedDegrees() const { return mDegrees; }
  unsigned int numVars() const { return static_cast<unsigned int>(mVariableNames.size()); }
  const PolynomialRing* degreeRing() const { return mDegreeRing; }
  const Monoid& degreeMonoid() const { return * mDegreeRing->getMonoid(); }

  // Monomial operations
  using MonomialInserter = std::vector<int>;

  void one(MonomialInserter& m) const;

  bool is_one(const Monom& m) const;

  void copy(const Monom& m, MonomialInserter& result) const;
  
  void mult(const Monom& m1, const Monom& m2, MonomialInserter& result) const;
  void mult3(const Monom& m1, const Monom& m2, const Monom& m3, MonomialInserter& result) const;

  int compare(const Monom& m1, const Monom& m2) const;

  // index_of_variable: returns 0..numgens-1, if monomial is that, otherwise returns -1.  
  int index_of_variable(const Monom& m) const; 

  void var(int v, MonomialInserter& result) const;

  // Determine the multidegree of the monomial m. Result is placed into
  // already_allocated_degree_vector which should have been allocated with
  // e.g. degreeMonoid().make_one()
  void multi_degree(const Monom& m, int* already_allocated_degree_vector) const;
  
  // display (to a buffer, and to a ostream)
  void elem_text_out(buffer& o, const Monom& m1) const;

  // transfer to Monomial, from Monomial

  // getMonomial:
  // Input is of the form: [len deg v1 v2 ... vn]
  //                        where len = n + 2 and deg is the sum of the degree of vi
  // The output is of the form, and stored in result.
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
  void getMonomial(Monom monom, std::vector<int>& result) const;
  void getMonomialReversed(Monom monom, std::vector<int>& result) const;

  // fromMonomial:
  // Input is of the form: [2n+1 v1 e1 v2 e2 ... vn en] (in 'varpower' format)
  // The output is of the form, and stored in result.
  // [len deg v1 v2 v3 ... vn], where each ei > 0, (in 'varpower' format)
  // where len = n+2 and deg = sum of the degrees of the vi 
  void fromMonomial(const int* monom, MonomialInserter& result) const;
  
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
