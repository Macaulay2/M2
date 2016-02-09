#ifndef _nc_algebra_hpp_
#define _nc_algebra_hpp_

#include "ringelem.hpp"
#include "engine.h"

class buffer;
class Ring;

#include "ring.hpp"

struct NCMonomial
{
  NCMonomial(const int* value) : mValue(value) {}
  const int* operator*() const { return mValue; }

  bool is_one_monomial() const { return (mValue[0] == 2 && mValue[1] == 0); }

private:
  const int* mValue; // points to an array of ints of form: [len, degree, v0, v1, v2, ..., vr]
    // We are visiting this monomial, we do not own it!
};

/**
 * \ingroup polynomialrings
 */
class NCPolynomial
{
public:  
  typedef std::vector<ring_elem> coeffVector;
  typedef std::vector<int> monomVector;

  typedef coeffVector::iterator coeffIterator;
  typedef monomVector::iterator monomIterator;

  typedef coeffVector::const_iterator coeffConstIterator;
  typedef monomVector::const_iterator monomConstIterator;

  // this class is an non-const_iterator for traversing the terms in a polynomial.
  class iterator
  {
  public:
    // useful typedefs
    typedef iterator self_type;
    typedef std::forward_iterator_tag iterator_category;
    
    // constructor
    iterator(coeffIterator coeffIt, monomIterator monIt) : mCoeffIt(coeffIt), mMonomIt(monIt) { }
    
    // iteration functions
    self_type & operator++()
    {
      // prefix ++ operator
      stepIterators();
      return *this;
    }

    self_type operator++(int junk)
    {
      // postfix ++ operator
      self_type i = *this;
      stepIterators();
      return i;
    }

    // accessor functions -- (unfortunately) replace the more convenient -> notation since
    // we have two vector iterators.
    ring_elem coeff() { return *(this->mCoeffIt); }
    // for the record, we are using &*it here to get the pointer that records where an iterator currently is
    // this seems like a bit of a hack, but it seems to be the way things are done.
    // FRANK: Do we want to be making a copy here?
    NCMonomial monom() { return NCMonomial((&*(this->mMonomIt))); }
    
    // (in)equality checks
    bool operator==(const self_type& rhs) const { return (this->mCoeffIt == rhs.mCoeffIt); }
    bool operator!=(const self_type& rhs) const { return (this->mCoeffIt != rhs.mCoeffIt); }

  private:
    coeffIterator mCoeffIt;
    monomIterator mMonomIt;
    void stepIterators ()
    {
      // this is the function that actually increments the various iterators
      // increment the ring element first
      mCoeffIt++;
      // increment to the end of the monomial
      mMonomIt += *mMonomIt; // move to next monomial
    }
  };

  // this class is an non-const_iterator for traversing the terms in a polynomial.
  class const_iterator
  {
  public:
    // useful typedefs
    typedef const_iterator self_type;
    typedef std::forward_iterator_tag iterator_category;
    
    // constructor
    const_iterator(coeffConstIterator coeffIt, monomConstIterator monIt) : mCoeffIt(coeffIt), mMonomIt(monIt) { }
    
    // iteration functions
    self_type & operator++()
    {
      // prefix ++ operator
      stepIterators();
      return *this;
    }

    self_type operator++(int junk)
    {
      // postfix ++ operator
      self_type i = *this;
      stepIterators();
      return i;
    }

    // accessor functions -- (unfortunately) replace the more convenient -> notation since
    // we have two vector iterators.
    const ring_elem coeff() const { return *(this->mCoeffIt); }
    // for the record, we are using &*it here to get the pointer that records where an iterator currently is
    // this seems like a bit of a hack, but it seems to be the way things are done.
    // FRANK: Same as above, do we want to make a copy here?
    NCMonomial monom() const { return NCMonomial((&*(this->mMonomIt))); }
    
    // (in)equality checks
    bool operator==(const self_type& rhs) const { return (this->mCoeffIt == rhs.mCoeffIt); }
    bool operator!=(const self_type& rhs) const { return (this->mCoeffIt != rhs.mCoeffIt); }

  private:
    coeffConstIterator mCoeffIt;
    monomConstIterator mMonomIt;
    void stepIterators ()
    {
      // this is the function that actually increments the various iterators
      // increment the ring element first
      mCoeffIt++;
      // increment to the end of the monomial
      mMonomIt += *mMonomIt; // move to next monomial
    }
  };

  iterator begin()
  {
    return iterator(mCoefficients.begin(), mMonomials.begin());
  }

  iterator end()
  {
    return iterator(mCoefficients.end(), mMonomials.end());
  }

  const_iterator cbegin() const
  {
    return const_iterator(mCoefficients.cbegin(), mMonomials.cbegin());
  }

  const_iterator cend() const
  {
    return const_iterator(mCoefficients.cend(), mMonomials.cend());
  }

  void push_backCoeff(const ring_elem & val) {mCoefficients.push_back(val); }
  void push_backMonom(const int & val) {mMonomials.push_back(val); }

  void reserveCoeff(coeffVector::size_type n) { mCoefficients.reserve(n); }
  void reserveMonom(monomVector::size_type n) { mMonomials.reserve(n); }

  void appendPolynomial(const NCPolynomial* g)
  {
    // note: reserve is not necessary here.  It slows things down significantly
    // for some reason.
    mCoefficients.insert(mCoefficients.end(), g->getCoeffVector().begin(), g->getCoeffVector().end());
    mMonomials.insert(mMonomials.end(), g->getMonomVector().begin(), g->getMonomVector().end());
  }

  coeffConstIterator cbeginCoeff() const { return mCoefficients.cbegin(); }
  monomConstIterator cbeginMonom() const { return mMonomials.cbegin(); }

  coeffConstIterator cendCoeff() const { return mCoefficients.cend(); }
  monomConstIterator cendMonom() const { return mMonomials.cend(); }

  void copyAllCoeffs(const coeffVector & rhs ) { mCoefficients = rhs; }
  void copyAllMonoms(const monomVector & rhs ) { mMonomials = rhs; }

  const coeffVector & getCoeffVector() const { return mCoefficients; }
  const monomVector & getMonomVector() const { return mMonomials; }
  
  coeffVector::size_type numTerms() const { return mCoefficients.size(); }

private:
  std::vector<ring_elem> mCoefficients;
  std::vector<int> mMonomials;
  // each monomial is of the form:
  //  <degree> <var1> <var2> ... <varn> <-1>
  // e.g. xyxy: 4 0 1 0 1 -1
  //   xy23x: 25 0 1 1 ... 1 0 -1
  // 2 monomials: xzx, xy
  //   3 0 2 1 -1 2 0 1 -1

};

class NCFreeAlgebra : public Ring
{
  NCFreeAlgebra(const Ring* K,
                M2_ArrayString names);
public:
  static NCFreeAlgebra* create(const Ring* K,
                               M2_ArrayString names,
                               const PolynomialRing* degreeRing);

  // these are all the functions from Ring that must exist for NCFreeAlgebra to be instantiated
  virtual void text_out(buffer &o) const;
  virtual unsigned int computeHashValue(const ring_elem a) const;
  virtual ring_elem from_long(long n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem from_rational(mpq_ptr q) const;
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
  
  // some internal functions for the above routines
  ring_elem mult_by_term_right(const ring_elem f, const ring_elem c, const NCMonomial) const;
  ring_elem mult_by_term_left(const ring_elem f, const ring_elem c, const NCMonomial) const;
  void add_to_end(ring_elem f, const ring_elem g) const;
  int compare_monoms(const NCMonomial m1, const NCMonomial m2) const;

  // casting functions
  virtual const NCFreeAlgebra * cast_to_NCFreeAlgebra()  const { return this; }
  virtual       NCFreeAlgebra * cast_to_NCFreeAlgebra()        { return this; }

private:
  std::vector<std::string> mVariableNames;
  const Ring& mCoefficientRing;
  unsigned int mNumVars;
};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
