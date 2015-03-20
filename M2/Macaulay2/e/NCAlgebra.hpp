#ifndef _nc_algebra_hpp_
#define _nc_algebra_hpp_

#include "ringelem.hpp"
#include "engine.h"

class buffer;
class Ring;

#include "ring.hpp"

/**
 * \ingroup polynomialrings
 */
class NCPolynomial
{
public:
  std::vector<ring_elem> mCoefficients;
  std::vector<int> mMonomials;
  // each monomial is of the form:
  //  <degree> <var1> <var2> ... <varn> <-1>
  // e.g. xyxy: 4 0 1 0 1 -1
  //   xy23x: 25 0 1 1 ... 1 0 -1
  // 2 monomials: xzx, xy
  //   3 0 2 1 -1 2 0 1 -1
  
  // this class is an iterator for traversing the terms in a polynomial.
  class iterator
  {
  public:
    // useful typedefs
    typedef iterator self_type;
    typedef std::vector<ring_elem>::iterator coeffIterator;
    typedef std::vector<int>::iterator monIterator;
    typedef std::forward_iterator_tag iterator_category;
    
    // constructor
    iterator(coeffIterator coeffIt, monIterator monIt) : coeffIt_(coeffIt), monIt_(monIt) { }
    
    // iteration functions
    self_type & operator++()
    {
      // prefix ++ operator
      self_type i = *this;
      stepIterators();
      return i;
    }

    self_type operator++(int junk)
    {
      // postfix ++ operator
      stepIterators();
      return *this;
    }

    // accessor functions -- (unfortunately) replace the more convenient -> notation since
    // we have two vector iterators.
    ring_elem coeff { return *(this->coeffIt_); }
    int monom { return *(this->monIt_); }
    
    // (in)equality checks
    bool operator==(const self_type& rhs) { return ((this->coeffIt_ == rhs.coeffIt_) && (this->monIt_ == rhs.monIt_)); }
    bool operator!=(const self_type& rhs) { return ((this->coeffIt_ != rhs.coeffIt_) || (this->monIt_ != rhs.monIt_)); }

  private:
    coeffIterator coeffIt_;
    monIterator monIt_;
    void stepIterators ()
    {
      // this is the function that actually increments the various iterators
      // increment the ring element first
      coeffIt_++;
      // increment to the end of the monomial
      while (*monIt_ != -1) monIt_++;
      // one more to the start of next (or the .end())
      monIt_++;
    }
  };

  iterator begin()
  {
    return iterator(coeffIt_.begin(), monIt_.begin());
  }

  iterator end()
  {
    return iterator(coeffIt_.end(), monIt_.end());
  }
};

class NCFreeAlgebra : public Ring
{
  NCFreeAlgebra(const Ring* K,
                M2_ArrayString names);
public:
  static NCFreeAlgebra* create(const Ring* K,
                               M2_ArrayString names);

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
  
private:
  std::vector<std::string> mVariableNames;
  const Ring& mCoefficientRing;
};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
