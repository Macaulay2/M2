#ifndef _polynomial_hpp_
#define _polynomial_hpp_

#include "ringelem.hpp"
#include "engine.h"

class buffer;
class Ring;

#include "ring.hpp"
#include <iostream>
class PolynomialAlgebra;

struct Monom
// Format for monomials:
  // A monomial is an array of ints, the first of which is the length of that array (including length field).
  // e.g. [2 0] (is currently the empty monomial, but this class knows nothing about specific monomials.
  // each monomial is of the form:
  //  <length: n+2> <degree: (currently) n> <var1> <var2> ... <varn>
  // e.g. xyxy: 6 4 0 1 0 1
  //   xy23x: 27 25 0 1 1 ... 1 0
  // 2 monomials: xzx, xy (dot is only there for readability)
  //   5 3 0 2 1 . 4 2 0 1
{
  Monom(const int* value) : mValue(value) {}
  //  const int* operator*() const { return mValue; }
  const int* operator+(int i) const { return mValue+i; }
  int operator[](int i) const { return mValue[i]; }

  int size() const { return *mValue; }
  
  const int* begin() const { return mValue; }
  const int* end() const { return mValue + *mValue; }

private:
  const int* mValue; // We are visiting this monomial, we do not own it!
};

class ModuleMonom
// Format for such a monomial:
// [len value hashval comp deg v1 v2 ... vr]
// where [len-3 deg v1 v2 ... vr] is a Monom.
{
  
public:
  ModuleMonom(int* begin) : mValue(begin) {}

  //  const int* operator*() const { return mValue; }
  const int* operator+(int i) const { return mValue+i; }
  int operator[](int i) const { return mValue[i]; }
  int* operator+(int i) { return mValue+i; }
  int& operator[](int i)  { return mValue[i]; }

  int size() const { return *mValue; }
  
  const int* begin() const { return mValue; }
  const int* end() const { return mValue + *mValue; }
  int* begin() { return mValue; }
  int* end() { return mValue + *mValue; }

  int component() const { return mValue[3]; }
  
  static int sizeOfCorrespondingModuleMonom(const Monom& m)
  {
    return m.size() + 3;
  }

  void setIndex(int idx)
  {
    mValue[1] = idx;
  }

  int index() const { return mValue[1]; }
  
  std::size_t hash() const
  {
    if (mValue[2] == 0) setHashValue();
    return mValue[2];
  }

  static int compare(const ModuleMonom& m1, const ModuleMonom& m2)
  {
    if (m1[2] > m2[2]) return GT;
    if (m1[2] < m2[2]) return LT;
    if (m1[3] > m2[3]) return GT;
    if (m1[3] < m2[3]) return LT;
    // at this stage, they have the same degree, so use lex order
    for (int j = 4; j < m1[0]; j++)
      {
        if (m1[j] > m2[j]) return LT;
        if (m1[j] < m2[j]) return GT;
      }
    // if we are here, the monomials are the same.
    return EQ;
  }

  bool operator==(const ModuleMonom &rhs) const
  {
    if (mValue[0] != rhs[0]) return false;
    for (int i=2; i < mValue[0]; ++i)
      if (mValue[i] != rhs[i]) return false;
    return true;
  }
private:
  void setHashValue() const
  {
    int result = 0;
    int* end = mValue + *mValue;
    for (auto i = mValue+3; i < end; ++i)
      result = 17*result + *i;
    const_cast<int*>(mValue)[2] = result;
  }
private:
  int* mValue; // We are visiting this monomial, we do not own it!
};

std::ostream& operator<<(std::ostream& o, const ModuleMonom& m);

inline ModuleMonom monomToModuleMonom(const Monom& a, int comp, std::pair<int*, int*> allocated_result)
{
  assert(allocated_result.second-allocated_result.first >= ModuleMonom::sizeOfCorrespondingModuleMonom(a));
  auto begin = allocated_result.first;
  begin[0] = ModuleMonom::sizeOfCorrespondingModuleMonom(a);
  begin[1] = 0; // index
  begin[2] = 0; // hashval
  begin[3] = comp;
  std::copy(a.begin()+1, a.end(), begin+4);
  return ModuleMonom(begin);
}

template<typename T>
void appendModuleMonomToMonom(const ModuleMonom& a, int& comp, T& inserter)
{
  inserter.push_back(a.size()-3);
  for (int i=4; i<a.size(); ++i)
    inserter.push_back(a[i]);
}

/**
 * \ingroup polynomialrings
 */
template<typename CoefficientRingType>
class Polynomial : public our_new_delete
{
  friend class PolynomialAlgebra;
  friend class FreeAlgebra;
  
  typedef typename CoefficientRingType::ElementType ElementType;
public:  
  typedef typename VECTOR(ElementType) coeffVector;
  typedef std::vector<int> monomVector;

  typedef typename coeffVector::iterator coeffIterator;
  typedef monomVector::iterator monomIterator;

  typedef typename coeffVector::const_iterator coeffConstIterator;
  typedef monomVector::const_iterator monomConstIterator;

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
    Monom monom() const { return Monom((&*(this->mMonomIt))); }

    // MES: added 2- Dec 2017:
    //    std::pair<ring_elem, Monom> operator*() const { return std::make_pair(coeff(), monom()); }
    
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

  const_iterator cbegin() const
  {
    return const_iterator(mCoefficients.cbegin(), mMonomials.cbegin());
  }

  const_iterator cend() const
  {
    return const_iterator(mCoefficients.cend(), mMonomials.cend());
  }

  coeffConstIterator cbeginCoeff() const { return mCoefficients.cbegin(); }
  monomConstIterator cbeginMonom() const { return mMonomials.cbegin(); }

  coeffConstIterator cendCoeff() const { return mCoefficients.cend(); }
  monomConstIterator cendMonom() const { return mMonomials.cend(); }

#if 0
  // TODO (Frank+Mike): put some of these back in once we need them
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


  void copyAllCoeffs(const coeffVector & rhs ) { mCoefficients = rhs; }
  void copyAllMonoms(const monomVector & rhs ) { mMonomials = rhs; }

  const coeffVector & getCoeffVector() const { return mCoefficients; }

#endif  
  const monomVector & getMonomVector() const { return mMonomials; }
  monomVector & getMonomInserter() { return mMonomials; }

  size_t numTerms() const { return mCoefficients.size(); }

private:
  coeffVector & getCoeffInserter() { return mCoefficients; }
  
private:
  VECTOR(ElementType) mCoefficients;
  std::vector<int> mMonomials;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
