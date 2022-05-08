#ifndef _polynomial_hpp_
#define _polynomial_hpp_

#include "newdelete.hpp"  // for VECTOR, our_new_delete
#include "ringelem.hpp"   // for ring_elem
#include "style.hpp"      // for GT, LT, EQ

#include <cassert>        // for assert
#include <algorithm>      // for copy
#include <iostream>       // for ostream
#include <iterator>       // for forward_iterator_tag
#include <utility>        // for pair, make_pair

using IntVector = VECTOR(int);
// using IntVector = std::vector<int>;

struct Monom
// Format for monomials:
  // A monomial is an array of ints, the first of which is the length of that array (including length field).
  // e.g. [2 0] (is currently the empty monomial, but this class knows nothing about specific monomials.
  // each monomial is of the form:
  //  <length: n+2> <degree: (currently) n> <var1> <var2> ... <varn>
  // e.g. xyxy: 6 4 0 1 0 1
  //   xy23x: 27 25 0 1 1 ... 1 0
  // 2 monomials: xzx, xy (dot is only there for readability)
  //   5 3 0 2 0 . 4 2 0 1
  // TODO: There are now weights in the monomial (which only the monoid knows about),
  //       so this needs to be updated.
{
  Monom() : mValue(nullptr) {}
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

std::ostream& operator<<(std::ostream& o, const Monom& m);

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
  friend class M2FreeAlgebra;
  friend class M2FreeAlgebraOrQuotient;
  friend class FreeAlgebra;
  friend class NCF4;
  
  typedef typename CoefficientRingType::ElementType ElementType;
public:  
  typedef typename VECTOR(ElementType) coeffVector;
  using monomVector = IntVector; // TODO: remove monomVector?

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

    std::pair<ring_elem, Monom> operator*() const { return std::make_pair(coeff(), monom()); }
    
    // (in)equality checks
    bool operator==(const self_type& rhs) const { return (this->mCoeffIt == rhs.mCoeffIt); }
    bool operator!=(const self_type& rhs) const { return (this->mCoeffIt != rhs.mCoeffIt); }

    coeffConstIterator cCoeffIterator() const { return mCoeffIt; }
    monomConstIterator cMonomIterator() const { return mMonomIt; }

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

  coeffIterator beginCoeff() { return mCoefficients.begin(); }
  coeffIterator endCoeff() { return mCoefficients.end(); }
  
  coeffConstIterator cbeginCoeff() const { return mCoefficients.cbegin(); }
  monomConstIterator cbeginMonom() const { return mMonomials.cbegin(); }

  coeffConstIterator cendCoeff() const { return mCoefficients.cend(); }
  monomConstIterator cendMonom() const { return mMonomials.cend(); }

  const coeffVector & getElementArray() const { return mCoefficients; }

  const monomVector & getMonomVector() const { return mMonomials; }
  monomVector & getMonomInserter() { return mMonomials; }

  size_t numTerms() const { return mCoefficients.size(); }

private:
  coeffVector & getCoeffInserter() { return mCoefficients; }
  
private:
  coeffVector mCoefficients;
  monomVector mMonomials;
};

struct CoefficientRingType
{
  typedef ring_elem ElementType;
};

using Poly = Polynomial<CoefficientRingType>;
using PolyList = VECTOR(Poly*);
using ConstPolyList = VECTOR(const Poly*);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
