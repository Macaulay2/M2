#pragma once

#include "../VectorArithmetic.hpp"
#include "../BasicPolyList.hpp"
#include "MonomialHashTable.hpp"
#include "MonomialTypes.hpp"
#include "PolynomialStream.hpp"

namespace newf4 {

class Polynomial;

// This class will store the input to the GB commands, as well as
// any intermediate polynomials encountered along the way.
class PolynomialList
{
 private:
  const VectorArithmetic& mVectorArithmetic;
  MonomialHashTable& mHashTable;
  std::vector<Polynomial> mPolynomials;

 public:
  PolynomialList(const VectorArithmetic& VA, MonomialHashTable& hashTable)
      : mVectorArithmetic(VA), mHashTable(hashTable)
  {
  }
  
  const MonomialHashTable& monomialHashTable() const { return mHashTable; }
  MonomialHashTable& monomialHashTable() { return mHashTable; }

  const VectorArithmetic& vectorArithmetic() const { return mVectorArithmetic; }

  Polynomial& operator[](int index) { return mPolynomials[index]; }
  const Polynomial& operator[](int index) const { return mPolynomials[index]; }

  size_t size() const { return mPolynomials.size(); }

  void push_back(const Polynomial& F)  { mPolynomials.push_back(F); }
};

class Polynomial
{
  friend class PolynomialListStream;
  //friend class PolynomialList;

public: // TODO: change back to private
  ElementArray                mCoefficients;
  std::vector<MonomialIndex>  mMonomials; // each monomial is an index into a vector of polynomials.
  std::vector<ComponentIndex> mComponents;
  PolynomialList&             mPolynomialList;

public:

  Polynomial(PolynomialList& polynomialList) :
     mPolynomialList(polynomialList)
  {
  }

  template< bool Const = false >
  class PolynomialIterator
  {
  public:
    using iterator_category = std::forward_iterator_tag;

    using ElementType = long; // not sure what to put here...

    // is ElementArray always just std::vector<ElementType>?

    //using CoeffIterator = typename std::conditional_t< Const,
    //                                                   std::vector<ElementType>::const_iterator,
    //                                                   std::vector<ElementType>::iterator>;

    // we will be using the VectorArithmetic object to access the data
    using CoeffIterator = typename std::size_t;

    using MonomIterator = typename std::conditional_t< Const,
                                                       std::vector<MonomialIndex>::const_iterator,
                                                       std::vector<MonomialIndex>::iterator>;
    
    using ComponentIterator = typename std::conditional_t< Const,
                                                           std::vector<ComponentIndex>::const_iterator,
                                                           std::vector<ComponentIndex>::iterator>;

    //using CoeffReference = typename std::conditional_t< Const, ElementType const &, ElementType & >;
    //using CoeffPointer = typename std::conditional_t< Const, ElementType const *, ElementType * >;    

    using MonViewReference = typename std::conditional_t< Const, MonomialView const &, MonomialView & >;
    using MonViewPointer = typename std::conditional_t< Const, MonomialView const *, MonomialView * >;    

    using MonIndexReference = typename std::conditional_t< Const, MonomialIndex const &, MonomialIndex & >;
    using MonIndexPointer = typename std::conditional_t< Const, MonomialIndex const *, MonomialIndex * >;    

    using CompReference = typename std::conditional_t< Const, ComponentIndex const &, ComponentIndex & >;
    using CompPointer = typename std::conditional_t< Const, ComponentIndex const *, ComponentIndex * >;    

    //using MonomIterator = MonomialView::MonomialIterator<Const>;
    // need to figure out a way to know the type of element array of polynomial type here.
    // probably will require another template parameter of the ring.

  private:
    CoeffIterator      mCoeffIterator;
    MonomIterator      mMonomIterator;
    ComponentIterator  mCompIterator;
    const Polynomial&  mPolynomial;

  public:

    PolynomialIterator(CoeffIterator coeffIterator,
                       MonomIterator monomIterator,
                       ComponentIterator compIterator,
                       const Polynomial& polynomial) :
      mCoeffIterator(coeffIterator),
      mMonomIterator(monomIterator),
      mCompIterator(compIterator),
      mPolynomial(polynomial)
    {
    }

    // SFINAE enables the const dereference operator or the non 
    // const variant depending on bool Const parameter
    // const version
#if 0    
    template< bool _Const = Const >
    std::enable_if_t< _Const, CoeffReference >
    coeff() const {
       return *mCoeffIterator;
    }
    
    // non-const version
    template< bool _Const = Const >
    std::enable_if_t< !_Const, CoeffReference >
    coeff() {
       return *mCoeffIterator;
    }
#endif

    // TODO: this is horrid, but I don't yet know how to make a coeff iterator through the vector arithmetic object
    ElementType coeff() const { return mPolynomial.polynomialList().vectorArithmetic().to_modp_long(mPolynomial.mCoefficients, mCoeffIterator); }

    // const version
    template< bool _Const = Const >
    std::enable_if_t< _Const, MonViewReference >
    monom() const {
       return mPolynomial.polynomialList().monomialHashTable().monomialAt(*mMonomIterator);
    }

    // non-const version
    template< bool _Const = Const >
    std::enable_if_t< !_Const, MonViewReference >
    monom() {
       return mPolynomial.polynomialList().monomialHashTable().monomialAt(*mMonomIterator);
    }

    // const version
    template< bool _Const = Const >
    std::enable_if_t< _Const, CompReference >
    comp() const {
       return *mCompIterator;
    }

    // non-const version
    template< bool _Const = Const >
    std::enable_if_t< !_Const, CompReference >
    comp() {
       return *mCompIterator;
    }
    
    bool operator==(const PolynomialIterator& rhs) const { return mMonomIterator == rhs.mMonomIterator; }
    bool operator!=(const PolynomialIterator& rhs) const { return mMonomIterator != rhs.mMonomIterator; }

  private:
    void stepIterators() { ++mMonomIterator; ++mCompIterator; ++mCoeffIterator; }
  };

 public:

  auto begin() const -> PolynomialIterator<true> { return PolynomialIterator<true>(0,
                                                                                   mMonomials.begin(),
                                                                                   mComponents.begin(),
                                                                                   *this); }
  auto end() const -> PolynomialIterator<true> { return PolynomialIterator<true>(mPolynomialList.vectorArithmetic().size(mCoefficients),
                                                                                 mMonomials.end(),
                                                                                 mComponents.end(),
                                                                                 *this); }
  auto begin() -> PolynomialIterator<false> { return PolynomialIterator<false>(0,
                                                                               mMonomials.begin(),
                                                                               mComponents.begin(),
                                                                               *this); }
  auto end() -> PolynomialIterator<false> { return PolynomialIterator<false>(mPolynomialList.vectorArithmetic().size(mCoefficients),
                                                                             mMonomials.end(),
                                                                             mComponents.end(),
                                                                             *this); }

  auto polynomialList() const -> const PolynomialList& { return mPolynomialList; }
  auto polynomialList() -> PolynomialList& { return mPolynomialList; }

  // creation (output iterator?)
  // iteration (for a const one) (similar to NC Poly)
  // access
};

/// implements the stream functions for creating a PolynomialList from a stream
class PolynomialListStreamCollector
{
public:
  using Coefficient = BasicPolyListStreamCollector::Coefficient;
  using VarIndex = BasicPolyListStreamCollector::VarIndex;
  using Exponent = BasicPolyListStreamCollector::Exponent;
  using Component = BasicPolyListStreamCollector::Component;

private:
  // We store these, as we need to be able to repsond to what they are,
  // but we don't use them here at all.
  Coefficient mModulus;
  VarIndex mVarCount;
  Component mComCount;

  // State during the construction
  std::vector<Coefficient> mCoefficients;
  std::vector<Exponent> mSparseMonomial; // format: [size in ints, v1, e1, v2, e2, ..., vn, en], size=2*n+1.
  long mCurrentPoly; // index of one we are working on
  long mCurrentTerm; // index of term we are working on

  PolynomialList& mValue; // previously constructed outside of this function??
public:
  PolynomialListStreamCollector(int modulus, int varCount, int comCount, PolynomialList& result)
    : mModulus(modulus),
      mVarCount(varCount),
      mComCount(comCount),
      mCoefficients(),
      mSparseMonomial(),
      mCurrentPoly(-1),
      mCurrentTerm(-1),
      mValue(result)
  {
  }

  // todo: value() should grab the resulting object, not just copy it.
  PolynomialList& value() const
  {
    return mValue;
  }

  // Fields required for the general stream interface (see mathicgb::mathicgb.h)

  Coefficient modulus() const  { return mModulus; }
  VarIndex varCount() const { return mVarCount; }
  Component comCount() const { return mComCount; }

  void idealBegin(size_t polyCount);
  void appendPolynomialBegin(size_t termCount);
  void appendTermBegin(Component com);
  void appendExponent(VarIndex index, Exponent exponent);
  void appendTermDone(Coefficient coefficient);
  void appendPolynomialDone();
  void idealDone();
};

// TODO: once we go to c++20, enable the concept PolynomialStream
//template<PolynomialStream S>
template<typename S>
void toStream(const PolynomialList& Fs, S &str)
{
  str.idealBegin(Fs.size());
  for (auto i=0; i<Fs.size(); ++i)
    {
      auto& F = Fs[i];
      str.appendPolynomialBegin(F.mMonomials.size());
      for (auto i=0; i<F.mMonomials.size(); ++i)
        {
          // get monomial
          // write it out here using appendTermBegin, appendExponent, appendTermDone.
          if (F.mComponents.empty())
            str.appendTermBegin(0);
          else
            str.appendTermBegin(F.mComponents[i]);
          MonomialView monom = Fs.monomialHashTable().monomialAt(F.mMonomials[i]);
          for (auto ve = monom.begin(); ve != monom.end(); ++ve)
            {
              str.appendExponent(ve.var(), ve.power());
            }
          long val = Fs.vectorArithmetic().to_modp_long(F.mCoefficients, i);
          str.appendTermDone(val);
        }
      str.appendPolynomialDone();
    }
  str.idealDone();
}

} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
