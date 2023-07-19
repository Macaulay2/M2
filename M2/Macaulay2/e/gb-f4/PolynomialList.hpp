#pragma once

#include "../VectorArithmetic.hpp"
#include "../BasicPolyList.hpp"
#include "MonomialHashTable.hpp"
#include "MonomialTypes.hpp"

namespace newf4 {

class Polynomial
{
  friend class PolynomialListStream;
public: // TODO: change back to private
  ElementArray mCoefficients;
  std::vector<ComponentIndex> mComponents;
  std::vector<MonomialIndex> mMonomials; // each monomial is an index into a vector of polynomials.

 public:
  // creation (output iterator?)
  // iteration (for a const one) (similar to NC Poly)
  // access
};

// This class will store the input to the GB commands, as well as
// any intermediate polynomials encountered along the way.
class PolynomialList
{
 private:
  const VectorArithmetic& mVectorArithmetic;
  MonomialHashTable mHashTable;
  std::vector<Polynomial> mPolynomials;

 public:
  PolynomialList(const VectorArithmetic& VA)
      : mVectorArithmetic(VA), mHashTable()
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

template<typename Stream>
void toStream(const PolynomialList& Fs, Stream &S)
{
  S.idealBegin(Fs.size());
  for (auto i=0; i<Fs.size(); ++i)
    {
      auto& F = Fs[i];
      S.appendPolynomialBegin(F.mMonomials.size());
      for (auto i=0; i<F.mMonomials.size(); ++i)
        {
          // get monomial
          // write it out here using appendTermBegin, appendExponent, appendTermDone.
          if (F.mComponents.empty())
            S.appendTermBegin(0);
          else
            S.appendTermBegin(F.mComponents[i]);
          MonomialView monom = Fs.monomialHashTable().monomialAt(F.mMonomials[i]);
          for (auto ve = monom.begin() + 1; ve != monom.end(); ve += 2)
            {
              S.appendExponent(*ve, *(ve+1));
            }
          long val = Fs.vectorArithmetic().to_modp_long(static_cast<ElementArray>(F.mCoefficients), i);
          S.appendTermDone(val);
        }
      S.appendPolynomialDone();
    }
  S.idealDone();
}

} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
