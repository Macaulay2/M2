#pragma once

#include "GBF4Computation.hpp"
#include "PolynomialList.hpp"

namespace newf4 {

/// implements the stream functions for creating a PolynomialList from a stream
class PolynomialListStream
{
public:
  using Coefficient = long; // or perhaps uint32_t?
  using VarIndex = uint32_t;
  using Exponent = uint32_t;
  using Component = uint32_t;

private:
  // We store these, as we need to be able to repsond to what they are,
  // but we don't use them here at all.
  Coefficient mModulus;
  VarIndex mVarCount;
  Component mComCount;

  // State during the construction
  std::vector<long> mCoefficients;
  std::vector<Exponent> mSparseMonomial; // format: [size in ints, v1, e1, v2, e2, ..., vn, en], size=2*n+1.
  long mCurrentPoly;
  long mCurrentTerm;

  PolynomialList& mValue;
public:
  PolynomialListStream(PolynomialList& result, int modulus, int varCount, int comCount)
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
  
class GBF4Interface
{
 private:
  GBF4Computation mComputation;
};

} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
