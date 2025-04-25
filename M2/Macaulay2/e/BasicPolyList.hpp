// BasicPolyList is a vector of polynomials (with components)
// which we can easily translate to and from other polynomial and matrix types.
// This class really doesn't require any ring.
// Current restriction: the coefficients must be an integral type. TODO: allow infinite precision integers too.
//   TODO: how should we handle coefficients which are: GF(p^n), QQ, fraction fields? or even polynomials?
#pragma once

#include "exceptions.hpp"

#include <stdexcept>
#include <string>
#include <vector>
#include <iostream>

#include "BasicPoly.hpp"
#include "PolynomialStream.hpp"

class FreeModule;
class Matrix;

using BasicPolyList = std::vector<BasicPoly>;

long bytesUsed(const BasicPolyList& F);

class BasicPolyListStreamCollector
{
public:
  using Coefficient = mpz_class;
  using VarIndex = int32_t; // TODO: these must match BasicPoly above.
  using Exponent = int32_t;
  using Component = int32_t;
  using ModulusType = int32_t;
private:
  BasicPolyList mValue;

  // We store these, as we need to be able to respond to what they are,
  // but we don't use them here at all.
  ModulusType mModulus;
  VarIndex mVarCount;
  Component mComCount;

  // State during the construction
  long mCurrentPoly;
  long mCurrentTerm;
  long mSizeEntryInMonomial;

public:
  // modulus, varCount, comCount can be set to 0, they are not used in this class.
  BasicPolyListStreamCollector(); // Set the three variables all to 0.
  BasicPolyListStreamCollector(Coefficient modulus, VarIndex varCount, Component comCount);
  ~BasicPolyListStreamCollector() = default;

  BasicPolyList value()
  {
    return mValue;
  }

  // Fields required for the general stream interface (see mathicgb::mathicgb.h)
  
  ModulusType modulus() const { return mModulus; }
  VarIndex varCount() const { return mVarCount; }
  Component comCount() const { return mComCount; }

  void idealBegin(size_t polyCount);
  void appendPolynomialBegin(size_t termCount);
  void appendTermBegin(Component com);
  void appendExponent(VarIndex index, Exponent exponent);
  void appendTermDone(const Coefficient& coefficient);
  void appendPolynomialDone();
  void idealDone();
};

//template<newf4::PolynomialStream S>
template<typename S>
void toStream(const BasicPolyList& Fs, S &str)
{
  str.idealBegin(Fs.size());
  for (auto& F : Fs)
    {
      str.appendPolynomialBegin(F.mCoefficients.size());
      int monomStart = 0;
      for (auto i=0; i<F.mCoefficients.size(); ++i)
        {
          auto monomEnd = monomStart + F.mMonomials[monomStart];
          if (F.mComponents.empty())
            str.appendTermBegin(0);
          else
            str.appendTermBegin(F.mComponents[i]);
          for (auto j=monomStart+1; j<monomEnd; j += 2)
            str.appendExponent(F.mMonomials[j], F.mMonomials[j+1]);
          str.appendTermDone(F.mCoefficients[i]);
          monomStart = monomEnd;
        }
      str.appendPolynomialDone();
    }
  str.idealDone();
}

const Matrix* toMatrix(const FreeModule *target, const BasicPolyList& Fs);

// The following can certainly throw an error.  You need to check that!
auto basicPolyListFromString(std::vector<std::string> varNames, std::string polyPerLine) -> BasicPolyList;
auto basicPolyListFromFile(std::vector<std::string> varNames, std::string fileName) -> BasicPolyList;

// Local Variables:
// indent-tabs-mode: nil
// End:
