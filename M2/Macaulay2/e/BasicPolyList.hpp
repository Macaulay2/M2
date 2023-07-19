// BasicPolyList is a vector of polynomials (with components)
// which we can easily translate to and from other polynoial and matrix types.
// This class really doesn't require any ring.
// Current restriction: the coefficients must be an integral type. TODO: allow infinite precision integers too.
//   TODO: how should we handle coefficients which are: GF(p^n), QQ, fraction fields? or even polynomials?
#pragma once

#include "exceptions.hpp"

#include <stdexcept>
#include <string>
#include <vector>
#include <iostream>

class FreeModule;
class Matrix;

struct parsing_error : public exc::engine_error
{
  explicit parsing_error(const std::string &msg) : exc::engine_error(msg) {}
};
  
class BasicPoly
{
public:
  std::vector<int> mCoefficients;
  std::vector<int> mComponents; // if zero length: all components are 0.
  std::vector<int> mMonomials; // a concatenated list of varpower monomials.  Each first entry is its length.

  void debug_display(std::ostream& o) const;
};

using BasicPolyList = std::vector<BasicPoly>;

class BasicPolyListStreamCollector
{
  using Coefficient = int32_t;
  using VarIndex = int32_t; // TODO: these must match BasicPoly above.
  using Exponent = int32_t;
  using Component = int32_t;
 private:
  BasicPolyList mValue;

  // We store these, as we need to be able to respond to what they are,
  // but we don't use them here at all.
  Coefficient mModulus;
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

  Coefficient modulus() const { return mModulus; }
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
void toStream(const BasicPolyList& Fs, Stream &S)
{
  S.idealBegin(Fs.size());
  for (auto& F : Fs)
    {
      S.appendPolynomialBegin(F.mCoefficients.size());
      int monomStart = 0;
      for (auto i=0; i<F.mCoefficients.size(); ++i)
        {
          auto monomEnd = monomStart + F.mMonomials[monomStart];
          if (F.mComponents.empty())
            S.appendTermBegin(0);
          else
            S.appendTermBegin(F.mComponents[i]);
          for (auto j=monomStart+1; j<monomEnd; j += 2)
            S.appendExponent(F.mMonomials[j], F.mMonomials[j+1]);
          S.appendTermDone(F.mCoefficients[i]);
          monomStart = monomEnd;
        }
      S.appendPolynomialDone();
    }
  S.idealDone();
}

const Matrix* toMatrix(const FreeModule *target, const BasicPolyList& Fs);

// The following can certainly throw an error.  You need to check that!
auto basicPolyListFromString(std::vector<std::string> varNames, std::string polyPerLine) -> BasicPolyList;
auto basicPolyListFromFile(std::vector<std::string> varNames, std::string fileName) -> BasicPolyList;

// Local Variables:
// indent-tabs-mode: nil
// End:
