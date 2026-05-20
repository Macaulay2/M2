/**
 * @file BasicPolyList.hpp
 * @brief Ring-agnostic polynomial-list transport type plus its streaming collector and emitter.
 *
 * `BasicPolyList` is a `std::vector<BasicPoly>` that acts as the
 * hub format between M2-side `Matrix` values, the F4
 * Groebner-basis engine in `gb-f4/`, file-format readers (msolve,
 * raw text), and the `BasicPolyListParser`. The type carries no
 * ring reference, which keeps each conversion as a single
 * `BasicPolyList`-to-other transcript rather than every pair
 * needing its own translator. Coefficient support currently
 * inherits `BasicPoly`'s `mpz_class`-only restriction; `GF(p^n)`,
 * `QQ`, fraction fields, and recursive polynomial coefficients
 * are flagged TODOs in the in-source comment.
 *
 * Two helpers wire `BasicPolyList` into the stream protocol from
 * `PolynomialStream.hpp`: `BasicPolyListStreamCollector` is the
 * **consumer** --- it builds a `BasicPolyList` from a sequence of
 * `idealBegin` / `appendPolynomialBegin` / term events; the
 * template `toStream<S>(Fs, str)` is the dual **producer** ---
 * it walks a `BasicPolyList` and emits the same event sequence
 * into any `S`. `toMatrix(FreeModule*, BasicPolyList)` composes
 * `toStream` with a `MatrixStream` (`matrix-stream.hpp`) to
 * produce an M2 `Matrix`. The free `bytesUsed`,
 * `basicPolyListFromString`, and `basicPolyListFromFile` helpers
 * round out the API.
 *
 * @see BasicPoly.hpp
 * @see PolynomialStream.hpp
 * @see BasicPolyListParser.hpp
 * @see matrix-stream.hpp
 */

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

/**
 * @brief Streaming consumer that builds a `BasicPolyList` from per-term
 * callbacks, matching the mathicgb / mgb stream interface.
 *
 * @details Receives `appendPolynomialBegin / appendTermBegin /
 * appendExponent / appendTermDone / appendPolynomialDone` calls
 * from a polynomial producer (typically the mathicgb stream
 * parser) and assembles the result into `mValue`. Stores the
 * declared modulus, variable count, and component count so the
 * caller can ask the collector to echo them back even though the
 * `BasicPoly` representation itself does not use them. Used for
 * portable polynomial I/O outside the typed `Ring` machinery.
 */
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
