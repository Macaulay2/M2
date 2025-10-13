#include "BasicPolyList.hpp"
#include "matrix.hpp"
#include "matrix-stream.hpp"
#include <sstream>
const Matrix* toMatrix(const FreeModule *target, const BasicPolyList& Fs)
{
  MatrixStream S(target);
  toStream(Fs, S);
  return S.value();
}

void BasicPolyListStreamCollector::idealBegin(size_t polyCount)
{
  mValue.clear();
  mCurrentPoly = -1;
  mValue.resize(polyCount);
}

void BasicPolyListStreamCollector::appendPolynomialBegin(size_t termCount)
{
  mCurrentPoly++;
  mCurrentTerm = -1;
  mValue[mCurrentPoly].mCoefficients.resize(termCount);
  mValue[mCurrentPoly].mComponents.resize(termCount);
  // We don't know the size of the monomial array yet.
}

void BasicPolyListStreamCollector::appendTermBegin(Component com)
{
  mCurrentTerm++;
  mValue[mCurrentPoly].mComponents[mCurrentTerm] = com;
  mSizeEntryInMonomial = mValue[mCurrentPoly].mMonomials.size();
  mValue[mCurrentPoly].mMonomials.push_back(1);
}

void BasicPolyListStreamCollector::appendExponent(VarIndex index, Exponent exponent)
{
  mValue[mCurrentPoly].mMonomials.push_back(index);
  mValue[mCurrentPoly].mMonomials.push_back(exponent);
  mValue[mCurrentPoly].mMonomials[mSizeEntryInMonomial] += 2;
}

void BasicPolyListStreamCollector::appendTermDone(const Coefficient& coefficient)
{
  mValue[mCurrentPoly].mCoefficients[mCurrentTerm] = coefficient;
}

void BasicPolyListStreamCollector::appendPolynomialDone()
{
  if (mCurrentTerm != mValue[mCurrentPoly].mComponents.size() - 1)
    throw exc::engine_error("internal error: building PolyList from stream has incorrect number of terms in a polynomial");
}

void BasicPolyListStreamCollector::idealDone()
{
  if (mCurrentPoly != mValue.size() - 1)
    throw exc::engine_error("internal error: building PolyList from stream has incorrect number of polynomials");
}

// TODO: output components too


long bytesUsed(const BasicPolyList& F)
{
  long result = sizeof(BasicPolyList);
  for (auto& f : F)
    result += f.bytesUsed();
  return result;
}


// Local Variables:
// indent-tabs-mode: nil
// End:
