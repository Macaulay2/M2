#include "PolynomialList.hpp"
#include "matrix.hpp"
#include "matrix-stream.hpp"

namespace newf4 {
const Matrix* toMatrix(const FreeModule *target, const PolynomialList& Fs)
{
  MatrixStream S(target);
  toStream(Fs, S);
  return S.value();
}

void PolynomialListStreamCollector::idealBegin(size_t polyCount)
{
  mCurrentPoly = -1;
}

void PolynomialListStreamCollector::appendPolynomialBegin(size_t termCount)
{
  mCurrentPoly++;
  mCurrentTerm = -1;
  Polynomial F;
  mValue.push_back(F);
  mCoefficients.resize(termCount);
  mValue[mCurrentPoly].mMonomials.resize(termCount);
  mValue[mCurrentPoly].mComponents.resize(termCount);
}

void PolynomialListStreamCollector::appendTermBegin(Component com)
{
  mCurrentTerm++;
  mValue[mCurrentPoly].mComponents[mCurrentTerm] = com;
  mSparseMonomial.clear();
  mSparseMonomial.push_back(0); // will be replaced before we lookup monomial in hash table
}

void PolynomialListStreamCollector::appendExponent(VarIndex index, Exponent exponent)
{
  mSparseMonomial.push_back(index);
  mSparseMonomial.push_back(exponent);
}

void PolynomialListStreamCollector::appendTermDone(Coefficient coefficient)
{
  // add monomial to hash table, place the monomial in mMonomials.
  mCoefficients[mCurrentTerm] = coefficient;
  mSparseMonomial[0] = static_cast<int>(mSparseMonomial.size());
  mValue[mCurrentPoly].mMonomials[mCurrentTerm] = mValue.monomialHashTable().find(MonomialView(mSparseMonomial.data()));
}

void PolynomialListStreamCollector::appendPolynomialDone()
{
  mValue[mCurrentPoly].mCoefficients = mValue.vectorArithmetic().elementArrayFromContainerOfLongs(mCoefficients);
  if (mCurrentTerm != mValue[mCurrentPoly].mComponents.size() - 1)
    throw exc::engine_error("internal error: building PolyList from stream has incorrect number of terms in a polynomial");
}

void PolynomialListStreamCollector::idealDone()
{
  if (mCurrentPoly != mValue.size() - 1)
    throw exc::engine_error("internal error: building PolyList from stream has incorrect number of polynomials");
}

} // end namespoace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
