#include "PolynomialList.hpp"
#include "matrix.hpp"
#include "matrix-stream.hpp"

namespace newf4 {

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



// // TODO: write me.
// template<typename Stream>
// void polynomialListToStream(const PolynomialList& Fs, Stream& stream)
// {
//   // TODO: need a better way to transfer coefficients
//   //  at least for now: we need a way to get integers from the coefficients
//   //  perhaps use to_modp_long.  BUT! Need to check that it is correct...
// }

// void PolynomialListStreamCollector::idealBegin(size_t polyCount)
// {
//   //  mValue.clear();
//   mCurrentPoly = -1;
//   //  mValue.resize(polyCount);
// }

// void PolynomialListStreamCollector::appendPolynomialBegin(size_t termCount)
// {
//   mCurrentPoly++;
//   mCurrentTerm = -1;
//   mCoefficients.resize(termCount);
//   mValue[mCurrentPoly].mComponents.resize(termCount);
//   mValue[mCurrentPoly].mMonomials.resize(termCount);
// }

// void PolynomialListStreamCollector::appendTermBegin(Component com)
// {
//   mCurrentTerm++;
//   mValue[mCurrentPoly].mComponents[mCurrentTerm] = com;
//   mSparseMonomial.push_back(1);
// }

// void PolynomialListStreamCollector::appendExponent(VarIndex index, Exponent exponent)
// {
//   // These need to go into an auxilliary std::vector.
//   mSparseMonomial.push_back(index);
//   mSparseMonomial.push_back(exponent);
//   mSparseMonomial[0] += 2; // length field.
// }

// void PolynomialListStreamCollector::appendTermDone(Coefficient coefficient)
// {
// #if 0  
//   auto monomindex = mValue.monomialHashTable().find(MonomialView(mSparseMonomial.data()), 0);
//   // TODO
//   mCoefficients.push_back(coefficient); // FIXME
//   //  mValue[mCurrentPoly].coeffs[mCurrentTerm] = coefficient; // REPLACE THIS LINE
//   mValue[mCurrentPoly].mMonomials.push_back(monomindex);
//   mSparseMonomial.clear();
//   mSparseMonomial.push_back(1);
// #endif  
// }

// void PolynomialListStreamCollector::appendPolynomialDone()
// {
//   if (mCurrentTerm != mValue[mCurrentPoly].mComponents.size() - 1)
//     throw exc::engine_error("internal error: building PolyList from stream has incorrect number of terms in a polynomial");
// }

// void PolynomialListStreamCollector::idealDone()
// {
//   if (mCurrentPoly != mValue.size() - 1)
//     throw exc::engine_error("internal error: building PolyList from stream has incorrect number of polynomials");
// }

// Local Variables:
// indent-tabs-mode: nil
// End:
