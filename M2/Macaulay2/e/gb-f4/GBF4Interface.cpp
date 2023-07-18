#include "GBF4Interface.hpp"

#include "../matrix-stream.hpp"
#include "PolynomialList.hpp"

//////////////////////////////////////////////////////////////////////////
// getting polynomials/ideals/submodules to/from this code to M2, files //
//////////////////////////////////////////////////////////////////////////

namespace newf4 {

// TODO: write me.
template<typename Stream>
void polynomialListToStream(const PolynomialList& Fs, Stream& stream)
{
  // TODO: need a better way to transfer coefficients
  //  at least for now: we need a way to get integers from the coefficients
  //  perhaps use to_modp_long.  BUT! Need to check that it is correct...
  
}

void PolynomialListStream::idealBegin(size_t polyCount)
{
  //  mValue.clear();
  mCurrentPoly = -1;
  //  mValue.resize(polyCount);
}

void PolynomialListStream::appendPolynomialBegin(size_t termCount)
{
  mCurrentPoly++;
  mCurrentTerm = -1;
  mCoefficients.resize(termCount);
  mValue[mCurrentPoly].mComponents.resize(termCount);
  mValue[mCurrentPoly].mMonomials.resize(termCount);
}

void PolynomialListStream::appendTermBegin(Component com)
{
  mCurrentTerm++;
  mValue[mCurrentPoly].mComponents[mCurrentTerm] = com;
  mSparseMonomial.push_back(1);
}

void PolynomialListStream::appendExponent(VarIndex index, Exponent exponent)
{
  // These need to go into an auxilliary std::vector.
  mSparseMonomial.push_back(index);
  mSparseMonomial.push_back(exponent);
  mSparseMonomial[0] += 2; // length field.
}

void PolynomialListStream::appendTermDone(Coefficient coefficient)
{
  auto monomindex = mValue.monomialHashTable().find(MonomialView(mSparseMonomial.data()), 0);
  // TODO
  mCoefficients.push_back(coefficient); // FIXME
  //  mValue[mCurrentPoly].coeffs[mCurrentTerm] = coefficient; // REPLACE THIS LINE
  mValue[mCurrentPoly].mMonomials.push_back(monomindex);
  mSparseMonomial.clear();
  mSparseMonomial.push_back(1);
}

void PolynomialListStream::appendPolynomialDone()
{
  if (mCurrentTerm != mValue[mCurrentPoly].mComponents.size() - 1)
    throw exc::engine_error("internal error: building PolyList from stream has incorrect number of terms in a polynomial");
}

void PolynomialListStream::idealDone()
{
  if (mCurrentPoly != mValue.size() - 1)
    throw exc::engine_error("internal error: building PolyList from stream has incorrect number of polynomials");
}

}; // namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
