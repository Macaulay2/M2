#include "BasicPolyList.hpp"
#include "matrix.hpp"
#include "matrix-stream.hpp"

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

void BasicPolyListStreamCollector::appendTermDone(Coefficient coefficient)
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

// TODO: take a list of varnames
// TODO: output components too

void BasicPoly::debug_display(std::ostream& o) const
  {
    o << "Poly([";
    bool first_term = true;
    for (auto a : mCoefficients)
      {
        if (first_term) 
          first_term = false;
        else
          o << ", ";
        o << a;
      }
    o << "]" << std::endl;
    int nextloc = 0;
    o << "  monomials[";
    for (int i=0; i<mMonomials.size(); ++i)
      {
        if (i == nextloc) o << " .";
        nextloc += mMonomials[i];
        o << " " << mMonomials[i];
      }
    o << "])" << std::endl;
  }


// Local Variables:
// indent-tabs-mode: nil
// End:
