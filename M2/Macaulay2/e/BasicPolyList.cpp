#include "BasicPolyList.hpp"
#include "matrix.hpp"
#include "matrix-stream.hpp"
#include "gb-f4/MonomialView.hpp"
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

template<typename T>
void BasicPoly::displayCoefficient(std::ostream& o, T val, bool print_plus, bool print_one)
{
  // print_one is true: if value is +1 or -1, then print the "1", else don't.
  bool is_negative = (val < 0);
  bool is_one = (val == 1 or val == -1);

  T negative_val = - val;
  if (not is_negative and print_plus) o << '+';
  if (is_one)
    {
      if (is_negative) o << '-';
      if (print_one) o << '1';
    }
  else
    {
      o << val;
    }
  
}

void BasicPoly::display(std::ostream& o,
                        const std::vector<std::string> & varnames,
                        bool print_one,
                        bool print_plus,
                        bool print_parens) const
{
  if (termCount() == 0)
    {
      o << '0';
      return;
    }
  
  bool more_than_one_term = (termCount() >= 2);
  int needs_parens = print_parens && more_than_one_term;
  
  if (needs_parens)
    {
      if (print_plus) o << '+';
      o << '(';
      print_plus = false;
    }
  
  // TODO: need BasicPolyIterator.  For now, we do it by hand
  // TODO: how to deal with components?
  const int* monom_loc = mMonomials.data();
  for (int i=0; i<termCount(); ++i)
    {
      bool monom_is_one = (*monom_loc == 1); // TODO: do *not* hand code this determination in!
      print_parens = !monom_is_one;
      bool p_one_this = (monom_is_one && needs_parens) || (monom_is_one && print_one);
      displayCoefficient(o, mCoefficients[i], print_plus, p_one_this);
      if (!monom_is_one)
        {
          if (mCoefficients[i] != 1 and mCoefficients[i] != -1)
            o << '*';
          newf4::MonomialView::display(o, varnames, newf4::MonomialView(const_cast<int*>(monom_loc)));
        }
      print_plus = true;
      monom_loc += *monom_loc;
    }
  if (needs_parens) o << ')';
}

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
