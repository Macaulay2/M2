#include "FreeAlgebra.hpp"

FreeAlgebra* FreeAlgebra::create(const Ring* K,
				 const std::vector<std::string>& names,
				 const PolynomialRing* degreeRing,
				 const std::vector<int>& degrees
				 )
{
  assert(K != nullptr);
  FreeMonoid *M = new FreeMonoid(names, degreeRing, degrees);
  FreeAlgebra* result = new FreeAlgebra(K, M);

  return result;
}

FreeAlgebra::FreeAlgebra(const Ring* K,
                         const FreeMonoid* M
                         )
  : mCoefficientRing(*K),
    mMonoid(*M)
{
}

void FreeAlgebra::clear(Poly& f) const
{
  // TODO: need Polynomial type to allow us access, or have a clear function itself.
}

void FreeAlgebra::var(Poly& result, int v) const
{
  result.getCoeffInserter().push_back(mCoefficientRing.from_long(1));
  monoid().var(v, result.getMonomInserter());
}

bool FreeAlgebra::is_equal(const Poly& f, const Poly& g) const
{
  if (f.numTerms() != g.numTerms()) return false;
  if (f.getMonomVector() != g.getMonomVector()) return false;
  auto fCoeffIt = f.cbeginCoeff();
  auto gCoeffIt = g.cbeginCoeff();
  auto fEnd = f.cendCoeff();
  for ( ; fCoeffIt != fEnd ; fCoeffIt++, gCoeffIt++)
    {
      bool cmp = mCoefficientRing.is_equal(*fCoeffIt, *gCoeffIt);
      if (!cmp) return false;
    }
  return true;
}

void FreeAlgebra::add(Poly& result, const Poly& f, const Poly& g) const
{
  auto fIt = f.cbegin();
  auto gIt = g.cbegin();
  auto fEnd = f.cend();
  auto gEnd = g.cend();

  auto& outcoeff = result.getCoeffInserter();
  auto& outmonom = result.getMonomInserter();
  
  // loop over the iterators for f and g, adding the bigger of the two to
  // the back of the monomial and coefficient vectors of the result.  If a tie, add the coefficients.
  while ((fIt != fEnd) && (gIt != gEnd))
    {
      auto fMon = fIt.monom();
      auto gMon = gIt.monom();
      auto fCoeff = fIt.coeff();
      auto gCoeff = gIt.coeff();
      switch(monoid().compare(fMon,gMon))
        {
        case LT:
          outcoeff.push_back(gCoeff);
          monoid().copy(gMon, outmonom);
          gIt++;
          break;
        case GT:
          outcoeff.push_back(fCoeff);
          monoid().copy(fMon, outmonom);
          fIt++;
          break;
        case EQ:
          ring_elem coeffResult = mCoefficientRing.add(fCoeff,gCoeff);
          if (!mCoefficientRing.is_zero(coeffResult))
            {
              outcoeff.push_back(coeffResult);
              monoid().copy(gMon, outmonom);
            }
          fIt++;
          gIt++;
        }
    }
  if (fIt == fEnd)
    {
      for ( ; gIt != gEnd; gIt++)
        {
          auto gMon = gIt.monom();
          auto gCoeff = gIt.coeff();
          outcoeff.push_back(gCoeff);
          monoid().copy(gMon, outmonom);
        }
    }
  if (gIt == gEnd)
    {
      for ( ; fIt != fEnd; fIt++)
        {
          auto fMon = fIt.monom();
          auto fCoeff = fIt.coeff();
          outcoeff.push_back(fCoeff);
          monoid().copy(fMon, outmonom);
        }
    }
}



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
