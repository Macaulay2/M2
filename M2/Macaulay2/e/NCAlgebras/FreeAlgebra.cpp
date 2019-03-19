#include "FreeAlgebra.hpp"

class FreeAlgebraHeap
{
  const FreeAlgebra& F;  // Our elements will be vectors in here
  Poly heap[GEOHEAP_SIZE];
  int top_of_heap;

 public:
  FreeAlgebraHeap(const FreeAlgebra& F);
  ~FreeAlgebraHeap();

  void add(const Poly& f);
  void value(Poly& result);  // Returns the linearized value, and resets the FreeAlgebraHeap.

  const Poly& debug_list(int i) const
  {
    return heap[i];
  }  // DO NOT USE, except for debugging purposes!
};

FreeAlgebraHeap::FreeAlgebraHeap(const FreeAlgebra& FF)
  : F(FF), top_of_heap(-1)
{
}

FreeAlgebraHeap::~FreeAlgebraHeap()
{
  // The user of this class must insure that the heap has been reset.
  // i.e. call value() routine.
}

void FreeAlgebraHeap::add(const Poly& p)
{
  auto len = p.numTerms();
  int i = 0;
  while (len >= heap_size[i]) i++;

  Poly tmp1;
  F.init(tmp1);
  F.add(tmp1, heap[i], p);
  std::swap(heap[i], tmp1);
  F.setZero(tmp1);
  
  len = heap[i].numTerms();
  while (len >= heap_size[i])
    {
      i++;
      F.add(tmp1, heap[i], heap[i-1]);
      std::swap(heap[i], tmp1);
      F.setZero(tmp1);
      F.setZero(heap[i-1]);
      
      len = heap[i].numTerms();
    }
  if (i > top_of_heap) top_of_heap = i;
}

void FreeAlgebraHeap::value(Poly& result) 
{
  Poly tmp1;
  F.init(tmp1);
  for (int i = 0; i <= top_of_heap; i++)
    {
      if (heap[i].numTerms() == 0) continue;
      F.add(tmp1, result, heap[i]);
      std::swap(result, tmp1);
      F.setZero(tmp1);
      F.setZero(heap[i]);
    }
  top_of_heap = -1;
  F.clear(tmp1);
}

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
  setZero(f);
}

void FreeAlgebra::setZero(Poly& f) const
{
  for (auto a : f.mCoefficients)
    mCoefficientRing.remove(a);
    
  f.mCoefficients.clear();
  f.mMonomials.clear();
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

void FreeAlgebra::mult(Poly& result, const Poly& f, const Poly& g) const
{
  FreeAlgebraHeap H {*this};
  Poly tmp;
  init(tmp);
  if (f.numTerms() <= g.numTerms())
    {
      for (auto fIt = f.cbegin(); fIt != f.cend(); fIt++)
        {
          mult_by_term_left(tmp, g, fIt.coeff(), fIt.monom());
          H.add(tmp);
          setZero(tmp);
        }
    }
  else
    {
      for (auto gIt = g.cbegin(); gIt != g.cend(); gIt++)
        {
          mult_by_term_right(tmp,f, gIt.coeff(), gIt.monom());
          H.add(tmp);
          setZero(tmp);
        }
    }
  H.value(result);
  clear(tmp);
}

void FreeAlgebra::mult_by_term_right(Poly& result,
                                     const Poly& f,
                                     const ring_elem c,
                                     const Monom m) const
  {
  // return f*c*m
  auto& outcoeff = result.getCoeffInserter();
  auto& outmonom = result.getMonomInserter();
  for(auto i=f.cbegin(); i != f.cend(); i++)
    {
      // multiply the coefficients
      ring_elem d = mCoefficientRing.mult(i.coeff(),c);
      if (mCoefficientRing.is_zero(d))
        continue;

      outcoeff.push_back(d);
      monoid().mult(i.monom(), m, outmonom);
    }
}

void FreeAlgebra::mult_by_term_left(Poly& result,
                                    const Poly& f,
                                    const ring_elem c,
                                    const Monom m) const
{
  // return (c*m)*f
  auto& outcoeff = result.getCoeffInserter();
  auto& outmonom = result.getMonomInserter();
  for(auto i=f.cbegin(); i != f.cend(); i++)
    {
      ring_elem d = mCoefficientRing.mult(c, i.coeff());
      if (mCoefficientRing.is_zero(d))
        continue;

      outcoeff.push_back(d);
      monoid().mult(m, i.monom(), outmonom);
    }
}

void FreeAlgebra::mult_by_term_left_and_right(Poly& result,
                                              const Poly& f,
                                              const ring_elem c,
                                              const Monom leftM,
                                              const Monom rightM) const
{
  // return (c*leftM)*f*rightM
  auto& outcoeff = result.getCoeffInserter();
  auto& outmonom = result.getMonomInserter();
  for(auto i=f.cbegin(); i != f.cend(); i++)
    {
      ring_elem d = mCoefficientRing.mult(c, i.coeff());
      if (mCoefficientRing.is_zero(d))
        continue;

      outcoeff.push_back(d);
      monoid().mult3(leftM, i.monom(), rightM, outmonom);
    }
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
