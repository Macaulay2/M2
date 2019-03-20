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

void FreeAlgebra::from_coefficient(Poly& result, const ring_elem a) const
{
  if (not mCoefficientRing.is_zero(a))
    {
      result.getCoeffInserter().push_back(a);
      monoid().one(result.getMonomInserter());
    }
}

void FreeAlgebra::from_long(Poly& result, long n) const
{
  from_coefficient(result, mCoefficientRing.from_long(n));
}

void FreeAlgebra::from_int(Poly& result, mpz_srcptr n) const
{
  from_coefficient(result, mCoefficientRing.from_int(n));
}

bool FreeAlgebra::from_rational(Poly& result, const mpq_ptr q) const
{
  ring_elem cq; // in coeff ring.
  bool worked = mCoefficientRing.from_rational(q, cq);
  if (!worked) return false;
  from_coefficient(result, cq);
  return true;
}

void FreeAlgebra::var(Poly& result, int v) const
{
  result.getCoeffInserter().push_back(mCoefficientRing.from_long(1));
  monoid().var(v, result.getMonomInserter());
}

void FreeAlgebra::from_word(Poly& result, ring_elem coeff, const std::vector<int>& word) const
{
  auto& resultCoeff = result.getCoeffInserter();
  auto& resultMonom = result.getMonomInserter();
  resultCoeff.push_back(coeff);
  resultMonom.push_back(word.size() + 2);
  resultMonom.push_back(word.size());
  for (auto v : word) resultMonom.push_back(v);  // std::insert?
}

void FreeAlgebra::from_word(Poly& result, const std::vector<int>& word) const
{
  from_word(result, mCoefficientRing.from_long(1), word);
}

bool FreeAlgebra::is_unit(const Poly& f) const
{
  if (f.numTerms() != 1) return false;
  auto i = f.cbegin();
  return monoid().is_one(i.monom()) && mCoefficientRing.is_unit(i.coeff());
}

int FreeAlgebra::compare_elems(const Poly& f, const Poly& g) const
{
  auto fIt = f.cbegin();
  auto gIt = g.cbegin();
  auto fEnd = f.cend();
  auto gEnd = g.cend();
  int cmp;
  for ( ; ; fIt++, gIt++)
    {
      if (fIt == fEnd)
        {
          if (gIt == gEnd) return EQ;
          return LT;
        }
      if (gIt == gEnd) return GT;
      // TODO: can we remove the following block?  Make sure monoid can handle zero variables...
      if (numVars() > 0)
        {
          cmp = monoid().compare(fIt.monom(),gIt.monom());
          if (cmp != 0) return cmp;
        }
      // if we are here, then the monomials are the same and we compare coefficients.
      // for example if a,b are in the base and a > b then ax > bx.
      cmp = mCoefficientRing.compare_elems(fIt.coeff(), fIt.coeff());
      if (cmp != 0) return cmp;
    }
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

void FreeAlgebra::negate(Poly& result, const Poly& f) const
{
  // eventually we will want to make this an 'in-place' operation.
  // for now, it is a copy.
  Poly tmp;
  from_long(tmp,-1);
  mult(result, f, tmp);
  clear(tmp);
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

void FreeAlgebra::mult_by_coeff(Poly& result, const Poly& f, const ring_elem c) const
{
  // return c*f -- perhaps make this in place later, but for now
  // create a new element
  Poly tmp;
  from_coefficient(tmp,c);
  mult(result, f, tmp);
  clear(tmp);
}

void FreeAlgebra::power(Poly& result, const Poly& f, int n) const
{
  from_long(result, 1);
  Poly tmp;
  for (int i=0; i<n; i++)
    {
      mult(tmp, result, f);
      result = tmp;
      setZero(tmp);
    }
}

void FreeAlgebra::power(Poly& result, const Poly& f, mpz_ptr n) const
{
  if (mpz_sgn(n) == 0) from_long(result, 1);
  else if (is_zero(f)) from_long(result, 0);
  else if (is_unit(f))  // really want a routine 'is_scalar'...
    {
      ring_elem coeff = f.cbegin().coeff();
      ring_elem a = mCoefficientRing.power(coeff, n);
      from_coefficient(result, a);
    }
  else
    {
      std::pair<bool, int> n1 = RingZZ::get_si(n);
      if (mpz_sgn(n) > 0 and n1.first)
          power(result, f, n1.second);
      else
        {
          ERROR("exponent too large");
          from_long(result, 0);
        }
    }
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
