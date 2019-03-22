#include "FreeAlgebra.hpp"
#include "WordTable.hpp"

using ExponentVector = int*;

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

class SumCollectorFreeAlgebraHeap : public SumCollector
{
  FreeAlgebraHeap H;

 public:
  SumCollectorFreeAlgebraHeap(const FreeAlgebra& F) : H(F) {}
  ~SumCollectorFreeAlgebraHeap() {}
  virtual void add(ring_elem f1)
  {
    auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
    H.add(*f);
  }
  virtual ring_elem getValue()
  {
    Poly* result = new Poly;
    H.value(*result);
    return ring_elem(reinterpret_cast<void *>(result));
  }
};

SumCollector* FreeAlgebra::make_SumCollector() const
{
  return new SumCollectorFreeAlgebraHeap(*this);
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
    coefficientRing()->remove(a);
    
  f.mCoefficients.clear();
  f.mMonomials.clear();
}

void FreeAlgebra::from_coefficient(Poly& result, const ring_elem a) const
{
  if (not coefficientRing()->is_zero(a))
    {
      result.getCoeffInserter().push_back(a);
      monoid().one(result.getMonomInserter());
    }
}

void FreeAlgebra::from_long(Poly& result, long n) const
{
  from_coefficient(result, coefficientRing()->from_long(n));
}

void FreeAlgebra::from_int(Poly& result, mpz_srcptr n) const
{
  from_coefficient(result, coefficientRing()->from_int(n));
}

bool FreeAlgebra::from_rational(Poly& result, const mpq_ptr q) const
{
  ring_elem cq; // in coeff ring.
  bool worked = coefficientRing()->from_rational(q, cq);
  if (!worked) return false;
  from_coefficient(result, cq);
  return true;
}

void FreeAlgebra::copy(Poly& result, const Poly& f) const
{
  clear(result);
  auto& resultCoeff = result.getCoeffInserter();
  auto& resultMonom = result.getMonomInserter();
  resultCoeff.insert(resultCoeff.end(),
                     f.cbeginCoeff(),
                     f.cendCoeff());
  resultMonom.insert(resultMonom.end(),
                     f.cbeginMonom(),
                     f.cendMonom());
}

void FreeAlgebra::swap(Poly& f, Poly& g) const
{
  
}

void FreeAlgebra::var(Poly& result, int v) const
{
  result.getCoeffInserter().push_back(coefficientRing()->from_long(1));
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
  from_word(result, coefficientRing()->from_long(1), word);
}

bool FreeAlgebra::is_unit(const Poly& f) const
{
  if (f.numTerms() != 1) return false;
  auto i = f.cbegin();
  return monoid().is_one(i.monom()) && coefficientRing()->is_unit(i.coeff());
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
      cmp = coefficientRing()->compare_elems(fIt.coeff(), fIt.coeff());
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
      bool cmp = coefficientRing()->is_equal(*fCoeffIt, *gCoeffIt);
      if (!cmp) return false;
    }
  return true;
}

void FreeAlgebra::negate(Poly& result, const Poly& f) const
{
  auto& outmonom = result.getMonomInserter();
  auto& outcoeff = result.getCoeffInserter();

  for (auto i = f.cbeginMonom(); i != f.cendMonom(); ++i)
    outmonom.push_back(*i);

  for (auto i=f.cbeginCoeff(); i != f.cendCoeff(); ++i)
    outcoeff.push_back(coefficientRing()->negate(*i));
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
          ring_elem coeffResult = coefficientRing()->add(fCoeff,gCoeff);
          if (!coefficientRing()->is_zero(coeffResult))
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

void FreeAlgebra::subtract(Poly& result, const Poly& f, const Poly& g) const
{
  auto fIt = f.cbegin();
  auto gIt = g.cbegin();
  auto fEnd = f.cend();
  auto gEnd = g.cend();

  auto& outcoeff = result.getCoeffInserter();
  auto& outmonom = result.getMonomInserter();

  // loop over the iterators for f and g, adding the bigger of the two to
  // the back of the monomial and coefficient vectors of the result.  If a tie, subtract the coefficients.
  // if the result is zero, do not insert the monomial
  while ((fIt != fEnd) && (gIt != gEnd))
    {
      auto fMon = fIt.monom();
      auto gMon = gIt.monom();
      auto fCoeff = fIt.coeff();
      auto gCoeff = gIt.coeff();
      switch(monoid().compare(fMon,gMon))
        {
        case LT:
          outcoeff.push_back(coefficientRing()->negate(gCoeff));
          monoid().copy(gMon, outmonom);
          gIt++;
          break;
        case GT:
          outcoeff.push_back(fCoeff);
          monoid().copy(fMon, outmonom);
          fIt++;
          break;
        case EQ:
          ring_elem coeffResult = coefficientRing()->subtract(fCoeff,gCoeff);
          if (!coefficientRing()->is_zero(coeffResult))
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
          outcoeff.push_back(coefficientRing()->negate(gCoeff));
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
      ring_elem d = coefficientRing()->mult(i.coeff(),c);
      if (coefficientRing()->is_zero(d))
        continue;

      outcoeff.push_back(d);
      monoid().mult(i.monom(), m, outmonom);
    }
}


void FreeAlgebra::mult_by_term_right(Poly& result,
                                     const Poly& f,
                                     const ring_elem c,
                                     const Word& w) const
{
  std::vector<int> tmp;
  Word::toAllocatedMonom(tmp,w);
  Monom tmpMonom(&*tmp.cbegin());
  mult_by_term_right(result,f,c,tmpMonom);
  tmp.clear();
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
      ring_elem d = coefficientRing()->mult(c, i.coeff());
      if (coefficientRing()->is_zero(d))
        continue;

      outcoeff.push_back(d);
      monoid().mult(m, i.monom(), outmonom);
    }
}

void FreeAlgebra::mult_by_term_left(Poly& result,
                                    const Poly& f,
                                    const ring_elem c,
                                    const Word& w) const
{
  std::vector<int> tmp;
  Word::toAllocatedMonom(tmp,w);
  Monom tmpMonom(&*tmp.cbegin());
  mult_by_term_left(result,f,c,tmpMonom);
  tmp.clear();
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
      ring_elem d = coefficientRing()->mult(c, i.coeff());
      if (coefficientRing()->is_zero(d))
        continue;

      outcoeff.push_back(d);
      monoid().mult3(leftM, i.monom(), rightM, outmonom);
    }
}

void FreeAlgebra::mult_by_term_left_and_right(Poly& result,
                                              const Poly& f,
                                              const ring_elem c,
                                              const Word& leftW,
                                              const Word& rightW) const
{
  std::vector<int> leftTmp, rightTmp;
  Word::toAllocatedMonom(leftTmp,leftW);
  Word::toAllocatedMonom(rightTmp,rightW);
  Monom leftTmpMonom(&*leftTmp.cbegin()), rightTmpMonom(&*rightTmp.cbegin());
  mult_by_term_left_and_right(result,f,c,leftTmpMonom,rightTmpMonom);
  leftTmp.clear();
  rightTmp.clear();
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

void FreeAlgebra::lead_term_as_poly(Poly& result, const Poly& f) const
{
  if (is_zero(f)) return;
  auto& outcoeff = result.getCoeffInserter();
  auto& outmonom = result.getMonomInserter();
  outcoeff.insert(outcoeff.end(),f.cbeginCoeff(),f.cbeginCoeff()+1);
  outmonom.insert(outmonom.end(),f.cbeginMonom(),f.cbeginMonom()+*(f.cbeginMonom()));
}

void FreeAlgebra::add_to_end(Poly& f, const Poly& g) const
{
  if (is_zero(g)) return;
  auto& outcoeff = f.getCoeffInserter();
  auto& outmonom = f.getMonomInserter();
  outcoeff.insert(outcoeff.end(),g.cbeginCoeff(),g.cendCoeff());
  outmonom.insert(outmonom.end(),g.cbeginMonom(),g.cendMonom());
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
      ring_elem a = coefficientRing()->power(coeff, n);
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

void FreeAlgebra::makeMonic(Poly& result, Poly& f) const
{
  auto& outmonom = result.getMonomInserter();
  auto& outcoeff = result.getCoeffInserter();

  for (auto i = f.cbeginMonom(); i != f.cendMonom(); ++i)
    outmonom.push_back(*i);

  for (auto i=f.cbeginCoeff(); i != f.cendCoeff(); ++i)
    outcoeff.push_back(coefficientRing()->divide(*i,f.cbegin().coeff()));
}

void FreeAlgebra::elem_text_out(buffer &o,
                                const Poly& f,
                                bool p_one,
                                bool p_plus,
                                bool p_parens) const
{
  if (f.numTerms() == 0)
    {
      o << "0";
      return;
    }

  bool two_terms = (f.numTerms() > 1);
  bool needs_parens = p_parens && two_terms;
  if (needs_parens)
    {
      if (p_plus) o << '+';
      o << '(';
      p_plus = false;
    }

  for (auto i = f.cbegin(); i != f.cend(); i++)
    {
      bool is_one = monoid().is_one(i.monom());
      p_parens = !is_one;
      bool p_one_this = (is_one && needs_parens) || (is_one && p_one);
      coefficientRing()->elem_text_out(o, i.coeff(), p_one_this, p_plus, p_parens);
      if (!is_one)
        monoid().elem_text_out(o, i.monom());
      p_plus = true;
    }

  if (needs_parens) o << ')';
}

bool FreeAlgebra::is_homogeneous(const Poly& f) const
{
  bool result = true;
  if (f.numTerms() <= 1) return true;
  ExponentVector e = degreeMonoid().make_one();
  ExponentVector degf = degreeMonoid().make_one();
  auto i = f.cbegin();
  auto end = f.cend();
  monoid().multi_degree(i.monom(), degf); // sets degf.
  for (++i; i != end; ++i)
    {
        monoid().multi_degree(i.monom(), e);
        if (not degreeMonoid().is_equal(e, degf))
          {
            result = false;
            break;
          }
    }
  degreeMonoid().remove(e);
  degreeMonoid().remove(degf);
  return result;
}

void FreeAlgebra::degree(const Poly& f, int *d) const
{
  multi_degree(f, d);
}

bool FreeAlgebra::multi_degree(const Poly& f,
                               int *already_allocated_degree_vector) const
{
  int* degVec = already_allocated_degree_vector;
  bool ishomog = true;
  auto i = f.cbegin();
  monoid().multi_degree(i.monom(), degVec);
  ExponentVector e = degreeMonoid().make_one();
  for (++i; i != f.cend(); ++i)
    {
      monoid().multi_degree(i.monom(), e);
      if (not degreeMonoid().is_equal(degVec, e))
        {
          ishomog = false;
          degreeMonoid().lcm(degVec, e, degVec);
        }
    }
  degreeMonoid().remove(e);
  return ishomog;
}

void FreeAlgebra::lead_word(Word& result, const Poly& f) const
{
  lead_word_prefix(result, f, *f.cbegin().monom().begin() - 2);
}

void FreeAlgebra::lead_word_prefix(Word& result, const Poly& f, int endIndex) const
{
  result.init(f.cbegin().monom().begin() + 2,f.cbegin().monom().begin() + 2 + endIndex);
}

void FreeAlgebra::lead_word_suffix(Word& result, const Poly& f, int beginIndex) const
{
  result.init(f.cbegin().monom().begin() + 2 + beginIndex, f.cbegin().monom().end());
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
