#include "FreeAlgebra.hpp"

#include "NCAlgebras/FreeMonoid.hpp"  // for FreeMonoid
#include "NCAlgebras/Word.hpp"        // for Word
#include "Polynomial.hpp"             // for Poly, Polynomial<>::const_iterator
#include "ZZ.hpp"                     // for RingZZ
#include "buffer.hpp"                 // for buffer
#include "error.h"                    // for ERROR
#include "monoid.hpp"                 // for Monoid
#include "ringmap.hpp"                // for RingMap

#include <cassert>                    // for assert
#include <ostream>                    // for operator<<, ostream, string
#include <type_traits>                // for swap
#include <utility>                    // for make_pair, pair

class PolynomialRing;  // lines 14-14

using ExponentVector = int*;

std::ostream& operator<<(std::ostream& o, const FreeAlgebraElement& f)
{
  buffer b;
  f.ring().elem_text_out(b, *f, true, false, false);
  o << b.str();
  return o;
}

// FreeAlgebraHeap functions

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

// SumCollector functions

SumCollector* FreeAlgebra::make_SumCollector() const
{
  return new SumCollectorFreeAlgebraHeap(*this);
}

// FreeAlgebra starts here

FreeAlgebra* FreeAlgebra::create(const Ring* K,
                                 const std::vector<std::string>& names,
                                 const PolynomialRing* degreeRing,
                                 const std::vector<int>& degrees,
                                 const std::vector<int>& wtvecs,
                                 const std::vector<int>& heftVector
                                 )
{
  assert(K != nullptr);
  FreeMonoid *M = new FreeMonoid(names, degreeRing, degrees, wtvecs, heftVector);
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

bool FreeAlgebra::from_rational(Poly& result, const mpq_srcptr q) const
{
  ring_elem cq; // in coeff ring.
  bool worked = coefficientRing()->from_rational(q, cq);
  if (!worked) return false;
  from_coefficient(result, cq);
  return true;
}

void FreeAlgebra::copy(Poly& result, Poly::const_iterator fBegin, Poly::const_iterator fEnd) const
{
  clear(result);
  auto& resultCoeff = result.getCoeffInserter();
  auto& resultMonom = result.getMonomInserter();
  resultCoeff.insert(resultCoeff.end(),
                     fBegin.cCoeffIterator(),
                     fEnd.cCoeffIterator());
  resultMonom.insert(resultMonom.end(),
                     fBegin.cMonomIterator(),
                     fEnd.cMonomIterator());
}
void FreeAlgebra::copy(Poly& result, const Poly& f) const
{
  copy(result, f.cbegin(), f.cend());
}

void FreeAlgebra::swap(Poly& f, Poly& g) const
{
  auto& fCoeff = f.getCoeffInserter();
  auto& fMonom = f.getMonomInserter();
  auto& gCoeff = g.getCoeffInserter();
  auto& gMonom = g.getMonomInserter();
  std::swap(fCoeff,gCoeff);
  std::swap(fMonom,gMonom);
}

void FreeAlgebra::var(Poly& result, int v) const
{
  result.getCoeffInserter().push_back(coefficientRing()->from_long(1));
  monoid().var(v, result.getMonomInserter());
}

// the below was copied over from poly.cpp.  Working on adding
// support to the freeAlgebra class.  WIP
M2_arrayint FreeAlgebra::support(const Poly& f) const
{
  int numVarsFound = 0;
  std::vector<int> exp, exp2;
  auto fIt = f.cbegin();
  auto fEnd = f.cend();
  
  for (auto i = 0; i < numVars(); i++)
     exp2.push_back(0);

  for ( ; fIt != fEnd; fIt++) 
  {
     monoid().support(fIt.monom(),exp);
     for (auto i : exp)
     {
       if (exp2[i] == 0)
       {
         exp2[i]++;
         numVarsFound++;
       }
     }
     if (numVarsFound == numVars()) break;
  }

  exp.clear();
  for (int i = 0; i < numVars(); i++)
    if (exp2[i] > 0) exp.push_back(i);

  M2_arrayint result = M2_makearrayint(exp.size());
  int next = 0;
  for (auto i : exp)
    result->array[next++] = i;
  return result;
}

void FreeAlgebra::from_word(Poly& result, ring_elem coeff, const std::vector<int>& word) const
{
  Word tmpWord;
  tmpWord.init(word.data(),word.data() + word.size());
  from_word(result,coeff,tmpWord);
}

void FreeAlgebra::from_word(Poly& result, ring_elem coeff, const Word& word) const
{
  auto& resultCoeff = result.getCoeffInserter();
  auto& resultMonom = result.getMonomInserter();
  resultCoeff.push_back(coeff);

  monoid().monomInsertFromWord(resultMonom,word);
}

void FreeAlgebra::from_word(Poly& result, const std::vector<int>& word) const
{
  from_word(result, coefficientRing()->from_long(1), word);
}

void FreeAlgebra::from_word(Poly& result, const Word& word) const
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

void FreeAlgebra::addScalarMultipleOf(Poly& result,
                                      Poly::const_iterator fBegin,
                                      Poly::const_iterator fEnd,
                                      Poly::const_iterator gBegin,
                                      Poly::const_iterator gEnd,
                                      ring_elem coeff) const
{
  auto& outcoeff = result.getCoeffInserter();
  auto& outmonom = result.getMonomInserter();
  
  // loop over the iterators for f and g, adding the bigger of the two to
  // the back of the monomial and coefficient vectors of the result.  If a tie, add the coefficients.
  while ((fBegin != fEnd) && (gBegin != gEnd))
    {
      auto fMon = fBegin.monom();
      auto gMon = gBegin.monom();
      auto fCoeff = fBegin.coeff();
      auto gCoeff = gBegin.coeff();
      ring_elem coeffResult;
      switch(monoid().compare(fMon,gMon))
        {
        case LT:
          coeffResult = coefficientRing()->mult(coeff,gCoeff);
          if (!coefficientRing()->is_zero(coeffResult))
          {
            outcoeff.push_back(coeffResult);
            monoid().copy(gMon, outmonom);
          }
          gBegin++;
          break;
        case GT:
          outcoeff.push_back(fCoeff);
          monoid().copy(fMon, outmonom);
          fBegin++;
          break;
        case EQ:
          ring_elem tempResult = coefficientRing()->mult(coeff,gCoeff);
          coeffResult = coefficientRing()->add(fCoeff,tempResult);
          if (!coefficientRing()->is_zero(coeffResult))
            {
              outcoeff.push_back(coeffResult);
              monoid().copy(gMon, outmonom);
            }
          fBegin++;
          gBegin++;
        }
    }
  if (fBegin == fEnd)
    {
      for ( ; gBegin != gEnd; gBegin++)
        {
          auto gMon = gBegin.monom();
          auto gCoeff = gBegin.coeff();
          ring_elem coeffResult = coefficientRing()->mult(coeff,gCoeff);
          if (!coefficientRing()->is_zero(coeffResult))
          {
            outcoeff.push_back(coeffResult);
            monoid().copy(gMon, outmonom);
          }
        }
    }
  if (gBegin == gEnd)
    {
      for ( ; fBegin != fEnd; fBegin++)
        {
          auto fMon = fBegin.monom();
          auto fCoeff = fBegin.coeff();
          outcoeff.push_back(fCoeff);
          monoid().copy(fMon, outmonom);
        }
    }
}

void FreeAlgebra::add(Poly& result, const Poly& f, const Poly& g) const
{
  addScalarMultipleOf(result,
                      f.cbegin(),
                      f.cend(),
                      g.cbegin(),
                      g.cend(),
                      coefficientRing()->one());
}

void FreeAlgebra::add(Poly& result,
                      Poly::const_iterator fBegin,
                      Poly::const_iterator fEnd,
                      Poly::const_iterator gBegin,
                      Poly::const_iterator gEnd) const
{
  addScalarMultipleOf(result,
                      fBegin,
                      fEnd,
                      gBegin,
                      gEnd,
                      coefficientRing()->one());
}


void FreeAlgebra::subtract(Poly& result,
                           const Poly& f,
                           const Poly& g) const
{
  addScalarMultipleOf(result,
                      f.cbegin(),
                      f.cend(),
                      g.cbegin(),
                      g.cend(),
                      coefficientRing()->minus_one());
}

void FreeAlgebra::subtractScalarMultipleOf(Poly& result,
                                           const Poly& f,
                                           const Poly& g,
                                           ring_elem coeff) const
{
  addScalarMultipleOf(result,
                      f.cbegin(),
                      f.cend(),
                      g.cbegin(),
                      g.cend(),
                      coefficientRing()->negate(coeff));
}

/*
  auto fIt = f.cbegin();
  auto gIt = g.cbegin();
  auto fEnd = f.cend();
  auto gEnd = g.cend();

  auto& outcoeff = result.getCoeffInserter();
  auto& outmonom = result.getMonomInserter();

  // loop over the iterators for f and g, adding the bigger of the two to
  // the back of the monomial and coefficient vectors of the result.
  // If a tie, subtract the coefficients.
  // If the result is zero, do not insert the monomial
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
*/

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
  IntVector tmp;
  monoid().monomInsertFromWord(tmp,w);
  Monom tmpMonom(tmp.data());
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
  IntVector tmp;
  monoid().monomInsertFromWord(tmp,w);
  Monom tmpMonom(tmp.data());
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
                                              const Monom leftM,
                                              const Monom rightM) const
{
  // return (c*leftM)*f*rightM
  auto& outcoeff = result.getCoeffInserter();
  auto& outmonom = result.getMonomInserter();
  for(auto i=f.cbegin(); i != f.cend(); i++)
    {
      outcoeff.push_back(i.coeff());
      monoid().mult3(leftM, i.monom(), rightM, outmonom);
    }
}

void FreeAlgebra::mult_by_term_left_and_right(Poly& result,
                                              const Poly& f,
                                              const ring_elem c,
                                              const Word& leftW,
                                              const Word& rightW) const
{
  IntVector leftTmp, rightTmp;
  monoid().monomInsertFromWord(leftTmp,leftW);
  monoid().monomInsertFromWord(rightTmp,rightW);
  Monom leftTmpMonom(leftTmp.data());
  Monom rightTmpMonom(rightTmp.data());
  mult_by_term_left_and_right(result,f,c,leftTmpMonom,rightTmpMonom);
  leftTmp.clear();
  rightTmp.clear();
}

void FreeAlgebra::mult_by_term_left_and_right(Poly& result,
                                              const Poly& f,
                                              const Word& leftW,
                                              const Word& rightW) const
{
  IntVector leftTmp, rightTmp;
  monoid().monomInsertFromWord(leftTmp,leftW);
  monoid().monomInsertFromWord(rightTmp,rightW);
  Monom leftTmpMonom(leftTmp.data());
  Monom rightTmpMonom(rightTmp.data());
  mult_by_term_left_and_right(result,f,leftTmpMonom,rightTmpMonom);
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

void FreeAlgebra::add_to_end(Poly& f, ring_elem coeff, const Monom& monom) const
{
  if (coefficientRing()->is_zero(coeff)) return;
  auto& outcoeff = f.getCoeffInserter();
  auto& outmonom = f.getMonomInserter();
  outcoeff.push_back(coeff);
  outmonom.insert(outmonom.end(),monom.begin(), monom.end());
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

void FreeAlgebra::power(Poly& result, const Poly& f, mpz_srcptr n) const
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

ring_elem FreeAlgebra::eval(const RingMap *map,
                       const Poly& f,
                       int first_var) const
{
  // map: R --> S, this = R.
  // f is an element in R
  // return an element of S - we don't know anything about S

  // plan: do it as in polyring:
  //  cast f to a Poly
  //  loop through the terms of f
  //    for each term: call map->eval_term, need varpower monomial here.
  //    add to a heap object
  // return value of the heap object

  const Ring* target = map->get_ring();
  SumCollector *H = target->make_SumCollector();

  std::vector<int> vp;
  for (auto i = f.cbegin(); i != f.cend(); ++i)
    {
      vp.clear();
      monoid().getMonomial(i.monom(), vp);
      ring_elem g = map->eval_term(coefficientRing(), i.coeff(), vp.data(), first_var, numVars());
      H->add(g);
    }
  ring_elem result = H->getValue();
  return result;
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

void FreeAlgebra::makeMonicInPlace(Poly& f) const
{
  ring_elem c = f.cbegin().coeff();
  for (auto iter = f.beginCoeff(); iter != f.endCoeff(); ++iter)
    *iter = coefficientRing()->divide(*iter,c);
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

std::pair<int, bool> FreeAlgebra::heft_degree(const Poly& f) const
{
  bool ishomog = true;
  Word tmp;
  auto i = f.cbegin();
  if (i == f.cend()) return std::make_pair(0, true);
  monoid().wordFromMonom(tmp, i.monom());
  int maxheft = monoid().wordHeft(tmp);
  for (++i; i != f.cend(); ++i)
    {
      monoid().wordFromMonom(tmp, i.monom());
      int thisheft = monoid().wordHeft(tmp);
      if (thisheft != maxheft)
        {
          ishomog = false;
          if (thisheft > maxheft)
            maxheft = thisheft;
        }
    }
  return std::make_pair(maxheft, ishomog);
}

Word FreeAlgebra::lead_word(const Poly& f) const
{
   Word result;
   monoid().wordFromMonom(result,f.cbegin().monom());
   return result;
}

Word FreeAlgebra::lead_word_prefix(const Poly& f, int endIndex) const
{
  Word result;
  monoid().wordPrefixFromMonom(result,f.cbegin().monom(),endIndex);
  return result;
}

Word FreeAlgebra::lead_word_suffix(const Poly& f, int beginIndex) const
{
  Word result;
  monoid().wordSuffixFromMonom(result,f.cbegin().monom(),beginIndex);
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
