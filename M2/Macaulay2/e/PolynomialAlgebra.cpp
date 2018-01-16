#include "PolynomialAlgebra.hpp"
#include "monomial.hpp"
#include "relem.hpp"

#include <vector>
#include <string>
#include <iostream>

void NCMonoid::one(MonomialInserter& m) const
{
  m.push_back(2);
  m.push_back(0);
}

void NCMonoid::var(int v, MonomialInserter& m) const
{
  m.push_back(3);
  m.push_back(1);
  m.push_back(v);
}

bool NCMonoid::is_one(const Monom& m) const
{
  return m[0] == 2;
}

int NCMonoid::index_of_variable(const Monom& m) const
{
  if (m[0] != 3 or m[1] != 1) return -1;
  return m[2];
}

void NCMonoid::copy(const Monom& m, MonomialInserter& result) const
{
  for (auto v : m) result.push_back(v);
  
  //  for (auto i = m.begin(); i != m.end(); ++i)
  //    result.push_back(*i);
  //  std::copy(m.begin(), m.end(), result);
}

void NCMonoid::mult(const Monom& m1, const Monom& m2, MonomialInserter& result) const
{
  result.push_back(m1[0] + m2[0] - 2);
  result.push_back(m1[1] + m2[1]);
  for (auto i = m1.begin()+2; i != m1.end(); ++i)
    result.push_back(*i);
  for (auto i = m2.begin()+2; i != m2.end(); ++i)
    result.push_back(*i);

  //  std::copy(std::begin(m1) + 2, std::end(m1), result);
  //  std::copy(std::begin(m2) + 2, std::end(m1), result);
}

int NCMonoid::compare(const Monom& m1, const Monom& m2) const
{
  if (m1[1] > m2[1]) return GT;
  if (m1[1] < m2[1]) return LT;
  // at this stage, they have the same degree, so use lex order
  for (int j = 2; j < m1[0]; j++)
    {
      if (m1[j] > m2[j]) return LT;
      if (m1[j] < m2[j]) return GT;
    }
  // if we are here, the monomials are the same.
  return EQ;
}

void NCMonoid::elem_text_out(buffer& o, const Monom& m1, const std::vector<std::string>& variableNames) const
{
  auto mon_length = m1[0] - 2;
  auto mon_ptr = m1 + 2;
  for (auto j = 0; j < mon_length; j++)
    {
      // for now, just output the string.
      int curvar = mon_ptr[j];
      int curvarPower = 0;
      o << variableNames[curvar];
      while ((j < mon_length) && (mon_ptr[j] == curvar))
        {
          j++;
          curvarPower++;
        }
      if (curvarPower > 1) o << "^" << curvarPower;
      // back j up one since we went too far looking ahead.
      j--;
    }
}

// This function should reverse the order of the varpower terms.
// as the front end reverses the order of terms in a monomial.
void NCMonoid::getMonomial(Monom monom, std::vector<int>& result) const
// Input is of the form: [len degree v1 v2 ... vn]
//                        where len = n + 2
// The output is of the following form, and appended to result.
// [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
{
  auto start = result.size();
  result.push_back(0);
  auto mon_length = monom[0] - 2;
  auto mon_ptr = monom + 2;
  for (auto j = mon_length-1; j >= 0; --j)
    {
      int curvar = mon_ptr[j];
      int curvarPower = 0;
      result.push_back(curvar);
      while ((j >= 0) && (mon_ptr[j] == curvar))
        {
          --j;
          curvarPower++;
        }
      result.push_back(curvarPower);
      // back j up one since we went too far looking ahead.
      j++;
    }
  result[start] = static_cast<int>(result.size() - start);
}

// This function should reverse the order of the varpower terms
void NCMonoid::fromMonomial(const int* monom, MonomialInserter& result) const
  // Input is of the form: [2n+1 v1 e1 v2 e2 ... vn en] (in 'varpower' format)
  // The output is of the following form, and stored in result.
  // [len deg v1 v2 v3 ... vn], where each ei > 0, (in 'varpower' format)
  // where len = n+2 and deg = sum of the degrees of the vi 
{
  int inputMonomLength = *monom;
  int monDeg = 0;
  int startMon = static_cast<int>(result.size());  
  // make a space for the length
  result.push_back(0);
  // make a space for the degree
  result.push_back(0);
  for (int j = inputMonomLength-2; j >= 1; j -= 2)
    {
      auto v = monom[j];
      int degv = 1;
      for (int k = 0; k < monom[j+1]; k++)
        {
          monDeg += degv;
          result.push_back(v);
        }
    }
  result[startMon] = static_cast<int>(result.size() - startMon);
  result[startMon+1] = monDeg;
}

///////////////////////////////////////////

PolynomialAlgebra* PolynomialAlgebra::create(const Ring* K,
                                     M2_ArrayString names,
                                     const PolynomialRing* degreeRing)
{
  assert(K != nullptr);
  PolynomialAlgebra* result = new PolynomialAlgebra(K, names);
  result->initialize_ring(K->characteristic(), degreeRing, nullptr);
  result->zeroV = result->from_long(0);
  result->oneV = result->from_long(1);
  result->minus_oneV = result->from_long(-1);

  return result;
}

PolynomialAlgebra::PolynomialAlgebra(const Ring* K,
                             M2_ArrayString names)
  : mCoefficientRing(*K),
    mNumVars(names->len)
{
  
  for (auto i=0; i<names->len; i++)
    // used emplace_back here.  C++11 feature which allows constructor arguments to be passed in place,
    // rather than having to create a temporary object to pass to push_back.
    mVariableNames.emplace_back(names->array[i]->array, names->array[i]->len);
}

void PolynomialAlgebra::text_out(buffer &o) const
{
  mCoefficientRing.text_out(o);
  o << "{";
  for (int i=0; i<mVariableNames.size(); i++)
    {
      if (i > 0) o << ",";
      o << mVariableNames[i];
    }
  o << "}";
}

unsigned int PolynomialAlgebra::computeHashValue(const ring_elem a) const
{
  return 0; // TODO: change this to a more reasonable hash code.
}

int PolynomialAlgebra::index_of_var(const ring_elem a) const
{
  // This function is needed at top level to be able to determine if a ring element is a variable
  //const Poly* f = reinterpret_cast<Poly*>(a.poly_val);
  auto f = reinterpret_cast<const Poly*>(a.mPolyVal);

  // f is a variable iff: #terms is 1, monomial is [3,1,v], coeff is 1
  if (f->numTerms() != 1) return -1;
  auto i = f->cbegin();
  if (!mCoefficientRing.is_equal(mCoefficientRing.one(), i.coeff())) return -1;
  return monoid().index_of_variable(i.monom());
}

ring_elem PolynomialAlgebra::from_coefficient(const ring_elem a) const
{
  auto result = new Poly;
  if (not mCoefficientRing.is_zero(a))
    {
      result->getCoeffInserter().push_back(a);
      monoid().one(result->getMonomInserter());
    }
  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::from_long(long n) const
{
  return from_coefficient(mCoefficientRing.from_long(n));
}

ring_elem PolynomialAlgebra::from_int(mpz_ptr n) const
{
  return from_coefficient(mCoefficientRing.from_int(n));
}

bool PolynomialAlgebra::from_rational(mpq_ptr q, ring_elem& result1) const
{
  ring_elem cq; // in coeff ring.
  bool worked = mCoefficientRing.from_rational(q, cq);
  if (!worked) return false;
  result1 = from_coefficient(cq);
  return true;
}

ring_elem PolynomialAlgebra::var(int v) const
{
  auto result = new Poly;

  result->getCoeffInserter().push_back(mCoefficientRing.from_long(1));
  monoid().var(v, result->getMonomInserter());

  return reinterpret_cast<Nterm*>(result);
}

bool PolynomialAlgebra::promote(const Ring *R, const ring_elem f, ring_elem &result) const
{
  std::cout << "called promote NC case" << std::endl;  
  // Currently the only case to handle is R = A --> this, and A is the coefficient ring of this.
  if (R == & mCoefficientRing)
    {
      result = from_coefficient(f);
      return true;
    }
  return false;
}

bool PolynomialAlgebra::lift(const Ring *R, const ring_elem f1, ring_elem &result) const
{
  // R is the target ring
  // f1 is an element of 'this'.
  // set result to be the "lift" of f in the ring R, return true if this is possible.
  // otherwise return false.

  // case: R is the coefficient ring of 'this'.
  std::cout << "called lift NC case" << std::endl;
  if (R == & mCoefficientRing)
    {
      auto f = reinterpret_cast<const Poly*>(f1.mPolyVal);
      if (f->numTerms() != 1) return false;
      auto i = f->cbegin();
      if (monoid().is_one(i.monom()))
        {
          result = mCoefficientRing.copy(i.coeff());
          return true;
        }
    }
  
  // at this point, we can't lift it.
  return false;
}

bool PolynomialAlgebra::is_unit(const ring_elem f1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.mPolyVal);

  if (f->numTerms() != 1) return false;
  auto i = f->cbegin();
  return monoid().is_one(i.monom()) && mCoefficientRing.is_unit(i.coeff());
}

long PolynomialAlgebra::n_terms(const ring_elem f1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.mPolyVal);
  return f->numTerms();
}

bool PolynomialAlgebra::is_zero(const ring_elem f1) const
{
  return n_terms(f1) == 0;
}

bool PolynomialAlgebra::is_equal(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.mPolyVal);
  auto g = reinterpret_cast<const Poly*>(g1.mPolyVal);
  if (f->numTerms() != g->numTerms()) return false;
  if (f->getMonomVector() != g->getMonomVector()) return false;
  auto fCoeffIt = f->cbeginCoeff();
  auto gCoeffIt = g->cbeginCoeff();
  auto fEnd = f->cendCoeff();
  for ( ; fCoeffIt != fEnd ; fCoeffIt++, gCoeffIt++)
    {
      bool cmp = mCoefficientRing.is_equal(*fCoeffIt, *gCoeffIt);
      if (!cmp) return false;
    }
  return true;
}

int PolynomialAlgebra::compare_elems(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.mPolyVal);
  auto g = reinterpret_cast<const Poly*>(g1.mPolyVal);
  auto fIt = f->cbegin();
  auto gIt = g->cbegin();
  auto fEnd = f->cend();
  auto gEnd = g->cend();
  int cmp;
  for ( ; ; fIt++, gIt++)
    {
      if (fIt == fEnd)
        {
          if (gIt == gEnd) return EQ;
          return LT;
        }
      if (gIt == gEnd) return GT;
      if (mNumVars > 0)
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

ring_elem PolynomialAlgebra::copy(const ring_elem f) const
{
  // FRANK: is this what we want to do?
  return f;
}

void PolynomialAlgebra::remove(ring_elem &f) const
{
  // do nothing
}

ring_elem PolynomialAlgebra::negate(const ring_elem f1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.mPolyVal);
  Poly* result = new Poly;

  auto& outmonom = result->getMonomInserter();
  auto& outcoeff = result->getCoeffInserter();

  for (auto i = f->cbeginMonom(); i != f->cendMonom(); ++i)
    outmonom.push_back(*i);

  for (auto i=f->cbeginCoeff(); i != f->cendCoeff(); ++i)
    outcoeff.push_back(mCoefficientRing.negate(*i));

  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::add(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.mPolyVal);
  auto g = reinterpret_cast<const Poly*>(g1.mPolyVal);
  auto result = new Poly;
  auto fIt = f->cbegin();
  auto gIt = g->cbegin();
  auto fEnd = f->cend();
  auto gEnd = g->cend();

  auto& outcoeff = result->getCoeffInserter();
  auto& outmonom = result->getMonomInserter();
  
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
  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::subtract(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.mPolyVal);
  auto g = reinterpret_cast<const Poly*>(g1.mPolyVal);
  auto result = new Poly;
  auto fIt = f->cbegin();
  auto gIt = g->cbegin();
  auto fEnd = f->cend();
  auto gEnd = g->cend();

  auto& outcoeff = result->getCoeffInserter();
  auto& outmonom = result->getMonomInserter();

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
          outcoeff.push_back(mCoefficientRing.negate(gCoeff));
          monoid().copy(gMon, outmonom);
          gIt++;
          break;
        case GT:
          outcoeff.push_back(fCoeff);
          monoid().copy(fMon, outmonom);
          fIt++;
          break;
        case EQ:
          ring_elem coeffResult = mCoefficientRing.subtract(fCoeff,gCoeff);
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
          outcoeff.push_back(mCoefficientRing.negate(gCoeff));
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
  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::mult(const ring_elem f1, const ring_elem g1) const
{
  // TODO: make this a geobucket heap multiply function?
  //
  auto f = reinterpret_cast<const Poly*>(f1.mPolyVal);
  auto g = reinterpret_cast<const Poly*>(g1.mPolyVal);
  auto result = new Poly;
  ring_elem resultW = reinterpret_cast<Nterm*>(result);

  if (f->numTerms() <= g->numTerms())
    {
      for (auto fIt = f->cbegin(); fIt != f->cend(); fIt++)
        {
          ring_elem tmp = mult_by_term_left(g1, fIt.coeff(), fIt.monom());
          ring_elem resultW1 = add(resultW, tmp);
          std::swap(resultW1, resultW);
          delete reinterpret_cast<Poly*>(tmp.mPolyVal);
          delete reinterpret_cast<Poly*>(resultW1.mPolyVal);
        }
    }
  else
    {
      for (auto gIt = g->cbegin(); gIt != g->cend(); gIt++)
        {
          ring_elem tmp = mult_by_term_right(f1, gIt.coeff(), gIt.monom());
          ring_elem resultW1 = add(resultW, tmp);
          std::swap(resultW1, resultW);
          delete reinterpret_cast<Poly*>(tmp.mPolyVal);
          delete reinterpret_cast<Poly*>(resultW1.mPolyVal);
        }
    }
  return resultW;
}

ring_elem PolynomialAlgebra::mult_by_term_right(const ring_elem f1,
                                            const ring_elem c, const Monom m) const
{
  // return f*c*m
  auto f = reinterpret_cast<const Poly*>(f1.mPolyVal);
  auto result = new Poly;
  auto& outcoeff = result->getCoeffInserter();
  auto& outmonom = result->getMonomInserter();
  for(auto i=f->cbegin(); i != f->cend(); i++)
    {
      // multiply the coefficients
      ring_elem d = mCoefficientRing.mult(i.coeff(),c);
      if (mCoefficientRing.is_zero(d))
        continue;

      outcoeff.push_back(d);
      monoid().mult(i.monom(), m, outmonom);
    }
  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::mult_by_term_left(const ring_elem f1,
                                           const ring_elem c, const Monom m) const
{
  // return (c*m)*f
  auto f = reinterpret_cast<const Poly*>(f1.mPolyVal);
  auto result = new Poly;
  auto& outcoeff = result->getCoeffInserter();
  auto& outmonom = result->getMonomInserter();
  for(auto i=f->cbegin(); i != f->cend(); i++)
    {
      ring_elem d = mCoefficientRing.mult(c, i.coeff());
      if (mCoefficientRing.is_zero(d))
        continue;

      outcoeff.push_back(d);
      monoid().mult(m, i.monom(), outmonom);
    }
  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::power(const ring_elem f1, mpz_t n) const
{
  if (mpz_sgn(n) == 0) return from_long(1);
  if (is_zero(f1)) return from_long(0);
  if (is_unit(f1))  // really want a routine 'is_scalar'...
    {
      ring_elem coeff = reinterpret_cast<Poly*>(f1.mPolyVal)->cbegin().coeff();
      ring_elem a = mCoefficientRing.power(coeff, n);
      return from_coefficient(a);
    }
  std::pair<bool, int> n1 = RingZZ::get_si(n);
  if (mpz_sgn(n) > 0 and n1.first)
    return power(f1, n1.second);
  ERROR("exponent too large");
  return from_long(0);
}

ring_elem PolynomialAlgebra::power(const ring_elem f, int n) const
{
  ring_elem result = from_long(1);
  for (int i=0; i<n; i++)
    {
      ring_elem g = mult(f,result);
      delete reinterpret_cast<Poly*>(result.mPolyVal);
      result = g;
    }
  return result;
}

ring_elem PolynomialAlgebra::invert(const ring_elem f) const
{
  
  return f; // TODO: bad return value.
}

ring_elem PolynomialAlgebra::divide(const ring_elem f, const ring_elem g) const
{
  return f; // TODO: bad return value.
}

void PolynomialAlgebra::syzygy(const ring_elem a, const ring_elem b,
                      ring_elem &x, ring_elem &y) const
{
  // TODO: In the commutative case, this function is to find x and y (as simple as possible)
  //       such that ax + by = 0.  No such x and y may exist in the noncommutative case, however.
  //       In this case, the function should return x = y = 0.
}

void PolynomialAlgebra::debug_display(const Poly* f) const
{
  std::cout << "coeffs: ";
  for (auto i=f->cbeginCoeff(); i != f->cendCoeff(); ++i)
    {
      buffer o;
      mCoefficientRing.elem_text_out(o, *i);
      std::cout << o.str() << " ";
    }
  std::cout << std::endl  << "  monoms: ";
  for (auto i=f->cbeginMonom(); i != f->cendMonom(); ++i)
    {
      std::cout << (*i) << " ";
    }
  std::cout << std::endl;
}

void PolynomialAlgebra::debug_display(const ring_elem ff) const

{
  auto f = reinterpret_cast<const Poly*>(ff.mPolyVal);
  debug_display(f);
}

ring_elem PolynomialAlgebra::makeTerm(const ring_elem a, const int* monom) const
  // 'monom' is in 'varpower' format
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
{
  auto result = new Poly;

  result->getCoeffInserter().push_back(a);
  monoid().fromMonomial(monom, result->getMonomInserter());
  return reinterpret_cast<Nterm*>(result);
  
}

void PolynomialAlgebra::elem_text_out(buffer &o,
                             const ring_elem ff,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{
  auto f = reinterpret_cast<const Poly*>(ff.mPolyVal);

  if (f->numTerms() == 0)
    {
      o << "0";
      return;
    }

  bool two_terms = (f->numTerms() > 1);
  bool needs_parens = p_parens && two_terms;
  if (needs_parens)
    {
      if (p_plus) o << '+';
      o << '(';
      p_plus = false;
    }

  for (auto i = f->cbegin(); i != f->cend(); i++)
    {
      bool is_one = monoid().is_one(i.monom());
      p_parens = !is_one;
      bool p_one_this = (is_one && needs_parens) || (is_one && p_one);
      mCoefficientRing.elem_text_out(o, i.coeff(), p_one_this, p_plus, p_parens);
      if (!is_one)
        monoid().elem_text_out(o, i.monom(), mVariableNames);
      p_plus = true;
    }

  if (needs_parens) o << ')';
}

ring_elem PolynomialAlgebra::eval(const RingMap *map, const ring_elem f, int first_var) const
{
  // evaluate a ring map at a ring element.
  // TODO
  return f; // TODO: Bad return value;
}

engine_RawArrayPairOrNull PolynomialAlgebra::list_form(const Ring *coeffR, const ring_elem ff) const
{
  // Either coeffR should be the actual coefficient ring (possible a "toField"ed ring)
  // or a polynomial ring.  If not, NULL is returned and an error given
  // In the latter case, the last set of variables are part of
  // the coefficients.

  auto f = reinterpret_cast<const Poly*>(ff.mPolyVal);
  if (coeffR != &mCoefficientRing)
    {
      ERROR("expected coefficient ring");
      return nullptr;
    }
  int nterms = static_cast<int>(f->numTerms());
  engine_RawMonomialArray monoms = GETMEM(engine_RawMonomialArray, sizeofarray(monoms,nterms));
  engine_RawRingElementArray coeffs = GETMEM(engine_RawRingElementArray, sizeofarray(coeffs,nterms));
  engine_RawArrayPair result = newitem(struct engine_RawArrayPair_struct);
  monoms->len = nterms;
  coeffs->len = nterms;
  result->monoms = monoms;
  result->coeffs = coeffs;

  // fill result
  std::vector<int> vp;
  int next = 0;
  for (auto i=f->cbegin(); i != f->cend(); ++i, ++next)
    {
      ring_elem c = mCoefficientRing.copy(i.coeff());
      vp.resize(0);
      monoid().getMonomial(i.monom(), vp); // should this instead reverse the monomial?
      coeffs->array[next] = RingElement::make_raw(coeffR, c);
      monoms->array[next] = Monomial::make(vp); // reverses the monomial
    }
  
  return result;
}

bool PolynomialAlgebra::is_homogeneous(const ring_elem f) const
{
#if 0
  // MES: being written, Dec 2017
  auto g = f.mPolyVal;
  if (g == nullptr or g->numTerms() == 0)
    return true;
  ExponentVector e = degree_monoid()->make_one();
  ExponentVector degf = degree_monoid()->make_one();
  iter i { f} ;
#endif
  return false;
}

void PolynomialAlgebra::degree(const ring_elem f, int *d) const
{
  // MES: being written, Dec 2017
}
bool PolynomialAlgebra::multi_degree(const ring_elem f, int *d) const
{
  // MES: being written, Dec 2017
  return false;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
