#include "NCAlgebra.hpp"
#include <vector>
#include <string>
#include <iostream>

NCFreeAlgebra* NCFreeAlgebra::create(const Ring* K,
                      M2_ArrayString names)
{
  M2_ASSERT(K != nullptr);
  return new NCFreeAlgebra(K, names);
}

NCFreeAlgebra::NCFreeAlgebra(const Ring* K,
                             M2_ArrayString names)
  : mCoefficientRing(*K),
    mNumVars(names->len)
{
  
  for (auto i=0; i<names->len; i++)
    // used emplace_back here.  C++11 feature which allows constructor arguments to be passed in place,
    // rather than having to create a temporary object to pass to push_back.
    mVariableNames.emplace_back(names->array[i]->array, names->array[i]->len);
}

void NCFreeAlgebra::text_out(buffer &o) const
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

unsigned int NCFreeAlgebra::computeHashValue(const ring_elem a) const
{

}

ring_elem NCFreeAlgebra::from_long(long n) const
{
  NCPolynomial* result = new NCPolynomial;
  result->push_backCoeff(mCoefficientRing.from_long(n));
  result->push_backMonom(2);  // length of the monomial data
  result->push_backMonom(0);  // degree of the monomial.  Will need to change if weights are present.
  return reinterpret_cast<Nterm*>(result);
}

ring_elem NCFreeAlgebra::from_int(mpz_ptr n) const
{
  NCPolynomial* result = new NCPolynomial;
  result->push_backCoeff(mCoefficientRing.from_int(n));
  result->push_backMonom(2);  // length of the monomial data
  result->push_backMonom(0);  // degree of the monomial.  Will need to change if weights are present.
  return reinterpret_cast<Nterm*>(result);
}

ring_elem NCFreeAlgebra::from_rational(mpq_ptr q) const
{
  NCPolynomial* result = new NCPolynomial;
  result->push_backCoeff(mCoefficientRing.from_rational(q));
  result->push_backMonom(2);  // length of the monomial data
  result->push_backMonom(0);  // degree of the monomial.  Will need to change if weights are present.
  return reinterpret_cast<Nterm*>(result);
}

ring_elem NCFreeAlgebra::var(int v) const
{
  NCPolynomial* result = new NCPolynomial;
  result->push_backCoeff(mCoefficientRing.from_long(1));
  result->push_backMonom(3);  // length of the monomial data
  result->push_backMonom(1);  // degree of the monomial.  Will need to change if weights are present.
  result->push_backMonom(v);  // variable
  return reinterpret_cast<Nterm*>(result);
}

bool NCFreeAlgebra::promote(const Ring *R, const ring_elem f, ring_elem &result) const
{
}

bool NCFreeAlgebra::lift(const Ring *R, const ring_elem f, ring_elem &result) const
{
}

bool NCFreeAlgebra::is_unit(const ring_elem f1) const
{
  const NCPolynomial* f = reinterpret_cast<NCPolynomial*>(f1.poly_val);
  return ((f->numTerms() == 1) &&                           // one term
          (f->getMonomVector().size() == 2) &&              // empty monomial
          mCoefficientRing.is_unit(f->cbegin().coeff()));   // unit coefficient
          
}

bool NCFreeAlgebra::is_zero(const ring_elem f1) const
{
  const NCPolynomial* f = reinterpret_cast<NCPolynomial*>(f1.poly_val);
  return (f->numTerms() == 0);
}

bool NCFreeAlgebra::is_equal(const ring_elem f, const ring_elem g) const
{
}

int NCFreeAlgebra::compare_elems(const ring_elem f1, const ring_elem g1) const
{
  const NCPolynomial* f = reinterpret_cast<NCPolynomial*>(f1.poly_val);
  const NCPolynomial* g = reinterpret_cast<NCPolynomial*>(g1.poly_val);
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
          cmp = compare_monoms(fIt.monom(),gIt.monom());
          if (cmp != 0) return cmp;
        }
      // if we are here, then the monomials are the same and we compare coefficients.
      // for example if a,b are in the base and a > b then ax > bx.
      cmp = mCoefficientRing.compare_elems(fIt.coeff(), fIt.coeff());
      if (cmp != 0) return cmp;
    }
}

int NCFreeAlgebra::compare_monoms(const NCMonomial m1, const NCMonomial m2) const
{
  // here, compare the monomials pointed to by fIt and gIt.
  // should probably make an NCMonoid class...
  // first compare degree
  if ((*m1)[1] > (*m2)[1]) return GT;
  if ((*m1)[1] < (*m2)[1]) return LT;
  // at this stage, they have the same degree, so use lex order
  for (int j = 2; j < **m1; j++)
    {
      if ((*m1)[j] > (*m2)[j]) return LT;
      if ((*m1)[j] < (*m2)[j]) return GT;
    }
  // if we are here, the monomials are the same.
  return EQ;
}

ring_elem NCFreeAlgebra::copy(const ring_elem f) const
{
  // FRANK: is this what we want to do?
  return f;
}

void NCFreeAlgebra::remove(ring_elem &f) const
{
  // do nothing
}

ring_elem NCFreeAlgebra::negate(const ring_elem f1) const
{
  const NCPolynomial* f = reinterpret_cast<NCPolynomial*>(f1.poly_val);
  NCPolynomial* result = new NCPolynomial;
  // use the same monomials
  result->copyAllMonoms(f->getMonomVector());
  // request the vector of the appropriate size
  result->reserveCoeff(f->numTerms());
  // negate all the coefficients 
  for (auto i=f->cbeginCoeff(); i != f->cendCoeff(); ++i)
    result->push_backCoeff(mCoefficientRing.negate(*i));
  return reinterpret_cast<Nterm*>(result);
}

ring_elem NCFreeAlgebra::add(const ring_elem f1, const ring_elem g1) const
{
  const NCPolynomial* f = reinterpret_cast<NCPolynomial*>(f1.poly_val);
  const NCPolynomial* g = reinterpret_cast<NCPolynomial*>(g1.poly_val);
  NCPolynomial* result = new NCPolynomial;
  auto fIt = f->cbegin();
  auto gIt = g->cbegin();
  auto fEnd = f->cend();
  auto gEnd = g->cend();

  // loop over the iterators for f and g, adding the bigger of the two to
  // the back of the monomial and coefficient vectors of the result.  If a tie, add the coefficients.
  while ((fIt != fEnd) && (gIt != gEnd))
    {
      auto fMon = fIt.monom();
      auto gMon = gIt.monom();
      auto fCoeff = fIt.coeff();
      auto gCoeff = gIt.coeff();
      switch(compare_monoms(fMon,gMon))
        {
        case LT:
          result->push_backCoeff(gCoeff);
          for (int j = 0; j < **gMon; j++) result->push_backMonom((*gMon)[j]);
          gIt++;
          break;
        case GT:
          result->push_backCoeff(fCoeff);
          for (int j = 0; j < **fMon; j++) result->push_backMonom((*fMon)[j]);
          fIt++;
          break;
        case EQ:
          ring_elem coeffResult = mCoefficientRing.add(fCoeff,gCoeff);
          if (!mCoefficientRing.is_zero(coeffResult))
            {
              result->push_backCoeff(coeffResult);
              for (int j = 0; j < **gMon; j++) result->push_backMonom((*gMon)[j]);
            }
          fIt++;
          gIt++;
        }
    }
  if (fIt == fEnd)
    {
      while (gIt != gEnd)
        {
          auto gMon = gIt.monom();
          auto gCoeff = gIt.coeff();
          result->push_backCoeff(gCoeff);
          for (int j = 0; j < **gMon; j++) result->push_backMonom((*gMon)[j]);
          gIt++;
        }
    }
  if (gIt == gEnd)
    {
      while (fIt != fEnd)
        {
          auto fMon = fIt.monom();
          auto fCoeff = fIt.coeff();
          result->push_backCoeff(fCoeff);
          for (int j = 0; j < **fMon; j++) result->push_backMonom((*fMon)[j]);
          fIt++;
        }
    }
  return reinterpret_cast<Nterm*>(result);
}

ring_elem NCFreeAlgebra::subtract(const ring_elem f1, const ring_elem g1) const
{
  const NCPolynomial* f = reinterpret_cast<NCPolynomial*>(f1.poly_val);
  const NCPolynomial* g = reinterpret_cast<NCPolynomial*>(g1.poly_val);
  NCPolynomial* result = new NCPolynomial;
  auto fIt = f->cbegin();
  auto gIt = g->cbegin();
  auto fEnd = f->cend();
  auto gEnd = g->cend();

  // loop over the iterators for f and g, adding the bigger of the two to
  // the back of the monomial and coefficient vectors of the result.  If a tie, subtract the coefficients.
  while ((fIt != fEnd) && (gIt != gEnd))
    {
      auto fMon = fIt.monom();
      auto gMon = gIt.monom();
      auto fCoeff = fIt.coeff();
      auto gCoeff = gIt.coeff();
      switch(compare_monoms(fMon,gMon))
        {
        case LT:
          result->push_backCoeff(mCoefficientRing.negate(gCoeff));
          for (int j = 0; j < **gMon; j++) result->push_backMonom((*gMon)[j]);
          gIt++;
          break;
        case GT:
          result->push_backCoeff(fCoeff);
          for (int j = 0; j < **fMon; j++) result->push_backMonom((*fMon)[j]);
          fIt++;
          break;
        case EQ:
          ring_elem coeffResult = mCoefficientRing.subtract(fCoeff,gCoeff);
          if (!mCoefficientRing.is_zero(coeffResult))
            {
              result->push_backCoeff(coeffResult);
              for (int j = 0; j < **gMon; j++) result->push_backMonom((*gMon)[j]);
            }
          fIt++;
          gIt++;
        }
    }
  if (fIt == fEnd)
    {
      while (gIt != gEnd)
        {
          auto gMon = gIt.monom();
          auto gCoeff = gIt.coeff();
          result->push_backCoeff(mCoefficientRing.negate(gCoeff));
          for (int j = 0; j < **gMon; j++) result->push_backMonom((*gMon)[j]);
          gIt++;
        }
    }
  if (gIt == gEnd)
    {
      while (fIt != fEnd)
        {
          auto fMon = fIt.monom();
          auto fCoeff = fIt.coeff();
          result->push_backCoeff(fCoeff);
          for (int j = 0; j < **fMon; j++) result->push_backMonom((*fMon)[j]);
          fIt++;
        }
    }
  return reinterpret_cast<Nterm*>(result);
}

ring_elem NCFreeAlgebra::mult(const ring_elem f1, const ring_elem g1) const
{
  const NCPolynomial* f = reinterpret_cast<NCPolynomial*>(f1.poly_val);
  NCPolynomial* zeroPoly = new NCPolynomial;
  ring_elem resultSoFar = reinterpret_cast<Nterm*>(zeroPoly);
  for (auto fIt = f->cbegin(); fIt != f->cend(); fIt++)
    {
      // this one sums in place, adding to the end.
      add_to_end(resultSoFar,mult_by_term_left(g1, fIt.coeff(), fIt.monom()));
    }
  std::cout << "Number of terms : " << zeroPoly->numTerms() << std::endl;
  return resultSoFar;
}

void NCFreeAlgebra::add_to_end(ring_elem f1, const ring_elem g1) const
{
  // this command adds the terms of g1 to the end of f1, so you must be sure that the
  // lead term of g1 is less than the last term of f1 to ensure that the order is preserved.
  NCPolynomial* f = reinterpret_cast<NCPolynomial*>(f1.poly_val);
  const NCPolynomial* g = reinterpret_cast<NCPolynomial*>(g1.poly_val);
  f->appendPolynomial(g);
}

ring_elem NCFreeAlgebra::mult_by_term_right(const ring_elem f1,
                                            const ring_elem c, const NCMonomial m) const
{
  // return f*c*m
  const NCPolynomial* f = reinterpret_cast<NCPolynomial*>(f1.poly_val);
  NCPolynomial* result = new NCPolynomial;
  for(auto i=f->cbegin(); i != f->cend(); i++)
    {
      // multiply the coefficients
      result->push_backCoeff(mCoefficientRing.mult(i.coeff(),c));
      // tack on the monomial pointed to by m to the end of current monomial.
      int lenm = (*m)[0];
      int degm = (*m)[1];
      const int* curMon = *(i.monom());
      result->push_backMonom(curMon[0] + lenm - 2);
      result->push_backMonom(curMon[1] + degm);
      // copy f's monomial
      for(auto j = 2; j < curMon[0]; j++)
        {
          result->push_backMonom(curMon[j]);
        }
      // copy m's monomial
      for(auto j = 2; j < (*m)[0]; j++)
        {
          result->push_backMonom((*m)[j]);
        }      
    }
  return reinterpret_cast<Nterm*>(result);
}

ring_elem NCFreeAlgebra::mult_by_term_left(const ring_elem f1,
                                           const ring_elem c, const NCMonomial m) const
{
  // return c*m*f
  const NCPolynomial* f = reinterpret_cast<NCPolynomial*>(f1.poly_val);
  NCPolynomial* result = new NCPolynomial;
  result->reserveCoeff(f->numTerms());
  for(auto i=f->cbegin(); i != f->cend(); i++)
    {
      // multiply the coefficients
      result->push_backCoeff(mCoefficientRing.mult(c,i.coeff()));
      // tack on the monomial pointed to by m to the end of current monomial.
      int lenm = (*m)[0];
      int degm = (*m)[1];
      const int* curMon = *(i.monom());
      result->push_backMonom(curMon[0] + lenm - 2);
      result->push_backMonom(curMon[1] + degm);
      // copy m's monomial
      for(auto j = 2; j < (*m)[0]; j++)
        {
          result->push_backMonom((*m)[j]);
        }      
      // copy f's monomial
      for(auto j = 2; j < curMon[0]; j++)
        {
          result->push_backMonom(curMon[j]);
        }
    }
  return reinterpret_cast<Nterm*>(result);
}

ring_elem NCFreeAlgebra::invert(const ring_elem f) const
{
}

ring_elem NCFreeAlgebra::divide(const ring_elem f, const ring_elem g) const
{
}

void NCFreeAlgebra::syzygy(const ring_elem a, const ring_elem b,
                      ring_elem &x, ring_elem &y) const
{
  // TODO?  what to place here?
}

void NCFreeAlgebra::elem_text_out(buffer &o,
                             const ring_elem ff,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{
  const NCPolynomial* f = reinterpret_cast<const NCPolynomial*>(ff.poly_val);

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
      bool is_one = i.monom().is_one_monomial();
      p_parens = !is_one;
      bool p_one_this = (is_one && needs_parens) || (is_one && p_one);
      mCoefficientRing.elem_text_out(o, i.coeff(), p_one_this, p_plus, p_parens);
      if (!is_one)
        {
          // if not the empty monomial, then output the monomial
          auto mon_length = **(i.monom()) - 2;
          auto mon_ptr = *(i.monom()) + 2;
          for (auto j = 0; j < mon_length; j++)
            {
              // for now, just output the string.
              int curvar = mon_ptr[j];
              int curvarPower = 0;
              o << mVariableNames[mon_ptr[j]];
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
      p_plus = true;
    }

  if (needs_parens) o << ')';
}

ring_elem NCFreeAlgebra::eval(const RingMap *map, const ring_elem f, int first_var) const
{
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
