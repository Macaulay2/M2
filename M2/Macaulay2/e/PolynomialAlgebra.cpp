#include "PolynomialAlgebra.hpp"
#include <vector>
#include <string>
#include <iostream>

PolynomialAlgebra* PolynomialAlgebra::create(const Ring* K,
                                     M2_ArrayString names,
                                     const PolynomialRing* degreeRing)
{
  M2_ASSERT(K != nullptr);
  PolynomialAlgebra* result = new PolynomialAlgebra(K, names);
  result->initialize_ring(K->characteristic(), degreeRing, nullptr);
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

  zeroV = from_long(0);
  oneV = from_long(1);
  minus_oneV = from_long(-1);
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
  const Poly* f = reinterpret_cast<Poly*>(a.poly_val);

  // f is a variable iff: #terms is 1, monomial is [3,1,v], coeff is 1
  if (f->numTerms() != 1) return -1;
  auto i = f->cbegin();
  if (!mCoefficientRing.is_equal(mCoefficientRing.one(), i.coeff())) return -1;
  const int* m = *i.monom();
  if (m[0] != 3 || m[1] != 1) return 0;
  return m[2];
}

ring_elem PolynomialAlgebra::from_long(long n) const
{
  Poly* result = new Poly;

  result->getCoeffInserter().push_back(mCoefficientRing.from_long(n));
  //monoid().make_one(result->getMonomInserter());
  result->getMonomInserter().push_back(2); // length of the monomial data
  result->getMonomInserter().push_back(0); // degree of the monomial.  Will need to change if weights are present.
  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::from_int(mpz_ptr n) const
{
  Poly* result = new Poly;
  result->getCoeffInserter().push_back(mCoefficientRing.from_int(n));
  //monoid().make_one(result->getMonomInserter());
  result->getMonomInserter().push_back(2); // length of the monomial data
  result->getMonomInserter().push_back(0); // degree of the monomial.  Will need to change if weights are present.
  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::from_rational(mpq_ptr q) const
{
  Poly* result = new Poly;
  result->getCoeffInserter().push_back(mCoefficientRing.from_rational(q));
  //monoid().make_one(result->getMonomInserter());
  result->getMonomInserter().push_back(2); // length of the monomial data
  result->getMonomInserter().push_back(0); // degree of the monomial.  Will need to change if weights are present.
  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::var(int v) const
{
  Poly* result = new Poly;

  result->getCoeffInserter().push_back(mCoefficientRing.from_long(1));
  // monoid().make_variable(result->getMonomInserter(), v)
  result->getMonomInserter().push_back(3); // length of the monomial data  
  result->getMonomInserter().push_back(1); // degree of the monomial.  Will need to change if weights are present.
  result->getMonomInserter().push_back(v); // variable

  return reinterpret_cast<Nterm*>(result);
}

bool PolynomialAlgebra::promote(const Ring *R, const ring_elem f, ring_elem &result) const
{
  // TODO
  return false;
}

bool PolynomialAlgebra::lift(const Ring *R, const ring_elem f, ring_elem &result) const
{
  // TODO
  return false;
}

bool PolynomialAlgebra::is_unit(const ring_elem f1) const
{
  const Poly* f = reinterpret_cast<Poly*>(f1.poly_val);

  if (f->numTerms() != 1) return false;
  auto i = f->cbegin();
  return **(i.monom()) == 2 && // obscure way to decide if monomial is one.  Will change TODO.
    // will change to monoid().is_one(i.monom())
    mCoefficientRing.is_unit(i.coeff());
}

bool PolynomialAlgebra::is_zero(const ring_elem f1) const
{
  const Poly* f = reinterpret_cast<Poly*>(f1.poly_val);
  return (f->numTerms() == 0);
}

bool PolynomialAlgebra::is_equal(const ring_elem f, const ring_elem g) const
{
  return false; // TODO
}

int PolynomialAlgebra::compare_elems(const ring_elem f1, const ring_elem g1) const
{
  const Poly* f = reinterpret_cast<Poly*>(f1.poly_val);
  const Poly* g = reinterpret_cast<Poly*>(g1.poly_val);
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

int PolynomialAlgebra::compare_monoms(const Monom m1, const Monom m2) const
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
  const Poly* f = reinterpret_cast<Poly*>(f1.poly_val);
  Poly* result = new Poly;

  auto outmonom = result->getMonomInserter();
  auto outcoeff = result->getCoeffInserter();

  for (auto i = f->cbeginMonom(); i != f->cendMonom(); ++i)
    outmonom.push_back(*i);

  for (auto i=f->cbeginCoeff(); i != f->cendCoeff(); ++i)
    outcoeff.push_back(mCoefficientRing.negate(*i));

  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::add(const ring_elem f1, const ring_elem g1) const
{
  const Poly* f = reinterpret_cast<Poly*>(f1.poly_val);
  const Poly* g = reinterpret_cast<Poly*>(g1.poly_val);
  Poly* result = new Poly;
  auto fIt = f->cbegin();
  auto gIt = g->cbegin();
  auto fEnd = f->cend();
  auto gEnd = g->cend();

  auto outcoeff = result->getCoeffInserter();
  auto outmonom = result->getMonomInserter();
  
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
          outcoeff.push_back(gCoeff);
          // this line:
          // monoid().copy(outmonom, gMon)
          // will replace these 2 lines:
          for (int j = 0; j < **gMon; j++)
            outmonom.push_back((*gMon)[j]);
          gIt++;
          break;
        case GT:
          outcoeff.push_back(fCoeff);
          // this line:
          // monoid().copy(outmonom, fMon)
          // will replace these 2 lines:
          for (int j = 0; j < **fMon; j++)
            outmonom.push_back((*fMon)[j]);
          fIt++;
          break;
        case EQ:
          ring_elem coeffResult = mCoefficientRing.add(fCoeff,gCoeff);
          if (!mCoefficientRing.is_zero(coeffResult))
            {
              outcoeff.push_back(coeffResult);
              // this line:
              // monoid().copy(outmonom, gMon)
              // will replace these 2 lines:
              for (int j = 0; j < **gMon; j++)
                outmonom.push_back((*gMon)[j]);
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
          // this line:
          // monoid().copy(outmonom, gMon)
          // will replace these 2 lines:
          for (int j = 0; j < **gMon; j++)
            outmonom.push_back((*gMon)[j]);
        }
    }
  if (gIt == gEnd)
    {
      for ( ; fIt != fEnd; fIt++)
        {
          auto fMon = fIt.monom();
          auto fCoeff = fIt.coeff();
          outcoeff.push_back(fCoeff);
          // this line:
          // monoid().copy(outmonom, fMon)
          // will replace these 2 lines:
          for (int j = 0; j < **fMon; j++)
            outmonom.push_back((*fMon)[j]);
        }
    }
  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::subtract(const ring_elem f1, const ring_elem g1) const
{
  const Poly* f = reinterpret_cast<Poly*>(f1.poly_val);
  const Poly* g = reinterpret_cast<Poly*>(g1.poly_val);
  Poly* result = new Poly;
  auto fIt = f->cbegin();
  auto gIt = g->cbegin();
  auto fEnd = f->cend();
  auto gEnd = g->cend();

  auto outcoeff = result->getCoeffInserter();
  auto outmonom = result->getMonomInserter();

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
          outcoeff.push_back(mCoefficientRing.negate(gCoeff));
          // this line:
          // monoid().copy(outmonom, gMon)
          // will replace these 2 lines:
          for (int j = 0; j < **gMon; j++)
            outmonom.push_back((*gMon)[j]);
          gIt++;
          break;
        case GT:
          outcoeff.push_back(fCoeff);
          // this line:
          // monoid().copy(outmonom, fMon)
          // will replace these 2 lines:
          for (int j = 0; j < **fMon; j++)
            outmonom.push_back((*fMon)[j]);
          fIt++;
          break;
        case EQ:
          ring_elem coeffResult = mCoefficientRing.subtract(fCoeff,gCoeff);
          if (!mCoefficientRing.is_zero(coeffResult))
            {
              outcoeff.push_back(coeffResult);
              // this line:
              // monoid().copy(outmonom, gMon)
              // will replace these 2 lines:
              for (int j = 0; j < **gMon; j++)
                outmonom.push_back((*gMon)[j]);
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
          // this line:
          // monoid().copy(outmonom, gMon)
          // will replace these 2 lines:
          for (int j = 0; j < **gMon; j++)
            outmonom.push_back((*gMon)[j]);
        }
    }
  if (gIt == gEnd)
    {
      for ( ; fIt != fEnd; fIt++)
        {
          auto fMon = fIt.monom();
          auto fCoeff = fIt.coeff();
          outcoeff.push_back(fCoeff);
          // this line:
          // monoid().copy(outmonom, fMon)
          // will replace these 2 lines:
          for (int j = 0; j < **fMon; j++)
            outmonom.push_back((*fMon)[j]);
        }
    }
  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::mult(const ring_elem f1, const ring_elem g1) const
{
  // TODO: make this a geobucket heap multiply function?
  //
  return f1; // BAD RETURN
#if 0  
  const Poly* f = reinterpret_cast<Poly*>(f1.poly_val);
  Poly* zeroPoly = new Poly;
  ring_elem resultSoFar = reinterpret_cast<Nterm*>(zeroPoly);
  for (auto fIt = f->cbegin(); fIt != f->cend(); fIt++)
    {
      // this one sums in place, adding to the end.
      add_to_end(resultSoFar,mult_by_term_left(g1, fIt.coeff(), fIt.monom()));
    }
  std::cout << "Number of terms : " << zeroPoly->numTerms() << std::endl;
  return resultSoFar;
#endif
}

ring_elem PolynomialAlgebra::mult_by_term_right(const ring_elem f1,
                                            const ring_elem c, const Monom m) const
{
  // return f*c*m
  const Poly* f = reinterpret_cast<Poly*>(f1.poly_val);
  Poly* result = new Poly;
  auto outcoeff = result->getCoeffInserter();
  auto outmonom = result->getMonomInserter();
  for(auto i=f->cbegin(); i != f->cend(); i++)
    {
      // multiply the coefficients
      outcoeff.push_back(mCoefficientRing.mult(i.coeff(),c));

      // The following should be replaced by
      // monoid().mult(i.monom(), m, outmonom);
      // tack on the monomial pointed to by m to the end of current monomial.
      int lenm = (*m)[0];
      int degm = (*m)[1];
      const int* curMon = *(i.monom());
      outmonom.push_back(curMon[0] + lenm - 2);
      outmonom.push_back(curMon[1] + degm);
      // copy f's monomial
      for(auto j = 2; j < curMon[0]; j++)
        {
          outmonom.push_back(curMon[j]);
        }
      // copy m's monomial
      for(auto j = 2; j < (*m)[0]; j++)
        {
          outmonom.push_back((*m)[j]);
        }      
    }
  return reinterpret_cast<Nterm*>(result);
}

ring_elem PolynomialAlgebra::mult_by_term_left(const ring_elem f1,
                                           const ring_elem c, const Monom m) const
{
  // return (c*m)*f
  const Poly* f = reinterpret_cast<Poly*>(f1.poly_val);
  Poly* result = new Poly;
  auto outcoeff = result->getCoeffInserter();
  auto outmonom = result->getMonomInserter();
  for(auto i=f->cbegin(); i != f->cend(); i++)
    {
      // multiply the coefficients
      outcoeff.push_back(mCoefficientRing.mult(c,i.coeff()));
      // tack on the monomial pointed to by m to the end of current monomial.
      int lenm = (*m)[0];
      int degm = (*m)[1];
      const int* curMon = *(i.monom());
      outmonom.push_back(curMon[0] + lenm - 2);
      outmonom.push_back(curMon[1] + degm);
      // copy m's monomial
      for(auto j = 2; j < (*m)[0]; j++)
        {
          outmonom.push_back((*m)[j]);
        }      
      // copy f's monomial
      for(auto j = 2; j < curMon[0]; j++)
        {
          outmonom.push_back(curMon[j]);
        }
    }
  return reinterpret_cast<Nterm*>(result);
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

void PolynomialAlgebra::elem_text_out(buffer &o,
                             const ring_elem ff,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{
  const Poly* f = reinterpret_cast<const Poly*>(ff.poly_val);

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
              o << mVariableNames[curvar];
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

ring_elem PolynomialAlgebra::eval(const RingMap *map, const ring_elem f, int first_var) const
{
  // evaluate a ring map at a ring element.
  // TODO
  return f; // TODO: Bad return value;
}

/*
engine_RawArrayPairOrNull PolynomialAlgebra::list_form(const Ring *coeffR, const ring_elem f) const
{
  // Either coeffR should be the actual coefficient ring (possible a "toField"ed ring)
  // or a polynomial ring.  If not, NULL is returned and an error given
  // In the latter case, the last set of variables are part of
  // the coefficients.
  int nvars0 = check_coeff_ring(coeffR, this);
  if (nvars0 < 0) return 0;
  int n = n_logical_terms(nvars0,f);
  engine_RawMonomialArray monoms = GETMEM(engine_RawMonomialArray, sizeofarray(monoms,n));
  engine_RawRingElementArray coeffs = GETMEM(engine_RawRingElementArray, sizeofarray(coeffs,n));
  monoms->len = n;
  coeffs->len = n;
  engine_RawArrayPair result = newitem(struct engine_RawArrayPair_struct);
  result->monoms = monoms;
  result->coeffs = coeffs;

  int *exp = newarray_atomic(int, n_vars());
  intarray resultvp;
  const Nterm *t = f;
  for (int next = 0; next < n; next++)
    {
      getMonoid()->to_expvector(t->monom, exp);
      ring_elem c = get_logical_coeff(coeffR, t); // increments t to the next term of f.
      varpower::from_ntuple(nvars0, exp, resultvp);
      monoms->array[next] = Monomial::make(resultvp.raw());
      coeffs->array[next] = RingElement::make_raw(coeffR, c);
      resultvp.shrink(0);

      assert( monoms->array[next] != NULL );
      assert( coeffs->array[next] != NULL );
    }
  deletearray(exp);
  return result;
}
*/

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
