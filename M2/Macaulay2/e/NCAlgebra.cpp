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
  : mCoefficientRing(*K)
{
  for (auto i=0; i<names->len; i++)
    mVariableNames.push_back(std::string(names->array[i]->array, names->array[i]->len));
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

bool NCFreeAlgebra::is_unit(const ring_elem f) const
{
}

bool NCFreeAlgebra::is_zero(const ring_elem f) const
{
}

bool NCFreeAlgebra::is_equal(const ring_elem f, const ring_elem g) const
{
}

int NCFreeAlgebra::compare_elems(const ring_elem f, const ring_elem g) const
{
}

ring_elem NCFreeAlgebra::copy(const ring_elem f) const
{
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
  result->copyMonoms(f->getMonomVector());
  // request the vector of the appropriate size
  result->reserveCoeff(f->numTerms());
  // negate all the coefficients 
  for (auto i=f->cbeginCoeff(); i != f->cendCoeff(); ++i)
    result->push_backCoeff(mCoefficientRing.negate(*i));
  return reinterpret_cast<Nterm*>(result);
}

ring_elem NCFreeAlgebra::add(const ring_elem f, const ring_elem g) const
{
}

ring_elem NCFreeAlgebra::subtract(const ring_elem f, const ring_elem g) const
{
}

ring_elem NCFreeAlgebra::mult(const ring_elem f, const ring_elem g) const
{
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
              o << mVariableNames[mon_ptr[j]];
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
