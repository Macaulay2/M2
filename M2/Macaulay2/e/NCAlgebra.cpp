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
}

ring_elem NCFreeAlgebra::from_int(mpz_ptr n) const
{
}

ring_elem NCFreeAlgebra::from_rational(mpq_ptr q) const
{
}

ring_elem NCFreeAlgebra::var(int v) const
{
  std::cout << "called var with v = " << v << std::endl;
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
  std::cout << "entered negate " << std::endl;
  NCPolynomial* result = new NCPolynomial;
  // use the same monomials
  result->copyMonoms(f->mMonomials);
  // request the vector of the appropriate size
  result->reserveCoeff(f->numTerms());
  // negate all the coefficients 
  for (auto i=f->cbeginCoeff(); i != f->cendCoeff(); ++i)
    result->push_backCoeff(mCoefficientRing.negate(*i));
  std::cout << "exited negate " << std::endl;
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
  o << "coeffs: [";
  for (int i=0; i<f->mCoefficients.size(); i++)
    {
      if (i>0) o << " ";
      mCoefficientRing.elem_text_out(o, f->mCoefficients[i]);
    }
  o << "] monoms: [";
  for (int i=0; i<f->mMonomials.size(); i++)
    {
      if (i>0) o << " ";
      o << f->mMonomials[i];
    }
  o << "]";

#if 0
  // new version
  bool hasTwoTerms = (f->numTerms > 1);
  for (auto i = f->begin(); i != f->end(); i++)
    {
      
    }

  // from poly.cpp
  Nterm *t = f;
  if (t == NULL)
    {
      o << '0';
      return;
    }

  int two_terms = (t->next != NULL);

  int needs_parens = p_parens && two_terms;
  if (needs_parens)
    {
      if (p_plus) o << '+';
      o << '(';
      p_plus = false;
    }

  for (t = f; t != NULL; t = t->next)
    {
      int isone = M_->is_one(t->monom);
      p_parens = !isone;
      bool p_one_this = (isone && needs_parens) || (isone && p_one);
      K_->elem_text_out(o,t->coeff, p_one_this,p_plus,p_parens);
      if (!isone)
        {
          M_->elem_text_out(o, t->monom, p_one_this);
        }
      p_plus = true;
    }
  if (needs_parens) o << ')';
 
  // from ntuple.cpp
  int len_ = 0;
  for (unsigned int v=0; v<nvars; v++)
    if (a[v] != 0) {
      len_++;
        if (varnames->len < v)
          o << ".";
        else
          o << varnames->array[v];
      int e = a[v];
      int single = (varnames->array[v]->len == 1);
      if (e > 1 && single) o << e;
      else if (e > 1) o << "^" << e;
      else if (e < 0) o << "^(" << e << ")";
    }
  if (len_ == 0 && p_one) o << "1";

#elseif

}

ring_elem NCFreeAlgebra::eval(const RingMap *map, const ring_elem f, int first_var) const
{
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
