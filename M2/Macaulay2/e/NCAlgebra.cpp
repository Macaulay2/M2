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
  result->mCoefficients.push_back(mCoefficientRing.from_long(1));
  result->mMonomials.push_back(1);
  result->mMonomials.push_back(v);
  result->mMonomials.push_back(-1);  
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

ring_elem NCFreeAlgebra::negate(const ring_elem f) const
{
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
  o << "[";
  for (int i=0; i<f->mMonomials.size(); i++)
    {
      if (i>0) o << " ";
      o << f->mMonomials[i];
    }
  o << "]";
}

ring_elem NCFreeAlgebra::eval(const RingMap *map, const ring_elem f, int first_var) const
{
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
