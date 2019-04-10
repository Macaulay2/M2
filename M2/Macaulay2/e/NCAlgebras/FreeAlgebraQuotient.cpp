#include "FreeAlgebraQuotient.hpp"
#include "NCGroebner.hpp"
//#include "WordTable.hpp"

using ExponentVector = int*;

SumCollector* FreeAlgebraQuotient::make_SumCollector() const
{
  return mFreeAlgebra.make_SumCollector();
}

FreeAlgebraQuotient::FreeAlgebraQuotient(const FreeAlgebra& A,
                                         std::unique_ptr<ConstPolyList> GB,
                                         int maxdeg)
  : mFreeAlgebra(A),
    mGroebner(std::move(GB)),
    mMaxdeg(maxdeg)
{
  // Build the word table for the reduction
  for (auto f : *mGroebner)
    mWordTable.insert(Word(f->cbegin().monom()));
}

void FreeAlgebraQuotient::normalizeInPlace(Poly& f) const
{
  // for now, we will simply reduce the poly f and copy the result into f.
  // TODO: Make this work 'in place'.
  auto fRed = NCGroebner::twoSidedReduction(freeAlgebra(),&f,*mGroebner,mWordTable);
  freeAlgebra().swap(f,*fRed);
}

void FreeAlgebraQuotient::clear(Poly& f) const
{
  // TODO: need Polynomial type to allow us access, or have a clear function itself.
  setZero(f);
}

void FreeAlgebraQuotient::setZero(Poly& f) const
{
  mFreeAlgebra.setZero(f);
}

void FreeAlgebraQuotient::from_coefficient(Poly& result, const ring_elem a) const
{
  mFreeAlgebra.from_coefficient(result, a);
  normalizeInPlace(result);
}

void FreeAlgebraQuotient::from_long(Poly& result, long n) const
{
  from_coefficient(result, coefficientRing()->from_long(n));
}

void FreeAlgebraQuotient::from_int(Poly& result, mpz_srcptr n) const
{
  from_coefficient(result, coefficientRing()->from_int(n));
}

bool FreeAlgebraQuotient::from_rational(Poly& result, const mpq_ptr q) const
{
  ring_elem cq; // in coeff ring.
  bool worked = coefficientRing()->from_rational(q, cq);
  if (!worked) return false;
  from_coefficient(result, cq);
  return true;
}

void FreeAlgebraQuotient::copy(Poly& result, const Poly& f) const
{
  mFreeAlgebra.copy(result, f);
}

void FreeAlgebraQuotient::swap(Poly& f, Poly& g) const
{
  mFreeAlgebra.swap(f,g);
}

void FreeAlgebraQuotient::var(Poly& result, int v) const
{
  mFreeAlgebra.var(result, v);
  normalizeInPlace(result);
}

void FreeAlgebraQuotient::from_word(Poly& result, ring_elem coeff, const std::vector<int>& word) const
{
  mFreeAlgebra.from_word(result, coeff, word);
  normalizeInPlace(result);
}

void FreeAlgebraQuotient::from_word(Poly& result, const std::vector<int>& word) const
{
  from_word(result, coefficientRing()->from_long(1), word);
}

bool FreeAlgebraQuotient::is_unit(const Poly& f) const
{
  return mFreeAlgebra.is_unit(f); // TODO: this is not correct: if f = 1 + (nilpotent).
}

int FreeAlgebraQuotient::compare_elems(const Poly& f, const Poly& g) const
{
  return mFreeAlgebra.compare_elems(f, g);
}

bool FreeAlgebraQuotient::is_equal(const Poly& f, const Poly& g) const
{
  return mFreeAlgebra.is_equal(f, g);
}

void FreeAlgebraQuotient::negate(Poly& result, const Poly& f) const
{
  return mFreeAlgebra.negate(result, f);
}

void FreeAlgebraQuotient::add(Poly& result, const Poly& f, const Poly& g) const
{
  return mFreeAlgebra.add(result, f, g);
}

void FreeAlgebraQuotient::subtract(Poly& result, const Poly& f, const Poly& g) const
{
  return mFreeAlgebra.subtract(result, f, g);
}

void FreeAlgebraQuotient::mult(Poly& result, const Poly& f, const Poly& g) const
{
  mFreeAlgebra.mult(result, f, g);
  normalizeInPlace(result);
}

void FreeAlgebraQuotient::power(Poly& result, const Poly& f, int n) const
{
  mFreeAlgebra.power(result, f, n);
  normalizeInPlace(result);
}

void FreeAlgebraQuotient::power(Poly& result, const Poly& f, mpz_ptr n) const
{
  mFreeAlgebra.power(result, f, n);
  normalizeInPlace(result);
}

void FreeAlgebraQuotient::elem_text_out(buffer &o,
                                const Poly& f,
                                bool p_one,
                                bool p_plus,
                                bool p_parens) const
{
  mFreeAlgebra.elem_text_out(o, f, p_one, p_plus, p_parens);
}

bool FreeAlgebraQuotient::is_homogeneous(const Poly& f) const
{
  return mFreeAlgebra.is_homogeneous(f);
}

void FreeAlgebraQuotient::degree(const Poly& f, int *d) const
{
  multi_degree(f, d);
}

bool FreeAlgebraQuotient::multi_degree(const Poly& f,
                               int *already_allocated_degree_vector) const
{
  return mFreeAlgebra.multi_degree(f, already_allocated_degree_vector);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
