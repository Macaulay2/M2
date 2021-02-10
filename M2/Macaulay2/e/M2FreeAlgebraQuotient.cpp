#include "M2FreeAlgebraQuotient.hpp"

#include <iostream>
#include <memory>
#include <utility>
#include <vector>

#include "NCAlgebras/FreeAlgebra.hpp"
#include "matrix.hpp"
#include "ring.hpp"

ConstPolyList copyMatrixToVector(const M2FreeAlgebra& F,
                             const Matrix* input)
{
  ConstPolyList result;
  result.reserve(input->n_cols());
  for (int i=0; i<input->n_cols(); i++)
    {
      ring_elem a = input->elem(0,i);
      auto f = reinterpret_cast<const Poly*>(a.get_Poly());
      auto g = new Poly;
      F.freeAlgebra().copy(*g, *f);
      result.push_back(g);
    }
  return result;
}

M2FreeAlgebraQuotient* M2FreeAlgebraQuotient::create(
                                                     const M2FreeAlgebra& F,
                                                     const Matrix* GB,
                                                     int maxdeg // TODO: need to handle use of 'maxdeg' in the class
                                                     )
{
  auto gbElements = copyMatrixToVector(F, GB);
  auto A = std::unique_ptr<FreeAlgebraQuotient> (new FreeAlgebraQuotient(F.freeAlgebra(), gbElements, maxdeg));
  M2FreeAlgebraQuotient* result = new M2FreeAlgebraQuotient(F, std::move(A));
  result->initialize_ring(F.coefficientRing()->characteristic(), F.degreeRing(), nullptr);
  result->zeroV = result->from_long(0);
  result->oneV = result->from_long(1);
  result->minus_oneV = result->from_long(-1);

  return result;
}

M2FreeAlgebraQuotient::M2FreeAlgebraQuotient(const M2FreeAlgebra& F,
                                             std::unique_ptr<FreeAlgebraQuotient> A)
  : mM2FreeAlgebra(F),
    mFreeAlgebraQuotient(std::move(A))
{
}

void M2FreeAlgebraQuotient::text_out(buffer &o) const
{
  o << "Quotient of ";
  m2FreeAlgebra().text_out(o);
}

unsigned int M2FreeAlgebraQuotient::computeHashValue(const ring_elem a) const
{
  return 0; // TODO: change this to a more reasonable hash code.
}

int M2FreeAlgebraQuotient::index_of_var(const ring_elem a) const
{
  return m2FreeAlgebra().index_of_var(a);
}

ring_elem M2FreeAlgebraQuotient::from_coefficient(const ring_elem a) const
{
  auto result = new Poly;
  freeAlgebraQuotient().from_coefficient(*result, a);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebraQuotient::from_long(long n) const
{
  return from_coefficient(coefficientRing()->from_long(n));
}

ring_elem M2FreeAlgebraQuotient::from_int(mpz_srcptr n) const
{
  return from_coefficient(coefficientRing()->from_int(n));
}

bool M2FreeAlgebraQuotient::from_rational(const mpq_srcptr q, ring_elem& result1) const
{
  ring_elem cq; // in coeff ring.
  bool worked = coefficientRing()->from_rational(q, cq);
  if (!worked) return false;
  result1 = from_coefficient(cq);
  return true;
}

ring_elem M2FreeAlgebraQuotient::var(int v) const
{
  auto result = new Poly;
  freeAlgebraQuotient().var(*result,v);
  return ring_elem(reinterpret_cast<void *>(result));
}

bool M2FreeAlgebraQuotient::promote(const Ring *R, const ring_elem f1, ring_elem &result) const
{
  // std::cout << "called promote NC case" << std::endl;  
  // Currently the only case to handle is R = A --> this, and A is the coefficient ring of this.
  if (R == coefficientRing())
    {
      result = from_coefficient(f1);
      return true;
    }
  if (R == &m2FreeAlgebra())
    {
      auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
      auto resultf = new Poly;
      freeAlgebraQuotient().copy(*resultf, *f);
      freeAlgebraQuotient().normalizeInPlace(*resultf);
      result = ring_elem(reinterpret_cast<void *>(resultf));
      return true;
    }
  return false;
}

bool M2FreeAlgebraQuotient::lift(const Ring *R, const ring_elem f1, ring_elem &result) const
{
  // R is the target ring
  // f1 is an element of 'this'.
  // set result to be the "lift" of f in the ring R, return true if this is possible.
  // otherwise return false.

  // case: R is the coefficient ring of 'this'.
  if (R == coefficientRing())
    {
      auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
      if (f->numTerms() != 1) return false;
      auto i = f->cbegin();
      if (monoid().is_one(i.monom()))
        {
          result = coefficientRing()->copy(i.coeff());
          return true;
        }
      return false;
    }
  if (R == &m2FreeAlgebra())
    {
      // just copy the element into result, considered in the free algebra.
      auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
      auto resultf = new Poly;
      freeAlgebra().copy(*resultf, *f);
      result = ring_elem(reinterpret_cast<void *>(resultf));
      return true;
    }
  
  // at this point, we can't lift it.
  return false;
}

bool M2FreeAlgebraQuotient::is_unit(const ring_elem f1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  return freeAlgebraQuotient().is_unit(*f);
}

long M2FreeAlgebraQuotient::n_terms(const ring_elem f1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  return freeAlgebraQuotient().n_terms(*f);
}

bool M2FreeAlgebraQuotient::is_zero(const ring_elem f1) const
{
  return n_terms(f1) == 0;
}

bool M2FreeAlgebraQuotient::is_equal(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  return freeAlgebraQuotient().is_equal(*f,*g);
}

int M2FreeAlgebraQuotient::compare_elems(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  return freeAlgebraQuotient().compare_elems(*f,*g);
}

ring_elem M2FreeAlgebraQuotient::copy(const ring_elem f) const
{
  // FRANK: is this what we want to do?
  return f;
}

void M2FreeAlgebraQuotient::remove(ring_elem &f) const
{
  // do nothing
}

ring_elem M2FreeAlgebraQuotient::negate(const ring_elem f1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  Poly* result = new Poly;
  freeAlgebraQuotient().negate(*result, *f);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebraQuotient::add(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  auto result = new Poly;
  freeAlgebraQuotient().add(*result,*f,*g);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebraQuotient::subtract(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  auto result = new Poly;
  freeAlgebraQuotient().subtract(*result,*f,*g);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebraQuotient::mult(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  auto result = new Poly;
  freeAlgebraQuotient().mult(*result,*f,*g);
  return ring_elem(reinterpret_cast<void *>(result));  
}

ring_elem M2FreeAlgebraQuotient::power(const ring_elem f1, mpz_srcptr n) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto result = new Poly;
  freeAlgebraQuotient().power(*result,*f,n);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebraQuotient::power(const ring_elem f1, int n) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto result = new Poly;
  freeAlgebraQuotient().power(*result,*f,n);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebraQuotient::invert(const ring_elem f) const
{
  return f; // TODO: bad return value.
}

ring_elem M2FreeAlgebraQuotient::divide(const ring_elem f, const ring_elem g) const
{
  return f; // TODO: bad return value.
}

void M2FreeAlgebraQuotient::syzygy(const ring_elem a, const ring_elem b,
                      ring_elem &x, ring_elem &y) const
{
  // TODO: In the commutative case, this function is to find x and y (as simple as possible)
  //       such that ax + by = 0.  No such x and y may exist in the noncommutative case, however.
  //       In this case, the function should return x = y = 0.
}

void M2FreeAlgebraQuotient::debug_display(const Poly* f) const
{
  std::cout << "coeffs: ";
  for (auto i=f->cbeginCoeff(); i != f->cendCoeff(); ++i)
    {
      buffer o;
      coefficientRing()->elem_text_out(o, *i);
      std::cout << o.str() << " ";
    }
  std::cout << std::endl  << "  monoms: ";
  for (auto i=f->cbeginMonom(); i != f->cendMonom(); ++i)
    {
      std::cout << (*i) << " ";
    }
  std::cout << std::endl;
}

void M2FreeAlgebraQuotient::debug_display(const ring_elem ff) const

{
  auto f = reinterpret_cast<const Poly*>(ff.get_Poly());
  debug_display(f);
}

void M2FreeAlgebraQuotient::makeTerm(Poly& result, const ring_elem a, const int* monom) const
{
  m2FreeAlgebra().makeTerm(result, a, monom);
  freeAlgebraQuotient().normalizeInPlace(result);
}

ring_elem M2FreeAlgebraQuotient::makeTerm(const ring_elem a, const int* monom) const
  // 'monom' is in 'varpower' format
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
{
  Poly* f = new Poly;
  makeTerm(*f, a, monom);
  return ring_elem(reinterpret_cast<void*>(f));
}

void M2FreeAlgebraQuotient::elem_text_out(buffer &o,
                             const ring_elem ff,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{
  auto f = reinterpret_cast<const Poly*>(ff.get_Poly());
  freeAlgebraQuotient().elem_text_out(o,*f,p_one,p_plus,p_parens);
}

ring_elem M2FreeAlgebraQuotient::eval(const RingMap *map, const ring_elem ff, int first_var) const
{
  // map: R --> S, this = R.
  // f is an ele ment in R
  // return an element of S.

  auto f = reinterpret_cast<const Poly*>(ff.get_Poly());
  auto g = freeAlgebraQuotient().eval(map, *f, first_var);
  return g;
}

engine_RawArrayPairOrNull M2FreeAlgebraQuotient::list_form(const Ring *coeffR, const ring_elem ff) const
{
  // Either coeffR should be the actual coefficient ring (possible a "toField"ed ring)
  // or a polynomial ring.  If not, NULL is returned and an error given
  // In the latter case, the last set of variables are part of
  // the coefficients.
  return m2FreeAlgebra().list_form(coeffR, ff);
}

ring_elem M2FreeAlgebraQuotient::lead_coefficient(const Ring* coeffRing, const Poly* f) const
{
  return m2FreeAlgebra().lead_coefficient(coeffRing, f);
}

bool M2FreeAlgebraQuotient::is_homogeneous(const ring_elem f1) const
{
  const Poly* f = reinterpret_cast<const Poly*>(f1.get_Poly());
  return is_homogeneous(f);
}

bool M2FreeAlgebraQuotient::is_homogeneous(const Poly* f) const
{
  if (f == nullptr) return true;
  return freeAlgebraQuotient().is_homogeneous(*f);
}

void M2FreeAlgebraQuotient::degree(const ring_elem f, int *d) const
{
  multi_degree(f, d);
}

bool M2FreeAlgebraQuotient::multi_degree(const ring_elem g, int *d) const
{
  const Poly* f = reinterpret_cast<const Poly*>(g.get_Poly());
  return multi_degree(f, d);
}

bool M2FreeAlgebraQuotient::multi_degree(const Poly* f, int *result) const
{
  return freeAlgebraQuotient().multi_degree(*f,result);
}

SumCollector* M2FreeAlgebraQuotient::make_SumCollector() const
{
  return freeAlgebraQuotient().make_SumCollector();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
