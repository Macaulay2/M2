#include "M2FreeAlgebra.hpp"

#include <assert.h>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

#include "exceptions.hpp"
#include "monomial.hpp"
#include "relem.hpp"

PolyList copyPolyVector(const M2FreeAlgebraOrQuotient* A,
                        const PolyList& polys)
{
  auto& A0 = A->freeAlgebra();
  PolyList result;
  auto end = polys.cend();
  for (auto i = polys.cbegin(); i != end; ++i)
    {
      Poly* f = new Poly;
      A0.copy(*f, *(*i));
      result.push_back(f);
    }
  return result;
}

void M2FreeAlgebraOrQuotient::appendFromModuleMonom(Poly& f, const ModuleMonom& m) const
{
  int comp_unused;
  f.getCoeffInserter().push_back(coefficientRing()->from_long(1));
  appendModuleMonomToMonom(m, comp_unused, f.getMonomInserter());
}
  
ring_elem M2FreeAlgebraOrQuotient::fromModuleMonom(const ModuleMonom& m) const
{
  auto result = new Poly;
  appendFromModuleMonom(*result, m);
  return fromPoly(result);
}



M2FreeAlgebra* M2FreeAlgebra::create(const Ring* K,
                                     const std::vector<std::string>& names,
                                     const PolynomialRing* degreeRing,
                                     const std::vector<int>& degrees,
                                     const std::vector<int>& wtvecs,
                                     const std::vector<int>& heftVector
                                     )
{
  assert(K != nullptr);
  auto F = std::unique_ptr<FreeAlgebra>(FreeAlgebra::create(K, names, degreeRing, degrees, wtvecs, heftVector));
  M2FreeAlgebra* result = new M2FreeAlgebra(std::move(F));
  result->initialize_ring(K->characteristic(), degreeRing, nullptr);
  result->zeroV = result->from_long(0);
  result->oneV = result->from_long(1);
  result->minus_oneV = result->from_long(-1);

  return result;
}

M2FreeAlgebra::M2FreeAlgebra(std::unique_ptr<FreeAlgebra> F)
  : mFreeAlgebra(std::move(F))
{
}

void M2FreeAlgebra::text_out(buffer &o) const
{
  coefficientRing()->text_out(o);
  o << "{";
  for (int i = 0; i < monoid().variableNames().size(); i++)
    {
      if (i > 0) o << ",";
      o << monoid().variableNames()[i];
    }
  o << "}";
}

unsigned int M2FreeAlgebra::computeHashValue(const ring_elem a) const
{
  return 0; // TODO: change this to a more reasonable hash code.
}

int M2FreeAlgebra::index_of_var(const ring_elem a) const
{
  // This function is needed at top level to be able to determine if a ring element is a variable
  //const Poly* f = reinterpret_cast<Poly*>(a.poly_val);
  auto f = reinterpret_cast<const Poly*>(a.get_Poly());

  // f is a variable iff: #terms is 1, monomial is [3,1,v], coeff is 1
  if (f->numTerms() != 1) return -1;
  auto i = f->cbegin();
  if (!coefficientRing()->is_equal(coefficientRing()->one(), i.coeff())) return -1;
  return monoid().index_of_variable(i.monom());
}

ring_elem M2FreeAlgebra::from_coefficient(const ring_elem a) const
{
  auto result = new Poly;
  freeAlgebra().from_coefficient(*result, a);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebra::from_long(long n) const
{
  return from_coefficient(coefficientRing()->from_long(n));
}

ring_elem M2FreeAlgebra::from_int(mpz_srcptr n) const
{
  return from_coefficient(coefficientRing()->from_int(n));
}

bool M2FreeAlgebra::from_rational(const mpq_srcptr q, ring_elem& result1) const
{
  ring_elem cq; // in coeff ring.
  bool worked = coefficientRing()->from_rational(q, cq);
  if (!worked) return false;
  result1 = from_coefficient(cq);
  return true;
}

ring_elem M2FreeAlgebra::var(int v) const
{
  auto result = new Poly;
  freeAlgebra().var(*result,v);
  return ring_elem(reinterpret_cast<void *>(result));
}

bool M2FreeAlgebra::promote(const Ring *R, const ring_elem f, ring_elem &result) const
{
  // std::cout << "called promote NC case" << std::endl;  
  // Currently the only case to handle is R = A --> this, and A is the coefficient ring of this.
  if (R == coefficientRing())
    {
      result = from_coefficient(f);
      return true;
    }
  return false;
}

bool M2FreeAlgebra::lift(const Ring *R, const ring_elem f1, ring_elem &result) const
{
  // R is the target ring
  // f1 is an element of 'this'.
  // set result to be the "lift" of f in the ring R, return true if this is possible.
  // otherwise return false.

  // case: R is the coefficient ring of 'this'.
  std::cout << "called lift NC case" << std::endl;
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
    }
  
  // at this point, we can't lift it.
  return false;
}

bool M2FreeAlgebra::is_unit(const ring_elem f1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  return freeAlgebra().is_unit(*f);
}

long M2FreeAlgebra::n_terms(const ring_elem f1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  return freeAlgebra().n_terms(*f);
}

bool M2FreeAlgebra::is_zero(const ring_elem f1) const
{
  return n_terms(f1) == 0;
}

bool M2FreeAlgebra::is_equal(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  return freeAlgebra().is_equal(*f,*g);
}

int M2FreeAlgebra::compare_elems(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  return freeAlgebra().compare_elems(*f,*g);
}

ring_elem M2FreeAlgebra::copy(const ring_elem f) const
{
  // FRANK: is this what we want to do?
  return f;
}

void M2FreeAlgebra::remove(ring_elem &f) const
{
  // do nothing
}

ring_elem M2FreeAlgebra::negate(const ring_elem f1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  Poly* result = new Poly;
  freeAlgebra().negate(*result, *f);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebra::add(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  auto result = new Poly;
  freeAlgebra().add(*result,*f,*g);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebra::subtract(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  auto result = new Poly;
  freeAlgebra().subtract(*result,*f,*g);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebra::mult(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  auto result = new Poly;
  freeAlgebra().mult(*result,*f,*g);
  return ring_elem(reinterpret_cast<void *>(result));  
}

ring_elem M2FreeAlgebra::power(const ring_elem f1, mpz_srcptr n) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto result = new Poly;
  freeAlgebra().power(*result,*f,n);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebra::power(const ring_elem f1, int n) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto result = new Poly;
  freeAlgebra().power(*result,*f,n);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebra::invert(const ring_elem f) const
{
  if (not is_unit(f))
    throw exc::engine_error("attempting to divide by a non-unit");

  ring_elem cf = lead_coefficient(coefficientRing(), f);
  // At this point, f should be an element of the coefficient ring, times the monomial 1.
  // The following will throw an error if it cannot invert the element.
  ring_elem finv = coefficientRing()->invert(cf);
  return from_coefficient(finv);
}

ring_elem M2FreeAlgebra::divide(const ring_elem f, const ring_elem g) const
{
  ring_elem ginv = invert(g); // this will throw an error unless g is invertible in the coeff ring
  return mult(ginv, f);
}

void M2FreeAlgebra::syzygy(const ring_elem a, const ring_elem b,
                      ring_elem &x, ring_elem &y) const
{
  throw exc::internal_error("M2FreeAlgebra::syzygy is not yet written!");

  // TODO: In the commutative case, this function is to find x and y (as simple as possible)
  //       such that ax + by = 0.  No such x and y may exist in the noncommutative case, however.
  //       In this case, the function should return x = y = 0.
}

void M2FreeAlgebra::debug_display(const Poly* f) const
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

void M2FreeAlgebra::debug_display(const ring_elem ff) const

{
  auto f = reinterpret_cast<const Poly*>(ff.get_Poly());
  debug_display(f);
}

void M2FreeAlgebra::makeTerm(Poly& result, const ring_elem a, const int* monom) const
  // 'monom' is in 'varpower' format
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
{
  result.getCoeffInserter().push_back(a);
  monoid().fromMonomial(monom, result.getMonomInserter());
}

ring_elem M2FreeAlgebra::makeTerm(const ring_elem a, const int* monom) const
  // 'monom' is in 'varpower' format
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
{
  auto result = new Poly;
  makeTerm(*result, a, monom);
  return reinterpret_cast<Nterm*>(result);
}

void M2FreeAlgebra::elem_text_out(buffer &o,
                             const ring_elem ff,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{
  auto f = reinterpret_cast<const Poly*>(ff.get_Poly());
  freeAlgebra().elem_text_out(o,*f,p_one,p_plus,p_parens);
}

ring_elem M2FreeAlgebra::eval(const RingMap *map, const ring_elem ff, int first_var) const
{
  // map: R --> S, this = R.
  // f is an ele ment in R
  // return an element of S.
  auto f = reinterpret_cast<const Poly*>(ff.get_Poly());
  return freeAlgebra().eval(map, *f, first_var);
}

engine_RawArrayPairOrNull M2FreeAlgebra::list_form(const Ring *coeffR, const ring_elem ff) const
{
  // Either coeffR should be the actual coefficient ring (possible a "toField"ed ring)
  // or a polynomial ring.  If not, NULL is returned and an error given
  // In the latter case, the last set of variables are part of
  // the coefficients.

  auto f = reinterpret_cast<const Poly*>(ff.get_Poly());
  if (coeffR != coefficientRing())
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
      ring_elem c = coefficientRing()->copy(i.coeff());
      vp.resize(0);
      monoid().getMonomialReversed(i.monom(), vp); // should this instead reverse the monomial?
      coeffs->array[next] = RingElement::make_raw(coeffR, c);
      monoms->array[next] = Monomial::make(vp); // reverses the monomial
    }
  
  return result;
}

ring_elem M2FreeAlgebra::lead_coefficient(const Ring* coeffRing, const Poly* f) const
{
  if (coeffRing != coefficientRing())
    {
      throw exc::engine_error("unexpected coefficient ring");
    }
  if (f->numTerms() == 0) return coeffRing->zero();
  return *(f->cbeginCoeff());
}

Poly* M2FreeAlgebra::get_terms(const Poly* f, int lo, int hi) const
{
  auto result = new Poly;
  auto& outcoeff = result->getCoeffInserter();
  auto& outmonom = result->getMonomInserter();
  int which = 0;
  for(auto i=f->cbegin(); i != f->cend() and which <= hi; ++i, ++which)
    {
      if (which < lo) continue;
      outcoeff.push_back(i.coeff());
      monoid().copy(i.monom(), outmonom);
    }
  return result;
}

M2_arrayint M2FreeAlgebra::support(const ring_elem a) const
{
  const Poly* f = reinterpret_cast<const Poly*>(a.get_Poly());
  return freeAlgebra().support(*f);
}

bool M2FreeAlgebra::is_homogeneous(const ring_elem f1) const
{
  const Poly* f = reinterpret_cast<const Poly*>(f1.get_Poly());
  return is_homogeneous(f);
}

bool M2FreeAlgebra::is_homogeneous(const Poly* f) const
{
  if (f == nullptr) return true;
  return freeAlgebra().is_homogeneous(*f);
}

void M2FreeAlgebra::degree(const ring_elem f, int *d) const
{
  multi_degree(f, d);
}

bool M2FreeAlgebra::multi_degree(const ring_elem g, int *d) const
{
  const Poly* f = reinterpret_cast<const Poly*>(g.get_Poly());
  return multi_degree(f, d);
}

bool M2FreeAlgebra::multi_degree(const Poly* f, int *result) const
{
  return freeAlgebra().multi_degree(*f,result);
}

SumCollector* M2FreeAlgebra::make_SumCollector() const
{
  return freeAlgebra().make_SumCollector();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
