#include "M2FreeAlgebraQuotient.hpp"
#include "monomial.hpp"
#include "relem.hpp"
#include "ringmap.hpp"

#include <vector>
#include <string>
#include <iostream>

M2FreeAlgebraQuotient* M2FreeAlgebraQuotient::create(const Ring* K,
                                             const std::vector<std::string>& names,
                                             const PolynomialRing* degreeRing,
                                             const std::vector<int>& degrees
                                             )
{
  assert(K != nullptr);
  FreeAlgebraQuotient* F = FreeAlgebraQuotient::create(K, names, degreeRing, degrees);
  M2FreeAlgebraQuotient* result = new M2FreeAlgebraQuotient(F);
  result->initialize_ring(K->characteristic(), degreeRing, nullptr);
  result->zeroV = result->from_long(0);
  result->oneV = result->from_long(1);
  result->minus_oneV = result->from_long(-1);

  return result;
}

M2FreeAlgebraQuotient::M2FreeAlgebraQuotient(const FreeAlgebraQuotient* F)
  : mFreeAlgebraQuotient(F)
{
}

void M2FreeAlgebraQuotient::text_out(buffer &o) const
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

unsigned int M2FreeAlgebraQuotient::computeHashValue(const ring_elem a) const
{
  return 0; // TODO: change this to a more reasonable hash code.
}

int M2FreeAlgebraQuotient::index_of_var(const ring_elem a) const
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

ring_elem M2FreeAlgebraQuotient::from_coefficient(const ring_elem a) const
{
  auto result = new Poly;
  freeAlgebra()->from_coefficient(*result, a);
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

bool M2FreeAlgebraQuotient::from_rational(const mpq_ptr q, ring_elem& result1) const
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
  freeAlgebra()->var(*result,v);
  return ring_elem(reinterpret_cast<void *>(result));
}

bool M2FreeAlgebraQuotient::promote(const Ring *R, const ring_elem f, ring_elem &result) const
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

bool M2FreeAlgebraQuotient::lift(const Ring *R, const ring_elem f1, ring_elem &result) const
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

bool M2FreeAlgebraQuotient::is_unit(const ring_elem f1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  return freeAlgebra()->is_unit(*f);
}

long M2FreeAlgebraQuotient::n_terms(const ring_elem f1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  return freeAlgebra()->n_terms(*f);
}

bool M2FreeAlgebraQuotient::is_zero(const ring_elem f1) const
{
  return n_terms(f1) == 0;
}

bool M2FreeAlgebraQuotient::is_equal(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  return freeAlgebra()->is_equal(*f,*g);
}

int M2FreeAlgebraQuotient::compare_elems(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  return freeAlgebra()->compare_elems(*f,*g);
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
  freeAlgebra()->negate(*result, *f);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebraQuotient::add(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  auto result = new Poly;
  freeAlgebra()->add(*result,*f,*g);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebraQuotient::subtract(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  auto result = new Poly;
  freeAlgebra()->subtract(*result,*f,*g);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebraQuotient::mult(const ring_elem f1, const ring_elem g1) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto g = reinterpret_cast<const Poly*>(g1.get_Poly());
  auto result = new Poly;
  freeAlgebra()->mult(*result,*f,*g);
  return ring_elem(reinterpret_cast<void *>(result));  
}

ring_elem M2FreeAlgebraQuotient::power(const ring_elem f1, mpz_t n) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto result = new Poly;
  freeAlgebra()->power(*result,*f,n);
  return ring_elem(reinterpret_cast<void *>(result));
}

ring_elem M2FreeAlgebraQuotient::power(const ring_elem f1, int n) const
{
  auto f = reinterpret_cast<const Poly*>(f1.get_Poly());
  auto result = new Poly;
  freeAlgebra()->power(*result,*f,n);
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

ring_elem M2FreeAlgebraQuotient::makeTerm(const ring_elem a, const int* monom) const
  // 'monom' is in 'varpower' format
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
{
  auto result = new Poly;

  result->getCoeffInserter().push_back(a);
  monoid().fromMonomial(monom, result->getMonomInserter());
  return reinterpret_cast<Nterm*>(result);
  
}

void M2FreeAlgebraQuotient::elem_text_out(buffer &o,
                             const ring_elem ff,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{
  auto f = reinterpret_cast<const Poly*>(ff.get_Poly());
  freeAlgebra()->elem_text_out(o,*f,p_one,p_plus,p_parens);
}

ring_elem M2FreeAlgebraQuotient::eval(const RingMap *map, const ring_elem ff, int first_var) const
{
  // map: R --> S, this = R.
  // f is an ele ment in R
  // return an element of S.

  auto f = reinterpret_cast<const Poly*>(ff.get_Poly());

  // plan: do it as in polyring:
  //  cast f to a Poly
  //  loop throug the terms of f
  //    for each term: call map->eval_term, need varpower monomial here.
  //    add to a heap object
  // return value of the heap object

  const Ring* target = map->get_ring();
  SumCollector *H = target->make_SumCollector();

  std::vector<int> vp;
  for (auto i = f->cbegin(); i != f->cend(); ++i)
    {
      vp.clear();
      monoid().getMonomial(i.monom(), vp);
      ring_elem g = map->eval_term(coefficientRing(), i.coeff(), vp.data(), first_var, n_vars());
      H->add(g);
    }
  ring_elem result = H->getValue();
  delete H;
  return result;
}

engine_RawArrayPairOrNull M2FreeAlgebraQuotient::list_form(const Ring *coeffR, const ring_elem ff) const
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

ring_elem M2FreeAlgebraQuotient::lead_coefficient(const Ring* coeffRing, const Poly* f) const
{
  if (coeffRing != coefficientRing())
    {
      throw exc::engine_error("unexpected coefficient ring");
    }
  if (f->numTerms() == 0) return coeffRing->zero();
  return *(f->cbeginCoeff());
}

Poly* M2FreeAlgebraQuotient::get_terms(const Poly* f, int lo, int hi) const
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

bool M2FreeAlgebraQuotient::is_homogeneous(const ring_elem f1) const
{
  const Poly* f = reinterpret_cast<const Poly*>(f1.get_Poly());
  return is_homogeneous(f);
}

bool M2FreeAlgebraQuotient::is_homogeneous(const Poly* f) const
{
  if (f == nullptr) return true;
  return freeAlgebra()->is_homogeneous(*f);
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
  return freeAlgebra()->multi_degree(*f,result);
}

void M2FreeAlgebraQuotient::appendFromModuleMonom(Poly& f, const ModuleMonom& m) const
{
  int comp_unused;
  f.getCoeffInserter().push_back(coefficientRing()->from_long(1));
  appendModuleMonomToMonom(m, comp_unused, f.getMonomInserter());
}
  
ring_elem M2FreeAlgebraQuotient::fromModuleMonom(const ModuleMonom& m) const
{
  auto result = new Poly;
  appendFromModuleMonom(*result, m);
  return fromPoly(result);
}

SumCollector* M2FreeAlgebraQuotient::make_SumCollector() const
{
  return freeAlgebra()->make_SumCollector();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
