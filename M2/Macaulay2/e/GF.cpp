// Copyright 1995 Michael E. Stillman

/*
Note from MES to MES, 27 Jan 2012
We could have a class which mediates between _originalR and this ring.


class GFTranslator
// needs to be able to find a "good" minimal polynomial for (p,n).
// needs to be able to tell if a specific polynomial is "good" (i.e. is
// needs to be able to find a primitive element mod a poly f(x).
{
public:
  GFTranslator(); // What input?

  const PolyRing &ring() const;
  ring_elem minimalPolynomial();
  ring_elem primitiveElement();

  ring_elem fromLog(int exp); // allow negative exponents too?
  int discreteLog(ring_elem a); // return the value r s.t. primitiveRoot^r == a.

  createTables();
private:

};
 */

#include "ZZ.hpp"
#include "GF.hpp"
#include "text-io.hpp"
#include "monoid.hpp"
#include "ringmap.hpp"
#include "poly.hpp"
#include "interrupted.hpp"

#include "aring-m2-gf.hpp"
#include <iostream>

bool GF::initialize_GF(const RingElement *prim)
{
  // set the GF ring tables.  Returns false if there is an error.
  _primitive_element = prim;
  _originalR = prim->get_ring()->cast_to_PolynomialRing();
  initialize_ring(_originalR->characteristic(),
                  PolyRing::get_trivial_poly_ring());

  declare_field();

  int i, j;

  if (_originalR->n_quotients() != 1)
    {
      ERROR(
          "rawGaloisField expected an element of a quotient ring of the form "
          "ZZ/p[x]/(f)");
      return false;
    }
  ring_elem f = _originalR->quotient_element(0);
  Nterm *t = f;
  int n = _originalR->getMonoid()->primary_degree(t->monom);

  Q_ = static_cast<int>(characteristic());
  for (i = 1; i < n; i++) Q_ *= static_cast<int>(characteristic());

  Qexp_ = n;
  Q1_ = Q_ - 1;
  _ZERO = 0;
  _ONE = Q1_;
  _MINUS_ONE = (characteristic() == 2 ? _ONE : Q1_ / 2);

  // Get ready to create the 'one_table'
  VECTOR(ring_elem) polys;
  polys.push_back(_originalR->from_long(0));
  ring_elem primelem = prim->get_value();
  polys.push_back(_originalR->copy(primelem));

  ring_elem oneR = _originalR->one();

  _x_exponent = -1;
  ring_elem x = _originalR->var(0);
  if (_originalR->is_equal(primelem, x)) _x_exponent = 1;
  for (i = 2; i < Q_; i++)
    {
      ring_elem g = _originalR->mult(polys[i - 1], primelem);
      polys.push_back(g);
      if (_originalR->is_equal(g, oneR)) break;
      if (_originalR->is_equal(g, x)) _x_exponent = i;
    }

  if (polys.size() != Q_)
    {
      ERROR("GF: primitive element expected");
      return false;
    }

  assert(_x_exponent >= 0);

  // Set 'one_table'.
  _one_table = newarray_atomic(int, Q_);
  _one_table[0] = Q_ - 1;
  for (i = 1; i <= Q_ - 1; i++)
    {
      if (system_interrupted()) return false;
      ring_elem f1 = _originalR->add(polys[i], oneR);
      for (j = 1; j <= Q_ - 1; j++)
        if (_originalR->is_equal(f1, polys[j])) break;
      _one_table[i] = j;
    }

  // Create the Z/P ---> GF(Q) inclusion map
  _from_int_table = newarray_atomic(int, characteristic());
  int a = _ONE;
  _from_int_table[0] = _ZERO;
  for (i = 1; i < characteristic(); i++)
    {
      _from_int_table[i] = a;
      a = _one_table[a];
    }

  zeroV = from_long(0);
  oneV = from_long(1);
  minus_oneV = from_long(-1);

  // M2::GaloisFieldTable G(*_originalR, primelem);
  //  G.display(std::cout);

  return true;
}

GF::GF() {}
GF::~GF() {}
GF *GF::create(const RingElement *prim)
{
  GF *result = new GF;
  if (!result->initialize_GF(prim)) return 0;

  return result;
}

void GF::text_out(buffer &o) const { o << "GF(" << Q_ << ")"; }
const RingElement *GF::getMinimalPolynomial() const
{
  const Ring *R = _originalR->getAmbientRing();
  ring_elem f = R->copy(_originalR->quotient_element(0));
  return RingElement::make_raw(R, f);
}

const RingElement *GF::getGenerator() const
{
  ring_elem a = ring_elem(1); // this is the primitive generator
  return RingElement::make_raw(this, a);
}

const RingElement *GF::getRepresentation(const ring_elem &a) const
{
  return RingElement::make_raw(
      _originalR,
      _originalR->power(_primitive_element->get_value(), a.get_int()));
}

ring_elem GF::get_rep(ring_elem a) const
// takes an element of this ring, and returns an element of _originalR->XXX()
{
  return _originalR->power(_primitive_element->get_value(), a.get_int());
}

int GF::discrete_log(ring_elem a) const
{
  if (a.get_int() == _ZERO) return -1;
  if (a.get_int() == Q1_) return 0;
  return a.get_int();
}

static inline int modulus_add(int a, int b, int p)
{
  int t = a + b;
  return (t <= p ? t : t - p);
}

static inline int modulus_sub(int a, int b, int p)
{
  int t = a - b;
  return (t <= 0 ? t + p : t);
}

ring_elem GF::random() const
{
  int exp = rawRandomInt((int32_t)Q_);
  return ring_elem(exp);
}

void GF::elem_text_out(buffer &o,
                       const ring_elem a,
                       bool p_one,
                       bool p_plus,
                       bool p_parens) const
{
  if (a.get_int() == _ZERO)
    {
      o << "0";
      return;
    }
  ring_elem h = _originalR->power(_primitive_element->get_value(), a.get_int());
  _originalR->elem_text_out(o, h, p_one, p_plus, p_parens);
  _originalR->remove(h);
}

ring_elem GF::eval(const RingMap *map, const ring_elem f, int first_var) const
{
  return map->get_ring()->power(map->elem(first_var), f.get_int());
}

ring_elem GF::from_long(long n) const
{
  long m1 = n % characteristic();
  if (m1 < 0) m1 += characteristic();
  int m = static_cast<int>(m1);
  m = _from_int_table[m];
  return ring_elem(m);
}

ring_elem GF::from_int(mpz_srcptr n) const
{
  mpz_t result;
  mpz_init(result);
  mpz_mod_ui(result, n, characteristic());
  long m1 = mpz_get_si(result);
  mpz_clear(result);
  if (m1 < 0) m1 += characteristic();
  int m = static_cast<int>(m1);
  m = _from_int_table[m];
  return ring_elem(m);
}

bool GF::from_rational(mpq_srcptr q, ring_elem &result) const
{
  // a will be an element of ZZ/p
  ring_elem a;
  bool ok1 = _originalR->getCoefficients()->from_rational(q, a);
  if (not ok1) return false;
  std::pair<bool, long> b =
      _originalR->getCoefficients()->coerceToLongInteger(a);
  if (b.first)
    {
      result = GF::from_long(b.second);
      return true;
    }
  return false;
}

ring_elem GF::var(int v) const
{
  if (v == 0) return _x_exponent;
  return _ZERO;
}
bool GF::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // Rf = Z/p[x]/F(x) ---> GF(p,n)
  // promotion: need to be able to know the value of 'x'.
  // lift: need to compute (primite_element)^e

  if (Rf != _originalR) return false;

  result = from_long(0);
  int exp[1];
  for (Nterm *t = f; t != NULL; t = t->next)
    {
      std::pair<bool, long> b =
          _originalR->getCoefficients()->coerceToLongInteger(t->coeff);
      assert(b.first);
      ring_elem coef = from_long(b.second);
      _originalR->getMonoid()->to_expvector(t->monom, exp);
      // exp[0] is the variable we want.  Notice that since the ring is a
      // quotient,
      // this degree is < n (where Q_ = P^n).
      ring_elem g = power(_x_exponent, exp[0]);
      g = mult(g, coef);
      result = ring_elem(internal_add(result.get_int(), g.get_int()));
    }
  return true;
}

bool GF::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // Rg = Z/p[x]/F(x) ---> GF(p,n)
  // promotion: need to be able to know the value of 'x'.
  // lift: need to compute (primite_element)^e

  if (Rg != _originalR) return false;

  int e = f.get_int();
  if (e == _ZERO)
    result = _originalR->from_long(0);
  else if (e == _ONE)
    result = _originalR->from_long(1);
  else
    result = _originalR->power(_primitive_element->get_value(), e);

  return true;
}

bool GF::is_unit(const ring_elem f) const { return (f.get_int() != _ZERO); }
bool GF::is_zero(const ring_elem f) const { return (f.get_int() == _ZERO); }
bool GF::is_equal(const ring_elem f, const ring_elem g) const
{
  return f.get_int() == g.get_int();
}

int GF::compare_elems(const ring_elem f, const ring_elem g) const
{
  int cmp = f.get_int() - g.get_int();
  if (cmp < 0) return -1;
  if (cmp == 0) return 0;
  return 1;
}

ring_elem GF::copy(const ring_elem f) const { return f; }

void GF::remove(ring_elem &) const
{
  // nothing needed to remove.
}

int GF::internal_negate(int f) const
{
  if (f == _ZERO) return _ZERO;
  return f = modulus_add(f, _MINUS_ONE, Q1_);
}

int GF::internal_add(int a, int b) const
{
  if (b == _ZERO) return a;
  if (a == _ZERO) return b;

  int n = a - b;
  if (n > 0)
    {
      if (n == _MINUS_ONE) return _ZERO;
      return modulus_add(b, _one_table[n], Q1_);
    }
  else if (n < 0)
    {
      if (-n == _MINUS_ONE) return _ZERO;
      return modulus_add(a, _one_table[-n], Q1_);
    }
  else
    {
      if (characteristic() == 2) return _ZERO;
      return modulus_add(a, _one_table[_ONE], Q1_);
    }
}

int GF::internal_subtract(int f, int g) const
{
  if (g == _ZERO) return f;
  if (f == g) return _ZERO;
  int g1 = modulus_add(g, _MINUS_ONE, Q1_);  // f = -g
  return internal_add(f, g1);
}

ring_elem GF::negate(const ring_elem f) const
{
  return ring_elem(internal_negate(f.get_int()));
}

ring_elem GF::add(const ring_elem f, const ring_elem g) const
{
  return ring_elem(internal_add(f.get_int(), g.get_int()));
}

ring_elem GF::subtract(const ring_elem f, const ring_elem g) const
{
  return ring_elem(internal_subtract(f.get_int(), g.get_int()));
}

ring_elem GF::mult(const ring_elem f, const ring_elem g) const
{
  if (f.get_int() == _ZERO || g.get_int() == _ZERO) return ring_elem(_ZERO);
  return ring_elem(modulus_add(f.get_int(), g.get_int(), Q1_));
}

ring_elem GF::power(const ring_elem f, int n) const
{
  if (f.get_int() == _ZERO) return ring_elem(_ZERO);
  int m = (f.get_int() * n) % Q1_;
  if (m <= 0) m += Q1_;
  return ring_elem(m);
}
ring_elem GF::power(const ring_elem f, mpz_srcptr n) const
{
  if (f.get_int() == _ZERO) return ring_elem(_ZERO);
  int exp = RingZZ::mod_ui(n, Q1_);
  int m = (f.get_int() * exp) % Q1_;
  if (m <= 0) m += Q1_;
  return ring_elem(m);
}

ring_elem GF::invert(const ring_elem f) const
{
  // error if f == _ZERO
  if (f.get_int() == _ONE) return ring_elem(_ONE);
  return ring_elem(Q1_ - f.get_int());
}

ring_elem GF::divide(const ring_elem f, const ring_elem g) const
{
  if (g.get_int() == _ZERO) assert(0);  // MES: raise an exception
  if (f.get_int() == _ZERO) return ring_elem(_ZERO);
  return modulus_sub(f.get_int(), g.get_int(), Q1_);
}

void GF::syzygy(const ring_elem a,
                const ring_elem b,
                ring_elem &x,
                ring_elem &y) const
{
  x = GF::from_long(1);
  y = GF::divide(a, b);
  y = ring_elem(internal_negate(y.get_int()));
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
