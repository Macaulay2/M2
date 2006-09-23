// Copyright 1995 Michael E. Stillman

#include "ZZ.hpp"
#include "GF.hpp"
#include "text_io.hpp"
#include "monoid.hpp"
#include "ringmap.hpp"
#include "polyring.hpp"
#include "random.hpp"
#include "gbring.hpp"

bool GF::initialize_GF(const RingElement *prim)
{
  // set the GF ring tables.  Returns false if there is an error.
  _primitive_element = prim;
  _originalR = prim->get_ring()->cast_to_PolynomialRing();
  initialize_ring(_originalR->charac(),
		  PolyRing::get_trivial_poly_ring());


  declare_field();

  int i,j;

  if (_originalR->n_quotients() != 1)
    {
      ERROR("rawGaloisField expected an element of a quotient ring of the form ZZ/p[x]/(f)");
      return false;
    }
  ring_elem f = _originalR->quotient_element(0);
  Nterm *t = f;
  int n = _originalR->getMonoid()->primary_degree(t->monom);

  Q_ = P;
  for (i=1; i<n; i++) Q_ *= P;

  Qexp_ = n;
  Q1_ = Q_-1;
  _ZERO = 0;
  _ONE = Q1_;
  _MINUS_ONE = (P == 2 ? _ONE : Q1_/2);

  // Get ready to create the 'one_table'
  array<ring_elem> polys;
  polys.append(_originalR->from_int(0));
  ring_elem primelem = prim->get_value();
  polys.append(_originalR->copy(primelem));

  ring_elem oneR = _originalR->one();

  _x_exponent = -1;
  ring_elem x = _originalR->var(0);
  if (_originalR->is_equal(primelem, x))
    _x_exponent = 1;
  for (i=2; i<Q_; i++)
    {
      ring_elem g = _originalR->mult(polys[i-1], primelem);
      polys.append(g);
      if (_originalR->is_equal(g, oneR)) break;
      if (_originalR->is_equal(g, x))
	_x_exponent = i;
    }

  if (polys.length() != Q_)
    {
      ERROR("GF: primitive element expected");
      return false;
    }

  assert(_x_exponent >= 0);

  // Set 'one_table'.
  _one_table = newarray(int,Q_);
  _one_table[0] = Q_-1;
  for (i=1; i<=Q_-1; i++)
    {
      ring_elem f1 = _originalR->add(polys[i], oneR);
      for (j=1; j<=Q_-1; j++)
	if (_originalR->is_equal(f1, polys[j]))
	  break;
      _one_table[i] = j;
    }

  // Create the Z/P ---> GF(Q) inclusion map
  _from_int_table = newarray(int,P);
  int a = _ONE;
  _from_int_table[0] = _ZERO;
  for (i=1; i<P; i++)
    {
      _from_int_table[i] = a;
      a = _one_table[a];
    }

  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  return true;
}

GF::GF() {}

#if 0
GF::GF(const RingElement *prim)
: Ring(prim->get_ring()->charac(),
	1,1,this /* Visual C WARNING */,Monoid::get_trivial_monoid(), 
	prim->get_ring()->degree_monoid()),
  K(prim->get_ring()->cast_to_PolynomialRing()),
  primitive_element(prim)
{
}
#endif

GF::~GF()
{
}

GF *GF::create(const RingElement *prim)
{
  GF *result = new GF;
  if (!result->initialize_GF(prim)) return 0;

  return result;
}

void GF::text_out(buffer &o) const
{
  o << "GF(" << Q_ << ")";
}

inline int GF::to_int(int) const
{
  // MES.  what to do here?
  return 1;
}

static inline int modulus_add(int a, int b, int p)
{
  int t = a+b;
  return (t <= p ? t : t-p);
}

static inline int modulus_sub(int a, int b, int p)
{
  int t = a-b;
  return (t <= 0 ? t+p : t);
}

ring_elem GF::random() const
{
  int exp = Random::random0((int32_t)Q_);
  return ring_elem(exp);
}

void GF::elem_text_out(buffer &o, const ring_elem a) const
{
  if (a == _ZERO) 
    {
      o << "0";
      return;
    }
  ring_elem h = _originalR->power(_primitive_element->get_value(), a.int_val);
  _originalR->elem_text_out(o, h);
  _originalR->remove(h);
}

ring_elem GF::eval(const RingMap *map, const ring_elem f, int first_var) const
{
  return map->get_ring()->power(map->elem(first_var), f.int_val);
}

ring_elem GF::from_int(int n) const
{
  int m = n % P;
  if (m < 0) m += P;
  m = _from_int_table[m];
  return ring_elem(m);
}

ring_elem GF::from_int(mpz_ptr n) const
{
  mpz_t result;
  mpz_init(result);
  mpz_mod_ui(result, n, P);
  int m = mpz_get_si(result);
  if (m < 0) m += P;
  m = _from_int_table[m];
  return ring_elem(m);
}

ring_elem GF::from_rational(mpq_ptr q) const
{
  // a should be an element of ZZ/p
  ring_elem a = _originalR->getCoefficients()->from_rational(q);
  int b = _originalR->getCoefficients()->coerce_to_int(a);
  return GF::from_int(b);
}

ring_elem GF::var(int v) const
{
  if (v == 0)
    return _x_exponent;
  return _ZERO;
}
bool GF::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // Rf = Z/p[x]/F(x) ---> GF(p,n)
  // promotion: need to be able to know the value of 'x'.
  // lift: need to compute (primite_element)^e

  if (Rf != _originalR) return false;

  result = from_int(0);
  int exp[1];
  for (Nterm *t = f; t != NULL; t = t->next)
    {
      ring_elem coef = from_int(_originalR->Ncoeffs()->coerce_to_int(t->coeff));
      _originalR->Nmonoms()->to_expvector(t->monom, exp);
      // exp[0] is the variable we want.  Notice that since the ring is a quotient,
      // this degree is < n (where Q_ = P^n).
      ring_elem g = power(_x_exponent, exp[0]);
      g = mult(g, coef);
      internal_add_to(result, g);
    }
  return true;
}

bool GF::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // Rg = Z/p[x]/F(x) ---> GF(p,n)
  // promotion: need to be able to know the value of 'x'.
  // lift: need to compute (primite_element)^e

  if (Rg != _originalR) return false;

  int e = f.int_val;
  if (e == _ZERO)
    result = _originalR->from_int(0);
  else if (e == _ONE)
    result = _originalR->from_int(1);
  else
    result = _originalR->power(_primitive_element->get_value(), e);
  
  return true;
}

bool GF::is_unit(const ring_elem f) const
{
  return (f != _ZERO);
}

bool GF::is_zero(const ring_elem f) const
{
  return (f == _ZERO);
}

bool GF::is_equal(const ring_elem f, const ring_elem g) const
{
  return f.int_val == g.int_val;
}

int GF::compare_elems(const ring_elem f, const ring_elem g) const
{
  int cmp = f.int_val - g.int_val;
  if (cmp < 0) return -1;
  if (cmp == 0) return 0;
  return 1;
}

ring_elem GF::copy(const ring_elem f) const
{
  return f;
}

void GF::remove(ring_elem &) const
{
  // nothing needed to remove.
}

void GF::internal_negate_to(ring_elem &f) const
{
  if (f != _ZERO)
    f = modulus_add(f, _MINUS_ONE, Q1_);
}

void GF::internal_add_to(ring_elem &f, ring_elem &g) const
{
  if (g == _ZERO) return;
  if (f == _ZERO) 
    f = g;
  else
    {
      int a = f.int_val;
      int b = g.int_val;
      int n = a-b;
      if (n > 0)
	{
	  if (n == _MINUS_ONE)
	    f = _ZERO;
	  else
	    f = modulus_add(b, _one_table[n], Q1_);
	}
      else if (n < 0)
	{
	  if (-n == _MINUS_ONE)
	    f = _ZERO;
	  else
	    f = modulus_add(a, _one_table[-n], Q1_);
	}
      else 
	{
	  if (P == 2) 
	    f = _ZERO;
	  else
	    f = modulus_add(a, _one_table[_ONE], Q1_);
	}
    }
}

void GF::internal_subtract_to(ring_elem &f, ring_elem &g) const
{
  if (g == _ZERO) return;
  if (f.int_val == g.int_val) { f = _ZERO; return; }
  ring_elem g1 = modulus_add(g, _MINUS_ONE, Q1_);  // f = -g
  internal_add_to(f, g1);
}

ring_elem GF::negate(const ring_elem f) const
{
  ring_elem result = f;
  internal_negate_to(result);
  return result;
}

ring_elem GF::add(const ring_elem f, const ring_elem g) const
{
  ring_elem result = f;
  ring_elem g1 = g;
  internal_add_to(result, g1);
  return result;
}

ring_elem GF::subtract(const ring_elem f, const ring_elem g) const
{
  ring_elem result = f;
  ring_elem g1 = g;
  internal_subtract_to(result, g1);
  return result;
}

ring_elem GF::mult(const ring_elem f, const ring_elem g) const
{
  if (f == _ZERO || g == _ZERO) return _ZERO;
  return modulus_add(f, g, Q1_);
}

ring_elem GF::power(const ring_elem f, int n) const
{
  if (f == _ZERO) return _ZERO;
  int m = (f * n) % Q1_;
  if (m <= 0) m += Q1_;
  return m;
}
ring_elem GF::power(const ring_elem f, mpz_t n) const
{
  if (f == _ZERO) return _ZERO;
  int exp = RingZZ::mod_ui(n, Q1_);
  int m = (f * exp) % Q1_;
  if (m <= 0) m += Q1_;
  return m;
}

ring_elem GF::invert(const ring_elem f) const
{
  // error if f == _ZERO
  if (f == _ONE) return _ONE;
  return ring_elem(Q1_ - f);
}

ring_elem GF::divide(const ring_elem f, const ring_elem g) const
{
  if (g == _ZERO) assert(0); // MES: raise an exception
  if (f == _ZERO) return _ZERO;
  return modulus_sub(f, g, Q1_);
}

void GF::syzygy(const ring_elem a, const ring_elem b,
		ring_elem &x, ring_elem &y) const
{
  x = GF::from_int(1);
  y = GF::divide(a,b);
  GF::internal_negate_to(y);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
