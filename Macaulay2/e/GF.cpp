// Copyright 1995 Michael E. Stillman

#include "Z.hpp"
#include "GF.hpp"
#include "text_io.hpp"
#include "bin_io.hpp"
#include "monoid.hpp"
#include "ringmap.hpp"
#include "polyring.hpp"
#include "random.hpp"
#include "serial.hpp"

GF::GF(const RingElement prim)
: Ring(prim.Ring_of()->charac(),
	1,1,this /* Visual C WARNING */,trivial_monoid, 
	prim.Ring_of()->degree_monoid()),
  K(prim.Ring_of()->cast_to_PolynomialRing()),
  primitive_element(prim)
{
  int i,j;
  bump_up((Ring *) K);

  ring_elem f = K->get_quotient_elem(0);

  int n = K->primary_degree(f);

  Q = P;
  for (i=1; i<n; i++) Q *= P;

  Qexp = n;
  Q1 = Q-1;
  ZERO = 0;
  ONE = Q1;
  MINUS_ONE = (P == 2 ? ONE : Q1/2);

  // Get ready to create the 'one_table'
  array<ring_elem> polys;
  polys.append(K->from_int(0));
  ring_elem primelem = prim.get_value();
  polys.append(K->copy(primelem));

  ring_elem one = K->from_int(1);

  x_exponent = -1;
  ring_elem x = K->var(0,1);
  if (K->is_equal(primelem, x))
    x_exponent = 1;
  for (i=2; i<Q; i++)
    {
      ring_elem g = K->mult(polys[i-1], primelem);
      polys.append(g);
      if (K->is_equal(g, one)) break;
      if (K->is_equal(g, x))
	x_exponent = i;
    }

  if (polys.length() != Q)
    {
      gError << "GF: primitive element expected";
      for (i=0; i<polys.length(); i++)
	{
	  K->remove(polys[i]);
	}
      K->remove(x);
      return;
    }

  assert(x_exponent >= 0);

  // Set 'one_table'.
  one_table = new int[Q];
  one_table[0] = Q-1;
  for (i=1; i<=Q-1; i++)
    {
      ring_elem f1 = K->add(polys[i], one);
      for (j=1; j<=Q-1; j++)
	if (K->is_equal(f1, polys[j]))
	  break;
      one_table[i] = j;
    }

  // Create the Z/P ---> GF(Q) inclusion map
  from_int_table = new int[P];
  int a = ONE;
  from_int_table[0] = ZERO;
  for (i=1; i<P; i++)
    {
      from_int_table[i] = a;
      a = one_table[a];
    }

  // We no longer need the 'polys' array, so remove it
  for (i=0; i<polys.length(); i++)
    K->remove(polys[i]);
}
GF::~GF()
{
  delete [] from_int_table;
  delete [] one_table;
  bump_down((Ring *) K);
}

GF *GF::create(const RingElement prim)
{
  GF *obj = new GF(prim);
  return (GF *) intern(obj);
}

bool GF::equals(const object_element *o) const
{
  if (o->class_id() != class_id())
    return false;

  const GF *R = (GF *)o;
  if (R->K != K) return false;
  if (R->primitive_element != primitive_element)
    return false;
  return true;
}

int GF::hash() const 
{ 
  return 0;
}

void GF::write_object(object_writer &o) const
{
  o << class_id() << *primitive_element;
}

GF *GF::read_object(object_reader &i)
{
  // The class id has already been consumed.
  object_element *obj;
  i >> obj;
  RingElement f = obj->cast_to_RingElement();
  return new GF(f);
}

void GF::write_element(object_writer &o, const ring_elem f) const
{
  o << f.int_val;
}

void GF::read_element(object_reader &i, ring_elem &result) const
{
  i >> result.int_val ;
}

void GF::text_out(buffer &o) const
{
  o << "GF(" << Q << ")";
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
  int exp = Random::random0(Q);
  return ring_elem(exp);
}

void GF::elem_text_out(buffer &o, const ring_elem a) const
{
  if (a == ZERO) 
    {
      o << "0";
      return;
    }
  ring_elem h = K->power(primitive_element.get_value(), a.int_val);
  K->elem_text_out(o, h);
  K->remove(h);
}

void GF::elem_bin_out(buffer &o, const ring_elem a) const
{
  bin_int_out(o, a.int_val);
}

ring_elem GF::eval(const RingMap *map, const ring_elem f) const
{
  return map->Ring_of()->power(map->elem(0), f.int_val);
}

ring_elem GF::from_int(int n) const
{
  int m = n % P;
  if (m < 0) m += P;
  m = from_int_table[m];
  return ring_elem(m);
}

ring_elem GF::from_int(mpz_ptr n) const
{
  mpz_t result;
  mpz_init(result);
  mpz_mod_ui(result, n, P);
  int m = mpz_get_si(result);
  if (m < 0) m += P;
  m = from_int_table[m];
  return ring_elem(m);
}

ring_elem GF::var(int v, int n) const
{
  if (v >= 1) return ZERO;
  if (v == 0)
    {
      int m = n % Q1;
      if (m <= 0) m += Q1;
      return ring_elem(m);
    }
  return ONE;
}
bool GF::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // Rf = Z/p[x]/F(x) ---> GF(p,n)
  // promotion: need to be able to know the value of 'x'.
  // lift: need to compute (primite_element)^e

  if (Rf != K) return false;

  result = from_int(0);
  int exp[1];
  for (Nterm *t = f; t != NULL; t = t->next)
    {
      ring_elem coef = from_int(K->Ncoeffs()->coerce_to_int(t->coeff));
      K->Nmonoms()->to_expvector(t->monom, exp);
      // exp[0] is the variable we want.  Notice that since the ring is a quotient,
      // this degree is < n (where Q = P^n).
      ring_elem g = power(x_exponent, exp[0]);
      g = mult(g, coef);
      add_to(result, g);
    }
  return true;
}

bool GF::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // Rg = Z/p[x]/F(x) ---> GF(p,n)
  // promotion: need to be able to know the value of 'x'.
  // lift: need to compute (primite_element)^e

  if (Rg != K) return false;

  int e = f.int_val;
  if (e == ZERO)
    result = K->from_int(0);
  else if (e == ONE)
    result = K->from_int(1);
  else
    result = K->power(primitive_element.get_value(), e);
  
  return true;
}

bool GF::is_unit(const ring_elem f) const
{
  return (f != ZERO);
}

bool GF::is_zero(const ring_elem f) const
{
  return (f == ZERO);
}

bool GF::is_equal(const ring_elem f, const ring_elem g) const
{
  return f.int_val == g.int_val;
}

ring_elem GF::copy(const ring_elem f) const
{
  return f;
}

void GF::remove(ring_elem &) const
{
  // nothing needed to remove.
}

void GF::negate_to(ring_elem &f) const
{
  if (f != ZERO)
    f = modulus_add(f, MINUS_ONE, Q1);
}

void GF::add_to(ring_elem &f, ring_elem &g) const
{
  if (g == ZERO) return;
  if (f == ZERO) 
    f = g;
  else
    {
      int a = f.int_val;
      int b = g.int_val;
      int n = a-b;
      if (n > 0)
	{
	  if (n == MINUS_ONE)
	    f = ZERO;
	  else
	    f = modulus_add(b, one_table[n], Q1);
	}
      else if (n < 0)
	{
	  if (-n == MINUS_ONE)
	    f = ZERO;
	  else
	    f = modulus_add(a, one_table[-n], Q1);
	}
      else 
	{
	  if (P == 2) 
	    f = ZERO;
	  else
	    f = modulus_add(a, one_table[ONE], Q1);
	}
    }
}

void GF::subtract_to(ring_elem &f, ring_elem &g) const
{
  if (g == ZERO) return;
  if (f.int_val == g.int_val) { f = ZERO; return; }
  ring_elem g1 = modulus_add(g, MINUS_ONE, Q1);  // f = -g
  add_to(f, g1);
}

ring_elem GF::negate(const ring_elem f) const
{
  ring_elem result = f;
  negate_to(result);
  return result;
}

ring_elem GF::add(const ring_elem f, const ring_elem g) const
{
  ring_elem result = f;
  ring_elem g1 = g;
  add_to(result, g1);
  return result;
}

ring_elem GF::subtract(const ring_elem f, const ring_elem g) const
{
  ring_elem result = f;
  ring_elem g1 = g;
  subtract_to(result, g1);
  return result;
}

ring_elem GF::mult(const ring_elem f, const ring_elem g) const
{
  if (f == ZERO || g == ZERO) return ZERO;
  return modulus_add(f, g, Q1);
}

ring_elem GF::power(const ring_elem f, int n) const
{
  if (f == ZERO) return ZERO;
  int m = (f * n) % Q1;
  if (m <= 0) m += Q1;
  return m;
}
ring_elem GF::power(const ring_elem f, mpz_t n) const
{
  if (f == ZERO) return ZERO;
  int exp = Z::mod_ui(n, Q1);
  int m = (f * exp) % Q1;
  if (m <= 0) m += Q1;
  return m;
}

ring_elem GF::invert(const ring_elem f) const
{
  // error if f == ZERO
  if (f == ONE) return ONE;
  return ring_elem(Q1 - f);
}

ring_elem GF::divide(const ring_elem f, const ring_elem g) const
{
  if (g == ZERO) assert(0); // MES: raise an exception
  if (f == ZERO) return ZERO;
  return modulus_sub(f, g, Q1);
}

ring_elem GF::divide(const ring_elem f, const ring_elem g, ring_elem &rem) const
{
  if (g == ZERO) assert(0); // MES: raise an exception
  if (f == ZERO) return ZERO;
  rem = ZERO;
  return modulus_sub(f, g, Q1);
}
ring_elem GF::gcd(const ring_elem f, const ring_elem g) const
{
  if (f == ZERO || g == ZERO) return ZERO;
  return ONE;
}

ring_elem GF::gcd_extended(const ring_elem f, const ring_elem, 
				ring_elem &u, ring_elem &v) const
{
  v = ZERO;
  u = invert(f);
  return ONE;
}

bool GF::is_homogeneous(const ring_elem) const
{
  return true;
}

void GF::degree(const ring_elem, int *d) const
{
  degree_monoid()->one(d);
}
void GF::degree_weights(const ring_elem, const int *, int &lo, int &hi) const
{
  lo = hi = 0;
}
int GF::primary_degree(const ring_elem) const
{
  return 0;
}

ring_elem GF::homogenize(const ring_elem f, int, int deg, const int *) const
{
  if (deg != 0) 
    gError << "homogenize: no homogenization exists";
  return f;
}

ring_elem GF::homogenize(const ring_elem f, int, const int *) const
{
  return f;
}

int GF::n_terms(const ring_elem) const
{
  return 1;
}
ring_elem GF::term(const ring_elem a, const int *) const
{
  return copy(a);
}
ring_elem GF::lead_coeff(const ring_elem f) const
{
  return f;
}
ring_elem GF::get_coeff(const ring_elem f, const int *) const
{
  return f;
}
ring_elem GF::get_terms(const ring_elem f, int, int) const
{
  return f;
}

