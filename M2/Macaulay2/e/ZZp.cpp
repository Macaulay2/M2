// Copyright 1995 Michael E. Stillman

#include "ZZp.hpp"
#include "text-io.hpp"
#include "monoid.hpp"
#include "ringmap.hpp"
#include "ZZ.hpp"
#include "random.hpp"
#include "gbring.hpp"

extern RingZZ *globalZZ;

bool Z_mod::initialize_Z_mod(int p)
{

  initialize_ring(p);
		  

  declare_field();
  int i,j,q,n;

  if (P==2)
    _minus_one = 0;
  else
    _minus_one = (P-1)/2;

  if (P==2)
    _prim_root = 1;
  else
    {
      j=1;
      for (i=2; (i<P && j<P-1); i++)
        for (q=i,j=1; (q!=1 && j<P); q=(q*i)%P,j++);
      _prim_root = i-1;
    }

  // cerr << "z_mod_p: creating table for P = " << P << endl;
  _log_table = newarray_atomic(int,P);
  _exp_table = newarray_atomic(int,P);
  for (i=0, n=1; i<P-1; i++, n=(n*_prim_root)%P)
    {
      _log_table[n] = i;  // i = log_(base _prim_root)(n)
      _exp_table[i] = n;  // n = (_prim_root)^i 
    }
  _ZERO            = P-1;
  _exp_table[_ZERO] = 0;
  _log_table[0]    = _ZERO;

  _P1 = P-1;

  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  coeffR = new CoefficientRingZZp(P,_log_table, _exp_table);
  return true;
}

Z_mod *Z_mod::create(int p)
{
  Z_mod *result = new Z_mod;
  if (!result->initialize_Z_mod(p)) return 0;

  return result;
}


void Z_mod::text_out(buffer &o) const
{
  o << "ZZ/" << P;
}

int Z_mod::to_int(int f) const
{
  int n = _exp_table[f];
  if (n > P/2) n -= P;
  return n;
}

int Z_mod::coerce_to_int(ring_elem a) const
{ 
  return to_int(a.int_val);
}

static inline int modulus_add(int a, int b, int p)
{
  int t = a+b;
  return (t < p ? t : t-p);
}

static inline int modulus_sub(int a, int b, int p)
{
  int t = a-b;
  return (t < 0 ? t+p : t);
}

inline int Z_mod::int_to_exp(int a) const
{
  int n = a % P;
  return _log_table[(n < 0 ? n+P : n)];
}

void Z_mod::elem_text_out(buffer &o, ring_elem a) const
{
  int n = to_int(a);
  if (n < 0) 
    {
      o << '-';
      n = -n;
    }
  else if (p_plus) 
    o << '+';
  if (p_one || n != 1) o << n;
}

ring_elem Z_mod::from_int(int n) const
{
  int m = n % P;
  if (m < 0) m += P;
  m = _log_table[m];
  return ring_elem(m);
}

ring_elem Z_mod::from_int(mpz_ptr n) const
{
//  cout << "from_int(";
//  bignum_text_out(cout, n);
//  cout << ") = " << endl;
  mpz_t result;
  mpz_init(result);
  mpz_mod_ui(result, n, P);
  int m = mpz_get_si(result);
//  cout << m << endl;
  if (m < 0) m += P;
  m = _log_table[m];
  return ring_elem(m);
}

ring_elem Z_mod::from_rational(mpq_ptr q) const
{
  ring_elem a = Z_mod::from_int(mpq_numref(q));
  ring_elem b = Z_mod::from_int(mpq_denref(q));
  if (b == _ZERO)
    return _ZERO;
  return Z_mod::divide(a,b);
}

bool Z_mod::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // Rf = Z ---> Z/p
  if (Rf == globalZZ)
    {
      result = from_int(f.get_mpz());
      return true;
    }
  return false;
}

bool Z_mod::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // Rg = Z ---> Z/p
  if (Rg == globalZZ)
    {
      result = Rg->from_int(coerce_to_int(f));
      return true;
    }
  return false;
}

bool Z_mod::is_unit(const ring_elem f) const
{
  return (f != _P1);
}

bool Z_mod::is_zero(const ring_elem f) const
{
  return (f == _P1);
}

bool Z_mod::is_equal(const ring_elem f, const ring_elem g) const
{
  return f.int_val == g.int_val;
}

int Z_mod::compare_elems(const ring_elem f, const ring_elem g) const
{
  int cmp = f.int_val - g.int_val;
  if (cmp < 0) return -1;
  if (cmp == 0) return 0;
  return 1;
}

ring_elem Z_mod::copy(const ring_elem f) const
{
  return f;
}

void Z_mod::remove(ring_elem &) const
{
  // nothing needed to remove.
}

void Z_mod::internal_negate_to(ring_elem &f) const
{
  if (f != _ZERO)
    f = modulus_add(f, _minus_one, _P1);
}

void Z_mod::internal_add_to(ring_elem &f, ring_elem &g) const
{
  if (g == _ZERO) return;
  if (f == _ZERO) 
    f = g;
  else
    {
      int n = modulus_add(_exp_table[f.int_val], _exp_table[g.int_val], P);
      f = _log_table[n];
    }
}

void Z_mod::internal_subtract_to(ring_elem &f, ring_elem &g) const
{
  if (g == _ZERO) return;
  if (f == _ZERO) 
    f = negate(g);
  else
    {
      int n = modulus_sub(_exp_table[f.int_val], _exp_table[g.int_val], P);
      f = _log_table[n];
    }
}

ring_elem Z_mod::negate(const ring_elem f) const
{
  ring_elem result = f;
  internal_negate_to(result);
  return result;
}

ring_elem Z_mod::add(const ring_elem f, const ring_elem g) const
{
  if (g == _ZERO)  return f;
  if (f == _ZERO)  return g;
  
  int n = modulus_add(_exp_table[f.int_val], _exp_table[g.int_val], P);
  return _log_table[n];
}

ring_elem Z_mod::subtract(const ring_elem f, const ring_elem g) const
{
  if (g == _ZERO)  return f;
  if (f == _ZERO)  return negate(g);
  
  int n = modulus_sub(_exp_table[f.int_val], _exp_table[g.int_val], P);
  return _log_table[n];
}

ring_elem Z_mod::mult(const ring_elem f, const ring_elem g) const
{
  if (f == _ZERO || g == _ZERO) return _ZERO;
  return modulus_add(f, g, _P1);
}

ring_elem Z_mod::power(const ring_elem f, int n) const
{
  if (f == _ZERO) return _ZERO;
  int m = (f * n) % _P1;
  if (m < 0) m += _P1;
  return m;
}
ring_elem Z_mod::power(const ring_elem f, mpz_t n) const
{
  if (f == _ZERO) return _ZERO;
  int n1 = RingZZ::mod_ui(n, _P1);
  int m = (f * n1) % _P1;
  if (m < 0) m += _P1;
  return m;
}

ring_elem Z_mod::invert(const ring_elem f) const
{
  // MES: error if f == _ZERO
  int a = f.int_val;
  if (a == 0) return 0; // this is the case f == ONE
  return ring_elem(P - 1 - a);
}

ring_elem Z_mod::divide(const ring_elem f, const ring_elem g) const
{
  if (g == _ZERO) assert(0); // MES: raise an exception
  if (f == _ZERO) return _ZERO;
  int h = modulus_sub(f, g, _P1);
  return h;
}

void Z_mod::syzygy(const ring_elem a, const ring_elem b,
		   ring_elem &x, ring_elem &y) const
{
  x = Z_mod::from_int(1);
  y = Z_mod::divide(a,b);
  internal_negate_to(y);
}


ring_elem Z_mod::eval(const RingMap *map, const ring_elem f, int) const
{
  int a = to_int(f);
  return map->get_ring()->from_int(a);
}

ring_elem Z_mod::random() const
{
  int exp = rawRandomInt((int32_t)P);
  return ring_elem(exp);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
