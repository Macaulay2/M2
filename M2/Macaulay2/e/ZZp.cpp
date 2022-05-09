// Copyright 1995 Michael E. Stillman

#include "ZZp.hpp"
#include "text-io.hpp"
#include "monoid.hpp"
#include "ringmap.hpp"
#include "ZZ.hpp"
#include "gbring.hpp"

#include "aring-zzp.hpp"
#include "error.h"

extern RingZZ *globalZZ;

bool Z_mod::initialize_Z_mod(int p)
{
  initialize_ring(p);
  P = p;

  declare_field();
  int i, j, q, n;

  if (P == 2)
    _minus_one = 0;
  else
    _minus_one = (P - 1) / 2;

  if (P == 2)
    _prim_root = 1;
  else
    {
      j = 1;
      for (i = 2; (i < P && j < P - 1); i++)
        for (q = i, j = 1; (q != 1 && j < P); q = (q * i) % P, j++)
          ;
      _prim_root = i - 1;
    }

  // cerr << "z_mod_p: creating table for P = " << P << endl;
  _log_table = newarray_atomic(int, P);
  _exp_table = newarray_atomic(int, P);
  for (i = 0, n = 1; i < P - 1; i++, n = (n * _prim_root) % P)
    {
      _log_table[n] = i;  // i = log_(base _prim_root)(n)
      _exp_table[i] = n;  // n = (_prim_root)^i
    }
  _ZERO = P - 1;
  _exp_table[_ZERO] = 0;
  _log_table[0] = _ZERO;

  _P1 = P - 1;

  zeroV = from_long(0);
  oneV = from_long(1);
  minus_oneV = from_long(-1);

  coeffR = new CoefficientRingZZp(P, _log_table, _exp_table);
  aringZZp = new M2::ARingZZp(P);  // WARNING: this uses that the primitive
                                   // element is the SAME as computed above!!
  // Remove this warning once Z_mod is not in existence any longer.
  return true;
}

Z_mod *Z_mod::create(int p)
{
  Z_mod *result = new Z_mod;
  if (!result->initialize_Z_mod(p)) return 0;

  return result;
}

void Z_mod::text_out(buffer &o) const { o << "ZZ/" << P; }
int Z_mod::to_int(int f) const
{
  int n = _exp_table[f];
  if (n > P / 2) n -= P;
  return n;
}

std::pair<bool, long> Z_mod::coerceToLongInteger(ring_elem a) const
{
  return std::pair<bool, long>(true, to_int(a.get_int()));
}

long Z_mod::discreteLog(const ring_elem &a) const
{
  if (a.get_int() == _ZERO) return -1;
  return a.get_int();
}

static inline int modulus_add(int a, int b, int p)
{
  int t = a + b;
  return (t < p ? t : t - p);
}

static inline int modulus_sub(int a, int b, int p)
{
  int t = a - b;
  return (t < 0 ? t + p : t);
}

inline int Z_mod::int_to_exp(int a) const
{
  int n = a % P;
  return _log_table[(n < 0 ? n + P : n)];
}

unsigned int Z_mod::computeHashValue(const ring_elem a) const
{
  return a.get_int();
}

void Z_mod::elem_text_out(buffer &o,
                          const ring_elem a,
                          bool p_one,
                          bool p_plus,
                          bool p_parens) const
{
  int n = to_int(a.get_int());
  if (n < 0)
    {
      o << '-';
      n = -n;
    }
  else if (p_plus)
    o << '+';
  if (p_one || n != 1) o << n;
}

ring_elem Z_mod::from_long(long n) const
{
  int m = static_cast<int>(n % P);
  if (m < 0) m += P;
  m = _log_table[m];
  return ring_elem(m);
}

ring_elem Z_mod::from_int(mpz_srcptr n) const
{
  //  cout << "from_int(";
  //  bignum_text_out(cout, n);
  //  cout << ") = " << endl;
  mpz_t result;
  mpz_init(result);
  mpz_mod_ui(result, n, P);
  int m = static_cast<int>(mpz_get_si(result));
  mpz_clear(result);
  //  cout << m << endl;
  if (m < 0) m += P;
  m = _log_table[m];
  return ring_elem(m);
}

bool Z_mod::from_rational(mpq_srcptr q, ring_elem &result) const
{
  ring_elem a = Z_mod::from_int(mpq_numref(q));
  ring_elem b = Z_mod::from_int(mpq_denref(q));
  if (b.get_int() == _ZERO) return false;
  result = Z_mod::divide(a, b);
  return true;
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
      result = Rg->from_long(to_int(f.get_int()));
      return true;
    }
  return false;
}

bool Z_mod::is_unit(const ring_elem f) const { return (f.get_int() != _P1); }
bool Z_mod::is_zero(const ring_elem f) const { return (f.get_int() == _P1); }
bool Z_mod::is_equal(const ring_elem f, const ring_elem g) const
{
  return f.get_int() == g.get_int();
}

int Z_mod::compare_elems(const ring_elem f, const ring_elem g) const
{
  int cmp = f.get_int() - g.get_int();
  if (cmp < 0) return -1;
  if (cmp == 0) return 0;
  return 1;
}

ring_elem Z_mod::copy(const ring_elem f) const { return f; }
void Z_mod::remove(ring_elem &) const
{
  // nothing needed to remove.
}

int Z_mod::internal_negate(int f) const
{
  if (f != _ZERO) return modulus_add(f, _minus_one, _P1);
  return _ZERO;
}

int Z_mod::internal_add(int f, int g) const
{
  if (g == _ZERO) return f;
  if (f == _ZERO) return g;
  int n = modulus_add(_exp_table[f], _exp_table[g], P);
  return _log_table[n];
}

int Z_mod::internal_subtract(int f, int g) const
{
  if (g == _ZERO) return f;
  if (f == _ZERO) return internal_negate(g);
  int n = modulus_sub(_exp_table[f], _exp_table[g], P);
  return _log_table[n];
}

ring_elem Z_mod::negate(const ring_elem f) const
{
  return ring_elem(internal_negate(f.get_int()));
}

ring_elem Z_mod::add(const ring_elem f, const ring_elem g) const
{
  return ring_elem(internal_add(f.get_int(), g.get_int()));
}

ring_elem Z_mod::subtract(const ring_elem f, const ring_elem g) const
{
  return ring_elem(internal_subtract(f.get_int(), g.get_int()));
}

ring_elem Z_mod::mult(const ring_elem f, const ring_elem g) const
{
  if (f.get_int() == _ZERO || g.get_int() == _ZERO) return ring_elem(_ZERO);
  return ring_elem(modulus_add(f.get_int(), g.get_int(), _P1));
}

ring_elem Z_mod::power(const ring_elem f, int n) const
{
  if (f.get_int() == _ZERO) {
    if (n < 0)
      throw exc::division_by_zero_error();
    else if (n == 0)
      return ring_elem(0); // this is the element one in this ring.  (P-1) is the zero element...
    return ring_elem(_ZERO);
  }
  int m = (f.get_int() * n) % _P1;
  if (m < 0) m += _P1;
  return ring_elem(m);
}
ring_elem Z_mod::power(const ring_elem f, mpz_srcptr n) const
{
  if (f.get_int() == _ZERO) {
    if (mpz_sgn(n)<0) throw exc::division_by_zero_error();
    else if (mpz_sgn(n) == 0)
      return ring_elem(0);
    return ring_elem(_ZERO);
  }
  int n1 = RingZZ::mod_ui(n, _P1);
  int m = (f.get_int() * n1) % _P1;
  if (m < 0) m += _P1;
  return ring_elem(m);
}

ring_elem Z_mod::invert(const ring_elem f) const
{
  int a = f.get_int();
  if (a == _ZERO) throw exc::division_by_zero_error();
  if (a == 0) return 0;  // this is the case f == ONE
  return ring_elem(P - 1 - a);
}

ring_elem Z_mod::divide(const ring_elem f, const ring_elem g) const
{
  if (g.get_int() == _ZERO) throw exc::division_by_zero_error();
  if (f.get_int() == _ZERO) return ring_elem(_ZERO);
  int h = modulus_sub(f.get_int(), g.get_int(), _P1);
  return ring_elem(h);
}

void Z_mod::syzygy(const ring_elem a,
                   const ring_elem b,
                   ring_elem &x,
                   ring_elem &y) const
{
  assert(!Z_mod::is_zero(b));
  x = Z_mod::from_long(1);
  y = Z_mod::divide(a, b);
  y = ring_elem(internal_negate(y.get_int()));
}

ring_elem Z_mod::eval(const RingMap *map, const ring_elem f, int) const
{
  int a = to_int(f.get_int());
  return map->get_ring()->from_long(a);
}

ring_elem Z_mod::random() const
{
  int exp = rawRandomInt((int32_t)P);
  return ring_elem(exp);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e ZZp.o "
// indent-tabs-mode: nil
// End:
