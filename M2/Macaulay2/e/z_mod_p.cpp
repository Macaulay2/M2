// Copyright 1995 Michael E. Stillman

#include "z_mod_p.hpp"
#include "text_io.hpp"
#include "bin_io.hpp"
#include "monoid.hpp"
#include "ringmap.hpp"
#include "Z.hpp"
#include "random.hpp"

extern Z *ZZ;

Z_mod::Z_mod(int p, const Monoid *D) 
: Ring(p,0,0,this, trivial_monoid, D)
{
  int i,j,q,n;

  if (P==2)
    minus_one = 0;
  else
    minus_one = (P-1)/2;

  if (P==2)
    prim_root = 1;
  else
    {
      j=1;
      for (i=2; (i<P && j<P-1); i++)
        for (q=i,j=1; (q!=1 && j<P); q=(q*i)%P,j++);
      prim_root = i-1;
    }

  // cerr << "z_mod_p: creating table for P = " << P << endl;
  log_table = new int[P];
  exp_table = new int[P];
  for (i=0, n=1; i<P-1; i++, n=(n*prim_root)%P)
    {
      log_table[n] = i;  // i = log_(base prim_root)(n)
      exp_table[i] = n;  // n = (prim_root)^i 
    }
  ZERO            = P-1;
  exp_table[ZERO] = 0;
  log_table[0]    = ZERO;

  P1 = P-1;
}

void Z_mod::text_out(ostream &o) const
{
  o << "Z/" << P;
}

inline int Z_mod::to_int(int f) const
{
  int n = exp_table[f];
  if (n > P/2) n -= P;
  return n;
}

int Z_mod::coerce_to_int(ring_elem a) const
{ 
  return to_int(a.int_val);
}

inline int modulus_add(int a, int b, int p)
{
  int t = a+b;
  return (t < p ? t : t-p);
}

inline int modulus_sub(int a, int b, int p)
{
  int t = a-b;
  return (t < 0 ? t+p : t);
}

inline int Z_mod::int_to_exp(int a) const
{
  int n = a % P;
  return log_table[(n < 0 ? n+P : n)];
}

void Z_mod::elem_text_out(ostream &o, ring_elem a) const
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

void Z_mod::elem_bin_out(ostream &o, ring_elem a) const
{
  int n = to_int(a);
  bin_int_out(o, n);
}

ring_elem Z_mod::from_int(int n) const
{
  int m = n % P;
  if (m < 0) m += P;
  m = log_table[m];
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
  m = log_table[m];
  return ring_elem(m);
}

ring_elem Z_mod::var(int v, int) const
{
  if (v >= 0) return ZERO;
  return from_int(1);
}

bool Z_mod::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // Rf = Z ---> Z/p
  if (Rf == ZZ)
    {
      result = from_int(MPZ_VAL(f));
      return true;
    }
  return false;
}

bool Z_mod::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // Rg = Z ---> Z/p
  if (Rg == ZZ)
    {
      result = Rg->from_int(coerce_to_int(f));
      return true;
    }
  return false;
}

bool Z_mod::is_unit(const ring_elem f) const
{
  return (f != P1);
}

bool Z_mod::is_zero(const ring_elem f) const
{
  return (f == P1);
}

bool Z_mod::is_equal(const ring_elem f, const ring_elem g) const
{
  return f.int_val == g.int_val;
}

ring_elem Z_mod::copy(const ring_elem f) const
{
  return f;
}

void Z_mod::remove(ring_elem &) const
{
  // nothing needed to remove.
}

void Z_mod::negate_to(ring_elem &f) const
{
  if (f != ZERO)
    f = modulus_add(f, minus_one, P1);
}

void Z_mod::add_to(ring_elem &f, ring_elem &g) const
{
  if (g == ZERO) return;
  if (f == ZERO) 
    f = g;
  else
    {
      int n = modulus_add(exp_table[f.int_val], exp_table[g.int_val], P);
      f = log_table[n];
    }
}

void Z_mod::subtract_to(ring_elem &f, ring_elem &g) const
{
  if (g == ZERO) return;
  if (f == ZERO) 
    f = negate(g);
  else
    {
      int n = modulus_sub(exp_table[f.int_val], exp_table[g.int_val], P);
      f = log_table[n];
    }
}

ring_elem Z_mod::negate(const ring_elem f) const
{
  ring_elem result = f;
  negate_to(result);
  return result;
}

ring_elem Z_mod::add(const ring_elem f, const ring_elem g) const
{
  if (g == ZERO)  return f;
  if (f == ZERO)  return g;
  
  int n = modulus_add(exp_table[f.int_val], exp_table[g.int_val], P);
  return log_table[n];
}

ring_elem Z_mod::subtract(const ring_elem f, const ring_elem g) const
{
  if (g == ZERO)  return f;
  if (f == ZERO)  return negate(g);
  
  int n = modulus_sub(exp_table[f.int_val], exp_table[g.int_val], P);
  return log_table[n];
}

ring_elem Z_mod::mult(const ring_elem f, const ring_elem g) const
{
  if (f == ZERO || g == ZERO) return ZERO;
  return modulus_add(f, g, P1);
}

ring_elem Z_mod::power(const ring_elem f, int n) const
{
  if (f == ZERO) return ZERO;
  int m = (f * n) % P1;
  if (m < 0) m += P1;
  return m;
}
ring_elem Z_mod::power(const ring_elem f, mpz_t n) const
{
  if (f == ZERO) return ZERO;
  int n1 = Z::mod_ui(n, P1);
  int m = (f * n1) % P1;
  if (m < 0) m += P1;
  return m;
}

ring_elem Z_mod::invert(const ring_elem f) const
{
  // MES: error if f == ZERO
  int a = f.int_val;
  if (a == 0) return 0; // this is the case f == ONE
  return ring_elem(P - 1 - a);
}

ring_elem Z_mod::divide(const ring_elem f, const ring_elem g) const
{
  if (g == ZERO) assert(0); // MES: raise an exception
  if (f == ZERO) return ZERO;
  return modulus_sub(f, g, P1);
}

ring_elem Z_mod::divide(const ring_elem f, const ring_elem g, ring_elem &rem) const
{
  if (g == ZERO) assert(0); // MES: raise an exception
  if (f == ZERO) return ZERO;
  rem = ZERO;
  return modulus_sub(f, g, P1);
}
ring_elem Z_mod::gcd(ring_elem f, ring_elem g) const
{
  if (f == ZERO || g == ZERO) return ZERO;
  return from_int(1);
}

ring_elem Z_mod::gcd_extended(ring_elem f, ring_elem, 
				ring_elem &u, ring_elem &v) const
{
  v = ZERO;
  u = invert(f);
  return from_int(1);
}

ring_elem Z_mod::eval(const RingMap &map, const ring_elem f) const
{
  int a = to_int(f);
  return map.Ring_of()->from_int(a);
}

ring_elem Z_mod::random() const
{
  int exp = Random::random0(P);
  return ring_elem(exp);
}
bool Z_mod::is_homogeneous(const ring_elem) const
{
  return true;
}

void Z_mod::degree(const ring_elem, int *d) const
{
  degree_monoid()->one(d);
}

void Z_mod::degree_weights(const ring_elem, const int *, int &lo, int &hi) const
{
  lo = hi = 0;
}
int Z_mod::primary_degree(const ring_elem) const
{
  return 0;
}

ring_elem Z_mod::homogenize(const ring_elem f, int, int deg, const int *) const
{
  if (deg != 0) 
    *gError << "homogenize: no homogenization exists";
  return f;
}

ring_elem Z_mod::homogenize(const ring_elem f, int, const int *) const
{
  return f;
}

int Z_mod::n_terms(const ring_elem) const
{
  return 1;
}
ring_elem Z_mod::term(const ring_elem a, const int *) const
{
  return copy(a);
}
ring_elem Z_mod::lead_coeff(const ring_elem f) const
{
  return f;
}
ring_elem Z_mod::get_coeff(const ring_elem f, const int *) const
{
  return f;
}
ring_elem Z_mod::get_terms(const ring_elem f, int, int) const
{
  return f;
}
