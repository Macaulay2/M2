// Copyright 1995 Michael E. Stillman

#include "GF2.hh"
#include "text_io.hh"
#include "monoid.hh"
#include "ringmap.hh"
#include "polyring.hh"

const int WORDBITS = 8 * sizeof(int);

#define GFVAL(f) ((unsigned int *) (f).poly_val)
#define GF8VAL(f) ((byte *) (f).poly_val)
#define GFRINGELEM(p) ((ring_elem) ((Nterm *) p))

GF2::GF2(const PolynomialRing *KK)
: Ring(KK->charac(),
	1,1,this,trivial_monoid, 
	KK->degree_monoid()),
  K(KK)
{
  bump_up((Ring *) K);

  nbits = K->primary_degree(K->get_quotient_elem(0));
  nbytes = (nbits + 7) / 8;
  nwords = (nbits + 8*sizeof(int) - 1) / (8*sizeof(int));

  // MES: check that numvars(KK) = 1

  GFstash = new stash("GF", nbytes);
  make_tables();
}
GF2::~GF2()
{
  bump_down((Ring *) K);
}

//--- Useful bit like routines --------------------------//

void GF2::xorbit(int d, byte *f) const
{
  unsigned int b = d / 8;
  unsigned int bit = d % 8;
  f[b] ^= (1 << bit);
}

void GF2::setbit(int d, byte *f, int val) const
{
  unsigned int b = d / 8;
  unsigned int bit = d % 8;
  byte v = (1 << bit);
  if (val == 0)
    v = ~v;
  f[b] |= v;
}

int GF2::bitval(int d, byte *f) const
{
  unsigned int b = d / 8;
  unsigned int bit = d % 8;
  if ((f[b] & (1 << bit)) == 0)
    return 0;
  return 1;
}
void GF2::display0(ostream &o, byte *p, int n) const
{
  // Display the polynomial represented by a bitstream of length n
  int notfirst = 0;
  for (int i=n-1; i>=0; i--)
    if (bitval(i,p) == 1)
      {
	if (notfirst) o << "+";
	if (i == 0) o << "1";
	else if (i == 1) o << "a";
	else o << "a" << i;
	notfirst = 1;
      }
  if (!notfirst) o << "0";

}
unsigned int GF2::mult0(unsigned int a, unsigned int b) const
{
  // a, b are 16 bit quantities
  unsigned int result = 0;
  while (a != 0)
    {
      if ((a % 2) != 0) result ^= b;
      a >>= 1;
      b <<= 1;
    }
  return result;
}

void GF2::mult1(unsigned int *aa, unsigned int *bb, unsigned int *res) const
     // a, b are nwords long
     // result is 2*nwords long
{
  int i,j;
  int nslots = 2*nwords;
  unsigned short *a = (unsigned short *) aa;
  unsigned short *b = (unsigned short *) bb;
  unsigned short *result = (unsigned short *) res;
  for (i=0; i<2*nslots; i++)
    result[i] = 0;
  for (i=0; i<nslots; i++)
    {
      if (a[i] != 0) 
	{
	  for (j=0; j<nslots; j++)
	    {
	      int ab = mult0(a[i],b[j]);
	      short abtop = ab >> 16;
	      short abbottom = ab % (1 << 16);
	      result[i+j] ^= abbottom;
	      result[i+j+1] ^= abtop;
	    }
	}
    }
}

void GF2::multx0(byte *a, byte *result) const
    // mult a by x, and reduce mod f
{
  // do we need to xor in, or just shift?
  int prev, i;
  int red = bitval(nbits-1,a);
  if (red == 0)
    {
      prev = 0;
      for (i=0; i<nbytes; i++)
	{
	  unsigned int ashifted = a[i] << 1;
	  if (prev) ashifted++;
	  prev = (ashifted > 255);
	  result[i] = ashifted;
	}
    }
  else
    {
      prev = 0;
      for (i=0; i<nbytes; i++)
	{
	  unsigned int ashifted = a[i] << 1;
	  if (prev) ashifted++;
	  prev = (ashifted > 255);
	  result[i] = ashifted ^ reductum[i];
	}
      xorbit(nbits, result);
    }

}

void GF2::reduce0(byte *a, byte *result) const
{
  // a has length 2*nbytes, result has length nbytes
  for (int thisbit = 2*nbits-1; thisbit >= nbits; thisbit--)
    {
      if (bitval(thisbit,a))
	{
	  int delta = thisbit - nbits;
	  for (int i=0; i<nreductum_bits; i++)
	    xorbit(delta+reductum_bits[i], a);
	}
    }
  // Now copy lower part back to result
  for (int i=0; i<nbytes; i++)
    result[i] = a[i];
}

//-------------------------------------------------------//

void GF2::make_tables()
{
  cerr << "nbits = " << nbits << endl;
  cerr << "nbytes = " << nbytes << endl;
  cerr << "nwords = " << nwords << endl;

  // make the polynomial f
  ring_elem f = K->get_quotient_elem(0);

  reductum = new_elem();
  int exp;
  int i;
  reductum_bits = new byte[nbits];
  reductum_bits[0] = nbits;
  nreductum_bits = 1;
  for (i=0; i<nbytes; i++) reductum[i] = 0;
  for (Nterm *t = f; t != NULL; t = t->next)
    {
      K->Nmonoms()->to_expvector(t->monom, &exp);
      if (exp != nbits)
	setbit(exp, reductum, 1);
      reductum_bits[++nreductum_bits] = exp;
    }
  cerr << "f = ";
  Nelem_text_out(cerr, GFRINGELEM(reductum));
  cerr << endl;

  // Time to make the tables
  typedef byte * bytestar;
  reduce_table = new bytestar[256];
  reduce_table[0] = reductum;
  for (i=1; i<16; i++)
    {
      reduce_table[i] = new_elem();
      multx0(reduce_table[i-1], reduce_table[i]);
      display0(cerr, reduce_table[i], nbits);
      cerr << endl;
    }

  // MES MES: this is where I am right now.
}

void GF2::text_out(ostream &o) const
{
  o << "GF2(" << nbits << ")";
}
void GF2::Nelem_text_out(ostream &o, const ring_elem a) const
{
  display0(o,GF8VAL(a),nbits);
}

void GF2::Nelem_bin_out(ostream &o, const ring_elem a) const
{
  int n = n_terms(a);
  bin_int_out(o,n);

  byte *p = GF8VAL(a);
  for (int i = nbits; i >= 0; i--)
      if (bitval(i,p))
	{
	  // Put monomial
	  if (i > 0) bin_int_out(o,1);
	  bin_int_out(o,i);
	  
	  // Coefficient
	  bin_int_out(o,1);
	}
}

byte *GF2::new_elem() const
{
  return (byte *) (GFstash->new_elem());
}

ring_elem GF2::eval(const RingMap &map, const ring_elem f) const
{
  // MES: rewrite
#if 0
  return map.Ring_of()->power(map.elem(0), f.int_val);
#endif
  return GFRINGELEM(NULL);
}

ring_elem GF2::from_int(int n) const
{
  int m = n % 2;
  if (m < 0) m = 1;
  int *result = (int *) new_elem();
  result[0] = m;
  for (int i=1; i<nwords; i++)
    result[i] = 0;
  return GFRINGELEM(result);
}

ring_elem GF2::from_int(mpz_ptr n) const
{
  mpz_t result;
  mpz_init(result);
  mpz_mod_ui(result, n, P);
  int m = mpz_get_si(result);
  if (m < 0) m = 1;
  int *res = (int *) new_elem();
  res[0] = m;
  for (int i=1; i<nwords; i++)
    res[i] = 0;
  return GFRINGELEM(res);
}

ring_elem GF2::var(int v, int n) const
{
  unsigned int *result = (unsigned int *) new_elem();
  for (int i=0; i<nwords; i++)
    result[i] = 0;
  if (v == 0)
    if (n >= 0 && n < nbits)
      setbit(n, (byte *) result, 1);
  return GFRINGELEM(result);
}
int GF2::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
#if 0
  // MES: rewrite
  if (Rf->is_zero(f))
    {
      result = from_int(0);
      return 1;
    }
  if (Rf == this)
    {
      result = f;
      return 1;
    }
  ring_elem a;
  if (K->promote(Rf, f, a))
    {
      // Evaluate the polynomial.  This should perhaps be a ringmap evaluation
      // but I'm not sure where to set that map.
      result = from_int(0);
      int exp[1];
      for (Nterm *t = a; t != NULL; t = t->next)
	{
	  ring_elem c = t->coeff;
	  ring_elem g = from_int(c);
	  K->Nmonoms()->to_expvector(t->monom, exp);
	  // exp[0] is the variable we want.  First reduce mod Q
	  int v = exp[0] / Q;
	  g = mult(g, v);
	  add_to(result, g);
	}
      return 1;
    }
#endif
  return 0;
}

int GF2::lift(const Ring *R, const ring_elem f, ring_elem &result) const
{
  // f is an element of R.  It can be lifted to GF only if R is a quotient (can't happen)
  // or R is a fraction field of GF (so R = GF).
#if 0
  if (R == this) 
    {
      result = f;
      return 1;
    }
#endif
  return 0;
}

int GF2::is_zero(const ring_elem f) const
{
  unsigned int *f1 = GFVAL(f);
  for (int i=0; i<nwords; i++)
    if (f1[i] != 0) return 0;
  return 1;
}

int GF2::is_unit(const ring_elem f) const
{
  return (!is_zero(f));
}

int GF2::is_equal(const ring_elem f, const ring_elem g) const
{
  unsigned int *f1 = GFVAL(f);
  unsigned int *g1 = GFVAL(g);
  for (int i=0; i<nwords; i++)
    if (f1[i] != g1[i]) return 0;
  return 1;
}

ring_elem GF2::copy(const ring_elem f) const
{
  unsigned int *result = (unsigned int *) new_elem();
  unsigned int *f1 = GFVAL(f);
  for (int i=0; i<nwords; i++)
    result[i] = f1[i];
  return GFRINGELEM(result);
}

void GF2::remove(ring_elem &f) const
{
  unsigned int *f1 = GFVAL(f);
//  GFstash->delete_elem(f1);
}

void GF2::negate_to(ring_elem &f) const
{
  return;
}

void GF2::add_to(ring_elem &f, ring_elem &g) const
{
  unsigned int *f1 = GFVAL(f);
  unsigned int *g1 = GFVAL(g);
  for (int i=0; i<nwords; i++)
    f1[i] ^= g1[i];
}

void GF2::subtract_to(ring_elem &f, ring_elem &g) const
{
  unsigned int *f1 = GFVAL(f);
  unsigned int *g1 = GFVAL(g);
  for (int i=0; i<nwords; i++)
    f1[i] ^= g1[i];
}

ring_elem GF2::negate(const ring_elem f) const
{
  return copy(f);
}

ring_elem GF2::add(const ring_elem f, const ring_elem g) const
{
  unsigned int *f1 = GFVAL(f);
  unsigned int *g1 = GFVAL(g);
  unsigned int *result = (unsigned int *) new_elem();
  for (int i=0; i<nwords; i++)
    result[i] = f1[i] ^ g1[i];
  return GFRINGELEM(result);
}

ring_elem GF2::subtract(const ring_elem f, const ring_elem g) const
{
  unsigned int *f1 = GFVAL(f);
  unsigned int *g1 = GFVAL(g);
  unsigned int *result = (unsigned int *) new_elem();
  for (int i=0; i<nwords; i++)
    result[i] = f1[i] ^ g1[i];
  return GFRINGELEM(result);
}

ring_elem GF2::mult(const ring_elem f, const ring_elem g) const
{
  unsigned int *a = GFVAL(f);
  unsigned int *b = GFVAL(g);
  unsigned int *result = (unsigned int *) new byte[4*nwords];
  mult1(a, b, result);
  display0(cerr, (byte *) result, 2*nbits-1);
  cerr << endl;
  byte *res = new_elem();
  reduce0((byte *)result, res);
  display0(cerr, res, nbits);
  cerr << endl;
  delete [] result;
  return GFRINGELEM(res);
}

ring_elem GF2::power(const ring_elem f, int n) const
{
  // MES: rewrite
  return 0;
}
ring_elem GF2::power(const ring_elem f, mpz_t n) const
{
  // MES: rewrite
  return 0;
}

ring_elem GF2::invert(const ring_elem f) const
{
  // MES: rewrite
  return 0;
}

ring_elem GF2::divide(const ring_elem f, const ring_elem g) const
{
  // MES: rewrite
  return 0;
}

ring_elem GF2::divide(const ring_elem f, const ring_elem g, ring_elem &rem) const
{
#if 0
  // MES: rewrite
  if (g == ZERO) assert(0); // MES: raise an exception
  if (f == ZERO) return ZERO;
  rem = ZERO;
  return modulus_sub(f, g, Q1);
#endif
  return 0;
}
ring_elem GF2::gcd(ring_elem f, ring_elem g) const
{
#if 0
  // MES: rewrite
  if (f == ZERO || g == ZERO) return ZERO;
  return ONE;
#endif
  return 0;
}

ring_elem GF2::gcd_extended(ring_elem f, ring_elem, 
				ring_elem &u, ring_elem &v) const
{
#if 0
  // MES: rewrite

  v = ZERO;
  u = invert(f);
  return ONE;
#endif
  return 0;
}

int GF2::is_homogeneous(const ring_elem) const
{
  return 1;
}

void GF2::degree(const ring_elem, int *d) const
{
  degree_monoid()->one(d);
}
void GF2::degree_weights(const ring_elem, const int *, int &lo, int &hi) const
{
  lo = hi = 0;
}
int GF2::primary_degree(const ring_elem) const
{
  return 0;
}

ring_elem GF2::homogenize(const ring_elem f, int, int deg, const int *) const
{
  if (deg != 0) 
    *gError << "homogenize: no homogenization exists";
  return f;
}

ring_elem GF2::homogenize(const ring_elem f, int, const int *) const
{
  return f;
}

int GF2::n_terms(const ring_elem a) const
{
  byte *p = GF8VAL(a);
  int result = 0;
  for (int i = nbits-1; i >= 0; i--)
    if (bitval(i,p)) result++;
  return result;
}
ring_elem GF2::term(const ring_elem a, const int *) const
{
  return copy(a);
}
ring_elem GF2::lead_coeff(const ring_elem f) const
{
  return f;
}
ring_elem GF2::get_coeff(const ring_elem f, const int *vp) const
{
  return f;
}
ring_elem GF2::get_terms(const ring_elem f, int, int) const
{
  return f;
}

