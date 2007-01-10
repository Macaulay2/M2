// Copyright 1995 Michael E. Stillman

#include "ring.hpp"
#include "monoid.hpp"
#include "monideal.hpp"
#include "res-a1-poly.hpp"
#include "polyring.hpp"

#include "freemod.hpp"

const Monoid * Ring::degree_monoid() const { return degree_ring->Nmonoms(); }

void Ring::initialize_ring(int P0,
			   const PolynomialRing *DR)
{
  // Remember: if this is a poly ring, the ring is K[M].
  // If this is a basic routine, K = this, M = trivial monoid.
  // If this is a frac field, K = R, M = trivial monoid.
  P = P0;
  if (DR == 0)
    degree_ring = PolyRing::get_trivial_poly_ring();
  else
    degree_ring = DR;

  _zero_divisor = ZERO_RINGELEM;
  _isfield = false;

  zeroV = ZERO_RINGELEM;
  oneV = ZERO_RINGELEM;
  minus_oneV = ZERO_RINGELEM;
}

Ring::~Ring()
{
}

FreeModule *Ring::make_FreeModule() const
{ 
  return new FreeModule(this,0,false); 
}

FreeModule *Ring::make_Schreyer_FreeModule() const
{ 
  return new FreeModule(this,0,true); 
}

FreeModule *Ring::make_FreeModule(int n) const
{ 
  return new FreeModule(this,n,false);
}

bool Ring::is_field() const 
{ 
  return _isfield; 
}
void Ring::declare_field() 
{ 
  _isfield = true; 
}
ring_elem Ring::get_zero_divisor() const 
{ 
  return copy(_zero_divisor); 
}

ring_elem Ring::var(int v) const
{
  // The default behavior is to just return 0.
  return zeroV;
}

ring_elem Ring::power(const ring_elem gg, mpz_t m) const
{
  bool negated = false;
  ring_elem ff = gg;
  int cmp = mpz_sgn(m);
  if (cmp == 0) return one();
  if (cmp < 0)
    {
      negated = true;
      mpz_neg(m,m);
      ff = invert(ff);
      if (is_zero(ff))
	{
	  ERROR("element is not invertible");
	  return ff;
	}
    }
  mpz_t n;
  mpz_init_set(n, m);
  ring_elem prod = from_int(1);
  ring_elem base = copy(ff);
  ring_elem tmp;

  for (;;)
    {
      if (RingZZ::mod_ui(n,2) == 1)
	{
	  tmp = mult(prod, base);
	  prod = tmp;
	}
      mpz_tdiv_q_2exp(n, n, 1);
      if (mpz_sgn(n) == 0)
	{
	  mpz_clear(n);
	  if (negated) mpz_neg(m,m);
	  return prod;
	}
      else
	{
	  tmp = mult(base, base);
	  base = tmp;
	}
    }

}

ring_elem Ring::power(const ring_elem gg, int n) const
{
  ring_elem ff = gg;
  if (n == 0) return one();
  if (n < 0)
    {
      n = -n;
      ff = invert(ff);
      if (is_zero(ff))
	{
	  ERROR("element is not invertible");
	  return ff;
	}
    }

  // The exponent 'n' should be > 0 here.
  ring_elem prod = from_int(1);
  ring_elem base = copy(ff);
  ring_elem tmp;

  for (;;)
    {
      if ((n % 2) != 0)
	{
	  tmp = mult(prod, base);
	  prod = tmp;
	}
      n >>= 1;
      if (n == 0)
	{
	  return prod;
	}
      else
	{
	  tmp = mult(base, base);
	  base = tmp;
	}
    }
}





void Ring::mult_to(ring_elem &f, const ring_elem g) const
{
  f = mult(f,g);
}

void Ring::add_to(ring_elem &f, ring_elem &g) const
{
  f = add(f,g);
}

void Ring::subtract_to(ring_elem &f, ring_elem &g) const
{
  f = subtract(f,g);
}

void Ring::negate_to(ring_elem &f) const
{
  f = negate(f);
}

ring_elem Ring::remainder(const ring_elem f, const ring_elem g) const
{
  if (is_zero(g))
    return f;
  return zero();
}

ring_elem Ring::quotient(const ring_elem f, const ring_elem g) const
{
  if (is_zero(g))
    return g;
  return divide(f,g);
}

ring_elem Ring::remainderAndQuotient(const ring_elem f, const ring_elem g, 
				     ring_elem &quot) const
{
  if (is_zero(g))
    {
      quot = g; // zero
      return f;
    }
  quot = divide(f,g);
  return zero();
}

int Ring::coerce_to_int(ring_elem) const
{
  ERROR("cannot coerce given ring element to an integer");
  return 0;
}

ring_elem Ring::from_double(double a) const
{
  mpz_t f;
  mpz_init(f);
  mpz_set_d(f,a);
  ring_elem result = from_int(f);
  mpz_clear(f);
  return result;
}

//ring_elem Ring::number_conversion(Ring *S, ring_elem fS)
//{
//  //     ZZ  QQ RR CC RRR CCC
//  // ZZ  id
//  // QQ  x
//  // RR  x   x
//  // CC  x   x  x  id  x
//  // RRR x   x  x  x   id
//  // CCC x   x  x  x   x   id
//}

//ring_elem Ring::from_rational(mpq_ptr q) const
//{
//#warning "not implemented yet"
//  return from_int(0);
//}

ring_elem Ring::from_BigReal(mpf_ptr a) const
{
#ifdef DEVELOPMENT
#warning "not implemented yet"
#endif
  return from_int(0);
}

ring_elem Ring::from_BigComplex(M2_CCC z) const
{
#ifdef DEVELOPMENT
#warning "not implemented yet"
#endif
  return from_int(0);
}

ring_elem Ring::from_complex(M2_CC z) const
{
#ifdef DEVELOPMENT
#warning "not implemented yet"
#endif
  return from_int(0);
}

ring_elem Ring::random() const
{
  ERROR("random scalar elements for this ring are not implemented");
  return 0;
}

ring_elem Ring::preferred_associate(ring_elem f) const
{
  // Here we assume that 'this' is a field:
  if (is_zero(f)) return from_int(1);
  return invert(f);
}

bool Ring::lower_associate_divisor(ring_elem &f, const ring_elem g) const
  // Implementation for a basic ring
{
  if (is_zero(f))
    {
      f = g;
      return !is_zero(f);
    }
  return true;
}

void Ring::monomial_divisor(const ring_elem a, int *exp) const
{
  // Do nothing
}

ring_elem Ring::diff(ring_elem a, ring_elem b, int use_coeff) const
{
  return mult(a,b);
}

bool Ring::in_subring(int nslots, const ring_elem a) const
{
  return true;
}

void Ring::degree_of_var(int n, const ring_elem a, int &lo, int &hi) const
{
  lo = 0;
  hi = 0;
}

ring_elem Ring::divide_by_var(int n, int d, const ring_elem a) const
{
  if (d == 0) return a;
  return from_int(0);
}

ring_elem Ring::divide_by_expvector(const int *exp, const ring_elem a) const
{
  return a;
}

ring_elem Ring::homogenize(const ring_elem f, int, int deg, M2_arrayint) const
{
  if (deg != 0) 
    ERROR("homogenize: no homogenization exists");
  return f;
}

ring_elem Ring::homogenize(const ring_elem f, int, M2_arrayint) const
{
  return f;
}

bool Ring::is_homogeneous(const ring_elem) const
{
  return true;
}

void Ring::degree(const ring_elem, int *d) const
{
  degree_monoid()->one(d);
}

bool Ring::multi_degree(const ring_elem f, int *d) const
  // returns true iff f is homogeneous
{
  degree_monoid()->one(d);
  return true;
}

void Ring::degree_weights(const ring_elem, M2_arrayint, int &lo, int &hi) const
{
  lo = hi = 0;
}
int Ring::index_of_var(const ring_elem a) const
{
  return -1;
}

M2_arrayint Ring::support(const ring_elem a) const
{
  M2_arrayint result = makearrayint(0);
  return result;
}


////////////////////////////////////////////
// Translation gbvector <--> ringelem/vec //
////////////////////////////////////////////
// This version is valid ONLY for base rings (not QQ!)
#if 0
// #include "gbring.hpp"
// Ring::trans_tag Ring::trans_type() const { return BASE; }
// 
// ring_elem Ring::trans_to_ringelem(ring_elem coeff, 
// 				  const int *exp) const
// {
//   return coeff;
// }
// 
// ring_elem Ring::trans_to_ringelem_denom(ring_elem coeff, 
// 					ring_elem denom, 
// 					int *exp) const
// {
//   // To use this, the corresponding ring MUST have division defined
//   return this->divide(coeff, denom);
// }
// 
// void Ring::trans_from_ringelem(gbvectorHeap &H, 
// 				    ring_elem coeff, 
// 				    int comp, 
// 				    int *exp,
// 				    int firstvar) const
// {
//   GBRing *GR = H.get_gb_ring();
//   const FreeModule *F = H.get_freemodule();
//   
//   gbvector *g = GR->gbvector_term_exponents(F, coeff, exp, comp);
// 
//   H.add(g);
// }
#endif

////////////////////////////////////////////
/////////////////////////////////////////////////////


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
