// Copyright 1996 Michael E. Stillman.
#include "newZ.hh"

static int is_small(Zelem a) { return a.small & 1; }
static int small_val(Zelem a) { return a.small >> 1; }

static int overflow(int a, int b, int c)
{
  return (a > 0 && b > 0 && c < 0)
	 ||  (a < 0 && b < 0 && c > 0);
}

static int TOPBIT = 1 << 31;

static int can_be_small(mpz_t a)
{
  return (a->_mp_size <= 1 && a->_mp_size >= -1)
    && !(a->_mp_d[0] & TOPBIT);	// MES: does this miss one value?
}

int myZ::sgn(Zelem a)	// Use to test for zero as well.
{
  if (a.small & 1) 
    {
      int b = a.small >> 1;
      if (b < 0) return -1;
      if (b > 0) return 1;
      return 0;
    }
  else return mpz_sgn(a.big);
}

int myZ::is_large(Zelem a);	// Returns 0 if small, else returns number of limbs

int myZ::cmp(Zelem a, Zelem b)
{
  if (a.small & 1)
    {
      if (b.small & 1)
	{
	  int c = a.small = b.small;
	  if (c > 0) return 1;
	  if (c < 0) return -1;
	  return 0;
	}
      else
	{
	  return - mpz_cmp_si(b.big, a.small >> 1);
	}
    }
  else if (b.small & 1)
    {
      return mpz_cmp_si(a.big, b.small >> 1);
    }
  else 
    {
      return mpz_cmp(a.big, b.big);
    }
}

void myZ::negate_to(Zelem &a);
void myZ::add_to(Zelem &a, Zelem b);
void myZ::subtract_to(Zelem &a, Zelem b);
void myZ::mult_to(Zelem &a, Zelem b);

Zelem myZ::negate(Zelem a);
Zelem myZ::add(Zelem a, Zelem b)
{
  if (resultval.big == NULL)
    resultval new_elem();	// Inits the mpz_t as well

  Zelem result;
  if (is_small(a))
    {
      if (is_small(b))
	{
	  int c = (a ^ 1) + b;
	  if (overflow(a^1, b, c))
	    return fromInt((a >> 1) + (b >> 1));
	  else
	    return c;
	}
      else
	{
	  mpz_init(result.big);
	  mpz_add_si(result, b.big, a.small >> 1);
	}
    }
  else 
    {
      mpz_init(result.big);
      if (is_small(b))
	  mpz_add_si(result, a.big, b.small);
      else 
	  mpz_add(result, a.big, b.big);
    }
  if (can_be_small(result))
    {
      return make_small();
    }

  Zelem result = resultval;
  resultval.big = NULL;
  return result;
}

#if 0
Zelem myZ::subtract(Zelem a, Zelem b);
Zelem myZ::mult(Zelem a, Zelem b);
Zelem myZ::div(Zelem a, Zelem b);
Zelem myZ::divmod(Zelem a, Zelem b, Zelem &rem);
Zelem myZ::power(Zelem a, int b);
Zelem myZ::gcd(Zelem a, Zelem b);
Zelem myZ::gcdExtended(Zelem a, Zelem b, Zelem &u, Zelem &v);

void myZ::add_to(Zelem &a, unsigned int b);
void myZ::subtract_to(Zelem &a, unsigned int b);
void myZ::mult_to(Zelem &a, unsigned int b);

Zelem myZ::add(Zelem a, unsigned int b);
Zelem myZ::subtract(Zelem a, unsigned int b);
Zelem myZ::mult(Zelem a, unsigned int b);
Zelem myZ::div(Zelem a, unsigned int b);
Zelem myZ::divmod(Zelem a, unsigned int b, unsigned int &rem);
int   myZ::gcd(Zelem a, unsigned int b);
int   myZ::gcdExtended(Zelem a, unsigned int b, Zelem &u, Zelem &v);

//// Display and input ////
int myZ::size(Zelem a); // In number of decimal digits
void myZ::elem_text_out(ostream &o, Zelem a);
void myZ::bignum_text_out(ostream &o, Zelem a);

void myZ::elem_bin_out(ostream &o, Zelem a);
Zelem myZ::elem_bin_in(char *&s, int &len);

//// To/from integers ////
Zelem myZ::fromInt(int a);
int myZ::toInt(Zelem a);	// Returns the smallest part of the integer.

//// Memory management ////
Zelem myZ::copy(Zelem a);
void myZ::remove(Zelem &a);
#endif

void test_Z()
{

}
