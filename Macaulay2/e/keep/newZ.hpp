// Copyright 1995 Michael E. Stillman.

#ifndef _myZ_hh_
#define _myZ_hh_

#include <gmp.h>

union Zelem
{
  mpz_t big;
  int   small;
};

union Qelem
{
  mpq_t big;
  int   small;
};

class myZ 
{
  static stash *Zstash;
  static Zelem make();
public:
  static Zelem ZERO;
  static Zelem ONE;

  static int sgn(Zelem a);	// Use to test for zero as well.
  static int is_large(Zelem a);	// Returns 0 if small, else returns number of limbs
  static int cmp(Zelem a, Zelem b);

  static void negate_to(Zelem &a);
  static void add_to(Zelem &a, Zelem b);
  static void subtract_to(Zelem &a, Zelem b);
  static void mult_to(Zelem &a, Zelem b);
  
  static Zelem negate(Zelem a);
  static Zelem add(Zelem a, Zelem b);
  static Zelem subtract(Zelem a, Zelem b);
  static Zelem mult(Zelem a, Zelem b);
  static Zelem div(Zelem a, Zelem b);
  static Zelem divmod(Zelem a, Zelem b, Zelem &rem);
  static Zelem power(Zelem a, int b);
  static Zelem gcd(Zelem a, Zelem b);
  static Zelem gcdExtended(Zelem a, Zelem b, Zelem &u, Zelem &v);

  static void add_to(Zelem &a, unsigned int b);
  static void subtract_to(Zelem &a, unsigned int b);
  static void mult_to(Zelem &a, unsigned int b);

  static Zelem add(Zelem a, unsigned int b);
  static Zelem subtract(Zelem a, unsigned int b);
  static Zelem mult(Zelem a, unsigned int b);
  static Zelem div(Zelem a, unsigned int b);
  static Zelem divmod(Zelem a, unsigned int b, unsigned int &rem);
  static int   gcd(Zelem a, unsigned int b);
  static int   gcdExtended(Zelem a, unsigned int b, Zelem &u, Zelem &v);

  //// Display and input ////
  static int size(Zelem a); // In number of decimal digits
  static void elem_text_out(ostream &o, Zelem a);
  static void bignum_text_out(ostream &o, Zelem a);

  static void elem_bin_out(ostream &o, Zelem a);
  static Zelem elem_bin_in(char *&s, int &len);

  //// To/from integers ////
  static Zelem fromInt(int a);
  static int toInt(Zelem a);	// Returns the smallest part of the integer.
				// Check using: is_large

  //// Memory management ////
  static Zelem copy(Zelem a);
  static void remove(Zelem &a);
};

void test_Z();
#endif
