// Copyright 1997 by Michael Stillman

#include "gmp.h"
#include "relem.hpp"

class Random
{
  static long seed;
  static RingElement maxint;
  static long maxlong;		// At the moment, this is the max bignum random
				// number that can be generated...
public:
  static void i_random();
  static void set_seed(long s);
  static void set_max_int(RingElement a);
  static RingElement get_max_int();

  static long random0();	// Return a random number in range 0..2^31-2
  static long random0(long r);
  static RingElement random();
};

