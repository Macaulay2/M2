// Copyright 1997 by Michael Stillman

// Mike, I replaced all uses of "long" by "int32", defined in targettypes.h, to make it work
// on machines where long can be longer.
#include "targettypes.h"
#include "../d/M2types.h"

#define Random RandomFoo

class RingElement;

class Random
{
  static int32 seed;
  static RingElement *maxint;
  static int32 maxint32;		// At the moment, this is the max bignum random
				// number that can be generated...

  // gmp random routines
  static gmp_randstate_t _st;
  static mpz_t _maxN;
public:
  static void i_random();
  static void set_seed(int32 s);
  static void set_max_int(RingElement *a);
  static RingElement *get_max_int();

  static int32 random0();	// Return a random number in range 0..2^31-2
  static int32 random0(int32 r);
  static inline int64 random0(int64 r) { 
    return random0((int32) r);	// not particularly correct! (drg)
  }
  static RingElement *random();

  static void set_max_int(M2_Integer N); // values will be in the range 0..N-1
  static void random_integer(M2_Integer a); // a should be an mpz_t which has been initialized
};


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
