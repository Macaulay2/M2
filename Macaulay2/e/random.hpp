// Copyright 1997 by Michael Stillman

// Mike, I replaced all uses of "long" by "int32", defined in targettypes.h, to make it work
// on machines where long can be longer.
#include "targettypes.h"
#include "../d/M2types.h"

#define Random RandomFoo

class RingElement;

extern int32 RandomSeed;

class Random
{
  static RingElement *maxint;
  // gmp random routines
  static gmp_randstate_t state;
  static mpz_t maxN;
  static bool maxNisSmall;
  static int32 maxNint;
public:
  static void i_random();
  static RingElement *get_max_int();

  static int32 random0();	// Return a random number in range 0..2^31-2
  static int32 random0(int32 r);
  static inline int64 random0(int64 r) { 
    return random0(int32(r));	// not particularly correct! (drg)
  }
  static RingElement *random();


  static void random_integer(M2_Integer a); // a should be an mpz_t which has been initialized

  static int32 set_seed(M2_Integer s); // returns previous seed value
  static void set_max_int(M2_Integer N); // values will be in the range 0..N-1
  static M2_Integer get_random_integer_0(M2_Integer maxN);
  static M2_Integer get_random_integer(M2_Integer maxN);
};


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
