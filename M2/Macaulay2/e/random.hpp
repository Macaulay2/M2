// Copyright 1997 by Michael Stillman

// Mike, I replaced all uses of "long" by "int32", defined in targettypes.h, to make it work
// on machines where long can be longer.
#include "../c2/targettypes.h"

#define Random RandomFoo

class RingElement;

class Random
{
  static int32 seed;
  static RingElement maxint;
  static int32 maxint32;		// At the moment, this is the max bignum random
				// number that can be generated...
public:
  static void i_random();
  static void set_seed(int32 s);
  static void set_max_int(RingElement a);
  static RingElement get_max_int();

  static int32 random0();	// Return a random number in range 0..2^31-2
  static int32 random0(int32 r);
  static inline int64 random0(int64 r) { 
    return random0((int32) r);	// not particularly correct! (drg)
  }
  static RingElement random();
};

