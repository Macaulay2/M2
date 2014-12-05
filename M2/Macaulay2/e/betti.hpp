// Copyright 2014 Michael E. Stillman.

#ifndef _betti_hpp_
#define _betti_hpp_

#include "buffer.hpp"

class BettiDisplay
{
public:
  BettiDisplay(int lodegree, int hidegree, int hilen); // initializes to all zero entries
  ~BettiDisplay();

  int& entry(int deg, int lev); // use this to modify the value

  M2_arrayint getBetti();
  void displayBetti(buffer& o);
private:
  // These two functions were in comp-res.cpp, that code was liftd directly to here.
  static M2_arrayint betti_make(int lo, int hi, int len, int *bettis);
  static void betti_display(buffer &o, M2_arrayint ar);

  int mLoDegree;
  int mHiDegree;
  int mHiLength;
  int mNLevels;
  int* mValues;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
