// Copyright 2014 Michael E. Stillman.

#ifndef _betti_hpp_
#define _betti_hpp_

#include <iostream>
#include <vector>

class BettiDisplay
{
public:
  BettiDisplay(); // sets everything to 0
  BettiDisplay(int lodegree, int hidegree, int hilen); // initializes to all zero entries
  BettiDisplay(const BettiDisplay& B); // copy values.
  BettiDisplay& operator=(const BettiDisplay& B);
  ~BettiDisplay();
  void swap(BettiDisplay& B);
  int& entry(int deg, int lev); // use this to modify the value

  std::vector<int> getBetti() const;
  void displayBetti(std::ostream& o) const;
  void output() const;
private:
  // These two functions were in comp-res.cpp, that code was liftd directly to here.
  static std::vector<int> betti_make(int lo, int hi, int len, int *bettis);
  static void betti_display(std::ostream& o, const std::vector<int>& ar);

  int mLoDegree;
  int mHiDegree;
  int mHiLength;
  int mNLevels;
  int* mValues;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions "
// indent-tabs-mode: nil
// End:
