// Copyright 2014 Michael E. Stillman.

#ifndef _betti_hpp_
#define _betti_hpp_

#include "buffer.hpp"

#include "memtailor.h"
#include <unordered_set>
#include <unordered_map>
#include <utility>

class BettiDisplay
{
 public:
  BettiDisplay();  // sets everything to 0
  BettiDisplay(int lodegree,
               int hidegree,
               int hilen);              // initializes to all zero entries
  BettiDisplay(const BettiDisplay& B);  // copy values.
  BettiDisplay& operator=(const BettiDisplay& B);
  ~BettiDisplay();
  void swap(BettiDisplay& B);
  int& entry(int deg, int lev);  // use this to modify the value

  M2_arrayint getBetti() const;
  void displayBetti(buffer& o) const;
  void output() const;

  void resize(int new_lo_degree, int new_hi_degree, int new_length);
  // The following three functions are the given bounds
  // The actual bounds where there is a non-zero entry are possibly smaller
  int loDegree() const { return mLoDegree; }
  int hiDegree() const { return mHiDegree; }
  int length() const { return mHiLength; }
 private:
  // These two functions were in comp-res.cpp, that code was liftd directly to
  // here.
  static M2_arrayint betti_make(int lo, int hi, int len, int* bettis);
  static void betti_display(buffer& o, M2_arrayint ar);

  int mLoDegree;
  int mHiDegree;
  int mHiLength;
  int mNLevels;
  int* mValues;
};

class BettiHashAndEq
{
public:
  using value = std::pair<const int*, int>;

  // hash function
  std::size_t operator()(value m) const
  {
    return reinterpret_cast<std::size_t>(const_cast<int*>(m.first)) + 13*m.second;
  }

  // equality function
  bool operator() (value a, value b) const
  {
    return a == b;
  }

private:
  int mSize;
};

class MultigradedBettiDisplay
{
public:
  using monomial = const int *;
  MultigradedBettiDisplay(int degree_size) :
    mSize(degree_size),
    mHash(100,
          BettiHashAndEq(),
          BettiHashAndEq()
          )
  {
  }

  int insert(monomial deg, int lev); // adds one to the value here, or sets value to be 1.
  int value(monomial deg, int lev); // returns the current value (0 if not present).
  std::vector<int> flatten() const; // flattens the data into format: {value, level, deg (inline), ...}
private:
  int mSize; // size of a monomial
  std::unordered_map<std::pair<monomial,int>, int, BettiHashAndEq, BettiHashAndEq> mHash;
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
