// Copyright 2000  Michael E. Stillman
#ifndef _EEhilbtrack_hh_
#define _EEhilbtrack_hh_

#include "matrix.hpp"

class HilbertTracker
{
  const PolynomialRing *HRing;
  const RingElement hf_orig;

  Matrix lead_terms;
  RingElement hf_diff;
  int n_left;
  bool needs_recomputing;
public:
  HilbertTracker(const RingElement &hf_orig, const FreeModule *F);

  ~HilbertTracker();

  bool update(int deg);
  // Computes the value for n_left.  If interrupted, false is returned.

  void increment(vec v);
  // Places the lead term v into a matrix, and decrements n_left.

  int n_left_in_degree() { return n_left; }
  // Returns the number of GB elements expected in the current degree.
  // Valid after update returns a value of true.
};

#endif
