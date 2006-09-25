// Copyright 2005, Michael E. Stillman

#ifndef _reducedgb_ZZ_hpp_
#define _reducedgb_ZZ_hpp_

#include "reducedgb.hpp"

class ReducedGB_ZZ : public ReducedGB
{
  friend ReducedGB *ReducedGB::create(const PolynomialRing *originalR0,
				      const FreeModule *F0,
				      const FreeModule *Fsyz0,
				      const GBWeight *wt0);

  enum divisor_type { DIVISOR_NONE, DIVISOR_RING, DIVISOR_MODULE};

  MonomialTableZZ *T;
  const MonomialTableZZ *ringtableZZ;

  enum divisor_type find_divisor(exponents exp, int comp, int &result_loc);

  ReducedGB_ZZ(GBRing *R0,
	       const PolynomialRing *originalR0,
	       const FreeModule *F0,
	       const FreeModule *Fsyz0);

public:

  virtual ~ReducedGB_ZZ();

  virtual void remove_gb() {}

  virtual void set_gb(VECTOR(POLY) &polys0);

  virtual void minimalize(const VECTOR(POLY) &polys0,
			  bool auto_reduced);
  // I have to decide: does this ADD to the existing set?

  virtual void remainder(POLY &f, bool use_denom, ring_elem &denom);
  // WARNING: this should only be used with term orders!
  // REALLY??
  virtual void remainder(gbvector *&f, bool use_denom, ring_elem &denom);

};	     

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
