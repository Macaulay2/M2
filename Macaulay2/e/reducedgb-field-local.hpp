// Copyright 2005, Michael E. Stillman

#ifndef _reducedgb_field_local_
#define _reducedgb_field_local_

#include "reducedgb-field.hpp"

class ReducedGB_Field_Local : public ReducedGB_Field
{
  friend ReducedGB *ReducedGB::create(const PolynomialRing *originalR0,
				      const FreeModule *F0,
				      const FreeModule *Fsyz0);
  // The polynomials themselves are in MinimalGB
  // The monomial ideals are in MinimalGB_Field
protected:
  vector<int, gc_allocator<int> > alpha;
  vector<int, gc_allocator<int> > deg;

  ReducedGB_Field_Local(GBRing *R0,
			const PolynomialRing *originalR0,
			const FreeModule *F0,
			const FreeModule *Fsyz0);

public:
  virtual ~ReducedGB_Field_Local();

  virtual void minimalize(const vector<POLY, gc_allocator<POLY> > &polys0);

  virtual void remainder(POLY &f, bool use_denom, ring_elem &denom);

  virtual void remainder(gbvector *&f, bool use_denom, ring_elem &denom);

};	     


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
