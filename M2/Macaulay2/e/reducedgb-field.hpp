// Copyright 2005, Michael E. Stillman

#ifndef _reducedgb_field_
#define _reducedgb_field_

#include "reducedgb.hpp"

class ReducedGB_Field : public ReducedGB
{
protected:
  MonomialTable *T;
  const MonomialIdeal *Rideal;
public:
  ReducedGB_Field(GBRing *R0,
		  const PolynomialRing *originalR0,
		  const FreeModule *F0,
		  const FreeModule *Fsyz0);

  virtual ~ReducedGB_Field();

  virtual void set_gb(vector<POLY, gc_allocator<POLY> > &polys0);

  virtual void minimalize(const vector<POLY, gc_allocator<POLY> > &polys0);
  // I have to decide: does this ADD to the existing set?

  virtual void remainder(POLY &f, bool use_denom, ring_elem &denom);

  virtual void remainder(gbvector *&f, bool use_denom, ring_elem &denom);

};	     

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
