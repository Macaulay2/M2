// Copyright 2005, Michael E. Stillman

#ifndef _marked_gb_
#define _marked_gb_

#include "reducedgb.hpp"

// Marked GB's: for now, they must be defined over a polynomial ring
// with no quotient elements, and the base must be field.
// AND the monomial order needs to be a term order.
//
// Sorting is NOT done

class MarkedGB : public ReducedGB
{
  friend ReducedGB *ReducedGB::create(const PolynomialRing *originalR0,
				      const FreeModule *F0,
				      const FreeModule *Fsyz0,
				      const GBWeight *wt0);
protected:
  MonomialTable *T;
  gbvector **leadterms;

  MarkedGB(
	   const PolynomialRing *originalR0,
	   const FreeModule *F0,
	   const FreeModule *Fsyz0);

  void auto_reduce();
public:
  static MarkedGB *create(
	   const PolynomialRing *originalR0,
	   const FreeModule *F0,
	   const FreeModule *Fsyz0);

  virtual ~MarkedGB();

  virtual void remove_gb() {}

  virtual void set_gb(VECTOR(POLY) &polys0);

  virtual void add_marked_elems(const VECTOR(gbvector *) &leadterms,
				const VECTOR(POLY) &polys0,
				bool auto_reduced);

  void marked_remainder(POLY &f, bool use_denom, ring_elem &denom, gbvector *marked_lead_term);
  // Do not reduce the marked_lead_term.  The coefficient of this term might be modified.
  // But it will still point to the same gbvector node.

  virtual void remainder(POLY &f, bool use_denom, ring_elem &denom);

  virtual void remainder(gbvector *&f, bool use_denom, ring_elem &denom);

  void geo_remainder(gbvector *&f, bool use_denom, ring_elem &denom);

  virtual const MatrixOrNull *get_initial(int nparts);

  virtual const MatrixOrNull *get_parallel_lead_terms(M2_arrayint w);

};	     

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
