// Copyright 2005, Michael E. Stillman

#ifndef _reducedgb_field_local_
#define _reducedgb_field_local_

#include "reducedgb-field.hpp"
class GBWeight;

/**
    @ingroup reducedgb

    @brief Computation of a reduced GB w.r.t. a local order, over a field
*/
class ReducedGB_Field_Local : public ReducedGB_Field
{
  friend ReducedGB *ReducedGB::create(const PolynomialRing *originalR0,
                                      const FreeModule *F0,
                                      const FreeModule *Fsyz0,
                                      const GBWeight *wt0);
  // The polynomials themselves are in MinimalGB
  // The monomial ideals are in MinimalGB_Field
 protected:
  struct divisor_info
  {
    POLY g;
    int size;
    int alpha;
  };

  MonomialTable *T1;  // elements added in
  const GBWeight *wt;
  VECTOR(divisor_info) ring_elems;  // these do not need to be saved.
  VECTOR(divisor_info)
  gb_elems;  // these polynomials are the same as in superclass
  VECTOR(divisor_info) new_poly_elems;  // polynomials here need to be freed

  VECTOR(int) alpha;         // for GB array
  VECTOR(int) ring_alpha;    // for quotient ring elements
  VECTOR(int) newpol_alpha;  // These next two are local values...
  VECTOR(POLY) newpol;

  bool find_good_divisor(exponents_t h_exp,
                         int h_comp,
                         int h_deg,
                         int &h_alpha,          // result value
                         POLY &result_g,        // result value
                         int &result_g_alpha);  // result value

  void reset_table();

  void store_in_table(const POLY &h, exponents_t h_exp, int h_comp, int h_alpha);

  ReducedGB_Field_Local(GBRing *R0,
                        const PolynomialRing *originalR0,
                        const FreeModule *F0,
                        const FreeModule *Fsyz0,
                        const GBWeight *wt0);

 public:
  virtual ~ReducedGB_Field_Local();

  virtual void remove_gb() {}
  virtual void minimalize(const VECTOR(POLY) & polys0,
                          bool auto_reduced);  // last argument is ignored

  virtual void remainder(POLY &f, bool use_denom, ring_elem &denom);

  virtual void remainder(gbvector *&f, bool use_denom, ring_elem &denom);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
