#ifndef __imonorder_h_
#define __imonorder_h_

/* This is the internal form of the monomial ordering */
/* Used in monomial encoding/decoding/comparison */

#include "engine.h"
#include "monordering.h"

typedef int * exponents;
typedef int * monomial;

typedef const int * const_exponents;
typedef const int * const_monomial;

struct mo_block {
  enum MonomialOrdering_type typ;
  int nvars;
  int nslots;
  int first_exp;
  int first_slot;
  int nweights;
  int *weights;
};

struct MonomialOrder_rec {
  int nvars;
  int nslots;
  int nblocks;
  int nblocks_before_component;
  int nslots_before_component;
  int component_up; /* bool */
#if 0
//   int component_slot; /* < 0 means that there is no component (or, it is assumed to be 0) */
#endif
  int *degs; /* 0..nvars: heuristic degree of each variable.  degs[nvars] = 1.
		   Assumption: degs[i] >= 1, for all i, and should be an integer.
		   Any graded rev lex block assumes graded wrt these degrees. */
  struct mo_block *blocks; /* 0..nblocks-1 with each entry a struct mo_block */
  int *is_laurent; /* 0..nvars-1: 0 or 1: 1 means negative exponents allowed */
#if 0
//   int *max_exponent; /* 0..nvars-1: maximum value for this exponent */
//   int *min_exponent; /* 0..nvars-1: minimum value for this exponent (often 0) */
//   int *slot_kind;    /* 0..nslots-1: 0 means normal int, 1 means packed 2, 2 means packed 4. */
#endif
};

typedef struct MonomialOrder_rec MonomialOrder;

#if defined(__cplusplus)
extern "C" {
#endif
  MonomialOrder *monomialOrderMake(const MonomialOrdering *mo);
  void monomialOrderFree(MonomialOrder *mo);
  void monomialOrderEncode(const MonomialOrder *mo, const_exponents a, monomial b);
  void monomialOrderDecode(const MonomialOrder *mo, const_monomial a, exponents b);

  int monomialOrderFromActualExponents(const MonomialOrder *mo, 
				       const_exponents expon, 
				       exponents result_exp);

  int monomialOrderToActualExponents(const MonomialOrder *mo, 
				     const_exponents expon, 
				     exponents result_exp);
  
#if defined(__cplusplus)
}
#endif


#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
*/
