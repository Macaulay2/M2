// Copyright 2009 Michael E. Stillman

#ifndef __imonorder_h_
#define __imonorder_h_

/* This is the internal form of the monomial ordering */
/* Used in monomial encoding/decoding/comparison */

#include "interface/monomial-ordering.h"
#include <vector>

typedef int *exponents;
typedef int *monomial;

typedef int32_t
    deg_t;  // this is the integer type to use for degrees and weights

typedef const int *const_exponents;
typedef const int *const_monomial;

struct mo_block
{
  enum MonomialOrdering_type typ;
  int nvars;
  int nslots;
  int first_exp;
  int first_slot;
  int nweights;
  deg_t *weights;
};

struct MonomialOrder_rec
{
  int nvars;
  int nslots;
  int nblocks;
  int nblocks_before_component;
  int nslots_before_component;
  int component_up; /* bool */
  deg_t
      *degs; /* 0..nvars: heuristic degree of each variable.  degs[nvars] = 1.
                 Assumption: degs[i] >= 1, for all i, and should be an integer.
                 Any graded rev lex block assumes graded wrt these degrees. */
  struct mo_block *blocks; /* 0..nblocks-1 with each entry a struct mo_block */
  int *is_laurent; /* 0..nvars-1: 0 or 1: 1 means negative exponents allowed */
};

typedef struct MonomialOrder_rec MonomialOrder;
MonomialOrder *monomialOrderMake(const MonomialOrdering *mo);
void monomialOrderFree(MonomialOrder *mo);
void monomialOrderEncodeFromActualExponents(const MonomialOrder *mo,
                                            const_exponents a,
                                            monomial b);
void monomialOrderDecodeToActualExponents(const MonomialOrder *mo,
                                          const_monomial a,
                                          exponents b);

std::vector<bool> laurentVariables(const MonomialOrder* mo);
#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
