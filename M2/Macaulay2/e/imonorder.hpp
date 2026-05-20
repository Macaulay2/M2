// Copyright 2009 Michael E. Stillman

#ifndef __imonorder_h_
#define __imonorder_h_

/**
 * @file imonorder.hpp
 * @brief Internal (runtime) form of a monomial ordering.
 *
 * Defines the encoded representation walked by the inner loop when two
 * monomials are compared --- the operational counterpart to the declarative
 * `MonomialOrdering` the user writes. The internal form is a list of
 * `mo_block`s; each block has a type (`MO_LEX`, `MO_GREVLEX`,
 * `MO_WEIGHTS`, `MO_REVLEX`, `MO_POSITION_UP`/`DOWN`, ...), a variable
 * count, and block-specific data. A comparison walks the block list and
 * returns at the first block that breaks the tie. Encoded monomials are
 * laid out so that the common case --- a leading degree or weight sum ---
 * resolves after reading a single slot, and many orderings reduce to a
 * `memcmp` on the packed bytes.
 *
 * Translation from the declarative `MonomialOrdering` to this internal
 * form happens once, at `Monoid` construction; the resulting
 * `MonomialOrder*` is then frozen and reused on every inner-loop compare.
 * Degrees and weights are 32-bit signed (`deg_t`); intermediate-degree
 * overflows are caught by `overflow.hpp`.
 *
 * @see interface/monomial-ordering.h
 * @see monordering.hpp
 * @see overflow.hpp
 */

#include <vector>

#include "interface/monomial-ordering.h"

#include "ExponentVector.hpp"

typedef int *monomial;

typedef int32_t
    deg_t;  // this is the integer type to use for degrees and weights

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

/**
 * @brief Internal compiled form of a monomial ordering, derived from a
 * front-end `MonomialOrdering` by `monomialOrderMake`.
 *
 * @details Holds the variable count (`nvars`), the encoded monomial word
 * width (`nslots`), the ordered list of `mo_block`s that make up
 * the order, the per-variable heuristic degree vector (`degs`),
 * and a `is_laurent[i]` flag per variable. `component_up` plus
 * the `*_before_component` slot counts decide where the
 * component slot sits in the encoded monomial. The companion
 * encode / decode functions translate between actual exponent
 * vectors and the packed monomial words the engine uses for fast
 * comparisons.
 */
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
                                          exponents_t b);

std::vector<bool> laurentVariables(const MonomialOrder* mo);
#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
