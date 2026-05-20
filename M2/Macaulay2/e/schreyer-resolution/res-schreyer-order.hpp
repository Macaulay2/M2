// Copyright 2016 Michael E. Stillman

#ifndef _res_schreyer_order_hpp_
#define _res_schreyer_order_hpp_

/**
 * @file schreyer-resolution/res-schreyer-order.hpp
 * @brief `ResSchreyerOrder` --- per-free-module-summand data implementing the Schreyer order on the next level.
 *
 * Declares the small struct each `SchreyerFrame` level carries
 * to define the order its child level inherits. For every
 * generator `i` of the current free module, `mTotalMonom[i]`
 * holds the encoded `res_packed_monomial` that the order will
 * compare against, and `mTieBreaker[i]` holds the
 * `component_index` used to break ties when two total
 * monomials agree. Comparison of two basis elements `e_i * m`
 * and `e_j * m'` walks `compare(mTotalMonom[i] * m,
 * mTotalMonom[j] * m')` under the ambient monoid order and
 * falls back to `mTieBreaker[i]` vs `mTieBreaker[j]` on
 * equality --- the standard Schreyer construction that lets
 * each homological level's leading-term arithmetic stay local.
 *
 * The in-file `Operations to include` comment block lists the
 * planned API surface (total-monomial / tie-breaker
 * construction, in-order checks, polynomial sorting, the
 * two-monomial comparator, and debug display); some of these
 * are filled in by `res-monomial-sorter.hpp` and
 * `res-schreyer-frame.hpp`, others remain open. The
 * resolution-specialised counterpart of the engine's general-
 * purpose `SchreyerOrder`, with plain `std::vector` storage
 * over `res_packed_monomial` instead of the gc-managed
 * top-level encoding.
 *
 * @see res-monomial-types.hpp
 * @see res-schreyer-frame.hpp
 * @see res-monomial-sorter.hpp
 * @see schorder.hpp
 */

#include "schreyer-resolution/res-monomial-types.hpp"  // for component_index
#include <vector>                                      // for vector

/**
 * @brief Per-level Schreyer-order data attached to a `SchreyerFrame::Level`.
 *
 * @details `mTotalMonom[i]` is the "total monomial" used as the comparison
 * key for the `i`-th frame element at this level: the underlying
 * monomial pre-multiplied by the parent-level base monomial.
 * `mTieBreaker[i]` carries the per-element tiebreaker (typically
 * the parent's `component_index`) so equal total monomials still
 * have a deterministic order. Together they form the inherited
 * data the next level's monomial order needs to behave
 * consistently across the resolution.
 */
struct ResSchreyerOrder
{
  std::vector<res_packed_monomial> mTotalMonom;
  std::vector<component_index> mTieBreaker;
  // keep a memory block for these monomials?  Probably...
};

// Operations to include:
//  . create total monomials and tiebreakers (or, just tiebreakers, given total
//  monomials?)
//  . check that a polynomial is in correct descending order w.r.t. this order
//  . sort a polynomial into this order
//  . provide a comparison operator for two monomials (not in total monomial
//  encoding)
//  . (debug) display data associated to this order

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
