/* Copyright 2006 by Michael E. Stillman */
#pragma once

/**
 * @file f4/varpower-monomial.hpp
 * @brief F4's `(variable, exponent)` sparse-monomial `ExponentList` specialisation (legacy).
 *
 * Aliases `varpower_monomials = ExponentList<long, false>` and
 * the matching `index_varpower_monomial` iterator, then exposes
 * the F4-side `varpower_word` / `varpower_monomial` /
 * `const_varpower_monomial` vocabulary so F4 source files do
 * not have to spell out the template instantiation. The
 * underlying encoding is the standard `[length, v_1, e_1, ...,
 * v_r, e_r]` varpower layout inherited from `ExponentList.hpp`;
 * the `false` `legacy_length` template flag selects the
 * current length convention over the older one.
 *
 * The newer `gb-f4/` engine prefers its own `MonomialTypes`
 * and `MonomialView`; this header survives so the older `f4/`
 * subdirectory keeps building. Consumers include `f4.hpp`,
 * `f4-spairs.hpp` (S-pair LCM monomials), and `moninfo.hpp`
 * (the packed `MonomialInfo` layout shares the same encoding).
 *
 * @see ExponentList.hpp
 * @see ntuple-monomial.hpp
 * @see moninfo.hpp
 */

#include "ExponentList.hpp"

// Legacy specialization
using varpower_monomials = ExponentList<long, false>;
using index_varpower_monomial = ExponentListIterator<long, false>;

typedef varpower_monomials::Exponent varpower_word;
typedef varpower_word *varpower_monomial;
typedef const varpower_word *const_varpower_monomial;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
