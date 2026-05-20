// TODO: move this file: these are interface routines

#ifndef __monordering_hpp_
#define __monordering_hpp_

/**
 * @file monordering.hpp
 * @brief `MonomialOrderings` --- C++ factories for the declarative `MonomialOrdering` blocks.
 *
 * Declares the static-only `MonomialOrderings` class whose
 * members build single `MonomialOrdering` blocks (`Lex` /
 * `Lex2` / `Lex4`, `GRevLex` / `GRevLex2` / `GRevLex4` with or
 * without a weight vector --- plus a packed
 * `GRevLex(wts, packing)` overload --- `RevLex`, `Weights`,
 * `GroupLex`, `GroupRevLex`, and the component-ordering
 * markers `PositionUp` / `PositionDown`), and compose them
 * (`product`, `join`) into the multi-block orderings the user
 * actually writes. `toString` renders an ordering back to the
 * textual form M2 displays. The free function
 * `monomialOrderingToMatrix` (defined in
 * `interface/monomial-ordering.cpp`) flattens an ordering into
 * an integer matrix together with `base_is_revlex` and
 * component-direction / row-position metadata --- the form
 * downstream code uses when it needs the order as data.
 *
 * This is the user-facing declarative side of the
 * monomial-order story --- what the user types and what M2
 * serialises --- paired with the operational `MonomialOrder`
 * produced by `imonorder.hpp`. A `Monoid` constructor consumes
 * the declarative output of these factories once at ring
 * construction and walks the encoded form thereafter.
 *
 * @see imonorder.hpp
 * @see monoid.hpp
 */

#include <string>
#include <vector>

/**
 * @brief Static-method namespace of constructors for the front-end
 * `MonomialOrdering` value type.
 *
 * @details Holds no state --- every method is `static`. Provides factories
 * for the standard monomial orders (`Lex`, `GRevLex`, `RevLex`,
 * `Weights`, `Position`, ...), variants with 2-byte and 4-byte
 * exponent packings (`Lex2` / `Lex4`, `GRevLex2` / `GRevLex4`),
 * weighted forms, plus the combinators `join` (lex-of-blocks)
 * and `product` (tensor-product order). `toString(mo)` renders a
 * `MonomialOrdering*` back to its M2 source form.
 */
class MonomialOrderings {
public:
  static std::string toString(const MonomialOrdering *mo);
  static MonomialOrdering* join(const std::vector<MonomialOrdering*>& M);
  static MonomialOrdering* product(const std::vector<MonomialOrdering*>& M);
  static MonomialOrdering* Lex(int nvars);
  static MonomialOrdering* Lex2(int nvars);
  static MonomialOrdering* Lex4(int nvars);
  static MonomialOrdering* GRevLex(int nvars);
  static MonomialOrdering* GRevLex2(int nvars);
  static MonomialOrdering* GRevLex4(int nvars);
  static MonomialOrdering* GRevLex(const std::vector<int>& wts);
  static MonomialOrdering* GRevLex2(const std::vector<int>& wts);
  static MonomialOrdering* GRevLex4(const std::vector<int>& wts);
  static MonomialOrdering* RevLex(int nvars);
  static MonomialOrdering* Weights(const std::vector<int>& wts);
  static MonomialOrdering* GroupLex(int nvars);
  static MonomialOrdering* GroupRevLex(int nvars);
  static MonomialOrdering* PositionUp();
  static MonomialOrdering* PositionDown();

  static MonomialOrdering* GRevLex(const std::vector<int>& wts, int packing);
};

// This is currently located in interface/monomial-ordering.cpp
bool monomialOrderingToMatrix(
    const struct MonomialOrdering &mo,
    std::vector<int> &mat,
    bool &base_is_revlex,
    int &component_direction,      // -1 is Down, +1 is Up, 0 is not present
    int &component_is_before_row);  // -1 means at the end, 0 means before the
                                   // order, and r means considered before row
                                   // 'r' of the matrix.

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
