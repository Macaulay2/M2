#pragma once

/**
 * @file gb-f4/SPairs.hpp
 * @brief `newf4::SPair` / `SPairSet` --- typed S-pair queue grouped by degree and S-pair flavour.
 *
 * Declares the per-pair record and the queue the refactored F4
 * will use to schedule the matrix it builds each degree. `SPair`
 * tags each entry with an `SPairType` --- `Ring` (between a
 * basis element and a ring-relation generator, for GB over a
 * quotient), `Exterior` (the `var * lead = 0` cancellations
 * exterior algebras require), `SPair` (between two basis
 * elements), or `Gen` (an original input still pending
 * insertion) --- and carries the basis indices `mFirst` /
 * `mLast`, the LCM's `MonomialIndex mLCM` into the S-pair
 * monomial hash table, the cached `MonomialInt mDegree` of the
 * LCM, and a still-tentative `mQuotient` field flagged "we
 * might not need" in the source.
 *
 * Storage is `std::map<std::pair<long, SPairType>,
 * vector<SPair>> mSPairsByDegree` (the `long` is sugar degree),
 * so the driver can pop the next degree's work in one chunk via
 * `getNextDegree`, and so within each degree the `std::map`
 * ordering puts `Ring` / `Exterior` ahead of `SPair` / `Gen`
 * (the enum is declared in that order) --- the standard
 * "lighter reductions first" pattern.
 *
 * This header is part of the long-running F4 refactor noted in
 * `TODO-refactor-f4`. The companion `SPairs.cpp` is currently
 * **empty**, so `updatePairs(basis, which)`,
 * `getNextDegree()`, and the placeholder `SPairIterator {}`
 * class are declared but have no bodies; the planned two-stage
 * "pre-S-pair / S-pair" split sketched in the in-file
 * brainstorming comments is still on the drawing board.
 *
 * @see Basis.hpp
 * @see MonomialView.hpp
 * @see MonomialTypes.hpp
 * @see MacaulayMatrix.hpp
 * @see GBF4Computation.hpp
 */

#include "MonomialTypes.hpp"
#include "MonomialView.hpp"
#include "Basis.hpp"

#include <map>

// Each spair's monomials will be included in a special "hash table".
// What about "pre-spairs"?  Are they in their own, or not yet inserted?
// Maybe try both?

// Inserting new spairs.
//   Making pre-spairs
//   Sort them
//   Find minimal elements (try via masks, via MonomialIdeal type structure, and maybe some mathic structures?)
//   insert each spair directly.
//   
// Grabbing a set of spairs to do (but if interrupted, don't lose the pairs!)
//   sort them? at least by degree...
//   how to remove a pair that isn't required? (the more complicated spair approach).

// Maybe what I should do before meeting Frank:
//  Translate Matrix to Basis.
//  Translate Basis to Matrix.
// Keep free module, ring around.

// Then: Frank and I can do:
//  1A. macaulay matrix build (maybe mostly use NCGB code)
//  1B. linear algebra part (use NCGB code)
//  2. spair part (this part can be based on f4 code)
//  3. tracing/QQ part (this part is new)

// Want: allow exterior algebra, maybe Weyl algebra.
//       allow quotient rings
//       of course allow modules

namespace newf4 {

// SPairType::Ring means between a ring element and GB element
// SPairType::Exterior corresponds to the fact that variable * lead monomial = 0
// SPairType::SPair means an SPair between 2 GB elements
// SPairType::Gen means this SPair is one of the initial generators

enum class SPairType { Ring, Exterior, SPair, Gen };

struct SPair
{
  SPairType mType;
  Index mFirst;               // index in Basis itself. later in gb indexing.
  Index mLast;
  MonomialIndex mLCM;
  // ComponentIndex mComponent;  // component of the SPair (may not need this, as it matches comp of mLast/mFirst)
  MonomialInt mDegree;        // degree of the LCM
  MonomialIndex mQuotient;    // we might not need
};

class SPairSet
{
 public:
  // What needs to be input here?
  SPairSet() = default;

  /// updatePairs: takes an element of the GB
  /// and computes all spairs needed with previous elements
  /// in the basis (and ring elements, and exterior pairs too),
  /// and adds them to 'this.mSPairs'.
  void updatePairs(const Basis& B, Index which);

  // If we keep them sorted in increasing degree, we can
  // grab the next set pretty easily.  How to remove them?
  auto getNextDegree() -> std::pair<MonomialInt, std::vector<SPair>>;
  // or maybe have an iterator for it?
  // we should make sure we don't "lose" those returned in case of interruption
  // should be an iterator to avoid this and make it easier to loop over.
  // also pass in a maximum number to return in a "chunk"

  class SPairIterator {};

  // sort in increasing degree, SPairType::Ring and SPairType::Exterior
  // before SPairType::SPair before SPairType::Gen and in each degree by LCM?

 private:
  // std::vector<SPair> mSPairs;
  // std::vector<std::vector<SPair>> mSPairsByDegree; ??

  // the long here may not be actual degree, but a "sugar" degree
  std::map<std::pair<long,SPairType>,std::vector<SPair>> mSPairsByDegree;
};

} // end namespace newf4

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
