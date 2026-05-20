#ifndef _OverlapTable_hpp_
#define _OverlapTable_hpp_

/**
 * @file NCAlgebras/OverlapTable.hpp
 * @brief `OverlapTable` --- degree-sorted queue of pending word overlaps for non-commutative GB drivers.
 *
 * Declares the non-commutative analogue of the commutative
 * `SPair` set. An `Overlap = std::tuple<int, int, int, bool>`
 * is `(i, j, k, computed)` recording that a suffix of basis
 * word `i` matches a prefix of basis word `k` starting at
 * position `j` in `i`; the trailing `bool` flags whether the
 * corresponding S-polynomial has been computed yet. The
 * container is `OverlapMap =
 * std::map<std::pair<int, bool>, std::deque<Overlap>>` where
 * the **key bool is `isGenerator`** (distinguishing overlaps
 * coming from original input generators from overlaps derived
 * from basis-element pairs) --- not the per-`Overlap`
 * `computed` flag. `nextDegreeOverlaps` pops the lowest-degree
 * `std::deque` in one go, `insert(deg, isGenerator, o)`
 * appends, `removeLowestDegree` advances the cursor (with an
 * in-source TODO flagging a logic bug in the inhomogeneous
 * case), and `isFinished()` / `isFinished(topDegree)` are the
 * termination tests the `NCGroebner` / `NCF4` outer loop uses.
 *
 * Each Buchberger-style pair in the commutative case becomes
 * potentially many overlaps in the NC case, since a leading
 * word can overlap a partner at every suffix-prefix match
 * position. The class also stores a bare `ConstPolyList*
 * mPolyList` (received via the second constructor) for
 * callers that need to consult the actual polynomial data
 * --- the only method in this header that exposes overlap
 * metadata is `overlapWordLength`; the chain-criterion-style
 * pruning lives outside this class.
 *
 * @see WordTable.hpp
 * @see NCGroebner.hpp
 * @see NCF4.hpp
 * @see Polynomial.hpp
 */

#include "Polynomial.hpp"  // for ConstPolyList

#include <deque>           // for deque
#include <map>             // for map
#include <ostream>         // for ostream
#include <tuple>           // for tuple
#include <utility>         // for pair

// tuple is (i,j,k) where i is index of first word,
// j is position of overlap in first word and k is
// index of the second word
// the bool at the end determines whether the overlap still needs to be computed
using Overlap = std::tuple<int,int,int,bool>;

using OverlapMap = std::map<std::pair<int,bool>,std::deque<Overlap>>;
    
/**
 * @brief Per-degree FIFO queue of pending overlaps for the NC Groebner
 * basis driver to process.
 *
 * @details `Overlap = (firstIndex, position, secondIndex, computedFlag)`
 * encodes one prefix/suffix overlap of two `mPolyList` entries;
 * the table groups entries by `(degree, isGenerator)` in a
 * `std::map` so the driver can pull all overlaps of the current
 * degree in one shot via `nextDegreeOverlaps`. `insert` appends
 * (creating the bucket on demand) and `isFinished` /
 * `isFinished(topDegree)` ask whether any pending work remains
 * at or below a degree cap.
 */
class OverlapTable
{
public:
  friend std::ostream& operator<<(std::ostream& ostr, const OverlapTable& overlapTable);

  OverlapTable() : mPolyList(nullptr) {};
  OverlapTable(ConstPolyList* polyList) : mPolyList(polyList) {};

  // will call find to see if degree exists, and if not will call
  // insert.  If degree exists, append overlap to value of degree
  auto insert(int deg, bool isGenerator, Overlap o) -> void;
  
  // is the overlap map empty?
  auto isFinished() const -> bool;
  // is the overlap map empty in degrees <= topDegree?
  auto isFinished(int topDegree) const -> bool;

  // returns the lowest degree and a pointer to the overlaps
  // in that degree if isFinished returns true.  Otherwise
  // return (-1,nullptr).
  auto nextDegreeOverlaps() -> std::pair<int,std::deque<Overlap>*>;

  auto size() const -> size_t;

  auto removeLowestDegree() -> void; // TODO: logic BUG in inhomogeneous case: won't delete what we want!

  auto overlapWordLength(Overlap o) const -> int;

  // wipes out the entire data structure
  auto clear() -> void { mOverlapMap.clear(); };

  auto dump(std::ostream& ostr, bool outputDeques) const -> std::ostream&;

  OverlapMap& overlapMap() { return mOverlapMap; }
  
private:
  ConstPolyList* mPolyList;
  OverlapMap mOverlapMap;
};

std::ostream& operator<<(std::ostream& ostr, const OverlapTable& overlapTable);
auto operator<<(std::ostream& o, Overlap& a) -> std::ostream&;

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
