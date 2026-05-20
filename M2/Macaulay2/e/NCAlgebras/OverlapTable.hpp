#ifndef _OverlapTable_hpp_
#define _OverlapTable_hpp_

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
