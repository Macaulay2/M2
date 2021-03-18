#include "NCAlgebras/OverlapTable.hpp"
#include "engine-exports.h"  // for M2_gbTrace
#include <iostream>          // for cout

// will call find to see if degree exists, and if not will call
// insert.  If degree exists, append overlap to value of degree
auto OverlapTable::insert(int deg, bool isGenerator, Overlap o) -> void
{
  if (M2_gbTrace >= 3)
    {
      std::cout << "overlapTable: inserting deg="
                << deg
                << " isGenerator=" << isGenerator
                << " overlap=" << o
                << std::endl;
    }
  std::deque<Overlap> emptyDeque;
  auto ret = mOverlapMap.insert(std::make_pair(std::make_pair(deg,isGenerator),
                                               emptyDeque));
  (ret.first)->second.push_back(o);
}
  
// is the overlap map empty?
auto OverlapTable::isFinished() const -> bool
{
  return mOverlapMap.empty();
}

// is the overlap map empty in degrees <= topDegree?
auto OverlapTable::isFinished(int topDegree) const -> bool
{
  auto beginIter = mOverlapMap.begin();
  if (beginIter == mOverlapMap.end()) return true;
  return (beginIter->first.first > topDegree);
}

// returns the lowest degree and a pointer to the overlaps
// in that degree if isFinished returns false.  Otherwise
// return (-1,nullptr).
auto OverlapTable::nextDegreeOverlaps() -> std::pair<int,std::deque<Overlap>*>
{
  auto iter = mOverlapMap.begin();
  if (iter == mOverlapMap.end()) return std::make_pair(-1,nullptr);
  return std::make_pair(iter->first.first, &(iter->second));
}

auto OverlapTable::size() const -> size_t
{
  size_t sum = 0;
  for (auto& i : mOverlapMap)
    {
      sum += i.second.size();
    }
  return sum;
}

auto OverlapTable::removeLowestDegree() -> void
{
  auto iter = mOverlapMap.begin();
  if (iter == mOverlapMap.end()) return;
  mOverlapMap.erase(iter);
}

auto operator<<(std::ostream& o, Overlap& a) -> std::ostream&
{
  o << "[" << std::get<0>(a) << ","
    << std::get<1>(a) << ","
    << std::get<2>(a) << "]";
  return o;
}

std::ostream& operator<<(std::ostream& o, const std::deque<Overlap>& val)
{
  int count = 0;
  for (auto a : val)
    {
      o << a;
      o << ",";
      count++;
      if (count % 10 == 0) o << std::endl;
    }
  return o;
}

auto OverlapTable::dump(std::ostream& ostr, bool outputDeques) const -> std::ostream&
{
  ostr << "OverlapTable with " << size() << " elements:" << std::endl;
  for (auto i : mOverlapMap)
    {
      if (!outputDeques)
        ostr << "  Degree [" << i.first.first
             << "," << i.first.second << "] # = " << i.second.size() << std::endl;
      else
        ostr << "  Degree [" << i.first.first
             << "," << i.first.second << "] # = " << i.second << std::endl;
    }
  ostr << std::endl;
  return ostr;
}

std::ostream& operator<<(std::ostream& ostr, const OverlapTable& overlapTable)
{
  return overlapTable.dump(ostr,false);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
