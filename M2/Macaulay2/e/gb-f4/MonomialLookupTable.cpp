//
// Created by Frank Moore on 7/5/23.
//

#include "MonomialLookupTable.hpp"

namespace newf4 {

// insert monomial(s) into table
void MonomialLookupTable::insertMonomial(const MonomialView monView, int index)
{
  MonomialInfo newMonomialInfo;
  newMonomialInfo.mIsUsed = true;
  newMonomialInfo.mSimpleDegree = monView.simpleDegree();
  newMonomialInfo.mMask = createMask(monView);
  newMonomialInfo.mOffset = mMonomialSpace.size();
  mMonomialSpace.insert(mMonomialSpace.end(), monView.dataBegin(), monView.dataEnd());
  newMonomialInfo.mValue = index;
  mMonomialInfo.push_back(newMonomialInfo);
}

// compactify
void MonomialLookupTable::compactify()
{
}

// find divisor
int MonomialLookupTable::findDivisor(const MonomialView monView) const
{
  std::vector<int> result;
  findAllDivisors(monView, result, true);
  return (result.size() > 0 ? result[0] : -1);
}

// find divisors
std::vector<int> MonomialLookupTable::findAllDivisors(const MonomialView monView) const
{
  std::vector<int> result;
  findAllDivisors(monView, result);
  return result;
}

// adds all divisors to the end of result.
void MonomialLookupTable::findAllDivisors(const MonomialView monView,
					  std::vector<int>& result,
					  bool stopAtOne) const
{
  int curIndex = 0;
  MonomialMask mask = createMask(monView);
  for (; curIndex < mMonomialInfo.size(); ++curIndex)
  {
    auto m = mMonomialInfo[curIndex];
    if (m.mIsUsed && maskDivides(m.mMask,mask))
    {
       // ugh with the const_cast, but MonomialInt doesn't store const pointers
       MonomialView mvDivisor(const_cast<MonomialInt*>(mMonomialSpace.data() + m.mOffset));
       if ((monView.simpleDegree() >= m.mSimpleDegree) &&
	   (MonomialView::monomialDivides(mvDivisor, monView)))
       {
	  result.push_back(curIndex);
	  if (stopAtOne) break;
       }
    }
  }
}

// find divisees
std::vector<int> MonomialLookupTable::findAllDivisees(const MonomialView monView) const
{
  std::vector<int> result;
  findAllDivisees(monView,result);
  return result;
}

void MonomialLookupTable::findAllDivisees(const MonomialView monView, std::vector<int>& result) const
{
  int curIndex = 0;
  MonomialMask mask = createMask(monView);
  for (; curIndex < mMonomialInfo.size(); ++curIndex)
  {
    auto m = mMonomialInfo[curIndex];
    if (m.mIsUsed && maskDivides(mask,m.mMask))
    {
       // ugh with the const_cast, but MonomialInt doesn't store const pointers
       MonomialView mvDivisee(const_cast<MonomialInt*>(mMonomialSpace.data() + m.mOffset));
       if ((monView.simpleDegree() <= m.mSimpleDegree) &&
	   (MonomialView::monomialDivides(monView, mvDivisee)))
       {
	  result.push_back(curIndex);
       }
    }
  }
}

// retire monomial(s)
std::vector<int> MonomialLookupTable::retireAllDivisees(const MonomialView monView)
{
  std::vector<int> result;
  retireAllDivisees(monView,result);
  return result;
}

void MonomialLookupTable::retireAllDivisees(const MonomialView monView, std::vector<int>& result)
{
  findAllDivisees(monView,result);
  for (auto m : result) retire(m);
}

// display
std::ostream& MonomialLookupTable::display(std::ostream& ostr) const
{
  return ostr;
}

// memoryUsed
long MonomialLookupTable::memoryUsed() const
{
  return 0;
}

}  // namespace newf4
