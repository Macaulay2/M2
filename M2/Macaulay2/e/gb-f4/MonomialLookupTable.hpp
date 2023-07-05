//
// Created by Frank Moore on 7/5/23.
//

#pragma once

#include "MonomialTypes.hpp"
#include "MonomialView.hpp"
#include <vector>
#include <iostream>

namespace newf4 {

class MonomialInfo {
 public:
  bool mIsUsed;
  int mSimpleDegree;
  MonomialMask mMask;
  int mMonomialIndex;
  int mOffset;
};

class MonomialLookupTable
{
  friend class MonomialLookupIterator;
 private:
  std::vector<int> mMonomialSpace;
  std::vector<MonomialInfo> mMonomialInfo;

  // statistics info

 public:
  class MonomialLookupIterator
  {
    using IterType = decltype(mMonomialInfo.cbegin());

   private:
    const MonomialLookupTable& mTable;
    IterType mIter;

   public:
    MonomialLookupIterator(const MonomialLookupTable& table,
                           IterType iter)
        : mTable(table),
          mIter(iter)
    {
    }

   public:
    bool isUsed() { return mIter->mIsUsed; }
    int simpleDegree() { return mIter->mSimpleDegree; }
    uint64_t mask() { return mIter->mMask; }
    int monomialIndex() { return mIter->mMonomialIndex; }
    int offset() { return mIter->mOffset; }
  };
  // insert monomial(s) into table
  void insertMonomial(const MonomialView monView, int index);

  // compactify
  void compactify() {}

  // begin and end (both const and not)
  MonomialLookupIterator begin() const { return MonomialLookupIterator(*this,mMonomialInfo.begin()); }
  MonomialLookupIterator end() const { return MonomialLookupIterator(*this,mMonomialInfo.end()); }

  // find divisor
  int findDivisor(const MonomialView monView);

  // find divisors
  std::vector<int> findAllDivisors(const MonomialView monView);
  void findAllDivisors(const MonomialView monView, std::vector<int>& result);

  // find divisees
  std::vector<int> findAllDivisees(const MonomialView monView);
  void findAllDivisees(const MonomialView monView, std::vector<int>& result);

  // retire monomial(s)
  void retire(int monIndex) { mMonomialInfo[monIndex].mIsUsed = false; }
  std::vector<int> retireAllDivisees(const MonomialView monView);
  void retireAllDivisees(const MonomialView monView, std::vector<int>& result);

  // display
  std::ostream& display(std::ostream& ostr) const;

  // memoryUsed
  long memoryUsed() const;

 private:
  // mask creation
  // mask comparison
  // monomial divisibility
};

}  // namespace newf4