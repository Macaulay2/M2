#pragma once

#include "../MemoryBlock.hpp"
#include "MonomialTypes.hpp"
#include <vector>

namespace newf4 {

class MonomialView
{
 private:
  MonomialInt* mData;  // We do not own the data pointed to
 public:
  explicit MonomialView(MonomialInt* data) : mData(data) {}
  explicit MonomialView(std::vector<MonomialInt>& data) : mData(data.data()) {}
  MonomialView(std::vector<MonomialInt> data, MemoryBlock& block)
  {
    MonomialView m(data.data());
    auto rng = block.allocateArray<MonomialInt>(m.size());
    mData = rng.first;
    std::copy(m.dataBegin(), m.dataEnd(), mData);
  }
  MonomialView(const MonomialView& m, MemoryBlock& block)
  {
    auto rng = block.allocateArray<MonomialInt>(m.size());
    mData = rng.first;
    std::copy(m.dataBegin(), m.dataEnd(), mData);
  }

  size_t size() const { return mData[0]; }

  bool operator==(const MonomialView& monom) const
  {
    if (size() != monom.size()) return false;
    
    // for loop version
    //for (auto i = 1; i < size(); ++i)
    //  if (mData[i] != monom.mData[i]) return false;
    //return true;

    // memcmp version
    return ((size() != 0) && ((std::memcmp(mData + 1,
                                           monom.mData + 1,
                                           sizeof(MonomialInt)*(size()-1)) == 0)));
  }

  template< bool Const = false >
  class MonomialIterator 
  {
  public:
    using iterator_category = std::forward_iterator_tag;
    using reference = typename std::conditional_t< Const, MonomialInt const &, MonomialInt & >;
    using pointer = typename std::conditional_t< Const, MonomialInt const *, MonomialInt * >;    

    // constructor
    MonomialIterator(MonomialInt* curLoc) : mCurLoc(reinterpret_cast<pointer>(curLoc)) {}
    
    // iteration functions
    MonomialIterator & operator++()
    {
      // prefix ++ operator
      stepIterator();
      return *this;
    }
    // left out postfix operator to encourage prefix use

    // accessor functions 

    // SFINAE enables the const dereference operator or the non 
    // const variant depending on bool Const parameter

    // const version
    template< bool _Const = Const >
    std::enable_if_t< _Const, reference >
    var() const {
       return *mCurLoc;
    }
    
    // non-const version
    template< bool _Const = Const >
    std::enable_if_t< !_Const, reference >
    var() {
       return *mCurLoc; 
    }

    // const version
    template< bool _Const = Const >
    std::enable_if_t< _Const, reference >
    power() const {
       return *(mCurLoc + 1);
    }

    // non-const version
    template< bool _Const = Const >
    std::enable_if_t< !_Const, reference >
    power() {
       return *(mCurLoc + 1); 
    }

    // not sure how to make this const/non-const as pairs can't hold references
    std::pair<MonomialInt, MonomialInt> operator*() const { return std::make_pair(var(), power()); }

    MonomialInt* loc() const { return mCurLoc; }

    // (in)equality checks
    bool operator==(const MonomialIterator& rhs) const { return (this->mCurLoc == rhs.mCurLoc); }
    bool operator!=(const MonomialIterator& rhs) const { return (this->mCurLoc != rhs.mCurLoc); }

  private:
    // points to the var part of a var,power sequence in a MonomialView
    pointer mCurLoc;  
    
    void stepIterator () { mCurLoc += 2; }
  };

  auto begin() const -> MonomialIterator<true> { return MonomialIterator<true>(mData + 1); }
  auto end() const -> MonomialIterator<true> { return MonomialIterator<true>(mData + size()); }
  auto begin() -> MonomialIterator<false> { return MonomialIterator<false>(mData + 1); }
  auto end() -> MonomialIterator<false> { return MonomialIterator<false>(mData + size()); }

  auto dataBegin() -> MonomialInt* { return mData; }
  auto dataEnd() -> MonomialInt* { return mData + size(); }
  auto dataBegin() const -> MonomialInt* { return mData; }
  auto dataEnd() const -> MonomialInt* { return mData + size(); }

};

} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
