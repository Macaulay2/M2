#pragma once

/**
 * @file gb-f4/MonomialView.hpp
 * @brief `newf4::MonomialView` --- non-owning view over a `[length, var_1, e_1, ...]`-encoded monomial.
 *
 * Declares the lightweight pointer-with-helpers type the new F4
 * passes around to refer to a monomial without copying its
 * encoded `MonomialInt` payload. The encoding is `mData[0] =
 * total length`, followed by alternating `(var, power)` pairs
 * over only the variables with positive exponent; equality is a
 * single `std::memcmp` of the tail, divisibility
 * (`monomialDivides`) is a single linear walk over the sparse
 * pairs, and `simpleDegree` is just the sum of powers. Four
 * constructors cover the non-allocating cases (raw pointer or
 * `std::vector`) plus the two allocating ones that copy from
 * `(std::vector, MemoryBlock&)` or `(MonomialView&, MemoryBlock&)`
 * into caller-supplied arena storage; the `combine` / `lcm` /
 * `product` / `quotient` static helpers route their result into
 * a `MemoryBlock` so the view returned points at fresh storage
 * with predictable lifetime.
 *
 * The class deliberately does not own its bytes: the same
 * encoded monomial gets referenced from `MonomialHashTable`
 * entries, polynomial-list rows, and S-pair LCM slots, and the
 * arena is reset at the end of each F4 step so every view is
 * invalidated together. The nested templated `MonomialIterator`
 * advances two `MonomialInt`s per `++` and exposes `var()` /
 * `power()` accessors so callers can read the sparse encoding
 * without indexing arithmetic.
 *
 * @see MemoryBlock.hpp
 * @see MonomialTypes.hpp
 * @see MonomialHashTable.hpp
 * @see MonomialLookupTable.hpp
 */

#include "../MemoryBlock.hpp"
#include "MonomialTypes.hpp"
#include <vector>
#include <functional>
#include <cstring> // For std::memcmp

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

  inline static bool monomialDivides(const MonomialView& divisor,
                                     const MonomialView& divisee)
  {
    if (divisor.size() > divisee.size()) return false;
    auto divisorIt = divisor.begin();
    auto diviseeIt = divisee.begin();
    auto divisorEnd = divisor.end();
    auto diviseeEnd = divisee.end();
    while (divisorIt != divisorEnd)
    {
      auto divisorVar = divisorIt.var();
      while (true)
      {
        if (diviseeIt == diviseeEnd) return false;
        if (diviseeIt.var() > divisorVar) return false;
        if (diviseeIt.var() == divisorVar) break;
        ++diviseeIt;
      }
      // at this point, both diviseeIt.var() and divisorIt.var() are equal
      if (divisorIt.power() > diviseeIt.power()) return false;
      ++divisorIt;
      ++diviseeIt;
    }
    return true;
  }
  
  // simple degree of a monomial view
  inline MonomialInt simpleDegree() const
  {
    MonomialInt result = 0;
    for (auto i = begin(); i != end(); ++i)
      result += i.power();
    return result;
  }

  static MonomialView combine(const MonomialView& left,
                              const MonomialView& right,
                              bool copyLeft,
                              bool copyRight,
                              std::function<int(int,int)>,
                              MemoryBlock &block);
  
  static MonomialView lcm(const MonomialView& left,
                          const MonomialView& right,
                          MemoryBlock &block);
  
  static MonomialView product(const MonomialView& left,
                              const MonomialView& right,
                              MemoryBlock &block);
  

  // returns left : right
  static MonomialView quotient(const MonomialView& left,
                               const MonomialView& right,
                               MemoryBlock &block);

  template< bool Const = false >
  class MonomialIterator 
  {
   public:
    using iterator_category = std::forward_iterator_tag;
    using reference = typename std::conditional_t< Const, MonomialInt const &, MonomialInt & >;
    using pointer = typename std::conditional_t< Const, MonomialInt const *, MonomialInt * >;    

   private:
    // points to the var part of a var,power sequence in a MonomialView
    pointer mCurLoc;  
    
   public:
    // constructor
    MonomialIterator(MonomialInt* curLoc) : mCurLoc(reinterpret_cast<pointer>(curLoc)) {}
    
    // iteration functions
    MonomialIterator& operator++()
    {
      // prefix ++ operator
      mCurLoc += 2;
      return *this;
    }
    // left out postfix operator to encourage prefix use
    MonomialIterator& operator+=(int offset)
    {
      mCurLoc += 2*offset;
      return *this;
    }

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

    pointer loc() const { return mCurLoc; }

    // (in)equality checks
    bool operator==(const MonomialIterator& rhs) const { return (mCurLoc == rhs.mCurLoc); }
    bool operator!=(const MonomialIterator& rhs) const { return (mCurLoc != rhs.mCurLoc); }

  };

  auto begin() const -> MonomialIterator<true> { return MonomialIterator<true>(mData + 1); }
  auto end() const -> MonomialIterator<true> { return MonomialIterator<true>(mData + size()); }
  auto begin() -> MonomialIterator<false> { return MonomialIterator<false>(mData + 1); }
  auto end() -> MonomialIterator<false> { return MonomialIterator<false>(mData + size()); }

  auto dataBegin() -> MonomialInt* { return mData; }
  auto dataEnd() -> MonomialInt* { return mData + size(); }
  auto dataBegin() const -> MonomialInt* { return mData; }
  auto dataEnd() const -> MonomialInt* { return mData + size(); }

  static void display(std::ostream& o,
                      const std::vector<std::string>& varnames,
                      const newf4::MonomialView& m
                      );
};

} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
