#ifndef __range__
#define __range__

/**
 * @file NCAlgebras/Range.hpp
 * @brief Home-rolled `std::span` substitute and zipped-range view for the NC engines.
 *
 * Declares the engine's C++17 stand-in for `std::span`.
 * `Range<T>` is a pair of `T*`s representing `[first, last)`
 * with `begin()` / `end()` accessors so it works in range-`for`,
 * plus constructors for raw pointer pairs, `std::pair<T*, T*>`
 * (the natural output of `MemoryBlock::allocateArray<T>`),
 * `std::vector<T>`, the engine's `VECTOR(T)` macro, and the
 * const-widening cross-type copy from `Range<S>`.
 * `PairRange<U, T>` couples two equal-sized ranges and exposes
 * `ZipIterator` / `ConstZipIterator` so a caller can walk
 * monomials and coefficients (or any other parallel arrays) in
 * a single loop without indexing arithmetic.
 *
 * Consumed by `NCF4` and `NCGroebner` for monomial / coefficient
 * row payloads and by `FreeAlgebra` whenever multiplication
 * yields a fresh stretch of arena-allocated terms. Bounds
 * checking is intentionally omitted --- the type stays exactly
 * as fast as raw pointers in the inner loop.
 *
 * @see MemoryBlock.hpp
 * @see NCF4.hpp
 * @see NCGroebner.hpp
 * @see FreeAlgebra.hpp
 */

#include "newdelete.hpp"
#include <utility>
#include <vector>

template<typename T>
class Range
{
private:
  T* mFirst;
  T* mLast;
public:
  Range() : mFirst(nullptr), mLast(nullptr) {}
  Range(T* first, T* last) : mFirst(first), mLast(last) {}
  Range(std::pair<T*, T*> a) : mFirst(a.first), mLast(a.second) {}

  template<class S> Range(const Range<S>& copy) : mFirst(copy.begin()), mLast(copy.end()) {}
  
  explicit Range(std::vector<T>& vec) : mFirst(vec.data()), mLast(vec.data() + vec.size()) {}
  explicit Range(VECTOR(T)& vec) : mFirst(vec.data()), mLast(vec.data() + vec.size()) {}

  int size() const { return mLast - mFirst; }
  T* begin() { return mFirst; }
  T* end() { return mLast; }

  const T* begin() const { return mFirst; }
  const T* end() const { return mLast; }

  T& operator[](int i) { return *(mFirst + i); }
  const T& operator[](int i) const { return *(mFirst + i); }
  
  const T* cbegin() const { return mFirst; }
  const T* cend() const { return mLast; }
};

template<typename U, typename T>
class ZipIterator
{
private:
  U* mPtr1;
  T* mPtr2;
public:
  ZipIterator(U* ptr1, T* ptr2) : mPtr1(ptr1), mPtr2(ptr2) {}

  std::pair<U,T> operator* () { return std::make_pair(*mPtr1,*mPtr2); }

  const ZipIterator& operator++ () { 
    ++mPtr1;
    ++mPtr2;
    return *this;
  }

  bool operator== (ZipIterator& it) { return (mPtr1 == it.mPtr1) && (mPtr2 == it.mPtr2); }
  bool operator!= (ZipIterator& it) { return (mPtr1 != it.mPtr1) && (mPtr2 != it.mPtr2); }
};

template<typename U, typename T>
class ConstZipIterator
{
private:
  const U* mPtr1;
  const T* mPtr2;
public:
  ConstZipIterator(const U* ptr1, const T* ptr2) : mPtr1(ptr1), mPtr2(ptr2) {}

  const std::pair<U,T> operator* () { return std::make_pair(*mPtr1,*mPtr2); }

  const ConstZipIterator& operator++ () { 
    ++mPtr1;
    ++mPtr2;
    return *this;
  }

  bool operator== (ConstZipIterator& it) { return (mPtr1 == it.mPtr1) && (mPtr2 == it.mPtr2); }
  bool operator!= (ConstZipIterator& it) { return (mPtr1 != it.mPtr1) && (mPtr2 != it.mPtr2); }
};

// both ranges *must* be the same size.  No bounds checking is done!!
template<typename U, typename T>
class PairRange
{
private:
  Range<U> mRange1;
  Range<T> mRange2;
public:
  PairRange(Range<U> range1, Range<T> range2) : mRange1(range1), mRange2(range2) {}

  int size() const { return mRange1.size(); }

  ZipIterator<U,T> begin() { return ZipIterator<U,T>(mRange1.begin(),mRange2.begin()); }
  ZipIterator<U,T> end() { return ZipIterator<U,T>(mRange1.end(),mRange2.end()); }

  ConstZipIterator<U,T> begin() const { return ConstZipIterator<U,T>(mRange1.cbegin(),mRange2.cbegin()); }
  ConstZipIterator<U,T> end() const { return ConstZipIterator<U,T>(mRange1.cend(),mRange2.cend()); }

  //ConstZipIterator<U,T> cbegin() const { return ConstZipIterator<U,T>(mRange1.cbegin(),
  //                                                                    mRange2.cbegin()); }
  //ConstZipIterator<U,T> cend() const { return ConstZipIterator<U,T>(mRange1.cend(),
  //                                                                  mRange2.cend()); }

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
