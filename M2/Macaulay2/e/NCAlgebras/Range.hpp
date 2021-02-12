#ifndef __range__
#define __range__

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
