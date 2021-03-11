// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_RAW_VECTOR_GUARD
#define MATHICGB_RAW_VECTOR_GUARD

#include <iterator>
#include <cstddef>
#include <memory>
#include <utility>
#include <algorithm>
#include <stdexcept>
#include <cstring>

MATHICGB_NAMESPACE_BEGIN

/// RawVector mimics std::vector except that it does not do memory management.
/// So you can directly access the pointers to memory and you can replace them
/// with any other pointers that you need to.
///
/// Warning: RawVector is called raw because it does not allocate any memory,
/// so it is an error to insert elements that there is not enough space for -
/// you need to make sure ahead of time that there is enough space.
///
/// This class makes it possible to have a lot of the convenience of the
/// std::vector interface even in places where std::vector cannot be used
/// because of a need for manual memory management.
template<class T>
class RawVector {
public:
  typedef T& reference;
  typedef const T& const_reference;
  typedef T* iterator;
  typedef const T* const_iterator;
  typedef size_t size_type;
  typedef ptrdiff_t difference_type;
  typedef T value_type;
  typedef T* pointer;
  typedef const T* const_pointer;
  typedef std::reverse_iterator<iterator> reverse_iterator;
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

  /// Initializes to the null state.
  RawVector(): mBegin(0), mEnd(0), mCapacityEnd(0) {}

  /// Copies the pointers from v. It is a shallow copy.
  RawVector(const RawVector& v):
    mBegin(v.mBegin),
    mEnd(v.mEnd),
    mCapacityEnd(v.mCapacityEnd) {}

  /// Copies the pointers from v. It is a shallow copy. Sets v to a null state.
  RawVector(RawVector&& v):
    mBegin(v.mBegin),
    mEnd(v.mEnd),
    mCapacityEnd(v.mCapacityEnd)
  {
    v.releaseMemory();
  }

  RawVector(pointer begin, pointer end):
    mBegin(begin),
    mEnd(end),
    mCapacityEnd(end) {}

  RawVector(pointer begin, pointer end, pointer capacityEnd):
    mBegin(begin),
    mEnd(end),
    mCapacityEnd(capacityEnd) {}

  /// As clear() --- does not free memory.
  ~RawVector() {
    clear();
  }

  /// There is no operator== to avoid confusion about whether it compares
  /// pointers or contents. Instead, there is this.
  bool contentsEqual(const RawVector<T>& v) const {
    return std::equal(begin(), end(), v.begin());
  }

  /// Copies the pointers from v. It is a shallow copy.
  RawVector<T>& operator=(const RawVector& v) {
    mBegin = v.mBegin;
    mEnd = v.mEnd;
    mCapacityEnd = v.mCapacityEnd;
    return *this;
  }

  /// Copies the pointers from v. It is a shallow copy. Sets v to the null state.
  RawVector<T>& operator=(RawVector&& v) {
    mBegin = v.mBegin;
    mEnd = v.mEnd;
    mCapacityEnd = v.mCapacityEnd;
    v.releaseMemory();
    return *this;
  }

  iterator begin() {return mBegin;}
  const_iterator begin() const {return mBegin;}
  iterator end() {return mEnd;}
  const_iterator end() const {return mEnd;}

  reverse_iterator rbegin() {return reverse_iterator(end());}
  const_reverse_iterator rbegin() const {return const_reverse_iterator(end());}
  reverse_iterator rend() {return reverse_iterator(begin());}
  const_reverse_iterator rend() const {return const_reverse_iterator(begin());}

  size_type size() const {return mEnd - mBegin;}
  size_type max_size() const {return std::numeric_limits<size_type>::max();}

  /// There must be enough capacity for the new size.
  void resize(const size_type newSize) {
    MATHICGB_ASSERT(newSize <= capacity());
    while (newSize > size()) {
      new (mEnd) T();
      ++mEnd;
    }
    while (newSize < size()) {
      --mEnd;
      mEnd->~T();
    }
    MATHICGB_ASSERT(newSize == size());
  }

  size_type capacity() const {return mCapacityEnd - mBegin;}

  bool empty() const {return mBegin == mEnd;}

  reference operator[](size_type index) {
    MATHICGB_ASSERT(index < size());
    return mBegin[index];
  }

  const_reference operator[](size_type index) const {
    MATHICGB_ASSERT(index < size());
    return mBegin[index];
  }

  reference at(size_type index) {
    if (index >= size())
      throw std::out_of_range("Invalid index in RawVector::at.");
    return mBegin[index];
  }

  const_reference at(size_type index) const {
    if (index >= size())
      throw std::out_of_range("Invalid index in RawVector::at.");
    return mBegin[index];
  }

  reference front() {
    MATHICGB_ASSERT(!empty());
    return *mBegin;
  }

  const_reference front() const {
    MATHICGB_ASSERT(!empty());
    return *mBegin;
  }

  reference back() {
    MATHICGB_ASSERT(!empty());
    return *(mEnd - 1);
  }

  const_reference back() const {
    MATHICGB_ASSERT(!empty());
    return *(mEnd - 1);
  }

  size_t memoryUse() const {
    return capacity() * sizeof(value_type);
  }

  size_t memoryUseTrimmed() const {
    return size() * sizeof(value_type);
  }

  /// There must be enough capacity for the new size.
  template<class Iter>
  void rawAssign(Iter begin, Iter end) {
    const size_t count = std::distance(begin, end);
    MATHICGB_ASSERT(count <= capacity());
    if (count > size())
      resize(count);
    std::copy(begin, end, mBegin);
  }

  /// There must be enough capacity for the new size.
  void rawAssign(size_type count, const T& t) {
    MATHICGB_ASSERT(count <= capacity());
    if (count > size())
      resize(count);
    std::fill_n(begin(), count, t);
  }

  /// There must be enough capacity for the new size. This is why there is no
  /// push_back and only a rawPushBack --- to remind you of the changed
  /// contract.
  void rawPushBack(const T& t) {
    MATHICGB_ASSERT(size() < capacity());
    new (mEnd) T(t);
    ++mEnd;
  }

  void pop_back() {
    MATHICGB_ASSERT(!empty());
    mEnd->~T();
    --mEnd;
  }

  void swap(RawVector<T>& v) {
    std::swap(mBegin, v.mBegin);
    std::swap(mEnd, v.mEnd);
    std::swap(mCapacityEnd, v.mCapacityEnd);
  }

  void clear() {
    while (!empty())
      pop_back();
  }

  // **** Extra functionality not available on std::vector

  // memcpy onto the end of the vector.
  void memcpy(const T* from, size_t countOfT) {
    MATHICGB_ASSERT(countOfT <= capacityToGo());
    std::memcpy(mEnd, from, countOfT * sizeof(T));
    mEnd += countOfT;
  }

  /// Unused capacity.
  size_t capacityToGo() const {
    return mCapacityEnd - mEnd;
  }

  /// Returns true if there is no more capacity left. That is, if
  /// capacity() == size().
  bool atCapacity() const {
    return mEnd == mCapacityEnd;
  }

  /// Sets this object to its null state without destructing memory. Returns
  /// a pointer to the previous beginning of the buffer.
  pointer releaseMemory() {
    const auto oldBegin = mBegin;
    mBegin = 0;
    mEnd = 0;
    mCapacityEnd = 0;
    return oldBegin;
  }

  /// Takes over the new memory that is passed in, copies the old values to the
  /// new memory and destructs the old values. Returns a pointer to the previous
  /// beginning of the buffer.
  pointer setMemoryAndCopy(
    pointer begin,
    pointer capacityEnd
  ) {
    MATHICGB_ASSERT(size() <=
      static_cast<size_type>(std::distance(begin, capacityEnd)));
    const auto oldBegin = mBegin;
    const auto end = std::copy(mBegin, mEnd, begin);
    *this = RawVector<T>(begin, end, capacityEnd);
    return oldBegin;
  }

  /// Takes over the new memory that is passed in without destructing memory.
  /// Returns a pointer to the previous beginning of the buffer.
  pointer releaseAndSetMemory(
    pointer begin,
    pointer end,
    pointer capacityEnd
  ) {
    const auto oldBegin = mBegin;
    mBegin = begin;
    mEnd = end;
    mCapacityEnd = capacityEnd;
    return oldBegin;
  }

  /// Destructs memory and then takes over the new memory that is passed in.
  /// Does not deallocate the backing memory. Returns a pointer to the previous
  /// beginning of the buffer.
  pointer clearAndSetMemory(
    pointer begin,
    pointer end,
    pointer capacityEnd
  ) {
    clear();
    const auto oldBegin = mBegin;
    mBegin = begin;
    mEnd = end;
    mCapacityEnd = capacityEnd;
    return oldBegin;
  }

private:
  T* mBegin;
  T* mEnd;
  T* mCapacityEnd;
};

MATHICGB_NAMESPACE_END

namespace std {
  template<class T>
  void swap(mgb::RawVector<T>& a, mgb::RawVector<T>& b) {
    a.swap(b);
  }
}

#endif
