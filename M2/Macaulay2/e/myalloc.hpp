#ifndef __myalloc_hpp_
#define __myalloc_hpp_

#include <iostream>

class StatsAlloc
{
public:
  static size_t mNumAllocs;
  static size_t mAllocSize;
  static size_t mNumDeallocs;
  static size_t mCurrentAllocSize;
  static size_t mHighWater;
  
  void reset() {
    mNumAllocs = 0;
    mAllocSize = 0;
    mNumDeallocs = 0;
    mCurrentAllocSize = 0;
    mHighWater = 0;
  }
};

std::ostream& operator<<(std::ostream& o, StatsAlloc a);

/* The following code is adapted from the book
 * "The C++ Standard Library - A Tutorial and Reference"
 * by Nicolai M. Josuttis, Addison-Wesley, 2nd edition, 2012.
 */

template <typename T>
class StatsAllocator {
public:
  typedef T value_type;
  
  StatsAllocator() noexcept {}
  
  template <typename U>
  StatsAllocator(const StatsAllocator<U>&) noexcept {}
  
  T* allocate(size_t num)
  {
    ++StatsAlloc::mNumAllocs;
    StatsAlloc::mAllocSize += num * sizeof(T);
    StatsAlloc::mCurrentAllocSize += num * sizeof(T);
    if (StatsAlloc::mCurrentAllocSize > StatsAlloc::mHighWater)
      StatsAlloc::mHighWater = StatsAlloc::mCurrentAllocSize;
    // std::cout << "allocating   " << num << " elements, each of size " << sizeof(T) << std::endl;
    return static_cast<T*>(::operator new(num * sizeof(T)));
  }
  
  void deallocate(T* p, std::size_t num)
  {
    ++StatsAlloc::mNumDeallocs;
    StatsAlloc::mCurrentAllocSize -= num * sizeof(T);
    // std::cout << "deallocating " << num << " elements, each of size " << sizeof(T) << std::endl;
    ::operator delete(p);
  }
};

// all specializations of this allocator are interchangeable
template <class T1, class T2>
bool operator== (const StatsAllocator<T1>&,
                 const StatsAllocator<T2>&) noexcept {
  return true;
}
template <class T1, class T2>
bool operator!= (const StatsAllocator<T1>&,
                 const StatsAllocator<T2>&) noexcept {
  return false;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
