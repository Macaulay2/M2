// Copyright 2019, The Macaulay2 team.

#ifndef __myalloc_hpp_
#define __myalloc_hpp_

#include <iostream>

// This class is static as it appears easiest if all allocator objects
// are essentially identical.  It could be a static member of StatsAllocator,
// but then each type T would have a different stats object.
//
// This class is meant for debugging/benchmark use only.
// This class is not thread safe.
//
// TODO: perhaps include mathicgb logging facility, or perhaps even boost.
// However, this is much simpler for the moment.
class AllocLogger
{
public:
  static size_t mNumAllocs;
  static size_t mAllocSize;
  static size_t mNumDeallocs;
  static long mCurrentAllocSize;
  static size_t mHighWater;
  
  static void reset() {
    mNumAllocs = 0;
    mAllocSize = 0;
    mNumDeallocs = 0;
    mCurrentAllocSize = 0;
    mHighWater = 0;
  }

  static void logAlloc(size_t num, size_t sz)
  {
    ++AllocLogger::mNumAllocs;
    AllocLogger::mAllocSize += num * sz;
    AllocLogger::mCurrentAllocSize += num * sz;
    if (AllocLogger::mCurrentAllocSize > AllocLogger::mHighWater)
      AllocLogger::mHighWater = AllocLogger::mCurrentAllocSize;
    //    std::cout << "allocating   " << num << " elements, each of size " << sz << std::endl;
  }

  static void logDealloc(size_t num, size_t sz)
  {
    ++AllocLogger::mNumDeallocs;
    AllocLogger::mCurrentAllocSize -= num * sz;
    // std::cout << "deallocating " << num << " elements, each of size " << sz << std::endl;
  }
};

std::ostream& operator<<(std::ostream& o, AllocLogger a);

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
    AllocLogger::logAlloc(num, sizeof(T));
    return static_cast<T*>(::operator new (num * sizeof(T)));
  }
  
  void deallocate(T* p, std::size_t num)
  {
    AllocLogger::logDealloc(num, sizeof(T));
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
