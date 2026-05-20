// Copyright 2019, The Macaulay2 team.

#ifndef __myalloc_hpp_
#define __myalloc_hpp_

/**
 * @file myalloc.hpp
 * @brief `AllocLogger` / `StatsAllocator` --- single-threaded debug/benchmark instrumentation.
 *
 * Declares `AllocLogger` with static counters (`mNumAllocs`,
 * `mAllocSize`, `mNumDeallocs`, `mCurrentAllocSize`,
 * `mHighWater`) and the `StatsAllocator` wrapper that bumps
 * those counters on every allocation / deallocation. The class
 * is intentionally minimal: counters are static so per-type
 * statistics share a collector, and there is no locking on the
 * counter increments (the in-source comment is explicit: "not
 * thread safe"). `StatsAllocator::allocate` / `deallocate` log
 * the size, then call straight through to `::operator new` /
 * `::operator delete`; there is no free-skipping debug mode.
 * It is for debugging and benchmarking only --- not safe in any
 * path the supervisor parallelises.
 *
 * Production allocations route through `newdelete.hpp`'s
 * `our_new_delete` (Boehm GC). The legacy `mem.hpp` size-class
 * `stash` allocator is now stubbed (its `new_elem` /
 * `delete_elem` short-circuit to `newarray_clear` / `freemem`),
 * so `AllocLogger` is the only opt-in counter source left ---
 * reach for it when you want to count allocations of a single
 * type in a benchmark run and dump the totals afterward.
 *
 * @see newdelete.hpp
 * @see mem.hpp
 */

#include <iostream>

// This class is static as it appears easiest if all allocator objects
// are essentially identical.  It could be a static member of StatsAllocator,
// but then each type T would have a different stats object.
//
/**
 * @brief Process-wide allocation counter used by `StatsAllocator` for
 * debugging and benchmarking.
 *
 * @details All counters (`mNumAllocs`, `mAllocSize`, `mNumDeallocs`,
 * `mCurrentAllocSize`, `mHighWater`) are `static`, so the logger
 * tracks every `StatsAllocator`-mediated allocation in the
 * process at once. `logAlloc` / `logDealloc` are called from the
 * allocator hooks; `reset()` zeroes everything for a fresh
 * measurement. Not thread-safe --- intended only for diagnostic
 * runs.
 */
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
