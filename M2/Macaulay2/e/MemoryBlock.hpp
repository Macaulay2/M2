#ifndef __memory_block_hpp__
#define __memory_block_hpp__

/**
 * @file MemoryBlock.hpp
 * @brief Bump-pointer arena allocator for transient inner-loop allocations.
 *
 * `MemoryBlock` is a thin wrapper around `memt::Arena` from the
 * `memtailor` submodule. `allocateArray<T>(nelems)` returns the
 * next `nelems * sizeof(T)` bytes as a `(begin, end)` pair;
 * `safeAllocateArray<T, MutexType>` does the same under a
 * caller-supplied scoped lock for shared-arena hot paths. Only
 * the top of the stack can be released: `freeTopArray(begin,
 * end)` unwinds the most-recent allocation, and
 * `shrinkLastAllocate` composes free-and-re-alloc to resize it.
 * `deallocateAll()` releases everything without destroying the
 * `MemoryBlock`; the destructor releases the underlying
 * `memt::Arena*`. This matches the "millions of tiny objects
 * allocated during one GB step, then all thrown away" shape of
 * the engine's hot paths in `f4/`, `gb-f4/`,
 * `schreyer-resolution/`, and `NCAlgebras/`.
 *
 * Memory comes from the system heap, not bdwgc, so anything
 * stored inside a `MemoryBlock` must be plain-old-data --- the
 * garbage collector will not scan these pages. The underlying
 * `allocArrayNoCon` also skips per-element constructor calls;
 * callers either initialise the bytes explicitly or treat them
 * as raw storage (encoded monomials, exponent vectors, S-pair
 * records).
 *
 * @see ExponentList.hpp
 * @see ExponentVector.hpp
 */

#include <memtailor/Arena.h>  // for Arena
#include <iostream>           // for operator<<, endl, basic_ostream, cout
#include <utility>            // for pair

/**
 * @brief Thin RAII wrapper around `memtailor::Arena` providing bump-pointer
 * array allocation with optional mutex protection.
 *
 * @details Heap-allocates the underlying `memt::Arena` so the wrapper has
 * a stable address (the `std::move` overloads are kept commented
 * out as a reminder that move semantics aren't currently
 * supported). `allocateArray<T>(n)` hands out a `[begin, end)`
 * pair pulled from the arena; `safeAllocateArray` does the same
 * under a caller-supplied mutex for use from `NCF4` /
 * `NCGroebner` worker threads. The arena is reset by destruction;
 * individual frees are not supported by design.
 */
class MemoryBlock
{
public:
  MemoryBlock() { mArena = new memt::Arena; }
  ~MemoryBlock() { delete mArena; }
  //MemoryBlock(MemoryBlock&& source) : mArena(std::move(source.mArena)) {}

  //MemoryBlock& operator=(MemoryBlock&& source) {
    // mArena = std::move(source.mArena);
    // return (*this);
  //}

  template<typename T>
  std::pair<T*, T*> allocateArray(size_t nelems)
  {
    return mArena->allocArrayNoCon<T>(nelems);
  }

  template<typename T, typename MutexType>
  std::pair<T*, T*> safeAllocateArray(size_t nelems, MutexType& mutex)
  {
    typename MutexType::scoped_lock myLock(mutex);
    return mArena->allocArrayNoCon<T>(nelems);
  }

  template<typename T>
  std::pair<T*, T*> shrinkLastAllocate(T* begin, T* end, T* newtop)
  {
    mArena->freeTopArray(begin, end);
    std::pair<T*, T*> result = mArena->allocArrayNoCon<T>(newtop - begin);
    if (result.first != begin) std::cout << "ooops: location changed" << std::endl;
    return result;
  }

  template<typename T>
  void freeTopArray(T* begin, T*end)
  {
    mArena->freeTopArray(begin,end);
  }

  void deallocateAll()
  {
    mArena->freeAllAllocs();
  }

  void swap(MemoryBlock& source) {
    memt::Arena* temp = mArena;
    mArena = source.mArena;
    source.mArena = temp;
  }

  size_t getMemoryUsedInBytes() { return mArena->getMemoryUse(); } 

private:
  memt::Arena* mArena;
};
  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
