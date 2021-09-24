#ifndef __memory_block_hpp__
#define __memory_block_hpp__

#include <memtailor/Arena.h>  // for Arena
#include <iostream>           // for operator<<, endl, basic_ostream, cout
#include <utility>            // for pair

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

  template<typename T, typename LockType>
  std::pair<T*, T*> safeAllocateArray(size_t nelems, LockType& lock)
  {
    typename LockType::scoped_lock myLock(lock);
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
