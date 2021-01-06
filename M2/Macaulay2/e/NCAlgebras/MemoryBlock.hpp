#ifndef __memory_block_hpp__
#define __memory_block_hpp__

#include <memtailor/Arena.h>  // for Arena
#include <iostream>           // for operator<<, endl, basic_ostream, cout
#include <utility>            // for pair

class MemoryBlock
{
public:
  template<typename T>
  std::pair<T*, T*> allocateArray(size_t nelems)
  {
    return mArena.allocArrayNoCon<T>(nelems);
  }

  template<typename T>
  std::pair<T*, T*> shrinkLastAllocate(T* begin, T* end, T* newtop)
  {
    mArena.freeTopArray(begin, end);
    std::pair<T*, T*> result = mArena.allocArrayNoCon<T>(newtop - begin);
    if (result.first != begin) std::cout << "ooops: location changed" << std::endl;
    return result;
  }

  void deallocateAll()
  {
    mArena.freeAllAllocs();
  }

  size_t getMemoryUsedInBytes() { return mArena.getMemoryUse(); } 

  //void moveFrom(MemoryBlock& source) {
  //  
  //}

private:
  memt::Arena mArena;
};
  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
