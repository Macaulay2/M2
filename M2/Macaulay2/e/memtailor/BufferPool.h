/* Copyright (C) 2011 Bjarke Hammersholt Roune (www.broune.com)
   MemTailor is distributed under the Modified BSD License. See license.txt. */
#ifndef MEMT_BUFFER_POOL_GUARD
#define MEMT_BUFFER_POOL_GUARD

#include "stdinc.h"
#include "MemoryBlocks.h"
#include <cstddef>

namespace memt {
  /** Allocator for allocating and freeing same-size buffers. Uses
      a free list. All allocations are automatically freed when
      the buffer pool is destructed. */
  class BufferPool {
  public:
    BufferPool(BufferPool&& pool);

    /** bufferSize is how many bytes are returned by each call to alloc. */
    BufferPool(size_t bufferSize);

    /** Returns a pointer to an array of getBufferSize() chars. The
        alignment is as for Arena. The lifetime of the buffer is until free
        is called with the returned value as parameter on this same object
        or clear() is called or this object is destructed.

        Do not pass the returned value to ::free, do not delete it and do
        not free it on a different BufferPool.  Throws an exception if no
        more memory can be allocated. Never returns null. */
    inline void* alloc();

    /** Makes the buffer at ptr available for reuse. ptr must be a value
        previously returned by alloc on this same object that hasn't been
        freed already since then. ptr must not be null. This method cannot
        throw an exception. */
    inline void free(void* ptr);

    /** Returns how many bytes are in each buffer. Can be a few bytes
        more than requested due to internal requirements on the size of the
        buffers. Will never be less than requested. */
    size_t getBufferSize() const {return _bufferSize;}

    /** Frees all allocated buffers. Does not deallocate all the
        internal backing memory, but may deallocate some of it. */
    void freeAllBuffers();

    /** Frees all allocated buffers and frees all internal backing
        memory too. */
    void freeAllBuffersAndBackingMemory();

    /** Returns the total amount of memory currently allocated by this
        object. Includes excess capacity that has not been allocated
        to a client yet. */
    size_t getMemoryUse() const {return _blocks.getMemoryUse();}

	/** Returns true if ptr is inside the memory area internally
	 allocated by this BufferPool. Only call this method on a valid
	 pointer. Note that pointers that have been allocated and then
	 freed on a pool are not guaranteed to be valid as the internal
	 backing memory could have been deallocated.

	 The intended use for this method is to assert that a given valid
	 pointer has been allocated from a given pool. It could also be
	 used to determine which of several pools a non-freed pointer
	 comes from, though needing to make this determination probably
	 indicates a design problem.

	 This method runs in logarithmic time in the maximum number of
	 allocations that have been live at the same time for this
	 pool. */
	bool fromPool(const void* ptr) const;

  private:
    BufferPool(const BufferPool&); // not available
    void operator=(const BufferPool&); // not available

    typedef MemoryBlocks::Block Block;
    Block& block() {return _blocks.getFrontBlock();}
    const Block& block() const {return _blocks.getFrontBlock();}

    /** Allocate another block of double the size. */
    void growCapacity();

    /** A node of the linked list of free buffers. */
    struct FreeNode {
      FreeNode* next;
    };

    const size_t _bufferSize; /// size of the buffers returned by alloc
    FreeNode* _free; /// null indicates that the free list is empty
    MemoryBlocks _blocks; /// internal backing memory
  };

  inline BufferPool::BufferPool(BufferPool&& pool):
    _bufferSize(pool._bufferSize),
    _free(pool._free),
    _blocks(std::move(pool._blocks))
  {
    pool._free = 0;
  }

  inline void* BufferPool::alloc() {
    void* ptr;
    if (_free != 0) {
      ptr = _free;
      _free = _free->next;
    } else {
      if (block().position() == block().end())
        growCapacity();
      ptr = block().position();
      block().setPosition(block().position() + getBufferSize());
    }
    return ptr;
  }

  inline void BufferPool::free(void* ptr) {
    MEMT_ASSERT(ptr != 0);
    MEMT_ASSERT(fromPool(ptr));
    FreeNode* node = reinterpret_cast<FreeNode*>(ptr);
    node->next = _free;
    _free = node;
  }
}

#endif
