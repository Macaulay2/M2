/* Copyright (C) 2011 Bjarke Hammersholt Roune (www.broune.com)
   MemTailor is distributed under the Modified BSD License. See license.txt. */
#include "Arena.h"

#include <limits>

namespace memt {
  Arena Arena::_scratchArena;

  Arena::Arena() {
  }

  Arena::~Arena() {
    freeAllAllocsAndBackingMemory();
  }

  void Arena::freeAllAllocs() {
    while (block().hasPreviousBlock())
      _blocks.freePreviousBlock();
    block().clear();
  }

  void Arena::freeAllAllocsAndBackingMemory() {
    _blocks.freeAllBlocks();
#ifdef MEMT_DEBUG
    std::vector<void const*>().swap(_debugAllocs);
#endif
  }

  bool Arena::fromArena(void const* ptr) {
	Block const* block = _blocks.blockOf(ptr);
	if (block == 0)
	  return false;
	// check < instead of <= as we do not count pointers that are
	// one-past-the-end.
	return ptr < block->position();
  }

  void Arena::growCapacity(const size_t needed) {
    // ** Calcuate size of block (doubles capacity)
    size_t size = std::max(needed, block().getBytesInBlock());
    if (size > std::numeric_limits<size_t>::max() / 2)
      throw std::bad_alloc(); // size * 2 overflows
    size *= 2;
    const size_t minimumAlloc = 16 * 1024 - sizeof(Block) - 16;
    size = std::max(size, minimumAlloc); // avoid many small blocks
    size = MemoryBlocks::alignThrowOnOverflow(size);

    MEMT_ASSERT(size >= needed);
    MEMT_ASSERT(size % MemoryAlignment == 0);
    _blocks.allocBlock(size);
  }

  void Arena::freeTopFromOldBlock(void* ptr) {
    MEMT_ASSERT(ptr != 0);
    MEMT_ASSERT(block().empty());
    MEMT_ASSERT(block().hasPreviousBlock());

    Block* previous = block().previousBlock();
    MEMT_ASSERT(previous->isInBlock(ptr));
    previous->setPosition(ptr);
    if (previous->empty())
      _blocks.freePreviousBlock();
  }

  void Arena::freeAndAllAfterFromOldBlock(void* ptr) {
    MEMT_ASSERT(!block().isInBlock(ptr));
    MEMT_ASSERT(block().previousBlock() != 0);

    block().setPosition(block().begin());
    while (!(block().previousBlock()->isInBlock(ptr))) {
      _blocks.freePreviousBlock();
      MEMT_ASSERT(block().hasPreviousBlock()); // ptr must be in some block
    }

    MEMT_ASSERT(block().previousBlock()->isInBlock(ptr));
    block().previousBlock()->setPosition(ptr);
    if (block().previousBlock()->empty())
      _blocks.freePreviousBlock();
  }
}
