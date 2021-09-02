/* Copyright (C) 2011 Bjarke Hammersholt Roune (www.broune.com)
   MemTailor is distributed under the Modified BSD License. See license.txt. */
#include "MemoryBlocks.h"

namespace memt {

  void MemoryBlocks::freeAllPreviousBlocks() {
    while (_block.hasPreviousBlock())
      freePreviousBlock();
  }

  void MemoryBlocks::freeAllBlocks() {
    freeAllPreviousBlocks();
    _block.free();
    _block.makeNull();
  }

  void MemoryBlocks::Block::newBlock(size_t capacityInBytes) {
    if (!isNull()) {
      // We set aside space for a block at the end of the memory. Use that
      // space to store the block for the old memory.
      const size_t blockStructOffset = alignNoOverflow(getBytesInBlock());
      Block* block = reinterpret_cast<Block*>(begin() + blockStructOffset);
      block->_previous = this->previousBlock();
      block->_begin = begin();
      block->_end = end();
      block->_position = position();
      _previous = block;
    }

    // make space for block information at end of new memory, but do not
    // use it yet.
    const size_t aligned = alignThrowOnOverflow(capacityInBytes);
    const size_t total = aligned + sizeof(Block);
    if (total < aligned) // check overflow
      throw std::bad_alloc();
    _begin = new char[total];
    _position = _begin;
    _end = _begin + capacityInBytes;

    MEMT_ASSERT(!isNull());
    MEMT_ASSERT(empty());
    MEMT_ASSERT(capacityInBytes == getBytesInBlock());
  }

  size_t MemoryBlocks::getMemoryUse() const {
    size_t sum = 0;
    const Block* block = &_block;
    do {
      sum += block->getBytesInBlock();
      block = block->previousBlock();
    } while (block != 0);
    return sum;
  }

  size_t MemoryBlocks::getMemoryUseToLeft() const {
    size_t sum = 0;
    const Block* block = &_block;
    do {
      sum += block->getBytesToLeft();
      block = block->previousBlock();
    } while (block != 0);
    return sum;
  }

  void MemoryBlocks::Block::freePrevious() {
    MEMT_ASSERT(hasPreviousBlock());
    Block* previousPrevious = previousBlock()->previousBlock();
    previousBlock()->free();
    _previous = previousPrevious;
  }

  MemoryBlocks::Block* MemoryBlocks::blockOf(const void* ptr) {
	if (ptr == 0)
	  return 0; // avoid saying that null is in a null block
    Block* block = &_block;
    do {
	  if (block->isInBlock(ptr))
		return block;
      block = block->previousBlock();
    } while (block != 0);
    return 0;
  }
}
