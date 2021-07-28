/* Copyright (C) 2011 Bjarke Hammersholt Roune (www.broune.com)
   MemTailor is distributed under the Modified BSD License. See license.txt. */
#ifndef MEMT_MEMORY_BLOCKS_GUARD
#define MEMT_MEMORY_BLOCKS_GUARD

#include "stdinc.h"
#include <cstddef>
#include <memory>

namespace memt {
  /** Handles a linked list of blocks of memory. Intended for use in
      implementing other memory allocators.  */
  class MemoryBlocks {
  public:
    class Block;

    /// Moves ownership of data and leaves the parameter in a
    /// valid empty state.
    MemoryBlocks(MemoryBlocks&& blocks);

    /** Makes the front block a null block object. No memory is
        allocated. */
    MemoryBlocks() {}

    /** Makes the front block have the given capacity. */
    MemoryBlocks(size_t capacityInBytes) {allocBlock(capacityInBytes);}

    /** Frees all blocks. */
    ~MemoryBlocks() {freeAllBlocks();}

    /** Create a new front block with the given capacity. If the
        previous front block is not null, it becomes the previous block of
        the new front block. This invalidates all block pointers. */
    Block& allocBlock(size_t capacityInBytes);

    /** Frees the block previous to the front block. The block before
        that, if any, becomes the new front block. There must be a previous
        block to call this method. */
    void freePreviousBlock() {_block.freePrevious();}

    /** Frees all blocks except the front block which is not changed. */
    void freeAllPreviousBlocks();

    /** Frees all blocks including the front block. The new front block
        will be a null block object. */
    void freeAllBlocks();

    /** Returns the current front block. Can be a null block object. */
    Block& getFrontBlock() {return _block;}

    /** Returns the current front block. Can be a null block object. */
    Block const& getFrontBlock() const {return _block;}

	/** Returns true if ptr is in some block of this object. Blocks that
     have been freed do not count. Note that stray pointers may randomly
     happen to hit a block. */
    bool inSomeBlock(void* ptr) const {return blockOf(ptr) != 0;}

    /** Returns the block that contains ptr, or null if there is no
	 such block. Always returns null if ptr is null, even though
     strictly speaking a null pointer will be in the range of
     a null block. */
    Block* blockOf(const void* ptr);
    const Block* blockOf(const void* ptr) const {
	  return const_cast<MemoryBlocks&>(*this).blockOf(ptr);
    }

    /** Returns the total amount of memory allocated by this object. */
    size_t getMemoryUse() const;

    /** Returns the total amount of memory in the left parts of the
     blocks of this object. */
    size_t getMemoryUseToLeft() const;

    /** Rounds value up to the nearest multiple of MemoryAlignment. This
        rounded up value must be representable in a size_t. */
    inline static size_t alignNoOverflow(size_t value);

    /** Rounds value up to the nearest multiple of
        MemoryAlignment. Throw std::bad_alloc if this rounded up value is
        not representable in a size_t. */
    inline static size_t alignThrowOnOverflow(size_t value);

    /** A block owns a range of memory [begin(), end()) and keeps track
        of a position within the range [begin(), end()] given by
        position(). It is possible for a block to own no memory, in which
        case begin(), end() and position are null - we call this a null
        block object. Do not create your own blocks - their lifetime is
        handled by MemoryBlocks. */
    class Block {
    public:
      char* begin() {return _begin;}
      char const* begin() const {return _begin;}
      char* end() {return _end;}
      char const* end() const {return _end;}
      char* position() {return _position;}
      char const* position() const {return _position;}
      inline void setPosition(const void* position);

      /** Returns true if ptr is in the range [begin(), end()). Be aware
       that this implies that the return value is true when ptr is null
       and the block is a null. */
      inline bool isInBlock(const void* ptr) const;

      /** Returns the number of bytes in the range [begin(), end()). */
      size_t getBytesInBlock() const {return _end - _begin;}

      /** Returns the number of bytes in the range [position(), end()). */
      size_t getBytesToRight() const {return _end - _position;}

      /** Returns the number of bytes in the range [begin(), position()). */
      size_t getBytesToLeft() const {return _position - _begin;}

      /** Returns true if position() == begin(). */
      bool empty() const {return position() == begin();}

      Block* previousBlock() {return _previous;}
      Block const* previousBlock() const {return _previous;}

      /** Returns true if previousBlock() is not null. */
      bool hasPreviousBlock() const {return _previous != 0;}

      /** Returns true this is a null block object. That is, if begin(),
          end() and position() are all null. */
      bool isNull() const {return begin() == 0;}

      /** Sets position to begin(). */
      void clear() {setPosition(begin());}

    private:
      friend class MemoryBlocks;

      /// Moves ownership of memory and leaves block in a valid null state.
      Block(Block&& block):
        _begin(block._begin),
        _position(block._position),
        _end(block._end),
        _previous(block._previous) {
        block.makeNull();
      }

      Block() {makeNull();}
      Block(size_t capacity, Block* previous);
      Block(Block const&); // unavailable
      void operator=(Block const&); // unavailable

      /// Makes this a null block object. Does NOT free the owned memory!
      void makeNull() {
        _previous = 0;
        _begin = 0;
        _position = 0;
        _end = 0;
      }

      /** Frees the memory for this block. */
      void free() {delete[] begin();}

      /** Frees the memory for the previous block. */
      void freePrevious();

      /** Makes new memory for this block and puts the old memory in a
          block previous to this block. */
      void newBlock(size_t capacity);

      char* _begin; /// beginning of current block (aligned)
      char* _position; /// pointer to first free byte (aligned)
      char* _end; /// one past last byte (aligned)
      Block* _previous; /// null if no previous block
    };

  private:
    MemoryBlocks(const MemoryBlocks&); // not available
    void operator=(const MemoryBlocks&); // not available

    Block _block;
  };

  inline MemoryBlocks::MemoryBlocks(MemoryBlocks&& blocks):
    _block(std::move(blocks._block)) {}

  inline MemoryBlocks::Block& MemoryBlocks::allocBlock(size_t capacityInBytes) {
    _block.newBlock(capacityInBytes);
    return _block;
  }

  inline void MemoryBlocks::Block::setPosition(void const* position) {
    MEMT_ASSERT(position == end() || isInBlock(position));
    _position = const_cast<char*>(reinterpret_cast<char const*>(position));
  }

  inline size_t MemoryBlocks::alignNoOverflow(const size_t value) {
    // this function might look big, but the total compiled code size is
    // one addition and one bitwise and.
    const size_t decAlign = MemoryAlignment - 1; // compile time constant

    MEMT_ASSERT((MemoryAlignment & (decAlign)) == 0); // power of 2
      // This works because MemoryAlignment is a power of 2.
	const size_t aligned = (value + decAlign) & (~decAlign);

    MEMT_ASSERT(aligned % MemoryAlignment == 0); // alignment
    MEMT_ASSERT(aligned >= value); // no overflow
    MEMT_ASSERT(aligned - value < MemoryAlignment); // adjustment minimal
    return aligned;
  }

  inline size_t MemoryBlocks::alignThrowOnOverflow(size_t value) {
    // this function might look big, but the total compiled code size is
    // one addition, one branch using a comparison and one bitwise and.
    const size_t decAlign = MemoryAlignment - 1; // compile time constant

    MEMT_ASSERT((MemoryAlignment & (decAlign)) == 0); // power of 2
      // This sum overflows if and only if rounding up overflows because
      // MemoryAlignment is a power of 2.
	const size_t sum = value + decAlign;
    if (sum < value)
      throw std::bad_alloc(); // overflow
    const size_t aligned = sum & (~decAlign);

    MEMT_ASSERT(aligned % MemoryAlignment == 0); // alignment
    MEMT_ASSERT(aligned >= value); // no overflow
    MEMT_ASSERT(aligned - value < MemoryAlignment); // adjustment minimal
    return aligned;
  }

  inline bool MemoryBlocks::Block::isInBlock(const void* ptr) const {
    // We use a trick to check this using one branch and two subtractions
    // instead of two branches.
    const char* p = static_cast<const char*>(ptr);
    const size_t offset = static_cast<size_t>(p - begin());
    // if _blockBegin > ptr then offset overflows to a large integer
    MEMT_ASSERT((offset < getBytesInBlock()) == (begin() <= p && p < end()));
    return offset < getBytesInBlock();
  }
}

#endif
