/* Frobby: Software for monomial ideal computations.
   Copyright (C) 2011 University of Aarhus
   Contact Bjarke Hammersholt Roune for license information (www.broune.com)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see http://www.gnu.org/licenses/.
*/
#include "memtailor/MemoryBlocks.h"
#include <gtest/gtest.h>
#include <vector>
#include <algorithm>

TEST(MemoryBlocks, NoOp) {
  memt::MemoryBlocks blocks;
  ASSERT_TRUE(blocks.getMemoryUse() == 0);
}

TEST(MemoryBlocks, MemoryUsage) {
  const size_t MaxOverhead =
    sizeof(memt::MemoryBlocks::Block) + (memt::MemoryAlignment - 1);
  memt::MemoryBlocks blocks;
  ASSERT_EQ(blocks.getMemoryUse(), 0);
  blocks.allocBlock(100);
  ASSERT_TRUE(blocks.getMemoryUse() >= 100);
  ASSERT_TRUE(blocks.getMemoryUse() <= 100 + MaxOverhead);

  blocks.allocBlock(200);
  blocks.allocBlock(400);
  ASSERT_TRUE(blocks.getMemoryUse() >= 700);
  ASSERT_TRUE(blocks.getMemoryUse() <= 700 + 3 * MaxOverhead);

  blocks.freeAllPreviousBlocks();
  ASSERT_TRUE(blocks.getMemoryUse() >= 400);
  ASSERT_TRUE(blocks.getMemoryUse() <= 400 + 3 * MaxOverhead);

  blocks.allocBlock(800);
  blocks.freeAllBlocks();
  ASSERT_EQ(blocks.getMemoryUse(), 0);
}

TEST(MemoryBlocks, NoLeak) {
  memt::MemoryBlocks blocks1;
  blocks1.allocBlock(100);

  memt::MemoryBlocks blocks2;
  blocks2.allocBlock(100);
  blocks2.allocBlock(1);
  blocks2.allocBlock(0);

  // if run with a leak detector, this should pick up a leak if
  // blocks are not freed automatically.
}

TEST(MemoryBlocks, Properties) {
  memt::MemoryBlocks blocks;
  ASSERT_TRUE(blocks.getFrontBlock().isNull());
  for (size_t i = 0; i <= 100; ++i) {
    memt::MemoryBlocks::Block& block = blocks.allocBlock(i);
    std::fill(block.begin(), block.end(), static_cast<char>(i));
    ASSERT_EQ(&block, &blocks.getFrontBlock());
    ASSERT_FALSE(block.isNull());
    ASSERT_EQ(block.position(), block.begin());
    ASSERT_EQ(block.getBytesInBlock(), i);
    ASSERT_EQ(block.getBytesToRight(), i);
    ASSERT_EQ(block.end(), block.begin() + i);
    ASSERT_TRUE(block.empty());

    block.setPosition(block.begin() + i / 2);
    if (i > 0) {
      ASSERT_EQ(block.position(), block.begin() + i / 2);
      if (i > 1) {
      ASSERT_FALSE(block.empty());
      }
      ASSERT_TRUE(block.isInBlock(block.begin()));
      ASSERT_TRUE(block.isInBlock(block.end() - 1));
    } else {
      ASSERT_EQ(block.begin(), block.end());
    }
    ASSERT_EQ(block.getBytesToRight(), i - i / 2);
    ASSERT_FALSE(block.isInBlock(block.end()));
    ASSERT_FALSE(block.isInBlock(block.begin() - 1));

    block.clear();
    ASSERT_EQ(block.position(), block.begin());
    ASSERT_TRUE(block.empty());
  }
  for (size_t i = 100; i > 0; --i) {
    ASSERT_TRUE(blocks.getFrontBlock().hasPreviousBlock());
    memt::MemoryBlocks::Block* previous =
	  blocks.getFrontBlock().previousBlock();
    for (const char* it = previous->begin(); it != previous->end(); ++it)
      ASSERT_EQ(*it, static_cast<char>(i - 1));
    ASSERT_EQ(previous->getBytesInBlock(), i - 1);
    blocks.freePreviousBlock();
  }
  ASSERT_FALSE(blocks.getFrontBlock().isNull());
  ASSERT_EQ(blocks.getFrontBlock().getBytesInBlock(), 100u);

  blocks.allocBlock(101);
  ASSERT_EQ(blocks.getFrontBlock().previousBlock()->getBytesInBlock(), 100u);
  blocks.allocBlock(102);
  ASSERT_EQ(blocks.getFrontBlock().previousBlock()->getBytesInBlock(), 101u);

  blocks.freeAllPreviousBlocks();
  ASSERT_FALSE(blocks.getFrontBlock().isNull());
  ASSERT_FALSE(blocks.getFrontBlock().hasPreviousBlock());
  ASSERT_EQ(blocks.getFrontBlock().getBytesInBlock(), 102u);

  blocks.allocBlock(103);
  blocks.freeAllBlocks();
  ASSERT_TRUE(blocks.getFrontBlock().isNull());
}
