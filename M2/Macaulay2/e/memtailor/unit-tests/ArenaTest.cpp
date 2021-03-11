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
#include "memtailor/Arena.h"
#include <gtest/gtest.h>
#include <algorithm>
#include <sstream>

TEST(Arena, NoOp) {
  memt::Arena arena;
}

TEST(Arena, GetMemoryUsage) {
  memt::Arena arena;
  ASSERT_EQ(arena.getMemoryUse(), 0);
  arena.alloc(100);
  ASSERT_TRUE(arena.getMemoryUse() >= 100);
}

TEST(Arena, Big) {
  memt::Arena arena;
  void* a = arena.alloc(5);
  arena.freeTop(arena.alloc(1024 * 1024));
  arena.freeTop(arena.alloc(1024 * 1024));
  ASSERT_FALSE(arena.isEmpty());
  arena.freeTop(a);
  ASSERT_TRUE(arena.isEmpty());
}

TEST(Arena, Zero) {
  memt::Arena arena;
  void* a = arena.alloc(0);
  void* b = arena.alloc(0);
  void* c = arena.alloc(0);
  ASSERT_NE(a, b);
  ASSERT_NE(a, c);
  ASSERT_NE(b, c);
  ASSERT_FALSE(arena.isEmpty());
}

TEST(Arena, Many) {
  memt::Arena arena;
  std::vector<std::pair<char*, char*> > allocs;

  for (size_t i = 3; i < 10; ++i) {
    for (size_t size = 0; size < 100; ++size) {
      char* a = static_cast<char*>(arena.alloc(size));
      std::pair<char*, char*> p(a, a + size);
      for (size_t j = 0; j < allocs.size(); ++j) {
        std::pair<char*, char*> p2 = allocs[j];
        if (p.first <= p2.first)
          ASSERT_FALSE(p2.first < p.second);
        else
          ASSERT_FALSE(p.first < p2.second);
      }
      std::fill(p.first, p.second, static_cast<char>(-1));
      allocs.push_back(p);
    }

    while (allocs.size() > 10 * i) {
      arena.freeTop(allocs.back().first);
      allocs.pop_back();
    }

    arena.freeAndAllAfter(allocs[5 * i].first);
    allocs.resize(5 * i);
  }
  ASSERT_FALSE(arena.isEmpty());
  arena.freeAndAllAfter(allocs.front().first);
  ASSERT_TRUE(arena.isEmpty());
}

TEST(Arena, BigAndOverflow) {
  memt::Arena arena;
  // aligning size causes overflow
  ASSERT_THROW(arena.alloc(static_cast<size_t>(-1)), std::bad_alloc);

  // 2x size is an overflow
  ASSERT_THROW(arena.alloc(static_cast<size_t>(-1)/2 + 1), std::bad_alloc);

  // causes attempt at allocating almost the entire virtual memory space
  // which cannot succeed
  ASSERT_THROW(arena.alloc(static_cast<size_t>(-1)/2 - 100), std::bad_alloc);

  // sizeof(long) * x overflows to a smaller value (0).
  const size_t smallerOverflow = 1ul << (8*sizeof(long) - 1);
  MEMT_ASSERT(smallerOverflow > 0);
  //ASSERT(smallerOverflow * sizeof(long) == 0); compiler warning
  ASSERT_THROW(arena.allocArray<long>(smallerOverflow), std::bad_alloc);

  // sizeof(int) * x overflows to a greater value
  const size_t greaterOverflow = (~(0ul)) >> 1;
  MEMT_ASSERT(sizeof(long) >= 4);
  //ASSERT(greaterOverflow * sizeof(long) > greaterOverflow); compiler warning
  //ASSERT(greaterOverflow != (greaterOverflow * sizeof(long)) / sizeof(long));
  ASSERT_THROW(arena.allocArray<long>(greaterOverflow), std::bad_alloc);

  ASSERT_TRUE(arena.isEmpty());
}

// Make a class that keeps track of constructions and destructions in
// a global string. MAKE_HELPER will make a separate instantiation for
// every test so the tests are still independent in spite of the use
// of global variables. This is also thread safe as long as two
// threads are not running the same test at the same time.
namespace {
  template<class T, size_t ThrowAt>
  class _frobby_Helper {
  public:
    _frobby_Helper() {
      _id = ++_count;
      if (_id == ThrowAt) {
        _log << 'T' << _id;
        throw _id;
      } else
        _log << '+' << _id;
    }

    ~_frobby_Helper() {
      _log << '-' << _id;
    }

    void setId(size_t id) {_id = id;}

    static std::string resetLog() {
	  std::string log = _log.str();
	  _log.clear();
	  _log.str("");
	  _count = 0;
	  return log;
	}

  private:
    size_t _id;
    static size_t _count;
    static std::ostringstream _log;
  };

  template<class T, size_t ThrowAt>
  size_t _frobby_Helper<T, ThrowAt>::_count = 0;

  template<class T, size_t ThrowAt>
  std::ostringstream _frobby_Helper<T, ThrowAt>::_log;
}
#define MAKE_HELPER(NAME, THROW_AT)                                     \
  namespace {                                                           \
    struct _frobby_##NAME##HelperTag {};                                \
    typedef _frobby_Helper<_frobby_##NAME##HelperTag, THROW_AT> NAME##Helper; \
  }

MAKE_HELPER(ConDecon, 0)
TEST(Arena, ConDecon) {
  memt::Arena arena;
  arena.freeTopArray(arena.allocArray<ConDeconHelper>(0));
  arena.freeTopArray(arena.allocArray<ConDeconHelper>(3));
  arena.freeTopArray(arena.allocArray<ConDeconHelper>(0));
  ASSERT_EQ(ConDeconHelper::resetLog(), "+1+2+3-3-2-1");
  ASSERT_TRUE(arena.isEmpty());
}

MAKE_HELPER(ConExcep, 4)
TEST(Arena, ConExcep) {
  memt::Arena arena;
  ASSERT_THROW(arena.allocArray<ConExcepHelper>(10), size_t);
  ASSERT_EQ(ConExcepHelper::resetLog(), "+1+2+3T4-3-2-1");
  ASSERT_TRUE(arena.isEmpty());
}

MAKE_HELPER(NoConDecon, 0)
TEST(Arena, NoConDecon) {
  memt::Arena arena;
  std::pair<NoConDeconHelper*, NoConDeconHelper*> p =
    arena.allocArrayNoCon<NoConDeconHelper>(3);
  p.first[0].setId(1);
  p.first[1].setId(2);
  p.first[2].setId(3);
  arena.freeTopArray(p);

  ASSERT_EQ(NoConDeconHelper::resetLog(), "-3-2-1");
  ASSERT_TRUE(arena.isEmpty());
}

MAKE_HELPER(ConNoDecon, 0)
TEST(Arena, ConNoDecon) {
  memt::Arena arena;
  arena.freeTop(arena.allocArray<ConNoDeconHelper>(3).first);
  ASSERT_EQ(ConNoDeconHelper::resetLog(), "+1+2+3");
  ASSERT_TRUE(arena.isEmpty());
}

MAKE_HELPER(PtrNoConNoDecon, 0)
TEST(Arena, PtrNoConNoDecon) {
  memt::Arena arena;
  ASSERT_TRUE(arena.isEmpty());
  {
	memt::Arena::PtrNoConNoDecon<PtrNoConNoDeconHelper> ptr1(arena);
	ASSERT_FALSE(arena.isEmpty());
	memt::Arena::PtrNoConNoDecon<PtrNoConNoDeconHelper> ptr2(arena);
	memt::Arena::PtrNoConNoDecon<PtrNoConNoDeconHelper> ptr3(arena);
	memt::Arena::PtrNoConNoDecon<PtrNoConNoDeconHelper> ptr4(arena);
	ASSERT_FALSE(arena.isEmpty());
  }
  ASSERT_TRUE(arena.isEmpty());
  ASSERT_EQ(PtrNoConNoDeconHelper::resetLog(), "");
}

TEST(Arena, Guard) {
  memt::Arena arena;
  {
	memt::Arena::Guard noop(arena); // no-op guard
  }

  {
	// double no-op guard
	memt::Arena::Guard guard1(arena);
	memt::Arena::Guard guard2(arena);
  }


  // guard for empty arena with no allocations on it ever
  memt::Arena::Guard emptyGuard(arena);
  arena.freeTop(arena.alloc(10000));
  // guard for empty arena which has allocated some memory previously
  memt::Arena::Guard emptyGuard2(arena);
  // memory use is exponential in number of iterations so do not
  // increase too much.
  void* fromIteration3 = 0;
  for (size_t i = 0; i < 6u; ++i) {
	if (i == 3)
	  fromIteration3 = arena.alloc(1);
	{
	  memt::Arena::Guard noop(arena); // no-op guard
	}
	void* tmp;
	{
	  memt::Arena::Guard g1(arena);
	  {
		memt::Arena::Guard g2(arena);
		tmp = arena.alloc(1);
  	    ASSERT_TRUE(arena.fromArena(tmp));
		g2.release();
	  }
      ASSERT_TRUE(arena.fromArena(tmp)); // released guard did not free
	  memt::Arena::Guard g3(arena);
	  memt::Arena::Guard g4(arena);
	  memt::Arena::Guard g5(arena);
	  memt::Arena::Guard g6(arena);
	  g6.release();
	  g3.release();
	  ASSERT_TRUE(arena.fromArena(tmp));
	}
	ASSERT_FALSE(arena.fromArena(tmp)); // freed despite some guards freed
	// allocate enough memory to force a new block of backing memory
	// inside the arena.
	arena.alloc(arena.getMemoryUse());
  }
  // free a middle allocation to force some blocks to go away
  ASSERT_TRUE(fromIteration3 != 0);
  ASSERT_TRUE(arena.fromArena(fromIteration3));
  arena.freeAndAllAfter(fromIteration3);
}
