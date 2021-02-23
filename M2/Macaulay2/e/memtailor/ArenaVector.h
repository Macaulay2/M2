/* Copyright (C) 2011 Bjarke Hammersholt Roune (www.broune.com)
   MemTailor is distributed under the Modified BSD License. See license.txt. */
#ifndef MEMT_ARENA_VECTOR_GUARD
#define MEMT_ARENA_VECTOR_GUARD

#include "stdinc.h"
#include "Arena.h"

namespace memt {
  /** Works as std::vector, except it cannot grow its capacity
      and the memory is taken from an Arena.

      Only frees its memory on destruction if FreeMemory
      is true. In that case that memory must be the top allocation
      on the Arena.

      Always calls the destructors of contained entries even
      when FreeMemory is false. */
  template<class T, bool FreeMemory>
  class ArenaVector {
  public:
    typedef T* iterator;
    typedef const T* const_iterator;
    typedef T& reference;
    typedef const T& const_reference;
    typedef T value_type;

    ArenaVector(Arena& arena, size_t capacity);
    ~ArenaVector() {
      clear();
      if (FreeMemory)
        _arena->freeTop(_begin);
    }

    bool empty() const {return _begin == _end;}
    size_t size() const {return _end - _begin;}
    T* begin() {return _begin;}
    const T* begin() const {return _begin;}
    T* end() {return _end;}
    const T* end() const {return _end;}

    void push_back(const T& t) {
      MEMT_ASSERT_NO_ASSUME(_end != _capacityEndDebug);
      new (_end) T(t);
      ++_end;
    }
    void pop_back() {
      MEMT_ASSERT(!empty());
      --_end;
      _end->~T();
    }
    void clear() {
      while (!empty())
        pop_back();
    }

  private:
    T* _begin;
    T* _end;
    Arena* _arena; /// @todo: only store if freeing memory
#ifdef MEMT_DEBUG
    T* _capacityEndDebug;
#endif
  };

  template<class T, bool DM>
  ArenaVector<T, DM>::ArenaVector(Arena& arena, size_t capacity) {
    _begin = arena.allocArrayNoCon<T>(capacity).first;
    _end = _begin;
    _arena = &arena;
#ifdef MEMT_DEBUG
    _capacityEndDebug = _begin + capacity;
#endif
  }
}

#endif
