// Copyright 2018  Michael E. Stillman

#ifndef _monomial_sets_hpp_
#define _monomial_sets_hpp_

/**
 * @file monomial-sets.hpp
 * @brief `MonomialSetFixedSize` / `MonomialSetVarSize` / `MonomialCollection*` / `MonomialMemorySpace` --- monomial-interning helpers.
 *
 * Declares a five-class kit for interning monomials stored as
 * contiguous `int` sequences. `MonomialMemorySpace` wraps a
 * `memt::Arena` (memtailor) and hands out bump-pointer ranges
 * via `alloc(int size)`; allocations can be popped LIFO
 * (`popLastAlloc`) or shrunk (`shrinkLastAlloc`), and the entire
 * arena is freed in one shot at destruction. The `Set`
 * variants --- `MonomialSetFixedSize` (constructor takes the
 * `int` size, **not** a `<NWords>` template parameter) and
 * `MonomialSetVarSize` (treats `*m` as a length prefix so the
 * monomial spans `[m, m + *m)`) --- wrap a
 * `std::unordered_set<const int*, ...>` and do no allocation
 * themselves; the caller owns the storage. The `Collection`
 * variants (`MonomialCollectionFixedSize` / `MonomialCollectionVarSize`)
 * compose a `Set` with their own `MonomialMemorySpace` and
 * implement the intern-or-pop pattern (`findOrInsert` copies
 * the monomial into the arena, looks it up, and pops the
 * allocation if a duplicate is already present).
 *
 * The included `MonomialHashAndEqFixedSize` /
 * `MonomialHashAndEqVarSize` functors implement equality
 * correctly via `std::equal`, but their `operator()` for
 * hashing is currently a `// TODO: do something good here.`
 * stub that returns `0` --- so the underlying
 * `std::unordered_set` degenerates to a single bucket and
 * comparisons fall through to the equality functor. This
 * matches the comment in `monomial-collection.hpp` ("improve
 * the hash function"); both files are mid-refactor scaffolding.
 * The only active consumer in the engine is
 * `schreyer-resolution/res-f4.hpp`, which holds a
 * `MonomialMemorySpace mMonomSpace2` (the arena alone --- it
 * does not use the `Set` / `Collection` classes here).
 *
 * @see monomial-collection.hpp
 * @see MemoryBlock.hpp
 */

// This file contains classes for keeping sets of monomials.
// There are two types of monomials:
//  a. fixed size (and that size is passed in as a parameter)
//  b. variable size, in which case the first int is the total length (so the range is [m, m + *m).
//
// Here are some assumptions about these monomials:
//  1. each monomial is a contiguous sequence of int's.
//  2. each monomial is hashed using all entries of the monomial.  Equality is equality of all elements.
//     (and size, if variable length).

// Features of these classes:


#include "memtailor.h"
#include <unordered_set>
#include <unordered_map>
#include <utility>
#include <cassert>

/**
 * @brief Bump-pointer arena for monomial storage, backed by a `memt::Arena`.
 *
 * @details Hands out contiguous `int` ranges via `alloc(size)` (returns
 * `[begin, end)`). Supports LIFO `popLastAlloc` and `shrinkLastAlloc`
 * so a monomial can be allocated at its maximum length, written in
 * place, then trimmed --- the pattern the variable-size `Collection`
 * classes use for intern-or-pop. The whole arena is reset with
 * `freeAllAllocs` or released to the OS with
 * `freeAllAllocsAndBackingMemory`.
 */
// MonomialMemorySpace:
//
class MonomialMemorySpace
{
public:
  MonomialMemorySpace() : mCount(0) {}

  // returns a range [begin, end), allocated in the block, such that end-begin == size.
  std::pair<int*,int*> alloc(int size) { mCount++; return mArena.allocArrayNoCon<int>(size); }

  int* allocate(int size) { auto result = alloc(size); return result.first; }
  
  // if m is the begin part of the last range allocated here, then this pops that.
  // Afterwards, one can pop the next, etc.  If they are popped out of order though,
  // undefined behavior results.
  void popLastAlloc(int* m) { mCount--; mArena.freeTop(m); }

  // If m is a pointer into the last monomial (i.e. >= a.first <= a.second), this resets the monomial
  // to be [a.first, m).  The public interface of memt::Arena should perhaps allow this use, but it does not.
  // However, the code works fine, unless debugging is turned on.  I might need to modify that code, and
  // document that this can be done.
  // Reasoning for wanting/using this: in the case when monomials are not fixed size, one can allocate
  // the maximum size it can be, compute it in place, then shrink the monomial.
  // UNSAFE.  Use of this function requires taking care.
  void shrinkLastAlloc(int* m) { mArena.freeTop(m); }
  
  size_t size() const { return mCount; }

  void freeAllAllocs() { mArena.freeAllAllocs(); }

  void freeAllAllocsAndBackingMemory() { mArena.freeAllAllocsAndBackingMemory(); }
private:
  size_t mCount;
  memt::Arena mArena;
};

/**
 * @brief Combined hash + equality functor for fixed-size monomials, plugged
 * into the `std::unordered_set` inside `MonomialSetFixedSize`.
 *
 * @details Equality is `std::equal` over `mMonomialSize` ints; the hash is a
 * `TODO` stub that returns 0 (so the underlying hash set degenerates
 * to a single bucket and falls back on the equality comparator).
 */
class MonomialHashAndEqFixedSize
{
public:
  MonomialHashAndEqFixedSize(int size) : mMonomialSize(size) {}

  // hash function
  // TODO: do something good here.
  std::size_t operator()(const int* m) const
  {
    (void) m;
    return 0;
  }

  bool operator() (const int* a, const int * b) const
  {
    return std::equal(a, a+mMonomialSize, b);
    //    for (int i=0; i<mMonomialSize; i++)
    //      if (*a++ != *b++) return false;
    //    return true;
  }

private:
  int mMonomialSize;
};

/**
 * @brief Combined hash + equality functor for variable-size monomials, plugged
 * into the `std::unordered_set` inside `MonomialSetVarSize`.
 *
 * @details Reads the leading int of each monomial as its length, then compares
 * the two ranges with `std::equal`. The hash is the same `TODO` stub
 * that returns 0 as in the fixed-size variant.
 */
class MonomialHashAndEqVarSize
{
public:
  MonomialHashAndEqVarSize() {}

  // hash function
  // TODO: do something good here.
  std::size_t operator()(const int* m) const
  {
    (void) m;
    return 0;
  }

  bool operator() (const int* a, const int * b) const
  {
    if (*a != *b) return false;
    return std::equal(a, a + *a, b);
  }
};

/// A set of monomials, all of the same fixed size.
/// This class assumes that a monomial is a contiguous sequence
/// of ints, the corresponding range (for a monomial m) is [m, m + elementSize())
/// Additionally, this class does no allocation of monomials.
class MonomialSetFixedSize
{
public:
  MonomialSetFixedSize(int monomialSize)
    : mMonomialSize(monomialSize),
      mMonomialHashAndEq(monomialSize),
      mHash(128, mMonomialHashAndEq, mMonomialHashAndEq)
  {
  }

  /// size of each monomial
  int elementSize() const { return mMonomialSize; }

  /// number of (unique) monomials in this set.
  std::size_t numElements() const { return mHash.size(); }

  /// monom should be the begin of an allocated and set range of ints [m, m + mMonomialSize).
  /// returned value is either 'monom' (if not present in the set), or the corresponding pointer
  /// in the set, whose contents are the same as those of monom.
  std::pair<const int*, bool> findOrInsert(const int* monom)
  {
    auto result = mHash.insert(monom);
    return std::make_pair(* result.first, result.second);
  }

  /// returns nullptr if monom is not present, otherwise returns the pointer equivalent to 'monom'.
  const int* find(const int* monom) const
  {
    auto result = mHash.find(monom); // result is an iterator
    bool found = result != mHash.end();
    if (found)
      return *result;
    else
      return nullptr;
  }
private:
  int mMonomialSize;
  MonomialHashAndEqFixedSize mMonomialHashAndEq;
  std::unordered_set<const int*, MonomialHashAndEqFixedSize, MonomialHashAndEqFixedSize> mHash;
};

/**
 * @brief Hash set of interned variable-size monomials --- the variable-length
 * counterpart of `MonomialSetFixedSize`.
 *
 * @details Each monomial is a contiguous range `[m, m + *m)` where the leading
 * int is the length, so the set itself stores only `const int*`
 * pointers; the storage is owned by the caller (typically a
 * `MonomialMemorySpace`). `findOrInsert` returns the canonical
 * pointer plus a flag indicating whether a new entry was created.
 */
class MonomialSetVarSize
{
public:
  MonomialSetVarSize()
    : mHash(128, mMonomialHashAndEq, mMonomialHashAndEq)
  {
  }

  /// number of (unique) monomials in this set.
  std::size_t numElements() const { return mHash.size(); }

  /// monom should be the begin of an allocated and set range of ints [m, m + mMonomialSize).
  /// returned value is either 'monom' (if not present in the set), or the corresponding pointer
  /// in the set, whose contents are the same as those of monom.
  std::pair<const int*, bool> findOrInsert(const int* monom)
  {
    auto result = mHash.insert(monom);
    return std::make_pair(* result.first, result.second);
  }

  /// returns nullptr if monom is not present, otherwise returns the pointer equivalent to 'monom'.
  const int* find(const int* monom) const
  {
    auto result = mHash.find(monom); // result is an iterator
    bool found = result != mHash.end();
    if (found)
      return *result;
    else
      return nullptr;
  }
private:
  MonomialHashAndEqVarSize mMonomialHashAndEq;
  std::unordered_set<const int*, MonomialHashAndEqVarSize, MonomialHashAndEqVarSize> mHash;
};

/**
 * @brief Interning collection that pairs a `MonomialSetFixedSize` with its own
 * `MonomialMemorySpace` to own monomial storage.
 *
 * @details `findOrInsert(monom)` copies the caller's monomial into the arena,
 * tries to insert the new pointer into the set, and pops the
 * allocation back off the arena if a duplicate was already present
 * (the "intern-or-pop" pattern). `monomialMemorySpace()` exposes the
 * arena so callers that allocate first and then insert can do so
 * without an extra copy.
 */
class MonomialCollectionFixedSize
{
public:
  MonomialCollectionFixedSize(int monomialSize)
    : mSet(monomialSize)
  {
  }

  // There are two ways to insert monomials.
  // 1. the monomial was just allocated on monomialMemorySpace().
  // 2. the momomial exists outside of this memory block.
  // In the latter case, the monomial will be copied to the block.
  //
  // Another way is to insert a monomial via a transformer function.  TODO.
  MonomialMemorySpace& monomialMemorySpace() { return mMonomialMemory; }

  int elementSize() const { return mSet.elementSize(); }
  
  // Number of (different) monomials in this set
  std::size_t size() const { return mMonomialMemory.size(); }

  // insert: two versions: depending on whether we need to copy the pointer.
  // decide if monom is in the set: if so, (equiv monom, false) is returned.
  // If it is not in the set, it is copied into the memory block, and (equiv monom, true) is returned.
  // Note: the only pointers this function returns (as the first part of the pair) are pointers into
  // the memory block.
  std::pair<const int*, bool> findOrInsert(const int* monom)
  {
    std::pair<int*, int*> mon { mMonomialMemory.alloc(elementSize()) };
    std::copy(monom, monom + elementSize(), mon.first);
    assert(mon.second - mon.first >= elementSize());
    return findOrInsertTopInternedMonomial(mon.first);
  }

  // assumption: 'monom' is currently the top (last) monomial on mMonomialMemory.
  // If the monomial is found in the set, then monom is popped off the memory block.
  std::pair<const int*, bool> findOrInsertTopInternedMonomial(int*& monom)
  {
    // if the monomial is found in the set, then monom is popped
    auto result = mSet.findOrInsert(monom);
    if (not result.second) // i.e. monomial was not needed.
      mMonomialMemory.popLastAlloc(monom);
    return result;
  }

  // find: this returns either: (nullptr,false), if not found, or the (canonicalized pointer, true).
  const int* find(const int* monom) const
  {
    return mSet.find(monom);
  }

  // way to loop through all such monomials

  // way to sort them?
  
private:
  MonomialSetFixedSize mSet;
  MonomialMemorySpace mMonomialMemory;
};


/**
 * @brief Variable-size counterpart of `MonomialCollectionFixedSize`: pairs a
 * `MonomialSetVarSize` with its own `MonomialMemorySpace`.
 *
 * @details `findOrInsert(monom)` allocates `*monom` ints in the arena, copies
 * the monomial in, attempts to intern the pointer, and pops on
 * duplicate. Same intern-or-pop pattern as the fixed-size variant
 * with the length read from the monomial's first int.
 */
class MonomialCollectionVarSize
{
public:
  MonomialCollectionVarSize() {}

  // There are two ways to insert monomials.
  // 1. the monomial was just allocated on monomialMemorySpace().
  // 2. the momomial exists outside of this memory block.
  // In the latter case, the monomial will be copied to the block.
  //
  // Another way is to insert a monomial via a transformer function.  TODO.

  MonomialMemorySpace& monomialMemorySpace() { return mMonomialMemory; }

  // Number of (different) monomials in this set
  std::size_t size() const { return mMonomialMemory.size(); }

  // insert: two versions: depending on whether we need to copy the pointer.
  // decide if monom is in the set: if so, (equiv monom, false) is returned.
  // If it is not in the set, it is copied into the memory block, and (equiv monom, true) is returned.
  // Note: the only pointers this function returns (as the first part of the pair) are pointers into
  // the memory block.
  std::pair<const int*, bool> findOrInsert(const int* monom)
  {
    std::pair<int*, int*> mon { mMonomialMemory.alloc(*monom) };
    std::copy(monom, monom + *monom, mon.first);
    assert(mon.second - mon.first >= *monom);
    return findOrInsertTopInternedMonomial(mon.first);
  }

  // assumption: 'monom' is currently the top (last) monomial on mMonomialMemory.
  // If the monomial is found in the set, then monom is popped off the memory block.
  std::pair<const int*, bool> findOrInsertTopInternedMonomial(int*& monom)
  {
    // if the monomial is found in the set, then monom is popped
    auto result = mSet.findOrInsert(monom);
    if (not result.second) // i.e. monomial was not needed.
      mMonomialMemory.popLastAlloc(monom);
    return result;
  }

  // find: this returns either: (nullptr,false), if not found, or the (canonicalized pointer, true).
  const int* find(const int* monom) const
  {
    return mSet.find(monom);
  }

  // way to loop through all such monomials

  // way to sort them?

private:
  MonomialSetVarSize mSet;
  MonomialMemorySpace mMonomialMemory;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

