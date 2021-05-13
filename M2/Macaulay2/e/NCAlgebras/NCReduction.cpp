#include "NCAlgebras/NCReduction.hpp"

#include "NCAlgebras/FreeAlgebra.hpp"  // for FreeAlgebra
#include "NCAlgebras/FreeMonoid.hpp"   // for MonomEq, FreeMonoid
#include "MemoryBlock.hpp"             // for MemoryBlock
#include "NCAlgebras/NCGroebner.hpp"   // for tryOutMathicCode
#include "NCAlgebras/Word.hpp"         // for Word
#include "myalloc.hpp"                 // for StatsAllocator
#include "ring.hpp"                    // for Ring
#include "style.hpp"                   // for EQ, LT, GT

#include <cassert>                     // for assert
#include <mathic/Geobucket.h>          // for Geobucket, GeoStoreSameSizeBuffer
#include <mathic/Heap.h>               // for Heap
#include <mathic/TourTree.h>           // for TourTree
#include <algorithm>                   // for copy
#include <iostream>                    // for string, operator<<, endl, basi...
#include <map>                         // for map, __map_iterator, operator!=
#include <memory>                      // for unique_ptr
#include <queue>                       // for priority_queue
#include <type_traits>                 // for swap
#include <vector>                      // for vector

#if 0
Reasoning about using these structures in noncomm reduction.
Possible ways:
1. Entry = [ringelem, monomial]
  Questions: how is the monomial represented?
             where is the allocated space for it?
  are they uniquely represented i.e. are we using deduplicate, hashtable, etc?

  Monomial Pool
  Entry = pointer to [ringelem, monomial pointer]
  deduplicate: write this.
  cmpLessThan: monomial comparison call.

  Monomial Pool
  Hash Table of monomials.  Monomial pointers are unique.
    Pointer value could contain space for the ringelem.
  Entry = pointer to [ringelem, monomial pointer]
  
2. Entry = PolyWithPosition
  QueueConfiguration could own these polynomials (i.e. the Queue itself could).
    we might need to write the 'value' function: keep popping off lead term, append to a poly.

    monomial pool: use memtailor to generate this space.
      make a new monomial in the pool.
    PolynomialWithPosition pool. [leadmonomial, coeff, lmon, rmon, index into the poly we are using, which poly]
    F - aGb - cGd - ...
#endif

class OurQueueConfiguration
{
public:
  using Entry = int; // think of each Entry as a monomial.
  enum class CompareResult {LT, EQ, GT};
  CompareResult compare(const Entry& a, const Entry& b) const
  {
    if (a < b) return CompareResult::LT;
    if (a > b) return CompareResult::GT;
    return CompareResult::EQ;
  }
  bool cmpLessThan(CompareResult a) const { return a == CompareResult::LT; }

  // Specific for Geobucket
  const size_t minBucketSize = 2;
  const size_t geoBase = 4;
  static const size_t insertFactor = 4;

  static const bool supportDeduplication = false;
  bool cmpEqual(CompareResult a) const; // no implementation needed
  Entry deduplicate(Entry a, Entry b) const; // no implementation needed

  static const bool minBucketBinarySearch = false;
  static const bool trackFront = true;
  static const bool premerge = false;
  static const bool collectMax = true;

  static const mathic::GeobucketBucketStorage bucketStorage = mathic::GeoStoreSameSizeBuffer;
};

class OurQueueConfiguration1
{
public:
  using Entry = int; // think of each Entry as a monomial.
  enum class CompareResult {LT, EQ, GT};
  CompareResult compare(const Entry& a, const Entry& b) const
  {
    if (a < b) return CompareResult::LT;
    if (a > b) return CompareResult::GT;
    return CompareResult::EQ;
  }
  bool cmpLessThan(CompareResult a) const { return a == CompareResult::LT; }

  // Specific for Geobucket
  const size_t minBucketSize = 2;
  const size_t geoBase = 4;
  static const size_t insertFactor = 4;

  static const bool supportDeduplication = true;

  bool cmpEqual(CompareResult a) const { return a == CompareResult::EQ; }

  Entry deduplicate(Entry a, Entry b) const { return a + b + 1000; }

  static const bool minBucketBinarySearch = false;
  static const bool trackFront = true;
  static const bool premerge = false;
  static const bool collectMax = true;

  static const mathic::GeobucketBucketStorage bucketStorage = mathic::GeoStoreSameSizeBuffer;
};

std::unique_ptr<mathic::Geobucket<OurQueueConfiguration>> makeQueue()
{
  OurQueueConfiguration C;
  
  return std::make_unique<mathic::Geobucket<OurQueueConfiguration>>(C);
}

std::unique_ptr<mathic::Geobucket<OurQueueConfiguration1>> makeQueue1()
{
  OurQueueConfiguration1 C;
  
  return std::make_unique<mathic::Geobucket<OurQueueConfiguration1>>(C);
}

class TrivialPolynomialHeap : public PolynomialHeap
{
public:  
  TrivialPolynomialHeap(const FreeAlgebra& F)
    : mRing(F),
      mValue{},
      mIter(mValue.cbegin())
  {
  }

  virtual ~TrivialPolynomialHeap() {}

  void clear() override { mRing.setZero(mValue); mIter = mValue.cbegin(); }
    
  // prevent copy and assignment constructors
  // allow move constructors, I guess?
  TrivialPolynomialHeap operator=(const TrivialPolynomialHeap&) = delete;
  TrivialPolynomialHeap(const TrivialPolynomialHeap&) = delete;

  PolynomialHeap& addPolynomial(const Poly& poly) override
  {
    Poly g;
    mRing.add(g, mIter, mValue.cend(), poly.cbegin(), poly.cend());
    std::swap(g, mValue);
    mIter = mValue.cbegin();
    return *this;
  }

  TrivialPolynomialHeap& addPolynomial(ring_elem coeff,
                                 Word left,
                                 Word right,
                                 const Poly& poly) override
  {
    mRing.setZero(f);
    mRing.setZero(g);
    // Create f = coeff * left * poly * right;
    mRing.mult_by_term_left_and_right(f, poly, coeff, left, right);
    mRing.add(g, mIter, mValue.cend(), f.cbegin(), f.cend());
    std::swap(g, mValue);
    mIter = mValue.cbegin();
    return *this;
  }

  bool isZero() override
  {
    return mIter == mValue.cend();
  }
  
  std::pair<Monom, ring_elem> viewLeadTerm() override
  {
    return std::make_pair(mIter.monom(), mIter.coeff());
  }

  void removeLeadTerm() override
  {
    assert(mIter != mValue.cend());
    ++mIter;
  }

  Poly* value() override
  {
    auto result = new Poly;
    mRing.copy(*result, mIter, mValue.cend());
    return result;
  }

  size_t getMemoryUsedInBytes() override
  {
    return mValue.numTerms();
  }

  std::string getName() const override { return std::string("Trivial Heap"); }
  
private:
  FreeAlgebra mRing;
  Poly mValue;
  Poly::const_iterator mIter;
  Poly f; // tmp values.  Remember to zero them out before use.
  Poly g;
};

////////////////////////////////////
// NaivePolynomialHeap /////////////
////////////////////////////////////

class NaiveQueueConfiguration
{
public:
  NaiveQueueConfiguration(const FreeAlgebra& F) : mRing(F) {}

  using Entry = std::pair<Monom, ring_elem>;
  
  enum class CompareResult {LT, EQ, GT, Error};
  CompareResult compare(const Entry& a, const Entry& b) const
  {
    int cmp = mRing.monoid().compare(a.first, b.first);
    if (cmp == LT) return CompareResult::LT;
    if (cmp == GT) return CompareResult::GT;
    if (cmp == EQ) return CompareResult::EQ;
    
    std::cout << "Unexpected monomial comparison error in heap." << std::endl << std::flush;
    return CompareResult::Error;
  }

  bool cmpLessThan(CompareResult a) const { return a == CompareResult::LT; }

  // Specific for Geobucket
  const size_t minBucketSize = 2;
  const size_t geoBase = 4;
  static const size_t insertFactor = 4;

  // specific for Heap
  static const bool fastIndex = false;
  // if set to true, a faster way of calculating indices is used
  // but for this to work, sizeof(Entry) must be a power of two (which it
  // should already be, since both Monom and ring_elem are really pointers?
  
  static const bool supportDeduplication = false;
  bool cmpEqual(CompareResult a) const; // no implementation needed
  Entry deduplicate(Entry a, Entry b) const; // no implementation needed

  static const bool minBucketBinarySearch = true;
  static const bool trackFront = true;
  static const bool premerge = false;
  static const bool collectMax = true;

  static const mathic::GeobucketBucketStorage bucketStorage = mathic::GeoStoreSameSizeBuffer;
private:
  const FreeAlgebra& mRing;
};

class NaiveDedupQueueConfiguration
{
public:
  NaiveDedupQueueConfiguration(const FreeAlgebra& F) : mRing(F) {}

  using Entry = std::pair<Monom, ring_elem>;
  
  enum class CompareResult {LT, EQ, GT, Error};
  CompareResult compare(const Entry& a, const Entry& b) const
  {
    int cmp = mRing.monoid().compare(a.first, b.first);
    if (cmp == LT) return CompareResult::LT;
    if (cmp == GT) return CompareResult::GT;
    if (cmp == EQ) return CompareResult::EQ;
    
    std::cout << "Unexpected monomial comparison error in heap." << std::endl << std::flush;
    return CompareResult::Error;
  }
  bool cmpLessThan(CompareResult a) const { return a == CompareResult::LT; }

  // Specific for Geobucket
  const size_t minBucketSize = 2;
  const size_t geoBase = 4;
  static const size_t insertFactor = 4;

  // specific for Heap
  static const bool fastIndex = false;
  // if set to true, a faster way of calculating indices is used
  // but for this to work, sizeof(Entry) must be a power of two (which it
  // should already be, since both Monom and ring_elem are really pointers?
  
  static const bool supportDeduplication = true;
  bool cmpEqual(CompareResult a) const { return a == CompareResult::EQ; }
  Entry deduplicate(Entry a, Entry b) const
  {
    ring_elem c = mRing.coefficientRing()->add(a.second, b.second);
    return Entry(a.first, c);
  }

  static const bool minBucketBinarySearch = true;
  static const bool trackFront = true;
  static const bool premerge = false;
  static const bool collectMax = true;

  static const mathic::GeobucketBucketStorage bucketStorage = mathic::GeoStoreSameSizeBuffer;
private:
  const FreeAlgebra& mRing;
};

template<template<typename> class Queue>
class NaivePolynomialHeap : public PolynomialHeap
{
public:
  using Entry = NaiveQueueConfiguration::Entry;

  NaivePolynomialHeap(const FreeAlgebra& F)
    : mRing(F),
      mQueue(NaiveQueueConfiguration(F)),
      mLeadTermSet(false),
      mLeadTerm(Monom(), F.coefficientRing()->zero())
  {
  }

  virtual ~NaivePolynomialHeap() {}

  void clear() override {
    // clear the heap.  The free algebra is kept the same
    // but all other aspects are reset.  The MonomialSpace
    // has all its data freed to the arena, but is available for
    // use without any new allocations for the next computation.
    mLeadTermSet = false;
    mQueue.clear();
    mMonomialSpace.deallocateAll();
  }
  
  // prevent copy and assignment constructors
  // allow move constructors, I guess?
  NaivePolynomialHeap operator=(const NaivePolynomialHeap&) = delete;
  NaivePolynomialHeap(const NaivePolynomialHeap&) = delete;

  NaivePolynomialHeap& addPolynomial(const Poly& poly) override
  {
    if (mLeadTermSet)
      {
        mQueue.push(mLeadTerm);
        mLeadTermSet = false;
      }
    for (auto i = poly.cbegin(); i != poly.cend(); ++i)
      {
        auto rg = mMonomialSpace.allocateArray<int>(i.monom().size());
        std::copy(i.monom().begin(), i.monom().end(), rg.first);
        mQueue.push(Entry(Monom(rg.first), i.coeff()));
      }
    return *this;
  }

  NaivePolynomialHeap& addPolynomial(ring_elem coeff,
                                 Word left,
                                 Word right,
                                const Poly& poly) override
  {
    Poly f;
    mRing.mult_by_term_left_and_right(f, poly, coeff, left, right);
    addPolynomial(f);
    return *this;
  }
    
  bool isZero() override
  {
    if (mLeadTermSet) return false;
    if (mQueue.empty()) return true;
    Entry lt = mQueue.pop();
    while (not mQueue.empty())
      {
        Entry e = mQueue.top();
        // this is bad - compare should not be used to check equality
        if (mRing.monoid().compare(e.first, lt.first) == EQ)
          {
            lt.second = mRing.coefficientRing()->add(lt.second, e.second);
            mQueue.pop();
          }
        else
          {
            if (not mRing.coefficientRing()->is_zero(lt.second))
              {
                mLeadTermSet = true;
                mLeadTerm = lt;
                return false;
              }
            else
              lt = mQueue.pop();
          }
      }
    if (not mRing.coefficientRing()->is_zero(lt.second))
      {
        mLeadTermSet = true;
        mLeadTerm = lt;
        return false;
      }
    return true;
  }
  
  std::pair<Monom, ring_elem> viewLeadTerm() override
  {
    if (isZero())
      {
        std::cout << "viewLeadTerm called without checking if polynomial is zero" << std::endl;
        assert(false);
      }
    assert(mLeadTermSet);
    return mLeadTerm;
  }

  void removeLeadTerm() override
  {
    if (isZero()) return;
    assert(mLeadTermSet); // should only be called if mLeadTermSet is true.
    if (mLeadTermSet)
      mLeadTermSet = false;
  }

  Poly* value() override
  {
    Poly* f = new Poly;
    if (mLeadTermSet)
      mQueue.push(mLeadTerm);
    mLeadTermSet = false;
    while (not isZero())
      {
        auto tm = viewLeadTerm();
        mRing.add_to_end(*f, tm.second, tm.first);
        removeLeadTerm();
      }
    addPolynomial(*f);
    return f;
  }

  size_t getMemoryUsedInBytes() override
  {
    return mQueue.getMemoryUse() + mMonomialSpace.getMemoryUsedInBytes();
  }

  std::string getName() const override { return mQueue.getName(); }
  
private:
  FreeAlgebra mRing;
  Queue<NaiveQueueConfiguration> mQueue;
  MemoryBlock mMonomialSpace;
  bool mLeadTermSet; // true means mLeadTerm is set, to a non-zero value.
  std::pair<Monom, ring_elem> mLeadTerm;
};


template<template<typename> class Queue>
class NaiveDedupPolynomialHeap : public PolynomialHeap
{
public:
  using Entry = NaiveDedupQueueConfiguration::Entry;

  NaiveDedupPolynomialHeap(const FreeAlgebra& F)
    : mRing(F),
      mQueue(NaiveDedupQueueConfiguration(F))
  {
  }

  virtual ~NaiveDedupPolynomialHeap() {}

  void clear() override {
    // clear the heap.  The free algebra is kept the same
    // but all other aspects are reset.  The MonomialSpace
    // has all its data freed to the arena, but is available for
    // use without any new allocations for the next computation.
    mQueue.clear();
    mMonomialSpace.deallocateAll();
  }
  
  // prevent copy and assignment constructors
  // allow move constructors, I guess?
  NaiveDedupPolynomialHeap operator=(const NaiveDedupPolynomialHeap&) = delete;
  NaiveDedupPolynomialHeap(const NaiveDedupPolynomialHeap&) = delete;

  NaiveDedupPolynomialHeap& addPolynomial(const Poly& poly) override
  {
    for (auto i = poly.cbegin(); i != poly.cend(); ++i)
      {
        auto rg = mMonomialSpace.allocateArray<int>(i.monom().size());
        std::copy(i.monom().begin(), i.monom().end(), rg.first);
        mQueue.push(Entry(Monom(rg.first), i.coeff()));
      }
    return *this;
  }

  NaiveDedupPolynomialHeap& addPolynomial(ring_elem coeff,
                                 Word left,
                                 Word right,
                                const Poly& poly) override
  {
    Poly f;
    mRing.mult_by_term_left_and_right(f, poly, coeff, left, right);
    addPolynomial(f);
    return *this;
  }
    
  bool isZero() override
  {
    // idempotent function.
    while (not mQueue.empty())
      {
        Entry e = mQueue.top();
        if (mRing.coefficientRing()->is_zero(e.second))
          mQueue.pop();
        else
          return false;
      }
    return true;
  }
  
  std::pair<Monom, ring_elem> viewLeadTerm() override
  {
    return mQueue.top();
  }

  void removeLeadTerm() override
  {
    mQueue.pop();
  }

  Poly* value() override
  {
    Poly* f = new Poly;
    while (not isZero())
      {
        auto tm = viewLeadTerm();
        mRing.add_to_end(*f, tm.second, tm.first);
        removeLeadTerm();
      }
    addPolynomial(*f);
    return f;
  }

  size_t getMemoryUsedInBytes() override
  {
    return mQueue.getMemoryUse() + mMonomialSpace.getMemoryUsedInBytes();
  }

  std::string getName() const override { return mQueue.getName(); }
  
private:
  FreeAlgebra mRing;
  Queue<NaiveDedupQueueConfiguration> mQueue;
  MemoryBlock mMonomialSpace;
};


class MapPolynomialHeap : public PolynomialHeap
{
public:
  using Entry = std::pair<Monom, ring_elem>;
  using ConstEntry = std::pair<const Monom, ring_elem>;

  MapPolynomialHeap(const FreeAlgebra& F)
    : mRing(F),
      mMonomEq(F.monoid()),
      mMap(mMonomEq)
  {
  }

  virtual ~MapPolynomialHeap() {}

  void clear() override {
    // clear the heap.  The free algebra is kept the same
    // but all other aspects are reset.  The MonomialSpace
    // has all its data freed to the arena, but is available for
    // use without any new allocations for the next computation.
    mMap.clear();
    mMonomialSpace.deallocateAll();
  }
  
  // prevent copy and assignment constructors
  // allow move constructors, I guess?
  MapPolynomialHeap operator=(const MapPolynomialHeap&) = delete;
  MapPolynomialHeap(const MapPolynomialHeap&) = delete;

  void addTerm(Entry tm)
  {
    auto result = mMap.find(tm.first);
    bool found = (result != mMap.end());
    if (found)
      {
        result->second = mRing.coefficientRing()->add(tm.second, result->second);
      }
    else
      {
        // need to allocate new term, and insert into hash table and the queue.
        auto rg = mMonomialSpace.allocateArray<int>(tm.first.size());
        std::copy(tm.first.begin(), tm.first.end(), rg.first);
        mMap.insert({Monom(rg.first), tm.second});
      }
  }
  
  MapPolynomialHeap& addPolynomial(const Poly& poly) override
  {
    for (auto i = poly.cbegin(); i != poly.cend(); ++i)
      {
        addTerm(Entry(i.monom(), i.coeff()));
      }
    return *this;
  }

  MapPolynomialHeap& addPolynomial(ring_elem coeff,
                                 Word left,
                                 Word right,
                                const Poly& poly) override
  {
    Poly f;
    mRing.mult_by_term_left_and_right(f, poly, coeff, left, right);
    addPolynomial(f);
    return *this;
  }
    
  bool isZero() override
  {
    // idempotent function.
    while (not mMap.empty())
      {
        const Entry& e = *(mMap.begin());
        if (mRing.coefficientRing()->is_zero(e.second))
          mMap.erase(mMap.begin());
        else
          return false;
      }
    return true;
  }
  
  Entry viewLeadTerm() override
  {
    return * (mMap.begin());
  }

  void removeLeadTerm() override
  {
    mMap.erase(mMap.begin());
  }

  Poly* value() override
  {
    Poly* f = new Poly;
    while (not isZero())
      {
        auto tm = viewLeadTerm();
        mRing.add_to_end(*f, tm.second, tm.first);
        removeLeadTerm();
      }
    addPolynomial(*f);
    return f;
  }

  size_t getMemoryUsedInBytes() override
  {
    return 0;
  }

  std::string getName() const override { return "map heap"; }
  
private:
  FreeAlgebra mRing;
  MonomEq mMonomEq;
  //std::map<Monom, ring_elem, MonomEq, StatsAllocator<ConstEntry>> mMap;
  std::map<Monom, ring_elem, MonomEq, gc_allocator<ConstEntry>> mMap;
  MemoryBlock mMonomialSpace;
};

#if 0
class HashedConfiguration
{
public:
 HashedConfiguration(const FreeAlgebra& F) : mRing(F) {}

  using Entry = Monom;
  using Term = Entry *;

  // Members needed for Geobucket, TourTree and Heap data structures.
  enum class CompareResult {LT, EQ, GT, Error};
  CompareResult compare(const Entry& a, const Entry& b) const
  {
    int cmp = mRing.monoid().compare(a->second, b->second);
    if (cmp == LT) return CompareResult::LT;
    if (cmp == GT) return CompareResult::GT;
    if (cmp == EQ) return CompareResult::EQ;
    
    std::cout << "Unexpected monomial comparison error in heap." << std::endl << std::flush;
    return CompareResult::Error;
  }

  bool cmpLessThan(CompareResult a) const { return a == CompareResult::LT; }

  // Specific for Geobucket
  const size_t minBucketSize = 2;
  const size_t geoBase = 4;
  static const size_t insertFactor = 4;

  // specific for Heap
  static const bool fastIndex = false;
  // if set to true, a faster way of calculating indices is used
  // but for this to work, sizeof(Entry) must be a power of two (which it
  // should already be, since both Monom and ring_elem are really pointers?
  
  static const bool supportDeduplication = false;
  bool cmpEqual(CompareResult a) const; // no implementation needed
  Entry deduplicate(Entry a, Entry b) const; // no implementation needed

  static const bool minBucketBinarySearch = true;
  static const bool trackFront = true;
  static const bool premerge = false;
  static const bool collectMax = true;

  static const mathic::GeobucketBucketStorage bucketStorage = mathic::GeoStoreSameSizeBuffer;

   // Members needed for unordered_set
  size_t operator()(const Term& term) const // hash value
  {
    return 0;
  }
  bool operator()(Term& term1, Term& term2) const   // check for equality
  {
    return compare(&term1, &term2) == CompareResult::EQ;
  }
private:
  const FreeAlgebra& mRing;
};

template<template<typename> class Queue>
class HashedPolynomialHeap : public PolynomialHeap
{
public:
  using Term = HashedConfiguration::Term;
  using Entry = HashedConfiguration::Entry;

  HashedPolynomialHeap(const FreeAlgebra& F)
    : mRing(F),
      mConfig(HashedConfiguration(F)),
      mQueue(mConfig),
      mHash(1024, mConfig, mConfig)
  {
  }

  virtual ~HashedPolynomialHeap() {}
  
  // prevent copy and assignment constructors
  // allow move constructors, I guess?
  HashedPolynomialHeap operator=(const HashedPolynomialHeap&) = delete;
  HashedPolynomialHeap(const HashedPolynomialHeap&) = delete;

  HashedPolynomialHeap& addTerm(Term tm)
  {
    auto result = mHash.find(tm);
    bool found = (result != mHash.end());
    if (found)
      {
        const_cast<ring_elem&>(result->first) = mRing.coefficientRing()->add(tm.first, result->first);
      }
    else
      {
        // need to allocate new term, and insert into hash table and the queue.
        auto rg = mMonomialSpace.allocateArray<int>(tm.second.size());
        std::copy(tm.second.begin(), tm.second.end(), rg.first);
        mQueue.push(&*(inserted.first));
        auto inserted = mHash.insert(Term(tm.first, rg.first)); // TODO: add in assert check that this insert succeeds
      }
    return *this;
  }
  
  HashedPolynomialHeap& addPolynomial(const Poly& poly) override
  {
    for (auto i = poly.cbegin(); i != poly.cend(); ++i)
      addTerm(Term(i.coeff(), i.monom()));
    return *this;
  }

  HashedPolynomialHeap& addPolynomial(ring_elem coeff,
                                 Word left,
                                 Word right,
                                const Poly& poly) override
  {
    Poly f;
    mRing.mult_by_term_left_and_right(f, poly, coeff, left, right);
    addPolynomial(f);
    return *this;
  }
    
  bool isZero() override
  {
    // idempotent function.
    while (not mQueue.empty())
      {
        Entry e = mQueue.top();
        if (mRing.coefficientRing()->is_zero(e->first))
          mQueue.pop();
        else
          return false;
      }
    return true;
  }
  
  std::pair<ring_elem, Monom> viewLeadTerm() override
  {
    return * mQueue.top();
    // loop
    // look at lead element in queue
    // if coeff is 0, pop it, else continue
  }

  void removeLeadTerm() override
  {
    mQueue.pop();
  }

  Poly* value() override
  {
    Poly* f = new Poly;
    while (not isZero())
      {
        auto tm = viewLeadTerm();
        mRing.add_to_end(*f, tm.first, tm.second);
        removeLeadTerm();
      }
    addPolynomial(*f);
    return f;
  }

  size_t getMemoryUsedInBytes() override
  {
    return mQueue.getMemoryUse() + mMonomialSpace.getMemoryUsedInBytes();
  }

  std::string getName() const override { return mQueue.getName(); }
  
private:
  FreeAlgebra mRing;
  HashedConfiguration mConfig;
  Queue<HashedConfiguration> mQueue;
  std::unordered_map<Monom, ring_elem, HashedConfiguration, HashedConfiguration> mHash;
  MemoryBlock mMonomialSpace;
};
#endif

class EntryConfig
{
public:

  using Entry = std::pair<Monom,ring_elem>;
    
  EntryConfig(const FreeMonoid& M) : mMonoid(M) {}

  size_t operator()(Monom m) const // hash function
  {
    return 0; 
  }

  bool operator() (Entry a, Entry b) const
  {
    // TODO: Need to be clear which one wants!
    //return mMonoid.compare(a.first, b.first) == GT;
    return mMonoid.compare(a.first, b.first) == LT;
  }
  
private:
  const FreeMonoid& mMonoid;
};

class PriorityQueuePolynomialHeap : public PolynomialHeap
{
public:
  using Entry = std::pair<Monom, ring_elem>;
  using Container = std::vector<Entry, StatsAllocator<Entry>>;

  PriorityQueuePolynomialHeap(const FreeAlgebra& F)
    : mRing(F),
      mEntryConfig(F.monoid()),
      mQueue(mEntryConfig),
      mLeadTermSet(false),
      mLeadTerm(Monom(), F.coefficientRing()->zero())
  {
  }

  virtual ~PriorityQueuePolynomialHeap() {}

  void clear() override {
    // clear the heap.  The free algebra is kept the same
    // but all other aspects are reset.  The MonomialSpace
    // has all its data freed to the arena, but is available for
    // use without any new allocations for the next computation.
    mLeadTermSet = false;
    while (!mQueue.empty())
      mQueue.pop();
    mMonomialSpace.deallocateAll();
  }
  
  // prevent copy and assignment constructors
  // allow move constructors, I guess?
  PriorityQueuePolynomialHeap operator=(const PriorityQueuePolynomialHeap&) = delete;
  PriorityQueuePolynomialHeap(const PriorityQueuePolynomialHeap&) = delete;

  PriorityQueuePolynomialHeap& addEntry(const Entry& entry)
  {
    auto rg = mMonomialSpace.allocateArray<int>(entry.first.size());
    std::copy(entry.first.begin(), entry.first.end(), rg.first);
    mQueue.push(Entry(Monom(rg.first), entry.second));
    return *this;
  }

  PriorityQueuePolynomialHeap& addPolynomial(const Poly& poly) override
  {
    if (mLeadTermSet)
      {
        mQueue.push(mLeadTerm);
        mLeadTermSet = false;
      }
    for (auto i = poly.cbegin(); i != poly.cend(); ++i)
      addEntry(Entry(i.monom(),i.coeff()));
    return *this;
  }

  PriorityQueuePolynomialHeap& addPolynomial(ring_elem coeff,
                                 Word left,
                                 Word right,
                                const Poly& poly) override
  {
    
    Poly f;
    mRing.mult_by_term_left_and_right(f, poly, coeff, left, right);
    addPolynomial(f);
    return *this;
  }
    
  bool isZero() override
  {
    if (mLeadTermSet) return false;
    if (mQueue.empty()) return true;
    Entry lt = mQueue.top();
    mQueue.pop();
    while (not mQueue.empty())
      {
        Entry e = mQueue.top();
        if (mRing.monoid().compare(e.first, lt.first) == EQ)
          {
            lt.second = mRing.coefficientRing()->add(lt.second, e.second);
            mQueue.pop();
          }
        else
          {
            if (not mRing.coefficientRing()->is_zero(lt.second))
              {
                mLeadTermSet = true;
                mLeadTerm = lt;
                return false;
              }
            else
              {
                lt = mQueue.top();
                mQueue.pop();
              }
          }
      }
    if (not mRing.coefficientRing()->is_zero(lt.second))
      {
        mLeadTermSet = true;
        mLeadTerm = lt;
        return false;
      }
    return true;
  }
  
  std::pair<Monom, ring_elem> viewLeadTerm() override
  {
    if (isZero())
      {
        std::cout << "viewLeadTerm called without checking if polynomial is zero" << std::endl;
        assert(false);
      }
    assert(mLeadTermSet);
    return mLeadTerm;
  }

  void removeLeadTerm() override
  {
    if (isZero()) return;
    assert(mLeadTermSet); // should only be called if mLeadTermSet is true.
    if (mLeadTermSet)
      mLeadTermSet = false;
  }

  Poly* value() override
  {
    Poly* f = new Poly;
    if (mLeadTermSet)
      mQueue.push(mLeadTerm);
    mLeadTermSet = false;
    while (not isZero())
      {
        auto tm = viewLeadTerm();
        mRing.add_to_end(*f, tm.second, tm.first);
        removeLeadTerm();
      }
    addPolynomial(*f);
    return f;
  }

  size_t getMemoryUsedInBytes() override
  {
    return (mQueue.size()*sizeof(Entry)) + mMonomialSpace.getMemoryUsedInBytes();
  }

  std::string getName() const override { return "Priority queue heap."; }
  
private:
  FreeAlgebra mRing;
  EntryConfig mEntryConfig;
  std::priority_queue<Entry,Container,EntryConfig> mQueue;
  MemoryBlock mMonomialSpace;
  bool mLeadTermSet; // true means mLeadTerm is set, to a non-zero value.
  Entry mLeadTerm;
};




HeapType getHeapType(int strategy)
{
  // The first 3 bits of strategy encode which heap type to use.
  switch (strategy & 7) {
  case 0: return HeapType::Map;  // good one
  case 1: return HeapType::PriorityQueue; // good one
  case 2: return HeapType::Trivial;
  case 3: return HeapType::NaiveDedupGeobucket;
  case 4: return HeapType::NaiveGeobucket;
  case 5: return HeapType::NaiveTourTree;
  case 6: return HeapType::NaiveHeap;
  default: return HeapType::Map;
  }
}

std::string getHeapName(HeapType type)
{
  switch (type) {
  case HeapType::Map:
    return "Map";
  case HeapType::PriorityQueue:
    return "PriorityQueue";
  case HeapType::Trivial:
    return "Trivial";
  case HeapType::NaiveDedupGeobucket:
    return "NaiveDedupGeobucket";
  case HeapType::NaiveGeobucket:
    return "NaiveGeobucket";
  case HeapType::NaiveTourTree:
    return "NaiveTourTree";
  case HeapType::NaiveHeap:
    return "NaiveHeap";
  default: return "Map"; // see above.
    //  case HeapType::HashedGeobucket:
    //    return "HashedGeobucket";
  };
}

std::unique_ptr<PolynomialHeap>
makePolynomialHeap(HeapType type, const FreeAlgebra& F)
{
  switch (type) {
    //  case HeapType::HashedGeobucket:
    //    return std::make_unique<HashedPolynomialHeap<mathic::Geobucket>>(F);
  case HeapType::Map:
    return std::make_unique<MapPolynomialHeap>(F);
  case HeapType::PriorityQueue:
    return std::make_unique<PriorityQueuePolynomialHeap>(F);
  case HeapType::Trivial:
    return std::make_unique<TrivialPolynomialHeap>(F);
  case HeapType::NaiveDedupGeobucket:
    return std::make_unique<NaiveDedupPolynomialHeap<mathic::Geobucket>>(F);
  case HeapType::NaiveGeobucket:
    return std::make_unique<NaivePolynomialHeap<mathic::Geobucket>>(F);
  case HeapType::NaiveTourTree:
    return std::make_unique<NaivePolynomialHeap<mathic::TourTree>>(F);
  case HeapType::NaiveHeap:
    return std::make_unique<NaivePolynomialHeap<mathic::Heap>>(F);
  };
  return nullptr;
}

void tryOutMathicCode()
{
  std::cout << "trying out mathic code!" << std::endl;

  auto Q = makeQueue();
  Q->push(3);
  Q->push(7);
  Q->push(2);
  Q->push(42);
  Q->push(7);
  while (not Q->empty())
    {
      int a = Q->pop();
      std::cout << "popped: " << a << std::endl;
    }


  std::cout << "trying out mathic code, part 2!" << std::endl;

  auto Q1 = makeQueue1();
  Q1->push(3);
  Q1->push(7);
  Q1->push(2);
  Q1->push(42);
  Q1->push(7);
  while (not Q1->empty())
    {
      int a = Q1->pop();
      std::cout << "popped: " << a << std::endl;
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
