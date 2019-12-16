#include <memory>
#include <iostream>
#include "stdinc-m2.hpp"
#include "NCGroebner.hpp"
#include "NCReduction.hpp"

#include <memtailor.h>
#include <mathic/TourTree.h>
#include <mathic/Geobucket.h>
#include <mathic/Heap.h>

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
  Hash Table of monomials.  Monomial pointers are unqiue.
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
private:
  memt::Arena mArena;
};
  
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
  
  return make_unique<mathic::Geobucket<OurQueueConfiguration>>(C);
}

std::unique_ptr<mathic::Geobucket<OurQueueConfiguration1>> makeQueue1()
{
  OurQueueConfiguration1 C;
  
  return make_unique<mathic::Geobucket<OurQueueConfiguration1>>(C);
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
  
  std::pair<ring_elem, Monom> viewLeadTerm() override
  {
    return std::make_pair(mIter.coeff(), mIter.monom());
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
    mValue.numTerms();
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

  using Entry = std::pair<ring_elem, Monom>;
  
  enum class CompareResult {LT, EQ, GT, Error};
  CompareResult compare(const Entry& a, const Entry& b) const
  {
    int cmp = mRing.monoid().compare(a.second, b.second);
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

  using Entry = std::pair<ring_elem, Monom>;
  
  enum class CompareResult {LT, EQ, GT, Error};
  CompareResult compare(const Entry& a, const Entry& b) const
  {
    int cmp = mRing.monoid().compare(a.second, b.second);
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
    ring_elem c = mRing.coefficientRing()->add(a.first, b.first);
    return Entry(c, a.second);
  }

  static const bool minBucketBinarySearch = true;
  static const bool trackFront = true;
  static const bool premerge = false;
  static const bool collectMax = true;

  static const mathic::GeobucketBucketStorage bucketStorage = mathic::GeoStoreSameSizeBuffer;
private:
  const FreeAlgebra& mRing;
};

template<template<typename> typename Queue>
class NaivePolynomialHeap : public PolynomialHeap
{
public:
  using Entry = NaiveQueueConfiguration::Entry;

  NaivePolynomialHeap(const FreeAlgebra& F)
    : mRing(F),
      mQueue(NaiveQueueConfiguration(F)),
      mLeadTermSet(false),
      mLeadTerm(F.coefficientRing()->zero(), Monom())
  {
  }

  virtual ~NaivePolynomialHeap() {}
  
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
        mQueue.push(Entry(i.coeff(), Monom(rg.first)));
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
        if (mRing.monoid().compare(e.second, lt.second) == EQ)
          {
            lt.first = mRing.coefficientRing()->add(lt.first, e.first);
            mQueue.pop();
          }
        else
          {
            if (not mRing.coefficientRing()->is_zero(lt.first))
              {
                mLeadTermSet = true;
                mLeadTerm = lt;
                return false;
              }
            else
              lt = mQueue.pop();
          }
      }
    if (not mRing.coefficientRing()->is_zero(lt.first))
      {
        mLeadTermSet = true;
        mLeadTerm = lt;
        return false;
      }
    return true;
  }
  
  std::pair<ring_elem, Monom> viewLeadTerm() override
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
  Queue<NaiveQueueConfiguration> mQueue;
  MemoryBlock mMonomialSpace;
  bool mLeadTermSet; // true means mLeadTerm is set, to a non-zero value.
  std::pair<ring_elem, Monom> mLeadTerm;
};


template<template<typename> typename Queue>
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
        mQueue.push(Entry(i.coeff(), Monom(rg.first)));
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
        if (mRing.coefficientRing()->is_zero(e.first))
          mQueue.pop();
        else
          return false;
      }
    return true;
  }
  
  std::pair<ring_elem, Monom> viewLeadTerm() override
  {
    return mQueue.top();
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
  Queue<NaiveDedupQueueConfiguration> mQueue;
  MemoryBlock mMonomialSpace;
};


bool testMemoryBlock()
{
  MemoryBlock B;
  for (size_t i = 0; i < 1000; ++i)
    {
      size_t sz = 4 + (32343 * i) % 10;
      auto range = B.allocateArray<int>(sz);
      for (int j = 0; j < sz; j++)
        range.first[j] = 100 * i + j;
      if (i % 93 == 0)
        {
          range = B.shrinkLastAllocate(range.first, range.second, range.first + 4);
          for (int j = 0; j < 4; j++)
            range.first[j] = 100 * i + j;
        }
      if ((range.second - range.first != sz) and (range.second - range.first != 4))
        return false;
      //      std::cout << "i = " << i << " sz = " << sz << " elems = ";
      //      for (int* a = range.first; a != range.second; ++a)
      //        std::cout << *a << " ";
      //      std::cout << std::endl << "memory usage: " << B.getMemoryUsedInBytes() << std::endl;
    }
  return true;
}

std::unique_ptr<PolynomialHeap>
makePolynomialHeap(HeapTypes type, const FreeAlgebra& F)
{
  if (type == HeapTypes::Trivial)
    return make_unique<TrivialPolynomialHeap>(F);
  if (type == HeapTypes::NaiveDedupGeobucket)
    return make_unique<NaiveDedupPolynomialHeap<mathic::Geobucket>>(F);
  if (type == HeapTypes::NaiveGeobucket)
    return make_unique<NaivePolynomialHeap<mathic::Geobucket>>(F);
  //  if (type == HeapTypes::NaiveTourTree)
  //    return make_unique<NaivePolynomialHeap<mathic::TourTree>>(F);
  //  if (type == HeapTypes::NaiveHeap)
  //     return make_unique<NaivePolynomialHeap<mathic::Heap>>(F);
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
