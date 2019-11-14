#include <memory>
#include <iostream>
#include "stdinc-m2.hpp"
#include "NCGroebner.hpp"

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


class ReductionQueueConfiguration1
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

std::unique_ptr<mathic::Geobucket<ReductionQueueConfiguration1>> makeQueue() //TODO: needs arguments
{
  ReductionQueueConfiguration1 C; //TODO: needs arguments
  
  return make_unique<mathic::Geobucket<ReductionQueueConfiguration1>>(C);
}


class PolynomialHeap1
{
public:
  PolynomialHeap1(const NCFreeAlgebra& F);

  // prevent copy and assignment constructors
  // allow move constructors, I guess?
  PolynomialHeap1 operator=(const PolynomialHeap1&) = delete;
  PolynomialHeap1(const PolynomialHeap1&) = delete;
  
  PolynomialHeap1& addPolynomial(ring_elem coeff,
                                 Word left,
                                 Word right,
                                 const Poly* poly);

  bool isZero();
  std::pair<ring_elem, Monom> viewLeadTerm();  // TODO: really want ConstMonom here...
  void removeLeadTerm(); // no-op if value is 0.

  // TODO: add in configuration code, and actual heap object.
private:
  // Monomial pool // TODO: make this into its own class.
  // Pool for Entry's.
};

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
